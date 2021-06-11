
# X_Testing Environmental Drivers ####

library(RColorBrewer); library(ggregplot); library(tidyverse); library(colorspace);
library(GGally); library(cowplot); library(INLA); library(patchwork); library(magrittr)
library(fs)

theme_set(theme_cowplot() + theme(strip.background = element_rect(fill = "white")))

dir_create("Output Files")

# 1	Social Models 	####

# Do individuals reduce their contacts as they age? ####

Resps = c("GroupSize")

Covar <- c("ReprodStatus", "Month", #"Age"
           "Year", "WindSpeedAvg", "AirTempAvg", #"NObs", 
           "PopN")

FullCovar <- c(Covar, "Age")

Cols <- c("Name", 
          Resps, 
          FullCovar,
          #"Year", #"PopN", 
          "Easting", "Northing")

TestCensuses <- Censuses %>%  
  mutate(Name = Code) %>% 
  left_join(PopSize, 
            by = c("Year" = "DeerYear")) %>% 
  left_join(PopSize %>% mutate_at("DeerYear", ~.x + 1), 
            by = c("Year" = "DeerYear"), suffix = c("", ".t0")) %>% 
  left_join(LBS[,c("Code", "LRS","LBS")], by = c("Name" = "Code"))


HindStatus %>% 
  dplyr::select(Female, ReprodStatus, DeerYear, Calf, CumulativeReprod, CumulativeWinter) %>% 
  mutate_at("ReprodStatus", ~.x %>% str_trim %>% 
              str_replace_all(c(" " = ".",
                                "Naïve" = "Naive",
                                "yeld" = "Yeld"))) %>% 
  left_join(TestCensuses, ., by = c("DeerYear", "Name" = "Female")) -> TestCensuses

TestCensuses %<>%
  mutate(Status = 
           
           case_when(
             
             Age == 0 ~ "Calf",
             Age == 1 ~ "Yearling", 
             Age == 2 ~ "2Y",
             Age>2 & Sex == "M" ~ "Stag", 
             TRUE ~ as.character(ReprodStatus)
             
           )
  )

ReprodReplace <- c("None", "None", "Summer", "Winter", "Winter")

names(ReprodReplace) <- c("Naive", "True.Yeld", "Summer.Yeld", "Winter.Yeld", "Milk")

TestCensuses %<>% mutate_at("ReprodStatus", ~.x %>% 
                              str_replace_all(c(ReprodReplace)))

TestCensuses %<>% separate(Date, sep = "/", into = c("Day", "Month", "Year"))

# Adding weather ####

WeatherHourly <- readxl::read_xlsx("Data/Weather/tblWeatherHourly.xlsx")

DailyWeather <- WeatherHourly %>% 
  mutate(Date = TimeStamp %>% str_split(" ") %>% map_chr(first)) %>% 
  group_by(Date) %>% summarise_if(is.numeric, ~mean(.x, na.rm = T))

DailyWeather %<>% separate(Date, "-", into = c("Year", "Month", "Day"))

TestCensuses %>% 
  left_join(DailyWeather, by = c("Year", "Month", "Day")) %>% 
  mutate(Date = lubridate::ymd(paste0(Year, Month, Day, sep = "-"))) -> TestCensuses

# Making into testable dataset ####

TestHinds <- TestCensuses %>% 
  filter(Sex == "F", Age>4) %>% 
  #filter(MeshInclude == 1) %>%
  dplyr::select(Cols %>% setdiff("2WindSpeedAvg"), Date) %>%
  mutate(
    #Degree = as.vector(scale(sqrt(Degree))),
    #Strength = as.vector(scale(sqrt(Strength))),
    GroupSize = as.vector((log(GroupSize))),
    fYear = as.factor(Year),
    Year = as.numeric(Year)
  )  %>% 
  na.omit() %>% 
  droplevels %>%
  as.data.frame()

TestHinds %>% nrow %>% print

TestHinds <- 
  TestHinds %>% group_by(Name, Date) %>% 
  mutate(N = 1:n()) %>% 
  filter(N == 1) %>% 
  ungroup

TestHinds %>% nrow %>% print

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","Easting","Northing"))

TestHinds[,paste0(Resps, ".Original")] <- TestHinds[,Resps]

TestHinds[,paste0(ToScale, ".Original")] <- TestHinds[,ToScale]

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

SpocialList <- list()

a <- 1

IM1 <- INLAModelAdd(
  
  Response = Resps[a],
  Data = TestHinds,
  Explanatory = FullCovar %>% c("Age:WindSpeedAvg","Age:AirTempAvg"),
  # Add = c("Age:WindSpeedAvg","Age:AirTempAvg"),
  #AllModels = T,
  Random = c("Name", "fYear"), 
  RandomModel = rep("iid", 2),
  Family = "gaussian", 
  AddSpatial = F, Coordinates = c("Easting", "Northing")
  
)

a <- Resps[1]

print(a)

TestHinds <- IM1$Data
SocMesh <- IM1$Spatial$Mesh

YSD <- 
  TestHinds[,paste0(a, ".Original")] %>% sd

YMean <- 
  TestHinds[,paste0(a, ".Original")] %>% mean

XSD <- 
  TestHinds[,paste0("AirTempAvg", ".Original")] %>% sd

XMean <- 
  TestHinds[,paste0("AirTempAvg", ".Original")] %>% mean

X = seq(min(TestHinds$AirTempAvg), 
        max(TestHinds$AirTempAvg), 
        length.out = nrow(TestHinds)) %>% c

IM1$FinalModel %>% 
  INLAFit(TestHinds, Covar, NDraw = 100, Draw = T) %>% map_dbl(mean) -> Intercepts

IM1$FinalModel %>% 
  GetEstimates("AirTempAvg", NDraw = 100, Draws = T) -> Slopes

B = mean(Slopes)
A = mean(Intercepts)
Y = A + B*X

1:length(Slopes) %>% map(~data.frame(X = X,
                                     Y = X*Slopes[[.x]] + Intercepts[[.x]]) %>% 
                           slice(1, n())) -> SlopeDF

SlopeDF %<>% bind_rows(.id = "Rep")

FitLine <- data.frame(
  AirTempAvg = rep(X, 1),
  Y = c(Y),
  Model = rep(c("Base"), length(X))
)

TestHinds$Y <- TestHinds[,a]

Est <- (IM1$FinalModel %>% 
          #GetEstimates(Variable = "Age") %>%
          GetEstimates(Variable = "AirTempAvg", Mode = "Numeric") %>% 
          multiply_by(YSD) %>% divide_by(XSD) %>% unlist %>% 
          round(3)) 

paste0(Est[[1]], " (", Est[[2]], ",", Est[[3]], ")") %>% 
  paste0("; P < ", IM1$FinalModel %>% INLAPValue("AirTempAvg")) -> 
  LabelList

LabelYList <- max(TestHinds$Y + diff(range(TestHinds$Y))/10)

LabelXList <- median(X)

LabelDf <- data.frame(
  
  AirTempAvg = LabelXList,
  Y = LabelYList,
  Label = LabelList
  
)

TestHinds %>% 
  RandomSlice %>% mutate(Group = factor(1:n())) %>%
  ggplot(aes(AirTempAvg, exp(Y/YSD + YMean) - 1)) + 
  geom_point(alpha = 0.05, aes(colour = Group)) + 
  geom_line(alpha = 0.05, data = SlopeDF, aes(X, group = Rep)) +
  scale_colour_discrete_sequential(palette = "Blues 3", 
                                   guide = F) +
  geom_line(data = FitLine, size = 1)  +
  labs(x = "Average temperature", y = "Group size") +
  # geom_text(data = LabelDf, aes(label = Label), hjust = 0.5,
  #           colour = AlberColours[[1]], size = 3) +
  scale_x_continuous(breaks = sort(unique(TestHinds$Age))[1:5*5-4], 
                     labels = c(1:5)*5) +
  scale_y_continuous(breaks = c(0:10*10)) + 
  ggsave("TempFigure.jpeg", units = "mm", height = 100, width = 100)


 # Adding the G1/G2 stuff ####

TestHinds <- TestCensuses %>% 
  filter(Sex == "F", Age>4) %>% 
  #filter(MeshInclude == 1) %>%
  dplyr::select(Cols %>% setdiff("2WindSpeedAvg"), GrazeType, Date) %>%
  mutate(
    #Degree = as.vector(scale(sqrt(Degree))),
    #Strength = as.vector(scale(sqrt(Strength))),
    Month = as.factor(Month),
    GroupSize = as.vector(scale(log(GroupSize))),
    fYear = as.factor(Year),
    Year = as.numeric(Year)
  )  %>% 
  filter(GrazeType %in% c("G1", "G2", "CT")) %>% 
  na.omit() %>% 
  droplevels %>%
  as.data.frame()

TestHinds %<>% mutate(G1 = as.numeric(str_detect(GrazeType, "G1")))

# TestHinds %<>% mutate(G1 = GrazeType)

TestHinds %>% nrow %>% print

TestHinds <- 
  TestHinds %>% group_by(Name, Date) %>% 
  mutate(N = 1:n()) %>% 
  filter(N == 1) %>% 
  ungroup

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","Easting","Northing"))

TestHinds[,paste0(Resps, ".Original")] <- TestHinds[,Resps]

TestHinds[,paste0(ToScale, ".Original")] <- TestHinds[,ToScale]

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

SpocialList <- list()

a <- 1

IM1 <- INLAModelAdd(
  
  Response = Resps[a],
  Data = TestHinds,
  Explanatory = (FullCovar %>% c(., "G1", "G1:Age", "G1:Age:Month")) %>% 
    setdiff(c("WindSpeedAvg", "AirTempAvg")),
  # Add = c("Graze:Age", "Graze:Age:Month"),
  #AllModels = T,
  Random = c("Name", "fYear"), 
  RandomModel = rep("iid", 2),
  Family = "gaussian", 
  AddSpatial = T, Coordinates = c("Easting", "Northing")
  
)

IM1$FinalModel %>% Efxplot +
  IM1$Spatial$Model %>% Efxplot

ggField(IM1$Spatial$Model, IM1$Spatial$Mesh) + scale_fill_discrete_sequential(AlberPalettes[[1]])

SpocialList[[Resps[a]]] <- IM1
