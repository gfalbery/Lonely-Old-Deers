
# 3_Social Senescence Figures ####

{
  
  library(RColorBrewer); library(ggregplot); library(tidyverse); library(colorspace);
  library(GGally); library(cowplot); library(INLA); library(patchwork); library(magrittr)
  library(MCMCglmm); library(magrittr); library(scales); library(fs)
  
  theme_set(theme_cowplot() + 
              theme(strip.background = element_rect(fill = "white", colour = "dark grey")))
  
  AlberPalettes <- c("YlGnBu","Reds","BuPu", "PiYG")
  AlberColours <- sapply(AlberPalettes, function(a) RColorBrewer::brewer.pal(5, a)[4])
  AlberColours[length(AlberColours)+1:2] <- RColorBrewer::brewer.pal(11, AlberPalettes[[4]])[c(2,10)]
  
  AlberColours %<>% c(Pink = "#FD6396", Blue = "#3C78D8")
  
  Resps = c("GroupSize", "Degree", "Strength")
  
  RespLabels = c("Group Size", "Degree", "Strength")
  names(RespLabels) <- Resps
  
  dir_create("Figures")
  
}

# Figure 1: Social effects ####

SpocialList <- readRDS("Output Files/SocialModels1.rds")
SocialModels3 <- readRDS("Output Files/SocialModels2.rds")

SpocialData <- SpocialList %>% map("Data")

Covar <- c("ReprodStatus", "Age",
           "Year", "PopN", "NObs")

a <- Resps[1]

Figure1aList <- LabelYList <- 
  LabelList <- LabelXList <- 
  LabelDfList <- FitLineList <- SlopeDFList <- 
  # TransList <- UnTransList <- TransTransList <-
  YMeanList <- YSDList <- 
  list()

for(a in Resps){
  
  print(a)
  
  TestHinds <- SpocialData[[a]]
  SocMesh <- SpocialList[[a]]$Spatial$Mesh
  
  YSD <- 
    TestHinds[,paste0(a, ".Original")] %>% sd
  
  YMean <- 
    TestHinds[,paste0(a, ".Original")] %>% mean
  
  XSD <- 
    TestHinds[,paste0("Age", ".Original")] %>% sd
  
  XMean <- 
    TestHinds[,paste0("Age", ".Original")] %>% mean
  
  X = seq(min(TestHinds$Age), 
          max(TestHinds$Age), 
          length.out = nrow(TestHinds)) %>% c
  
  B2 = summary(SpocialList[[a]]$Spatial$Model)$fixed["Age","mean"]
  A2 = summary(SpocialList[[a]]$Spatial$Model)$fixed[,"mean"]%*%t(SpocialList[[a]]$Spatial$Model$model.matrix) %>% mean
  Y2 = A2 + B2*X
  
  SpocialList[[a]]$FinalModel %>% 
    INLAFit(TestHinds, Covar, NDraw = 100, Draw = T) %>% map_dbl(mean) -> Intercepts
  
  SpocialList[[a]]$FinalModel %>% 
    GetEstimates("Age", NDraw = 100, Draws = T) -> Slopes
  
  B = mean(Slopes)
  A = mean(Intercepts)
  Y = A + B*X
  
  1:length(Slopes) %>% map(~data.frame(X = X,
                                       Y = X*Slopes[[.x]] + Intercepts[[.x]]) %>% 
                             slice(1, n())) -> SlopeDF
  
  SlopeDFList[[a]] <- SlopeDF %<>% bind_rows(.id = "Rep")
  
  FitLineList[[a]] <- 
    FitLine <- 
    data.frame(
      Age = rep(X, 2),
      Y = c(Y, Y2),
      Model = rep(c("Base","SPDE"), each = length(X))
    )
  
  TestHinds$Y <- c(scale(TestHinds[,a]))
  
  Est <- (SpocialList[[a]]$FinalModel %>% 
            GetEstimates(Variable = "Age", Mode = "Numeric") %>% 
            multiply_by(YSD) %>% divide_by(XSD) %>% unlist %>% 
            round(3)) 
  
  paste0(Est[[1]], " (", Est[[2]], ",", Est[[3]], ")") %>% 
    paste0("; P < ", SpocialList[[a]]$FinalModel %>% INLAPValue("Age")) -> 
    LabelList[[a]]
  
  LabelYList[[a]] <- max(TestHinds$Y) + diff(range(TestHinds$Y))/10
  
  LabelXList[[a]] <- median(X)
  
  LabelDfList[[a]] <- data.frame(
    
    Age = LabelXList[[a]],
    Y = LabelYList[[a]],
    Label = LabelList[[a]]
    
  )
  
}

Figure1aList[[1]] <- 
  TestHinds %>% 
  RandomSlice %>% mutate(Group = factor(1:n())) %>%
  ggplot(aes(Age, (Y + TestHinds[,"GroupSize.Original"] %>% mean/
                     TestHinds[,paste0("GroupSize.Original")] %>% sd)*
               TestHinds[,paste0("GroupSize.Original")] %>% sd)) + 
  geom_point(alpha = 0.1, aes(colour = Group, 
                              y = GroupSize)) + 
  geom_line(alpha = 0.05, data = SlopeDFList[[1]], aes(X, group = Rep)) +
  scale_colour_discrete_sequential(palette = "Blues 3", 
                                   guide = F) +
  geom_line(data = FitLineList[[1]], aes(lty = Model, group = Model), size = 1)  +
  labs(y = RespLabels[[1]], x = "Age") +
  geom_text(data = LabelDfList[[1]], aes(label = Label), hjust = 0.5,
            colour = AlberColours[[1]], size = 2.7) +
  scale_x_continuous(breaks = sort(unique(TestHinds$Age))[1:5*5-4], 
                     labels = c(1:5)*5) +
  lims(y = c(0, NA))

Figure1aList[[2]] <- 
  TestHinds %>% 
  RandomSlice %>% mutate(Group = factor(1:n())) %>%
  ggplot(aes(Age, (Y + TestHinds[,"Degree.Original"] %>% mean/
                     TestHinds[,paste0("Degree.Original")] %>% sd)*
               TestHinds[,paste0("Degree.Original")] %>% sd)) + 
  geom_point(alpha = 0.1, aes(colour = Group, 
                              y = Degree)) + 
  geom_line(alpha = 0.05, data = SlopeDFList[[2]], aes(X, group = Rep)) +
  scale_colour_discrete_sequential(palette = "Blues 3", 
                                   guide = F) +
  geom_line(data = FitLineList[[2]], aes(lty = Model, group = Model), size = 1)  +
  labs(y = RespLabels[[2]], x = "Age") +
  geom_text(data = LabelDfList[[2]], aes(label = Label), hjust = 0.5,
            colour = AlberColours[[1]], size = 2.7) +
  scale_x_continuous(breaks = sort(unique(TestHinds$Age))[1:5*5-4], 
                     labels = c(1:5)*5) +
  lims(y = c(0, NA))

Figure1aList[[3]] <- 
  TestHinds %>% 
  RandomSlice %>% mutate(Group = factor(1:n())) %>%
  ggplot(aes(Age, (Y + TestHinds[,"Strength.Original"] %>% mean/
                     TestHinds[,paste0("Strength.Original")] %>% sd)*
               TestHinds[,paste0("Strength.Original")] %>% sd)) + 
  geom_point(alpha = 0.1, aes(colour = Group, 
                              y = Strength.Original)) + 
  geom_line(alpha = 0.05, data = SlopeDFList[[3]], aes(X, group = Rep)) +
  scale_colour_discrete_sequential(palette = "Blues 3", 
                                   guide = F) +
  geom_line(data = FitLineList[[3]], aes(lty = Model, group = Model), size = 1)  +
  labs(y = RespLabels[[3]], x = "Age") +
  geom_text(data = LabelDfList[[3]], aes(label = Label), hjust = 0.5,
            colour = AlberColours[[1]], size = 2.7) +
  scale_x_continuous(breaks = sort(unique(TestHinds$Age))[1:5*5-4], 
                     labels = c(1:5)*5) +
  lims(y = c(0, NA))

Resps %>% lapply(function(b){
  
  names(SocialModels3[[b]]) <- c("Base", "+ ID", "+ Longevity", "+ SPDE")
  
  SocialModels3[[b]] %>% map("summary.fixed") %>% lapply(function(a){
    
    a %>% rownames_to_column("Explanatory") %>% 
      rename(Lower = `0.025quant`, Upper = `0.975quant`) %>% 
      dplyr::select(Explanatory, Mean = mean, Lower, Upper) %>% 
      filter(Explanatory %in% c("Age", "Longevity"))
    
  }) %>% bind_rows(.id = "Model")
}) %>% bind_rows(.id = "Response") %>% 
  mutate_at("Response", ~RespLabels[as.numeric(.x)] %>% factor(levels = RespLabels)) %>%
  mutate_at("Model", ~factor(.x, levels = c("Base", "+ ID", "+ Longevity", "+ SPDE"))) %>% 
  ggplot(aes(Explanatory, Mean, colour = Model, group = Model)) + facet_grid(~Response, scales = "free") +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  geom_point(colour = "black", size = 3,
             aes(shape = Model),
             position = position_dodge(w = 0.7)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                position = position_dodge(w = 0.7),
                width = 0.3) +
  #geom_errorbar(colour = "black", aes(ymin = X0.025quant, ymax = X0.975quant),
  #              position = position_dodge(w = 0.7),
  #              width = 0.3) +
  geom_point(position = position_dodge(w = 0.7), 
             aes(shape = Model),
             size = 2) +
  labs(x = NULL, y = "Effect estimate") +
  scale_colour_discrete_sequential(palette = AlberPalettes[[3]], nmax = 6, order = c(3:6)) +
  theme(legend.position = "right") ->
  Figure1b

(((Figure1aList[[1]] | Figure1aList[[2]] | Figure1aList[[3]]) + 
    plot_layout(guides = "collect")) /Figure1b) +
  #plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") +
  plot_layout(heights = c(2, 1.5))

ggsave("Figures/Figure1.jpeg", 
       width = 250, height = 150, dpi = 600, units = "mm")

ggsave("Figures/Figure1.pdf",
       width = 250, height = 150, dpi = 600, units = "mm")

# Outputting Table ####

SpocialList %>% 
  map(c("FinalModel", "summary.fixed", data.frame, rownames_to_column)) %>% 
  bind_rows(.id = "Response") %>% 
  dplyr::select(Response, 
                Variable = rowname, Mean = mean, 
                Lower = `X0.025quant`, Upper = `X0.975quant`) %>% 
  mutate_if(is.numeric, ~round(.x, 3)) %>% 
  mutate(" " = c("", "*")[as.numeric(Lower*Upper > 0)+1]) %>% 
  cbind(
    
    SpocialList %>% 
      map(c("Spatial", "Model", "summary.fixed", data.frame, rownames_to_column)) %>% 
      bind_rows(.id = "Response") %>% 
      dplyr::select(Mean = mean, 
                    Lower = `X0.025quant`, Upper = `X0.975quant`) %>% 
      mutate_if(is.numeric, ~round(.x, 3)) %>% 
      rename_all(~paste0("SPDE", .x)) %>% 
      mutate(" " = c("", "*")[as.numeric(SPDELower*SPDEUpper > 0)+1])
    
  ) %>% write.csv("Output Files/SocialModelTable.csv", row.names = F)


# Figure 2: Age and Grazing ####

{
  
  library(RColorBrewer); library(ggregplot); library(tidyverse); library(colorspace);
  library(GGally); library(cowplot); library(INLA); library(patchwork); library(magrittr)
  library(MCMCglmm); library(magrittr); library(broom)
  
  theme_set(theme_cowplot() + 
              theme(strip.background = element_rect(fill = "white", colour = NA)))
  
  AlberPalettes <- c("YlGnBu","Reds","BuPu", "PiYG")
  AlberColours <- sapply(AlberPalettes, function(a) RColorBrewer::brewer.pal(5, a)[4])
  AlberColours[length(AlberColours)+1:2] <- RColorBrewer::brewer.pal(11, AlberPalettes[[4]])[c(2,10)]
  
  AlberColours %<>% c(Pink = "#FD6396", Blue = "#3C78D8")
  
  Resps = c("GroupSize", "Degree", "Strength")
  
  RespLabels = c("Group Size", "Degree", "Strength")
  names(RespLabels) <- Resps
  
}

GrazeModels <- readRDS("Output Files/GrazeModels1.rds")

TestDF <- Deer %>% Grelect(Name, Age, E, N) %>% na.omit %>% droplevels %>% 
  mutate_at("Age", ~.x - min(.x) + 1)

IM1 <- INLAModelAdd("Age", Explanatory = 1, Data = TestDF, 
                    Family = "nbinomial",
                    Random = "Name", RandomModel = "iid",
                    AddSpatial = T, Coordinates = c("E", "N"))

IM1$FinalModel %>% MDIC -
  (IM1$Spatial$Model %>% MDIC)

XLim <- c(1350, 1395)
YLim <- c(7985, 8051)

AgeMap <-
  IM1$Spatial$Model %>% ggField(IM1$Spatial$Mesh,
                                # Points = IM1$Data[,c("E", "N")], 
                                # Boundary = Boundary,
                                PointAlpha = 0.05) +
  scale_fill_discrete_sequential(palette = "Mint", name = "Age") +
  lims(x = XLim, y = YLim) + 
  geom_point(data = UberCentroid, aes(x = Easting, y = Northing), shape = 2)

GrazeMap <- 
  ggField(GrazeModels$Spatial$Model, 
          Mesh = GrazeModels$Spatial$Mesh) +
  labs(fill = "Distance") +
  scale_fill_discrete_sequential(palette = "Mint",
                                 name = "Grazing\nquality") +
  lims(x = XLim, y = YLim) + 
  geom_point(data = UberCentroid, aes(x = Easting, y = Northing), shape = 2, colour = "white")

AgeMap +
  GrazeMap +
  plot_annotation(tag_levels = "A")

ggsave("Figures/Figure2.jpeg", 
       width = 220, height = 120, dpi = 600, units = "mm")

ggsave("Figures/Figure2.pdf", 
       width = 220, height = 120, dpi = 600, units = "mm")

# Figure 3: Spatial Effects ####

SpatialModels2 <- readRDS("Output Files/SpatialModels1.rds")
SpatialModels3 <- readRDS("Output Files/SpatialModels2.rds")
SpatialModels4 <- readRDS("Output Files/SpatialModels3.rds")
GrazeModels <- readRDS("Output Files/GrazeModels1.rds")

SpatialList <- list(AnnualDensity = SpatialModels2$AnnualDensity) %>% 
  append(SpatialModels3) %>% append(SpatialModels4) %>% 
  append(list(GrazeType = GrazeModels))

SpatialResps <-
  c("AnnualDensity") %>% 
  c("AnnualDistance", "LifetimeDistance", "EarlyDistance", "UberDistance") %>% 
  c("HRAShrink", "MCPShrink", "HRA", "MCPArea", "GrazeType")

SpatialLabels <- c("Density", 
                   "Annual centroid distance", 
                   "Lifetime centroid distance", 
                   "Natal centroid distance", 
                   "Pop'n centroid distance",
                   "Home range overlap", "Polygon overlap",
                   "Home range area", "Polygon area", "Grazing quality")

names(SpatialLabels) <- SpatialResps

Covar <- c("ReprodStatus", "Age",
           "Year", "PopN", "NObs")

Figure2aList <- LabelYList <- LabelList <- LabelXList <- list()

for(a in SpatialResps){
  
  print(a)
  
  TestHinds <- SpatialList[[a]]$Data
  SocMesh <- SpatialList[[a]]$Spatial$Mesh
  
  X = seq(min(TestHinds$Age), 
          max(TestHinds$Age), 
          length.out = nrow(TestHinds)) %>% c
  
  SpatialList[[a]]$FinalModel %>% 
    INLAFit(TestHinds, Covar, NDraw = 100, Draw = T) %>% 
    map_dbl(mean) -> 
    Intercepts
  
  SpatialList[[a]]$FinalModel %>% 
    GetEstimates("Age", NDraw = 100, Draws = T) -> 
    Slopes
  
  B = mean(Slopes)
  A = mean(Intercepts)
  Y = A + B*X
  
  B2 = summary(SpatialList[[a]]$Spatial$Model)$fixed["Age","mean"]
  A2 = A
  Y2 = A2 + B2*X
  
  1:length(Slopes) %>% map(~data.frame(X = X,
                                       Y = X*Slopes[[.x]] + Intercepts[[.x]]) %>% 
                             slice(1, n())) -> SlopeDF
  
  SlopeDF %<>% bind_rows(.id = "Rep")
  
  FitLine <- data.frame(
    Age = rep(X, 2),
    Y = c(Y, Y2),
    Model = rep(c("Base","SPDE"), each = length(X))
  )
  
  TestHinds$Y <- TestHinds[,a]
  
  P <- SpatialList[[a]]$FinalModel %>% INLAPValue("Age")
  
  if(P[[1]]>10^-4){
    
    P <- P[[1]] %>% round(4)
    
  }
  
  SpatialList[[a]]$FinalModel %>% 
    
    GetEstimates(Variable = "Age") %>%
    paste0("; P = ", P) -> 
    LabelList[[a]]
  
  LabelYList[[a]] <- max(TestHinds$Y + diff(range(TestHinds$Y))/10)
  
  LabelXList[[a]] <- median(X)
  
  LabelDf <- data.frame(
    
    Age = LabelXList[[a]],
    Y = LabelYList[[a]],
    Label = LabelList[[a]]
    
  )
  
  if(a %in% c("AnnualDensity", SpatialResps[str_detect(SpatialResps, "Distance")][2:4])){
    
    FitLine %<>% filter(Model == "Base")
    
  }
  
  Plot <- 
    TestHinds %>% RandomSlice %>% mutate(Group = factor(1:n())) %>%
    ggplot(aes(Age, Y)) + 
    geom_point(alpha = 0.1, aes(colour = Group)) + 
    geom_line(alpha = 0.05, data = SlopeDF, aes(X, group = Rep)) +
    scale_colour_discrete_sequential(palette = "Reds", 
                                     guide = F) +
    geom_line(data = FitLine, # %>% filter(Model == "Base"), 
              aes(lty = Model),
              size = 1)  +
    labs(y = SpatialLabels[[a]], x = "Age") +
    geom_text(data = LabelDf, aes(label = Label), hjust = 0.5,
              colour = AlberColours[[2]], size = 3) +
    scale_linetype_manual(limits = c("Base", "SPDE"), values = c(1, 2)) +
    scale_x_continuous(breaks = sort(unique(TestHinds$Age))[1:5*5 - 4], 
                       labels = c(1:5*5)) # %>% plot
  
  Figure2aList[[a]] <- Plot
  
}

# SpatialRespsOrder <- c(SpatialResps[c(1, 2, 4, 5, 8, 9, 6, 7)])

SpatialRespsOrder <- SpatialResps[c(1, 2, 5, 8, 6, 10)]

Figure2aList[SpatialRespsOrder] %>% ArrangeCowplot +
  plot_layout(guides = "collect") + plot_layout(nrow = 2) +
  plot_annotation(tag_levels = "A")

ggsave("Figures/Figure3.jpeg", 
       width = 220, height = 160, dpi = 600, units = "mm")

ggsave("Figures/Figure3.pdf", 
       width = 220, height = 160, dpi = 600, units = "mm")

# Outputting Table ####

SpatialList[SpatialRespsOrder] %>% 
  map(c("FinalModel", "summary.fixed", data.frame, rownames_to_column)) %>% 
  bind_rows(.id = "Response") %>% 
  dplyr::select(Response, 
                Variable = rowname, Mean = mean, 
                Lower = `X0.025quant`, Upper = `X0.975quant`) %>% 
  mutate_if(is.numeric, ~round(.x, 3)) %>% 
  mutate(" " = c("", "*")[as.numeric(Lower*Upper > 0)+1]) %>% 
  full_join(
    
    SpatialList[SpatialRespsOrder] %>% 
      map(c("Spatial", "Model", "summary.fixed", data.frame, rownames_to_column)) %>% 
      bind_rows(.id = "Response") %>% 
      dplyr::select(Response, 
                    Variable = rowname, 
                    Mean = mean, 
                    Lower = `X0.025quant`, Upper = `X0.975quant`) %>% 
      mutate_if(is.numeric, ~round(.x, 3)) %>% 
      mutate_if(is.numeric, 
                ~ifelse(Response %in% c("AnnualDensity", "UberDistance"), NA, .x)) %>% 
      mutate_at("Variable", ~str_replace(.x, "Intercept", "(Intercept)")) %>% 
      rename_all(~paste0("SPDE", .x)) %>% 
      mutate(" " = c("", "*")[as.numeric(SPDELower*SPDEUpper > 0)+1]),
    
    by = c("Response" = "SPDEResponse", "Variable" = "SPDEVariable")
    
  ) %>% filter(!Variable %in% c("Age_AnnualDistance", "Age:AnnualDistance")) %>% 
  write.csv("Output Files/SpatialModelTable.csv", row.names = F)

# ~~~~~ Supplementary ####

# Sup Fig 1: Spatial selective disappearance ####

SpatialResps <-
  c("AnnualDensity") %>%
  c("AnnualDistance", "LifetimeDistance", "EarlyDistance", "UberDistance") %>%
  c("HRAShrink", "MCPShrink", "HRA", "MCPArea", "GrazeType")

SpatialLabels <- c("Density",
                   "Annual centroid distance",
                   "Lifetime centroid distance",
                   "Natal centroid distance",
                   "Pop'n centroid distance",
                   "Home range overlap", "Polygon overlap",
                   "Home range area", "Polygon area", "Grazing quality")

names(SpatialLabels) <- SpatialResps

SpatialRespsOrder <- SpatialResps[c(1, 2, 5, 8, 6, 10)]

SpatialLabels <- 
  SpatialLabels[SpatialRespsOrder]

SpatialLabels2 <- 
  glue::glue("{LETTERS[1:length(SpatialLabels)]}) {SpatialLabels}") %>% 
  as.character()

names(SpatialLabels2) <- names(SpatialLabels)

"Output Files" %>% 
  list.files(pattern = "SelectiveSpatial", full.names = T) ->
  SpatialFiles

SpatialFiles %>% 
  readRDS -> 
  SelectiveSpatialList

SelectiveSpatialList %>% 
  Unlist1 %>%
  map(c("summary.fixed", as.data.frame, rownames_to_column)) %>% 
  bind_rows(.id = "Model") %>% 
  rename(Mean = mean, Lower = `0.025quant`, Upper = `0.975quant`) %>% 
  as.data.frame %>% filter(rowname %in% c("Age", "Longevity")) %>% 
  mutate(Response = substr(Model, 1, (nchar(Model) - 1))) %>% 
  mutate(Model = substr(Model, nchar(Model), nchar(Model))) ->
  SelectiveSpatialDF

SelectiveSpatialDF %>% 
  filter(Response %in% SpatialRespsOrder) %>% 
  # filter(!Response == "LifetimeDistance") %>% 
  mutate_at("Response", ~factor(.x, levels = SpatialRespsOrder)) %>% 
  filter(Model < 4 | !(Response %in% c("AnnualDensity", "UberDistance", "AnnualDistance"))) %>% 
  mutate_at("Model", ~factor(c("Base", "+ ID", "+ Longevity", "+ SPDE")[as.numeric(.x)], 
                             levels = c("Base", "+ ID", "+ Longevity", "+ SPDE"))) %>% 
  filter(!is.na(Model)) %>% 
  ggplot(aes(rowname, Mean, colour = Model, group = Model)) + 
  facet_wrap(~Response, ncol = 2, 
             scales = "free_y", labeller = as_labeller(SpatialLabels2)) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  geom_point(colour = "black", size = 3,
             position = position_dodge(w = 0.7)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                position = position_dodge(w = 0.7),
                width = 0.3) +
  #geom_errorbar(colour = "black", aes(ymin = X0.025quant, ymax = X0.975quant),
  #              position = position_dodge(w = 0.7),
  #              width = 0.3) +
  geom_point(position = position_dodge(w = 0.7), size = 2) +
  labs(x = NULL, y = "Effect estimate") +
  scale_colour_discrete_sequential(palette = AlberPalettes[[3]], nmax = 6, order = c(3:6)) +
  theme(legend.position = "right") ->
  SelectiveSpatialPlot

SelectiveSpatialPlot

ggsave("Figures/FigureSI1.jpeg", units = "mm", width = 200, height = 200, dpi = 300)

# Sup Fig 2: Combined spatial-social model effects ####

CombinedModels <- readRDS("Output Files/CombinedModels3.rds")

CombnVar <- c("Age", "AnnualDensity", "UberDistance", "HRA", "Longevity")
CombnLabels <- c("Age", "Density", "Pop'n Distance", "HRA", "Longevity")

Resps <- c("GroupSize", "Degree", "Strength")

CombinedFXa <- 
  CombinedModels %>% 
  map("FinalModel") %>% 
  Efxplot(VarOrder = CombnVar, 
          VarNames = CombnVar, # %>% str_replace("MCPArea", "HRA"),
          PointOutline = T,
          ModelNames = Resps) +
  scale_x_discrete(limits = CombnVar %>% rev,
                   labels = CombnLabels %>% rev) +
  #scale_colour_manual(values = c(AlberColours[[1]], AlberColours[[2]], AlberColours[[3]]))
  scale_colour_discrete_sequential(palette = "Mint", nmax = 5, 
                                   order = c(3:6-1),
                                   labels = RespLabels)

CombinedFXb <- 
  CombinedModels %>% 
  map(c("Spatial", "Model")) %>% 
  Efxplot(VarOrder = CombnVar, 
          VarNames = CombnVar, # %>% str_replace("MCPArea", "HRA"),
          PointOutline = T,
          ModelNames = Resps) +
  scale_x_discrete(limits = CombnVar %>% rev,
                   labels = CombnLabels %>% rev) +
  #scale_colour_manual(values = c(AlberColours[[1]], AlberColours[[2]], AlberColours[[3]]))
  scale_colour_discrete_sequential(palette = "Mint", nmax = 5, 
                                   order = c(3:6-1),
                                   labels = RespLabels)

CombinedFXa + CombinedFXb +
  plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = "A")

ggsave("Figures/FigureSI2.jpeg", units = "mm", width = 200, height = 200, dpi = 300)

# Sup Fig 3: Combined effects 2 ####

SocialModels3 <- readRDS("Output Files/SocialModels2.rds")

EffectCompare1 <- 
  SocialModels3 %>% 
  map(c(3)) %>%
  # map("FinalModel")
  map("summary.fixed") %>% 
  map(rownames_to_column) %>% 
  bind_rows(.id = "Response") %>% 
  bind_rows(
    CombinedModels %>% 
      map("FinalModel") %>% 
      map("summary.fixed") %>% 
      map(rownames_to_column) %>% 
      bind_rows(.id = "Response"),
    .id = "ModelSet"
  ) %>% 
  rename(Mean = mean,
         Lower = `0.025quant`,
         Upper = `0.975quant`) %>% 
  filter(rowname == "Age") %>% 
  ggplot(aes(Response, Mean, colour = ModelSet)) + 
  geom_hline(yintercept = 0, lty = 2, alpha = 0.3) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                position = position_dodge(w = 0.5),
                width = 0.3) +
  geom_point(position = position_dodge(w = 0.5)) +
  coord_flip()

EffectCompare2 <- 
  SocialModels3 %>% 
  map(c(4)) %>% 
  map("summary.fixed") %>% 
  map(rownames_to_column) %>% 
  bind_rows(.id = "Response") %>% 
  bind_rows(
    CombinedModels %>% 
      map(c("Spatial", "Model")) %>% 
      map("summary.fixed") %>% 
      map(rownames_to_column) %>% 
      bind_rows(.id = "Response"),
    .id = "ModelSet"
  ) %>% 
  rename(Mean = mean,
         Lower = `0.025quant`,
         Upper = `0.975quant`) %>% 
  filter(rowname == "Age") %>% 
  ggplot(aes(Response, Mean, colour = ModelSet)) + 
  geom_hline(yintercept = 0, lty = 2, alpha = 0.3) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                position = position_dodge(w = 0.5),
                width = 0.3) +
  geom_point(position = position_dodge(w = 0.5)) +
  coord_flip()

(EffectCompare1 + labs(y = "Effect",
                       x = "",
                       colour = "Model set") + 
    scale_colour_manual(labels = c("Base", "+ Spatial behaviours"),
                        limits = c(1, 2),
                        values = c(AlberColours[[1]], 
                                   AlberColours[[2]]))) + 
  (EffectCompare2 + labs(y = "Effect", 
                         colour = "Model set",
                         x = "") + 
     scale_x_discrete(labels = NULL) + 
     scale_colour_manual(labels = c("Base", "+ Spatial behaviours"),
                         limits = c(1, 2),
                         values = c(AlberColours[[1]], 
                                    AlberColours[[2]]))) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A")

ggsave("Figures/FigureSI3.jpeg", units = "mm", 
       width = 220, height = 150, 
       dpi = 300)

CombinedModels %>% map(c("dDIC", DICTableGet)) %>% 
  bind_rows(.id = "Response") %>% filter(!Round == 4) %>% 
  write.csv("Figures/SpocialDIC.csv", row.names = F)

# Sup Fig 4: Correlations among behaviours ####

# Deer %>% 
#   dplyr::select(GroupSize, Degree, Strength) %>% 
#   GGally::ggpairs()

SpatialResps <-
  c("AnnualDensity") %>% 
  c("AnnualDistance", "LifetimeDistance", 
    "EarlyDistance", "UberDistance") %>% 
  c("HRAShrink", "MCPShrink", "HRA", "MCPArea", "GrazeType")

SpatialLabels <- c("Density", 
                   "Annual centroid distance", 
                   "Lifetime centroid distance", 
                   "Natal centroid distance", 
                   "Pop'n centroid distance",
                   "Home range overlap", "Polygon overlap",
                   "Home range area", "Polygon area", "Grazing quality")

names(SpatialLabels) <- SpatialResps

SpatialRespsOrder <- SpatialResps[c(1, 2, 5, 8, 6, 10)]

Deer %>% 
  dplyr::select(GroupSize, Degree, Strength)  %>% 
  cor(use = "complete.obs") %>% 
  data.frame %>% 
  mutate_all(~round(.x, 2)) %>% 
  as.matrix %>% 
  reshape2::melt() %>% 
  # mutate_at(c("Var1", "Var2"), ~str_replace_all(.x, SpatialLabels)) %>% 
  ggplot(aes(Var1, Var2, fill = value)) +
  geom_tile() + 
  geom_text(aes(label = value)) +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_gradient2(
    low = AlberColours[[1]], 
    high = AlberColours[[2]], 
    mid = "white",
    limits = c(-1, 1)) +
  labs(x = NULL, y = NULL, fill = "R") +
  
  Deer %>% 
  dplyr::select(all_of(SpatialRespsOrder)) %>% 
  cor(use = "complete.obs") %>% 
  data.frame %>% 
  mutate_all(~round(.x, 2)) %>% 
  as.matrix %>% 
  reshape2::melt() %>% 
  mutate_at(c("Var1", "Var2"), ~str_replace_all(.x, SpatialLabels)) %>% 
  ggplot(aes(Var1, Var2, fill = value)) +
  geom_tile() + 
  geom_text(aes(label = value)) +
  coord_fixed() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_gradient2(
    low = AlberColours[[1]], 
    high = AlberColours[[2]], 
    mid = "white",
    limits = c(-1, 1)) +
  labs(x = NULL, y = NULL, fill = "R") +
  plot_layout(guides = "collect") +
  plot_layout(nrow = 2) +
  plot_annotation(tag_levels = "A")

ggsave("Figures/Figure SI4.jpeg", units = "mm", 
       height = 250, 
       width = 150)
