
# Social Senescence Models ####

library(RColorBrewer); library(ggregplot); library(tidyverse); library(colorspace);
library(GGally); library(cowplot); library(INLA); library(patchwork); library(magrittr)
library(fs)

theme_set(theme_cowplot() + theme(strip.background = element_rect(fill = "white")))

dir_create("Output Files")

# 1	Social Models 	####

# Do individuals reduce their contacts as they age? ####

Resps = c("GroupSize", "Degree", "Strength")

Covar <- c("ReprodStatus", #"Age"
           "Year", "NObs", 
           "PopN")

FullCovar <- c(Covar, "Age")

Cols <- c("Name", 
          Resps, 
          FullCovar,
          "Year", "PopN", 
          "E", "N")

TestHinds <- Deer %>% 
  #filter(MeshInclude == 1) %>%
  dplyr::select(Cols) %>%
  mutate(
    #Degree = as.vector(scale(sqrt(Degree))),
    #Strength = as.vector(scale(sqrt(Strength))),
    #GroupSize = as.vector(scale(kader:::cuberoot(GroupSize))),
    fYear = as.factor(Year)
  )  %>% 
  na.omit() %>% 
  droplevels %>%
  as.data.frame()

TestHinds %>% nrow %>% print

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","E","N"))

TestHinds[,paste0(Resps, ".Original")] <- TestHinds[,Resps]

TestHinds[,paste0(ToScale, ".Original")] <- TestHinds[,ToScale]

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

SpocialList <- list()

a <- 1

for(a in a:length(Resps)){
  
  print(Resps[a])
  
  IM1 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = FullCovar,
    #AllModels = T,
    Random = c("Name", "fYear"), 
    RandomModel = rep("iid", 2),
    Family = "gaussian", Beep = F, 
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  SpocialList[[Resps[a]]] <- IM1
  
}

SpocialList %>% saveRDS(file = "Output Files/SocialModels1.rds")

# How does selective disappearance affect the models’ findings? ####

Resps <- c("GroupSize", "Degree", "Strength")

Covar <- c("ReprodStatus", "Age",
           "Year", "PopN", "NObs")

FullCovar <- c(Covar, "Longevity")

Cols <- c("Name", 
          Resps, 
          FullCovar,
          "Year", "PopN", 
          "E", "N")

TestHinds <- Deer %>% 
  #  filter(MeshInclude == 1) %>%
  dplyr::select(Cols) %>%
  mutate(
    fYear = as.factor(Year)
  )  %>% 
  na.omit() %>% 
  droplevels %>%
  as.data.frame()

TestHinds %>% nrow %>% print

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","E","N"))

TestHinds[,paste0(Resps, ".Original")] <- TestHinds[,Resps]

TestHinds[,paste0(ToScale, ".Original")] <- TestHinds[,ToScale]

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

SelectiveList <- list()

a <- 1

for(a in a:length(Resps)){
  
  print(Resps[a])
  
  IM0 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar,
    Random = c("fYear"), 
    RandomModel = rep("iid", 1),
    Family = "gaussian"
    
  )
  
  IM1 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar,
    Random = c("Name", "fYear"), 
    RandomModel = rep("iid", 2),
    Family = "gaussian"
    
  )
  
  IM2 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar %>% c("Longevity"),
    Random = c("Name", "fYear"), 
    RandomModel = rep("iid", 2),
    Family = "gaussian", Beep = F, 
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  SelectiveList[[Resps[a]]] <- list(IM0$FinalModel,
                                    IM1$FinalModel,
                                    IM2$FinalModel,
                                    IM2$Spatial$Model)
  
}

SelectiveList %>% saveRDS(file = "Output Files/SocialModels2.rds")

# 2	Spatial Models 	####

# Do individuals move to areas of lower density as they age? ####

Deer %>% 
  dplyr::select(contains("Density")) %>%
  dplyr::select(-contains("t0")) %>%
  extract(1:2) %>% 
  
  names ->
  
  DensityCovar

Covar <- c("ReprodStatus", #"Age"
           "Year", "PopN", "NObs")

FullCovar <- c(Covar, "Age")

Cols <- c("Name", 
          DensityCovar, 
          FullCovar,
          "Year", "PopN", 
          "E", "N")

TestHinds <- Deer %>% 
  #filter(MeshInclude == 1) %>%
  dplyr::select(Cols) %>%
  mutate(
    fYear = as.factor(Year)
  )  %>% 
  mutate_at(DensityCovar, ~c(scale(log(.x + 1)))) %>% 
  na.omit() %>% 
  droplevels %>%
  as.data.frame()

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","E","N"))

TestHinds[,paste0(DensityCovar, ".Original")] <- TestHinds[,DensityCovar]

TestHinds[,paste0(ToScale, ".Original")] <- TestHinds[,ToScale]

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

DensityList <- list()

a <- 1

for(a in a:length(DensityCovar)){
  
  print(DensityCovar[a])
  
  IM1 <- INLAModelAdd(
    
    Response = DensityCovar[a],
    Data = TestHinds,
    Explanatory = FullCovar,
    #AllModels = T,
    Random = c("Name", "fYear"), 
    RandomModel = rep("iid", 2),
    Family = "gaussian", Beep = F, 
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  DensityList[[DensityCovar[a]]] <- IM1
  
}

DensityList %>% saveRDS(file = "Output Files/SpatialModels1.rds")

DensityList %>% map("FinalModel") %>% Efxplot +
  DensityList %>% map(c("Spatial", "Model")) %>% Efxplot

# Do they move less between years? #####
# Do they move further from their home range as they age?

{
  
  Resps = c("AnnualDistance", "LifetimeDistance", "EarlyDistance", "UberDistance")
  
  Covar <- c("ReprodStatus", "Age",
             "Year", "PopN", "NObs")
  
  Cols <- c("Name", 
            Resps, 
            Covar,
            "Year", "PopN", 
            "E", "N")
  
  TestHinds <- Deer %>% 
    filter(MeshInclude == 1) %>%
    dplyr::select(Cols) %>%
    mutate(
      fYear = as.factor(Year)
    )  %>% 
    mutate_at(vars(contains("Distance")), ~log(.x + 1)) %>%
    na.omit() %>% 
    droplevels %>%
    as.data.frame()
  
  N <- nrow(TestHinds); N
  
  Classes <- TestHinds %>% sapply(class)
  
  ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
    setdiff(c("Eigenvector","Eigenvector2","E","N"))
  
  TestHinds[,paste0(ToScale, ".Original")] <- TestHinds[,ToScale]
  
  TestHinds[,paste0(Resps, ".Original")] <- TestHinds[,Resps]
  
  TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))
  
  IMList <- list()
  
  a = 1
  
  IM1 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar,
    #AllModels = T,
    Random = c("Name", "fYear"), 
    RandomModel = rep("iid", 2),
    Family = "gaussian", Beep = F, 
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  IM1$FinalModel %>% Efxplot
  
  IMList[[Resps[a]]] <- IM1
  
  a = 2
  
  IM1 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar %>% c("AnnualDistance"),
    Add = "Age:AnnualDistance",
    # AllModels = T,
    Random = c("Name", "fYear"), 
    RandomModel = rep("iid", 2),
    Family = "gaussian", Beep = F, 
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  IM1$FinalModel %>% Efxplot
  
  IMList[[Resps[a]]] <- IM1
  
  a = 3
  
  IM1 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar %>% c("AnnualDistance"),
    Add = "Age:AnnualDistance",
    #AllModels = T,
    Random = c("Name", "fYear"), 
    RandomModel = rep("iid", 2),
    Family = "gaussian", Beep = F, 
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  IM1$Spatial$Model %>% Efxplot
  IM1$FinalModel %>% Efxplot
  
  IMList[[Resps[a]]] <- IM1
  
  a = 4
  
  IM1 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar %>% c("AnnualDistance"),
    Add = "Age:AnnualDistance",
    #AllModels = T,
    Random = c("Name", "fYear"), 
    RandomModel = rep("iid", 2),
    Family = "gaussian", Beep = F, 
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  IM1$Spatial$Model %>% Efxplot
  IM1$FinalModel %>% Efxplot
  
  IMList[[Resps[a]]] <- IM1
  
  IMList %>% map(c("Spatial", "Model")) %>% Efxplot() +
    IMList %>% map(c("FinalModel")) %>% Efxplot()
  
}

saveRDS(IMList, file = "Output Files/SpatialModels2.rds")

# Do their home ranges overlap year-on-year? ####

Resps = c("HRAShrink", "MCPShrink", "HRA", "MCPArea")

Covar <- c("ReprodStatus", "Age",
           "Year", "PopN", "NObs")

IMList <- list()

a <- 1

for(a in a:length(Resps)){
  
  print(Resps[a])
  
  Cols <- c("Name", 
            Resps[a], 
            Covar,
            "Year", "PopN", 
            "E", "N")
  
  TestHinds <- Deer %>% 
    #filter(MeshInclude == 1) %>%
    dplyr::select(Cols) %>%
    mutate(
      fYear = as.factor(Year)
    )  %>% 
    mutate_at(vars(matches("^HRA$")), ~log(.x)) %>%
    mutate_at(vars(matches("MCPArea")), ~kader:::cuberoot(.x)) %>%
    #mutate_at(vars(matches("MCPShrink")), ~.x^2) %>%
    mutate_at(vars(matches("HRAShrink")), ~.x^3) %>%
    na.omit() %>% 
    droplevels %>%
    as.data.frame()
  
  N <- nrow(TestHinds); N
  
  Classes <- TestHinds %>% sapply(class)
  
  ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
    setdiff(c("Eigenvector","Eigenvector2","E","N"))
  
  TestHinds[,paste0(ToScale, ".Original")] <- TestHinds[,ToScale]
  
  TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))
  
  IM1 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar %>% setdiff("NObs"),
    #AllModels = T,
    Random = c("Name", "fYear"), 
    RandomModel = rep("iid", 2),
    Family = "gaussian", Beep = F, 
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  IM1$FinalModel %>% Efxplot
  
  IMList[[Resps[a]]] <- IM1
  
}

IMList %>% map(c("FinalModel")) %>% Efxplot() +
  IMList %>% map(c("Spatial", "Model")) %>% Efxplot()

saveRDS(IMList, file = "Output Files/SpatialModels3.rds")

# 3_Demographic Models ####

Resps = c("GroupSize", "Degree", "Strength")

Covar <- c("ReprodStatus", "Age",
           "Year", "PopN", "NObs")

AddCovar <- Deer %>% 
  dplyr::select(matches("DeadStrength|ShotStrength")) %>% names

FullCovar <- c(Covar, AddCovar)

Cols <- c("Name", 
          Resps, 
          FullCovar,
          "Year", "PopN", 
          "E", "N")

TestHinds <- Deer %>% 
  #filter(MeshInclude == 1) %>%
  #filter(Age>5) %>% 
  dplyr::select(Cols) %>%
  mutate(
    fYear = as.factor(Year)
  )  %>% 
  mutate_at(AddCovar, ~log(.x + 1)) %>% 
  na.omit() %>% 
  droplevels %>%
  as.data.frame()

TestHinds %>% nrow %>% print

TestHinds %<>% 
  mutate_at("DeadStrength.t2", ~.x + DeadStrength) %>% 
  mutate_at("DeadStrength.t3", ~.x + DeadStrength.t2) %>% 
  mutate_at("ShotStrength.t2", ~.x + ShotStrength) %>% 
  mutate_at("ShotStrength.t3", ~.x + ShotStrength.t2)

if(1){
  
  #TestHinds %<>% filter(ShotStrength<0.5)
  
  TestHinds %<>% 
    mutate_at(vars(contains("DeadStrength")), ~log(.x + 1)) %>% 
    mutate_at(vars(contains("ShotStrength")), ~log(.x + 1))
  
}

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","E","N", AddCovar))

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

SpocialList <- list()

a <- 1

for(a in a:length(Resps)){
  
  print(Resps[a])
  
  IM1 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar, # %>% c("DeadStrength", "DeadStrength:Age"),
    Add = AddCovar[1:2], # %>% setdiff(c("DeadStrength", "ShotStrength")),
    AllModels = T,
    Random = c("Name", "fYear"), 
    RandomModel = rep("iid", 2),
    Family = "gaussian", Beep = F, 
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  SpocialList[[Resps[a]]] <- IM1
  
}

SpocialList %>% saveRDS(file = "Output Files/DemographicModels1.rds")

# Binary ####

Resps = c("GroupSize", "Degree", "Strength")

Covar <- c("ReprodStatus", "Age",
           "Year", "PopN", "NObs")

AddCovar <- Deer %>% 
  dplyr::select(matches("DeadStrength|ShotStrength")) %>% names

FullCovar <- c(Covar, AddCovar)

Cols <- c("Name", 
          Resps, 
          FullCovar,
          "Year", "PopN", 
          "E", "N")

TestHinds <- Deer %>% 
  #filter(MeshInclude == 1) %>%
  #filter(Age>5) %>% 
  dplyr::select(Cols) %>%
  mutate(
    fYear = as.factor(Year)
  )  %>% 
  mutate_at(AddCovar, ~log(.x + 1)) %>% 
  na.omit() %>% 
  droplevels %>%
  as.data.frame()

TestHinds %>% nrow %>% print

TestHinds %<>% 
  mutate_at("DeadStrength.t2", ~.x + DeadStrength) %>% 
  mutate_at("DeadStrength.t3", ~.x + DeadStrength.t2) %>% 
  mutate_at("ShotStrength.t2", ~.x + ShotStrength) %>% 
  mutate_at("ShotStrength.t3", ~.x + ShotStrength.t2)

if(1){
  
  TestHinds %<>% 
    mutate_at(vars(contains("DeadStrength")), AsBinary) %>% 
    mutate_at(vars(contains("ShotStrength")), AsBinary)
  
}

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","E","N", AddCovar))

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

SpocialList <- list()

a <- 1

for(a in a:length(Resps)){
  
  print(Resps[a])
  
  IM1 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar, # %>% c("DeadStrength", "DeadStrength:Age"),
    Add = AddCovar[1:2], # %>% setdiff(c("DeadStrength", "ShotStrength")),
    AllModels = T,
    Random = c("Name", "fYear"), 
    RandomModel = rep("iid", 2),
    Family = "gaussian", Beep = F, 
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  SpocialList[[paste0(Resps[a], ".Binary")]] <- IM1
  
}

SpocialList %>% saveRDS(file = "Output Files/DemographicModels2.rds")

# 4_Social-Spatial Causality 1 ####

Resps = c("GroupSize", "Degree", "Strength")

Covar <- c("ReprodStatus", "Age",
           "Year", "PopN", "NObs")

Deer %>% dplyr::select(contains("Density")) %>% names ->
  
  DensityCovar

AddCovar <- #Deer %>% #select(matches("DeadStrength|ShotStrength")) %>% names %>% 
  c(DensityCovar[2]) %>%
  #c("Longevity") %>% 
  c("MCPArea", "HRA", "EarlyDistance", "UberDistance")

ClashList <- list(DensityCovar, c("MCPArea", "HRA"), c("EarlyDistance", "UberDistance"))

FullCovar <- c(Covar, AddCovar)

Cols <- c("Name", 
          Resps, 
          FullCovar,
          "Year", "PopN", 
          "E", "N")

TestHinds <- Deer %>% 
  #filter(MeshInclude == 1) %>%
  # filter(Age>5) %>% 
  dplyr::select(Cols) %>%
  mutate(
    #Degree = as.vector(scale(sqrt(Degree))),
    #Strength = as.vector(scale(sqrt(Strength))),
    #GroupSize = as.vector(scale(kader:::cuberoot(GroupSize))),
    fYear = as.factor(Year)
  )  %>% 
  mutate_at(AddCovar, ~log(.x + 1)) %>% 
  na.omit() %>% 
  droplevels %>%
  as.data.frame()

if(1){
  
  TestHinds %<>% 
    mutate_at("MCPArea", ~kader:::cuberoot(.x)) %>% 
    mutate_at("EarlyDistance", ~sqrt(.x)) %>% 
    mutate_at("HRA", ~log(.x))
  
}

if(0){
  
  TestHinds %<>% 
    mutate_at("DeadStrength.t2", ~.x + DeadStrength) %>% 
    mutate_at("DeadStrength.t3", ~.x + DeadStrength.t2) %>% 
    mutate_at("ShotStrength.t2", ~.x + ShotStrength) %>% 
    mutate_at("ShotStrength.t3", ~.x + ShotStrength.t2)
  
}

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","E","N"))

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

SpocialList <- list()

a <- 1

for(a in a:length(Resps)){
  
  print(Resps[a])
  
  IM1 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar,
    Add = AddCovar,
    #AllModels = T,
    Random = c("Name", "fYear"), 
    RandomModel = rep("iid", 2),
    Clashes = ClashList,
    Family = "gaussian", Beep = F, 
    BaseModel = T,
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  SpocialList[[Resps[a]]] <- IM1
  
}

SpocialList %>% saveRDS(file = "Output Files/CombinedModels1.rds")

# 4_Social-Spatial Causality 2 ####

Resps = c("GroupSize", "Degree", "Strength")

Covar <- c("ReprodStatus", "Age",
           "Year", "PopN", "NObs")

Deer %>% dplyr::select(contains("Density")) %>% names ->
  
  DensityCovar

AddCovar <- #Deer %>% #select(matches("DeadStrength|ShotStrength")) %>% names %>% 
  c(DensityCovar[2]) %>%
  c("ShotStrength") %>% 
  #c("Longevity") %>% 
  c("MCPArea", "HRA", "EarlyDistance", "UberDistance")

ClashList <- list(DensityCovar, c("MCPArea", "HRA"), c("EarlyDistance", "UberDistance"))

FullCovar <- c(Covar, AddCovar)

Cols <- c("Name", 
          Resps, 
          FullCovar,
          "Year", "PopN", 
          "E", "N")

TestHinds <- Deer %>% 
  #filter(MeshInclude == 1) %>%
  # filter(Age>5) %>% 
  dplyr::select(Cols) %>%
  mutate(
    fYear = as.factor(Year)
  )  %>% 
  mutate_at(AddCovar, ~log(.x + 1)) %>% 
  na.omit() %>% 
  droplevels %>%
  as.data.frame()

if(1){
  
  TestHinds %<>% 
    mutate_at("MCPArea", ~kader:::cuberoot(.x)) %>% 
    mutate_at("EarlyDistance", ~sqrt(.x)) %>% 
    mutate_at("HRA", ~log(.x))
  
}

if(0){
  
  TestHinds %<>% filter(ShotStrength<0.5)
  
  TestHinds %<>% 
    mutate_at(vars(contains("DeadStrength")), ~log(.x + 1)) %>% 
    mutate_at(vars(contains("ShotStrength")), ~log(.x + 1))
  
}

if(1){
  
  TestHinds %<>% 
    mutate_at(vars(contains("DeadStrength")), AsBinary) %>% 
    mutate_at(vars(contains("ShotStrength")), AsBinary)
  
}

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","E","N"))

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

SpocialList <- list()

a <- 1

for(a in a:length(Resps)){
  
  print(Resps[a])
  
  IM1 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar %>% c("UberDistance", "AnnualDensity", "HRA", "ShotStrength"),
    #Add = AddCovar,
    #AllModels = T,
    Random = c("Name", "fYear"), 
    RandomModel = rep("iid", 2),
    Clashes = ClashList,
    Family = "gaussian", Beep = F, 
    BaseModel = T,
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  SpocialList[[Resps[a]]] <- IM1
  
}

SpocialList %>% saveRDS(file = "Output Files/CombinedModels2.rds")

# 4_Social-Spatial Causality Plus Longevity ####

Resps = c("GroupSize", "Degree", "Strength")

Covar <- c("ReprodStatus", "Age", "Longevity",
           "Year", "PopN", "NObs")

Deer %>% dplyr::select(contains("Density")) %>% names ->
  
  DensityCovar

AddCovar <- #Deer %>% #select(matches("DeadStrength|ShotStrength")) %>% names %>% 
  c(DensityCovar[2]) %>%
  #c("Longevity") %>% 
  c("MCPArea", "HRA", "EarlyDistance", "UberDistance")

ClashList <- list(DensityCovar, c("MCPArea", "HRA"), c("EarlyDistance", "UberDistance"))

FullCovar <- c(Covar, AddCovar)

Cols <- c("Name", 
          Resps, 
          FullCovar,
          "Year", "PopN", 
          "E", "N")

TestHinds <- Deer %>% 
  #filter(MeshInclude == 1) %>%
  # filter(Age>5) %>% 
  dplyr::select(Cols) %>%
  mutate(
    fYear = as.factor(Year)
  )  %>% 
  mutate_at(AddCovar, ~log(.x + 1)) %>% 
  na.omit() %>% 
  droplevels %>%
  as.data.frame()

if(1){
  
  TestHinds %<>% 
    mutate_at("MCPArea", ~kader:::cuberoot(.x)) %>% 
    mutate_at("EarlyDistance", ~sqrt(.x)) %>% 
    mutate_at("HRA", ~log(.x))
  
}

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","E","N"))

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

SpocialList <- list()

a <- 1

for(a in a:length(Resps)){
  
  print(Resps[a])
  
  IM1 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar, # %>% 
    Add =  c("UberDistance", "AnnualDensity", "HRA"),
    #Add = AddCovar,
    #AllModels = T,
    Random = c("Name", "fYear"), 
    RandomModel = rep("iid", 2),
    Clashes = ClashList,
    Family = "gaussian", Beep = F, 
    BaseModel = T,
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  SpocialList[[Resps[a]]] <- IM1
  
}

SpocialList %>% saveRDS(file = "Output Files/CombinedModels3.rds")

# 4_Social-Spatial Causality 4 ####

Resps = c("GroupSize", "Degree", "Strength")

Covar <- c("ReprodStatus", "Age", "Longevity",
           "Year", "PopN", "NObs")

Deer %>% dplyr::select(contains("Density")) %>% names ->
  
  DensityCovar

AddCovar <- #Deer %>% #select(matches("DeadStrength|ShotStrength")) %>% names %>% 
  c(DensityCovar[2]) %>%
  c("ShotStrength", "DeadStrength") %>% 
  #c("Longevity") %>% 
  c("MCPArea", "HRA", "EarlyDistance", "UberDistance") %>% 
  c("GrazeType")

ClashList <- list(DensityCovar, c("MCPArea", "HRA"), c("EarlyDistance", "UberDistance"))

FullCovar <- c(Covar, AddCovar)

Cols <- c("Name", 
          Resps, 
          FullCovar,
          "Year", "PopN", 
          "E", "N")

TestHinds <- Deer %>% 
  #filter(MeshInclude == 1) %>%
  # filter(Age>5) %>% 
  dplyr::select(Cols) %>%
  mutate(
    fYear = as.factor(Year)
  )  %>% 
  mutate_at(AddCovar, ~log(.x + 1)) %>% 
  na.omit() %>% 
  droplevels %>%
  as.data.frame()

if(1){
  
  TestHinds %<>% 
    mutate_at("MCPArea", ~kader:::cuberoot(.x)) %>% 
    mutate_at("EarlyDistance", ~sqrt(.x)) %>% 
    mutate_at("HRA", ~log(.x))
  
}

if(0){
  
  TestHinds %<>% filter(ShotStrength<0.5)
  
  TestHinds %<>% 
    mutate_at(vars(contains("DeadStrength")), ~log(.x + 1)) %>% 
    mutate_at(vars(contains("ShotStrength")), ~log(.x + 1))
  
}

if(1){
  
  TestHinds %<>% 
    mutate_at(vars(contains("DeadStrength")), AsBinary) %>% 
    mutate_at(vars(contains("ShotStrength")), AsBinary)
  
}

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","E","N"))

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

SpocialList <- list()

a <- 1

for(a in a:length(Resps)){
  
  print(Resps[a])
  
  IM1 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar,
    Add =  c("UberDistance", "AnnualDensity", 
             "GrazeType",
             "HRA", "ShotStrength", "DeadStrength"),
    #AllModels = T,
    Random = c("Name", "fYear"), 
    RandomModel = rep("iid", 2),
    Clashes = ClashList,
    Family = "gaussian", Beep = F, 
    BaseModel = T,
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  SpocialList[[Resps[a]]] <- IM1
  
}

SpocialList %>% saveRDS(file = "Output Files/CombinedModels4.rds")

# 6a_Investigating pregnancy ####

Resps = c("GroupSize", "Degree", "Strength")

Covar <- c("ReprodStatus", "Age",
           "Year", "PopN") #, "NObs")

Deer %>% dplyr::select(contains("Density")) %>% names ->
  
  DensityCovar

AddCovar <- #Deer %>% #select(matches("DeadStrength|ShotStrength")) %>% names %>% 
  c(DensityCovar[2]) %>%
  #c("Longevity") %>% 
  c("MCPArea", "HRA", "EarlyDistance", "UberDistance")

ClashList <- list(DensityCovar, c("MCPArea", "HRA"), 
                  c("EarlyDistance", "UberDistance"),
                  Resps)

FullCovar <- c(Covar, AddCovar)

Cols <- c("Name", 
          Resps, 
          FullCovar,
          "Year", "PopN", 
          "E", "N")

TestHinds <- Deer %>% 
  #filter(Survived1 == 1) %>% 
  filter(MeshInclude == 1) %>%
  #filter(Age>5) %>% 
  dplyr::select(Cols, Pregnant) %>%
  mutate(
    Degree = as.vector(scale(sqrt(Degree))),
    Strength = as.vector(scale(sqrt(Strength))),
    GroupSize = as.vector(scale(kader:::cuberoot(GroupSize))),
    fYear = as.factor(Year)
  )  %>% 
  #mutate_at(AddCovar, ~log(.x + 1)) %>% 
  na.omit() %>% 
  droplevels %>%
  as.data.frame()

if(1){
  
  TestHinds %<>% 
    mutate_at("MCPArea", ~kader:::cuberoot(.x)) %>% 
    mutate_at("EarlyDistance", ~log(.x)) %>% 
    mutate_at("HRA", ~log(.x))
  
}

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","E","N", "Survived1", "Pregnant"))

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

FullAddCovar <- AddCovar %>% c(Resps)

#TestHinds %<>% 
#  group_by(Name) %>% 
#  mutate_at(FullAddCovar, ~ .x - mean(.x)) %>% 
#  ungroup

a <- 1

print("Pregnancy")

IM1 <- INLAModelAdd(
  
  Response = "Pregnant",
  Data = TestHinds,
  Explanatory = Covar,
  Add = FullAddCovar,
  #AllModels = T,
  Random = c("Name", "fYear"), 
  RandomModel = rep("iid", 2),
  Clashes = ClashList,
  Family = "binomial", 
  BaseModel = T,
  AddSpatial = T, Coordinates = c("E", "N")
  
)

IM1$FinalModel %>% Efxplot +
  IM1$Spatial$Model %>% Efxplot

IM1 %>% saveRDS("Output Files/PregnancyModels.rds")

list(IM1$FinalModel, IM1$Spatial$Model) %>% Efxplot +
  list(IM1$FinalModel, IM1$Spatial$Model) %>% INLADICFig

# 6b_Investigating pregnancy ####

AddCovar <- 
  c("MCPArea", "HRA", "EarlyDistance", "UberDistance") %>% 
  c(Resps) %>% 
  c("AnnualDensity")

AddCovar <- c(
  paste0(AddCovar, ".Mean.t0"),
  paste0(AddCovar, ".Within.t0"))

AddList <- 1:8 %>% map(~AddCovar[c(.x, .x + 8)])

ClashList <- list(Resps, 
                  c("MCPArea", "HRA"), 
                  #c("EarlyDistance", "UberDistance"),
                  "AnnualDensity") %>% 
  map(~c(paste0(.x, ".Mean.t0"), paste0(.x, ".Within.t0")))

FullCovar <- c(Covar, AddCovar)

Cols <- c("Name", 
          Resps, 
          FullCovar,
          "Year", "PopN", 
          "E", "N")

TestHinds <- Deer %>% 
  #filter(Survived1 == 1) %>% 
  filter(MeshInclude == 1) %>%
  #filter(Age>5) %>% 
  dplyr::select(Cols, Pregnant) %>%
  mutate(
    Degree = as.vector(scale(sqrt(Degree))),
    Strength = as.vector(scale(sqrt(Strength))),
    GroupSize = as.vector(scale(kader:::cuberoot(GroupSize))),
    fYear = as.factor(Year)
  )  %>% 
  #mutate_at(AddCovar, ~log(.x + 1)) %>% 
  na.omit() %>% 
  droplevels %>%
  as.data.frame()

if(0){
  
  TestHinds %<>% 
    mutate_at("MCPArea", ~kader:::cuberoot(.x)) %>% 
    mutate_at("EarlyDistance", ~log(.x)) %>% 
    mutate_at("HRA", ~log(.x))
  
}

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","E","N", "Survived1", "Pregnant"))

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

#TestHinds %<>% 
#  group_by(Name) %>% 
#  mutate_at(FullAddCovar, ~ .x - mean(.x)) %>% 
#  ungroup

a <- 1

print("Pregnancy")

IM1 <- INLAModelAdd(
  
  Response = "Pregnant",
  Data = TestHinds,
  Explanatory = Covar,
  Add = AddList,
  #AllModels = T,
  Random = c("Name", "fYear"), 
  RandomModel = rep("iid", 2),
  Clashes = ClashList,
  Family = "binomial", 
  BaseModel = T,
  AddSpatial = T, Coordinates = c("E", "N")
  
)

IM1$FinalModel %>% Efxplot +
  IM1$Spatial$Model %>% Efxplot

IM1 %>% saveRDS("Output Files/PregnancyModelsB.rds")

# 5_Grase Type ####

Covar <- c("ReprodStatus", #"Age"
           "Year", "NObs", 
           "PopN")

FullCovar <- c(Covar, "Age")

Cols <- c("Name", 
          FullCovar,
          "Year", "PopN", 
          "E", "N")

TestHinds <- Deer %>% 
  #filter(MeshInclude == 1) %>%
  dplyr::select(Cols, GrazeType) %>%
  mutate(
    #Degree = as.vector(scale(sqrt(Degree))),
    #Strength = as.vector(scale(sqrt(Strength))),
    #GroupSize = as.vector(scale(kader:::cuberoot(GroupSize))),
    fYear = as.factor(Year)
  )  %>% 
  na.omit() %>% 
  droplevels %>%
  as.data.frame()

TestHinds %>% nrow %>% print

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","E","N"))

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

IM1 <- INLAModelAdd(
    
    Response = "GrazeType",
    Data = TestHinds,
    Explanatory = FullCovar,
    #AllModels = T,
    Random = c("Name", "fYear"), 
    RandomModel = rep("iid", 2),
    Family = "gaussian", Beep = F, 
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
IM1 %>% saveRDS(file = "Output Files/GrazeModels1.rds")

# How does selective disappearance affect the models’ findings? ####

Covar <- c("ReprodStatus", "Age",
           "Year", "PopN", "NObs")

FullCovar <- c(Covar, "Longevity")

Cols <- c("Name", 
          FullCovar,
          "Year", "PopN", 
          "E", "N")

TestHinds <- Deer %>% 
  #  filter(MeshInclude == 1) %>%
  dplyr::select(Cols, GrazeType) %>%
  mutate(
    fYear = as.factor(Year)
  )  %>% 
  na.omit() %>% 
  droplevels %>%
  as.data.frame()

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","E","N"))

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

SelectiveList <- list()

  
  IM0 <- INLAModelAdd(
    
    Response = "GrazeType",
    Data = TestHinds,
    Explanatory = Covar,
    Random = c("fYear"), 
    RandomModel = rep("iid", 1),
    Family = "gaussian"
    
  )
  
  IM1 <- INLAModelAdd(
    
    Response = "GrazeType",
    Data = TestHinds,
    Explanatory = Covar,
    Random = c("Name", "fYear"), 
    RandomModel = rep("iid", 2),
    Family = "gaussian"
    
  )
  
  IM2 <- INLAModelAdd(
    
    Response = "GrazeType",
    Data = TestHinds,
    Explanatory = Covar %>% c("Longevity"),
    Random = c("Name", "fYear"), 
    RandomModel = rep("iid", 2),
    Family = "gaussian", Beep = F, 
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  SelectiveList[["GrazeType"]] <- list(IM0$FinalModel,
                                    IM1$FinalModel,
                                    IM2$FinalModel,
                                    IM2$Spatial$Model)
  
SelectiveList[[1]] %>% INLADICFig()
  
SelectiveList %>% saveRDS(file = "Output Files/SocialModels2.rds")
