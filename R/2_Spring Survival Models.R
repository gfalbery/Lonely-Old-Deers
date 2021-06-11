
# 5a_Investigating survival ####

library(tidyverse);library(reshape2);library(igraph); library(ggregplot); 
library(INLA); library(gsheet); library(magrittr); library(readxl); library(adehabitatHR)
library(cowplot)

theme_set(theme_cowplot())

Resps = c("GroupSize", "Degree", "Strength")

Covar <- c("ReprodStatus", "Age", "ReprodStatus.t0", #"NObs",
           "Year", "PopN")

SurvivalDeer %>% dplyr::select(contains("Density")) %>% names ->
  
  DensityCovar

AddCovar <- #Deer %>% #select(matches("DeadStrength|ShotStrength")) %>% names %>% 
  #c("Longevity") %>% 
  c("MCPArea", "HRA", "EarlyDistance", "UberDistance") %>% 
  c(Resps) %>% 
  c("AnnualDensity") %>% 
  c("GrazeType") %>% 
  paste0(., ".t0") %>%   
  #c(DensityCovar[c(2,4)]) %>%
  #c("EarlyDistance", "UberDistance") %>%
  intersect(names(Deer))

ClashList <- list(Resps, 
                  c("MCPArea", "HRA"), 
                  c("EarlyDistance", "UberDistance")) %>% 
  map(~paste0(.x, ".t0")) %>% 
  append(list(DensityCovar))

FullCovar <- c(Covar, AddCovar)

Cols <- c("Name", 
          #Resps, 
          FullCovar,
          "Year", "PopN", 
          "E", "N")

TestHinds <- 
  SurvivalDeer %>% 
  #filter(MeshInclude == 1) %>%
  #filter(Age>5) %>% 
  #filter(Survived0 == 1) %>% 
  #filter(!ReprodStatus == "Naive") %>% 
  filter(!Shot == 1) %>% 
  dplyr::select(Cols, Survived1) %>%
  mutate(
    fYear = as.factor(Year)
  )  %>% 
  #mutate_at(AddCovar, ~log(.x + 1)) %>% 
  na.omit() %>% 
  droplevels %>%
  as.data.frame()

TestHinds %>% nrow %>% print

if(1){
  
  TestHinds %<>% 
    mutate_at("MCPArea.t0", ~kader:::cuberoot(.x)) %>% 
    mutate_at("EarlyDistance.t0", ~log(.x + 1)) %>% 
    mutate_at("UberDistance.t0", ~log(.x + 1)) %>% 
    mutate_at(vars(contains("HRA.t0")), ~log(.x)) %>% 
    mutate_at(vars(contains("Degree")), ~sqrt(.x)) %>% 
    mutate_at(vars(contains("Strength")), ~sqrt(.x)) %>% 
    mutate_at(vars(contains("GroupSize")), ~kader:::cuberoot(.x))
  
}

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","E","N", "Survived1", "Survived2"))

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

print("Survival")

IM1 <- INLAModelAdd(
  
  Response = "Survived1",
  Data = TestHinds,
  Explanatory = Covar,
  Add = AddCovar,
  Random = c("Name", "fYear"), 
  RandomModel = rep("iid", 2),
  Clashes = ClashList,
  Family = "binomial", 
  BaseModel = T,
  AddSpatial = T, Coordinates = c("E", "N")
  
)

IM1$FinalModel %>% Efxplot +
  IM1$Spatial$Model %>% Efxplot

IM1 %>% saveRDS("Output Files/SurvivalModelsA.rds")

# Survival b_Within individual ####

Resps = c("GroupSize", "Degree", "Strength")

Covar <- c("ReprodStatus", "Age", "ReprodStatus.t0", #"NObs",
           "Year", "PopN")

SurvivalDeer %>% dplyr::select(contains("Density")) %>% names ->
  
  DensityCovar

AddCovar <- 
  c("MCPArea", "HRA", "EarlyDistance", "UberDistance") %>% 
  c(Resps) %>% 
  c("GrazeType") %>% 
  c("AnnualDensity")

AddCovar <- c(
  paste0(AddCovar, ".Mean.t0"),
  paste0(AddCovar, ".Within.t0"))

AddList <- 1:(length(AddCovar)/2) %>% map(~AddCovar[c(.x, .x + length(AddCovar)/2)])

ClashList <- list(Resps, 
                  c("MCPArea", "HRA"), 
                  c("EarlyDistance", "UberDistance"),
                  "AnnualDensity") %>% 
  map(~c(paste0(.x, ".Mean.t0"), paste0(.x, ".Within.t0")))

FullCovar <- c(Covar, AddCovar)

Cols <- c("Name", 
          #Resps, 
          FullCovar,
          "Year", "PopN", 
          "E", "N")

TestHinds <- SurvivalDeer %>% 
  #filter(MeshInclude == 1) %>%
  #filter(Age>5) %>% 
  #filter(Survived0 == 1) %>% 
  #filter(!ReprodStatus == "Naive") %>% 
  filter(!Shot == 1) %>% 
  dplyr::select(Cols, Survived1) %>%
  mutate(
    fYear = as.factor(Year)
  )  %>% 
  #mutate_at(AddCovar, ~log(.x + 1)) %>% 
  na.omit() %>% 
  droplevels %>%
  as.data.frame()

TestHinds %>% nrow %>% print

if(0){
  
  TestHinds %<>% 
    mutate_at("MCPArea.t0", ~kader:::cuberoot(.x)) %>% 
    mutate_at("EarlyDistance.t0", ~log(.x + 1)) %>% 
    mutate_at("UberDistance.t0", ~log(.x + 1)) %>% 
    mutate_at(vars(contains("HRA.t0")), ~log(.x)) %>% 
    mutate_at(vars(contains("Degree")), ~sqrt(.x)) %>% 
    mutate_at(vars(contains("Strength")), ~sqrt(.x)) %>% 
    mutate_at(vars(contains("GroupSize")), ~kader:::cuberoot(.x))
  
}

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","E","N", "Survived1", "Survived2"))

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

if(0){
  
  TestHinds %<>% 
    group_by(Name) %>% 
    mutate_at(AddCovar, ~ .x - mean(.x)) %>% 
    ungroup
  
  TestHinds %<>% 
    filter_at(AddCovar, ~between(.x, -2, 2))
  
}

print("Survival")

IM1 <- INLAModelAdd(
  
  Response = "Survived1",
  Data = TestHinds,
  Explanatory = Covar,
  Add = AddList[c(1,2,5:9)],
  Random = c("Name", "fYear"), 
  RandomModel = rep("iid", 2),
  Clashes = ClashList,
  Family = "binomial", 
  BaseModel = T,
  AddSpatial = T, Coordinates = c("E", "N")
  
)

IM1$FinalModel %>% Efxplot +
  IM1$Spatial$Model %>% Efxplot

IM1 %>% saveRDS("Output Files/SurvivalModelsB.rds")

# Multivariate ####

library(MCMCglmm)

Resps = c("GroupSize", "Degree", "Strength")

Covar <- c("ReprodStatus", "Age", "ReprodStatus.t0",
           "Year", "PopN", "NObs")

DensityCovar <- Deer %>% dplyr::select(contains("Density")) %>% names

AddCovar <- 
  Deer %>% dplyr::select(HRA.t0, AnnualDensity.t0, EarlyDistance.t0, Degree.t0) %>% 
  names

ClashList <- list(DensityCovar, c("MCPArea", "HRA"))

FullCovar <- c(Covar, AddCovar)

Cols <- c("Name", 
          #Resps, 
          FullCovar,
          "Year", "PopN", 
          "E", "N")

TestHinds <- SurvivalDeer %>% 
  filter(MeshInclude == 1) %>%
  filter(Age>5) %>% 
  dplyr::select(Cols, Survived1) %>%
  mutate(
    #Degree = as.vector(scale(sqrt(Degree))),
    #Strength = as.vector(scale(sqrt(Strength))),
    #GroupSize = as.vector(scale(kader:::cuberoot(GroupSize))),
    fYear = as.factor(Year)
  )  %>% 
  #mutate_at(AddCovar, ~log(.x + 1)) %>% 
  na.omit() %>% 
  droplevels %>%
  as.data.frame()

if(1){
  
  TestHinds %<>% 
    #mutate_at("MCPArea", ~kader:::cuberoot(.x)) %>% 
    mutate_at("EarlyDistance.t0", ~sqrt(.x)) %>% 
    mutate_at("HRA.t0", ~log(.x))
  
}

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","E","N", "Survived1"))

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

SpocialList <- list()

a <- 1

MultivResps <- c("HRA.t0", "AnnualDensity.t0", "EarlyDistance.t0", 
                 #"GroupSize", "Strength",
                 "Degree.t0", "Survived1")

N <- length(MultivResps)

Prior <- 
  list(R = list(V = diag(N), nu = N, fix = N), 
       G = list(G1 = list(V = diag(N), nu = N,
                          alpha.mu = rep(0, N),
                          alpha.V = diag(N)*1),
                G2 = list(V = diag(N), nu = N,
                          alpha.mu = rep(0, N),
                          alpha.V = diag(N)*1)))

Iterations <- 11000
Burnin <- 1000
Thin <- 10

Multiplier <- 2

M1 <-
  MCMCglmm(data = TestHinds, 
           #cbind(HRA, AnnualDensity, EarlyDistance, GroupSize, Strength, Degree, Survived1) ~ trait - 1 + 
           cbind(HRA.t0, AnnualDensity.t0, EarlyDistance.t0, Degree.t0, Survived1) ~ trait - 1 + 
             trait:(ReprodStatus + ReprodStatus.t0 + Age + Year + PopN + NObs),
           random =~ us(trait):Name + us(trait):fYear,
           prior = Prior,
           nitt = Iterations*Multiplier, burnin = Burnin*Multiplier, thin = Thin*Multiplier,
           family = rep("gaussian", N-1) %>% c("categorical"), 
           rcov =~ us(trait):units)

M1 %>% saveRDS("Senescence Code/Output Files/MultivariateSurvivalModel.rds")

M1 %>% PhenCorr(3)

M1 %>% PhenCorr(3) %>% 
  rownames_to_column() %>% 
  ggplot(aes(rowname, Mode)) + 
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.3) + 
  geom_point() + coord_flip()
