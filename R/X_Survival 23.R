# 5a_Investigating survival ####

Resps = c("GroupSize", "Degree", "Strength")

Covar <- c("ReprodStatus", "Age",
           "Year", "PopN") #, "NObs")

Deer %>% dplyr::select(contains("Density")) %>% names ->
  
  DensityCovar

AddCovar <- #Deer %>% #select(matches("DeadStrength|ShotStrength")) %>% names %>% 
  c(DensityCovar[2]) %>%
  #c("Longevity") %>% 
  c("MCPArea", "HRA", "EarlyDistance", "UberDistance")# %>% setdiff("EarlyDistance")

ClashList <- list(Resps, 
                  DensityCovar, 
                  c("MCPArea", "HRA"), 
                  c("EarlyDistance", "UberDistance"))

FullCovar <- c(Covar, AddCovar)

Cols <- c("Name", 
          Resps, 
          FullCovar,
          "Year", "PopN", 
          "E", "N")

TestHinds <- Deer %>% 
  #filter(MeshInclude == 1) %>%
  #filter(Age>5) %>% 
  filter(Survived0 == 1) %>% 
  filter(!ReprodStatus == "Naive") %>% 
  filter(!Shot == 1) %>% 
  dplyr::select(Cols, Survived2) %>%
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

TestHinds %>% nrow %>% print

if(1){
  
  TestHinds %<>% 
    mutate_at("MCPArea", ~kader:::cuberoot(.x)) %>% 
    mutate_at("EarlyDistance", ~log(.x)) %>% 
    mutate_at("HRA", ~log(.x))
  
}

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","E","N", "Survived2"))

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

FullAddCovar <- AddCovar %>% c(Resps)

a <- 1

IM1 <- INLAModelAdd(
  
  Response = "Survived2",
  Data = TestHinds,
  Explanatory = Covar,
  Add = FullAddCovar,
  #AllModels = T,
  Random = c("Name", "fYear"), 
  RandomModel = rep("iid", 2),
  Clashes = ClashList,
  Family = "binomial", 
  #BaseModel = T,
  AddSpatial = T, Coordinates = c("E", "N")
  
)

IM1$FinalModel %>% Efxplot
IM1$Spatial$Model %>% Efxplot

IM1 %>% saveRDS("Output Files/SurvivalModels2.rds")

list(IM1$FinalModel, IM1$Spatial$Model) %>% Efxplot +
  list(IM1$FinalModel, IM1$Spatial$Model) %>% INLADICFig

# 5b_Investigating survival with deviance ####

Resps = c("GroupSize", "Degree", "Strength")

Covar <- c("ReprodStatus", "Age",
           "Year", "PopN") #, "NObs")

Deer %>% dplyr::select(contains("Density")) %>% names ->
  
  DensityCovar

AddCovar <- #Deer %>% #select(matches("DeadStrength|ShotStrength")) %>% names %>% 
  c(DensityCovar[2]) %>%
  #c("Longevity") %>% 
  c("MCPArea", "HRA", "EarlyDistance", "UberDistance")# %>% setdiff("EarlyDistance")

ClashList <- list(Resps, 
                  DensityCovar, 
                  c("MCPArea", "HRA"), 
                  c("EarlyDistance", "UberDistance"))

FullCovar <- c(Covar, AddCovar)

Cols <- c("Name", 
          Resps, 
          FullCovar,
          "Year", "PopN", 
          "E", "N")

TestHinds <- Deer %>% 
  #filter(MeshInclude == 1) %>%
  #filter(Age>5) %>% 
  filter(Survived0 == 1) %>% 
  filter(!ReprodStatus == "Naive") %>% 
  filter(!Shot == 1) %>% 
  dplyr::select(Cols, Survived2) %>%
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

TestHinds %>% nrow %>% print

if(1){
  
  TestHinds %<>% 
    mutate_at("MCPArea", ~kader:::cuberoot(.x)) %>% 
    mutate_at("EarlyDistance", ~log(.x)) %>% 
    mutate_at("UberDistance", ~log(.x)) %>% 
    mutate_at("HRA", ~log(.x))
  
}

Classes <- TestHinds %>% sapply(class)

ToScale <- names(Classes[Classes %in% c("integer", "numeric")]) %>% 
  setdiff(c("Eigenvector","Eigenvector2","E","N", "Survived2"))

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

FullAddCovar <- AddCovar %>% c(Resps)

TestHinds %<>% 
  group_by(Name) %>% 
  mutate_at(FullAddCovar, ~ .x - mean(.x)) %>% 
  ungroup

a <- 1

print(Resps[a])

IM1 <- INLAModelAdd(
  
  Response = "Survived2",
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

IM1 %>% saveRDS("Output Files/SurvivalModels2b.rds")

list(IM1$FinalModel, IM1$Spatial$Model) %>% Efxplot +
  list(IM1$FinalModel, IM1$Spatial$Model) %>% INLADICFig

IM1 <- INLAModelAdd(
  
  Response = "Survived2",
  Data = TestHinds,
  Explanatory = Covar %>% c("AnnualDensity", "HRA", "UberDistance"),
  #Add = FullAddCovar,
  #AllModels = T,
  Random = c("Name", "fYear"), 
  RandomModel = rep("iid", 2),
  Clashes = ClashList,
  Family = "zeroinflatedbinomial1", 
  BaseModel = T,
  AddSpatial = T, Coordinates = c("E", "N")
  
)

IM1 %>% saveRDS("Output Files/SurvivalModels2ZI.rds")
