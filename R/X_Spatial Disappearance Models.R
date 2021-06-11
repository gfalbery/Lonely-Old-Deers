# 2	Spatial Selective Disappearance Models 	####

# Do individuals move to areas of lower density as they age? ####

Deer %>% 
  dplyr::select(contains("Density")) %>%
  dplyr::select(-contains("t0")) %>%
  
  dplyr::select(1:2) %>% 
  
  names ->
  
  DensityCovar

Covar <- c("ReprodStatus", #"Age"
           "Year", "PopN", "NObs")

FullCovar <- c(Covar, "Age")

Cols <- c("Name", 
          DensityCovar, 
          FullCovar, "Longevity",
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

TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))

DensityList <- list()

a <- 1

for(a in a:length(DensityCovar)){
  
  print(DensityCovar[a])
  
  IM1 <- INLAModelAdd(
    
    Response = DensityCovar[a],
    Data = TestHinds,
    Explanatory = FullCovar,
    Add = c("f(Name, model = 'iid')", "Longevity"),
    AllModels = T,
    Random = c("Name", "fYear")[2], 
    RandomModel = rep("iid", 2)[1],
    Family = "gaussian", 
    Delta = -Inf,
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  DensityList[[DensityCovar[a]]] <- IM1
  
}

DensityList %>% saveRDS(file = "Output Files/SelectiveSpatialModels1.rds")

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
            Covar, "Longevity",
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
  
  TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))
  
  IMList <- list()
  
  a = 1
  
  IM1 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar,
    Add = c("f(Name, model = 'iid')", "Longevity"),
    AllModels = T,
    Random = c("Name", "fYear")[2], 
    RandomModel = rep("iid", 2)[2],
    Delta = -Inf,
    Family = "gaussian", 
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  IM1$FinalModel %>% Efxplot
  
  IMList[[Resps[a]]] <- IM1
  
  a = 2
  
  IM1 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar %>% c("AnnualDistance"),
    Add = c("f(Name, model = 'iid')", "Longevity"),
    AllModels = T,
    Random = c("Name", "fYear")[2], 
    RandomModel = rep("iid", 2)[2],
    Delta = -Inf,
    Family = "gaussian", 
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  IM1$FinalModel %>% Efxplot
  
  IMList[[Resps[a]]] <- IM1
  
  a = 3
  
  IM1 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar %>% c("AnnualDistance"),
    Add = c("f(Name, model = 'iid')", "Longevity"),
    AllModels = T,
    Random = c("Name", "fYear")[2], 
    RandomModel = rep("iid", 2)[2],
    Delta = -Inf,
    Family = "gaussian", 
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
    Add = c("f(Name, model = 'iid')", "Longevity"),
    AllModels = T,
    Random = c("Name", "fYear")[2], 
    RandomModel = rep("iid", 2)[2],
    Family = "gaussian", 
    Delta = -Inf,
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  IM1$Spatial$Model %>% Efxplot
  IM1$FinalModel %>% Efxplot
  
  IMList[[Resps[a]]] <- IM1
  
  IMList %>% map(c("Spatial", "Model")) %>% Efxplot() +
    IMList %>% map(c("FinalModel")) %>% Efxplot()
  
}

saveRDS(IMList, file = "Output Files/SelectiveSpatialModels2.rds")

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
            Covar, "Longevity",
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
  
  TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))
  
  IM1 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar %>% setdiff("NObs"),
    Add = c("f(Name, model = 'iid')", "Longevity"),
    AllModels = T,
    Random = c("Name", "fYear")[2], 
    RandomModel = rep("iid", 2)[2],
    Delta = -Inf,
    Family = "gaussian", 
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  IM1$FinalModel %>% Efxplot
  
  IMList[[Resps[a]]] <- IM1
  
}

IMList %>% map(c("FinalModel")) %>% Efxplot() +
  IMList %>% map(c("Spatial", "Model")) %>% Efxplot()

saveRDS(IMList, file = "Output Files/SelectiveSpatialModels3.rds")

# Does graze quality decrease? ####

Resps = "GrazeType"

Covar <- c("ReprodStatus", "Age",
           "Year", "PopN", "NObs")

IMList <- list()

a <- 1

for(a in a:length(Resps)){
  
  print(Resps[a])
  
  Cols <- c("Name", 
            Resps[a], 
            Covar, "Longevity",
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
  
  TestHinds %<>% mutate_at(ToScale, ~c(scale(.x)))
  
  IM1 <- INLAModelAdd(
    
    Response = Resps[a],
    Data = TestHinds,
    Explanatory = Covar %>% setdiff("NObs"),
    Add = c("f(Name, model = 'iid')", "Longevity"),
    AllModels = T,
    Random = c("Name", "fYear")[2], 
    RandomModel = rep("iid", 2)[2],
    Delta = -Inf,
    Family = "gaussian", 
    AddSpatial = T, Coordinates = c("E", "N")
    
  )
  
  IM1$FinalModel %>% Efxplot
  
  IMList[[Resps[a]]] <- IM1
  
}

# IMList %>% map(c("FinalModel")) %>% Efxplot() +
#   IMList %>% map(c("Spatial", "Model")) %>% Efxplot()

saveRDS(IMList, file = "Output Files/SelectiveSpatialModels4.rds")
