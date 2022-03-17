
# Deer Phenotyping ####

library(tidyverse);library(reshape2);library(igraph); library(ggregplot); library(INLA); library(gsheet); library(magrittr)
library(readxl); library(adehabitatHR)

"Data/Phenotypes" %>% 
  list.files(full.names = T) %>% 
  map(read_xlsx) ->
  PhenotypeList

names(PhenotypeList) <- "Data/Phenotypes" %>% list.files

# Get these directly from Deer Main access file
Individuals <- PhenotypeList$tblLife
Names <- PhenotypeList$tblNames.xlsx

# Get these from queries
HindStatus <- PhenotypeList$HindStatus.xlsx
LBS <- PhenotypeList$LBS.xlsx
PopSize <- PhenotypeList$PopSize.xlsx
BirthWts <- PhenotypeList$BirthWt.xlsx

BirthDates <- Individuals %>% 
  mutate_at(vars(BirthDay:DeathYear), 
            ~factor(as.factor(.x),levels = c(0:10000, paste(0, 1:10, sep=""))) %>% 
              str_pad(2, "left", 0)) %>%
  mutate_at(c("BirthYear", "DeathYear"), as.numeric) %>% 
  mutate(BirthDate = glue::glue("{BirthDay}/{BirthMonth}/{BirthYear}") %>% lubridate::dmy()) %>% 
  group_by(BirthYear) %>% 
  mutate(cBirthDate = BirthDate - median(BirthDate, na.rm = T)) %>% 
  ungroup %>% 
  dplyr::select(Code, BirthDate, cBirthDate)

Individuals %<>% 
  left_join(Names, by = "Code") %>% 
  mutate_at(vars(contains("Name")), as.character) %>% 
  mutate_at("GivenName", ~ifelse(is.na(.x), FamilyName, .x)) %>% 
  mutate(Animal = GivenName) %>% # Dates
  mutate_at("Sex", ~cut(.x,
                        breaks = c(0:3 + 0.5),
                        labels = c("F","M","3")))

Individuals %<>% rename(DeathStatus = Status)

# Getting every individual deer:year combo ####

DeerNames <- unique(Individuals$Code)

Censuses <- read.csv(paste0("Data/Behaviour/FullCensuses.csv"))

Censuses %>% separate(Date, "/", into = c("Day", "Month", "Year")) %>% 
  dplyr::select(c("Day", "Month", "Year")) %>% 
  mutate_all(as.numeric) ->
  Censuses[,c("Day", "Month", "Year")]

Censuses %<>% mutate(DeerYear = ifelse(Month<5, Year - 1, Year))

Censuses %>% 
  filter(Code %in% DeerNames) %>% 
  dplyr::select(Name = Code, Year = DeerYear) %>% unique %>% 
  arrange(Name, Year) -> IDYearDF

IDYearDF %<>% left_join(Individuals, by = c("Name" = "Code"))

IDYearDF %<>% 
  mutate(Age = Year - BirthYear) %>% 
  mutate(Hind = as.numeric(Age>2 & Sex == "F")) %>% 
  mutate(AgeCat = cut(Age, breaks = c(0, 0.5, 1.5, 2.5, Inf), 
                      labels = c("C", "Y", "2Y", "A"), include.lowest = T))

IDYearDF %<>% filter(Age > -1)

# Reproductive Status ####

HindStatus %<>% arrange(Female, DeerYear) %>% 
  mutate(
    
    Reprod = as.numeric(!ReprodStatus %in% c("Naive", "True.Yeld", NA)),
    
    Winter = as.numeric(ReprodStatus %in% c("Winter.Yeld", "Milk"))
    
  ) %>% 
  group_by(Female) %>% 
  mutate(
    
    CumulativeReprod = cumsum(Reprod),
    CumulativeWinter = cumsum(Winter)
    
  ) %>% ungroup

HindStatus %>% 
  dplyr::select(Female, ReprodStatus, DeerYear, Calf, CumulativeReprod, CumulativeWinter) %>% 
  mutate_at("ReprodStatus", ~.x %>% str_trim %>% 
              str_replace_all(c(" " = ".",
                                "Naïve" = "Naive",
                                "yeld" = "Yeld"))) %>% 
  left_join(IDYearDF, ., by = c("Name" = "Female", "Year" = "DeerYear")) ->
  IDYearDF

IDYearDF %>% 
  filter(ReprodStatus %in% c("Winter.Yeld", "Summer.Yeld", "Milk")) %>% 
  group_by(Name) %>% filter(Age == min(Age)) %>% 
  dplyr::select(Name, AgeAtFirst = Age) %>% 
  left_join(IDYearDF, .) %>% 
  mutate_at("ReprodStatus", ~ifelse(is.na(.x) & Age < AgeAtFirst, "Naive", .x)) %>% 
  mutate_at("ReprodStatus", ~ifelse(is.na(.x), "True.Yeld", .x)) %>% 
  mutate_at("ReprodStatus", ~ifelse(Hind == 1, .x, NA)) ->
  IDYearDF

IDYearDF %<>%
  mutate(Status = 
           
           case_when(
             
             Age == 0 ~ "Calf",
             Age == 1 ~ "Yearling", 
             Age == 2 ~ "2Y",
             Age>2 & Sex == "M" ~ "Stag", 
             TRUE ~ as.character(ReprodStatus)
             
           )
  )

IDYearDF %<>% 
  left_join(PopSize, 
            by = c("Year" = "DeerYear")) %>% 
  left_join(PopSize %>% mutate_at("DeerYear", ~.x + 1), 
            by = c("Year" = "DeerYear"), suffix = c("", ".t0")) %>% 
  left_join(LBS[,c("Code", "LRS","LBS")], by = c("Name" = "Code"))

IDYearDF %>% 
  dplyr::select(Name, Year, ReprodStatus, Age) %>% 
  mutate_at(c("Year", "Age"), ~.x + 1) %>% 
  left_join(IDYearDF, ., by = c("MumCode" = "Name", "BirthYear" = "Year"),
            suffix = c("", ".Mum")) ->
  IDYearDF

ReprodReplace <- c("None", "None", "Summer", "Winter", "Winter")

names(ReprodReplace) <- c("Naive", "True.Yeld", "Summer.Yeld", "Winter.Yeld", "Milk")

IDYearDF %<>% mutate_at("ReprodStatus", ~.x %>% 
                          str_replace_all(c(ReprodReplace)))

# Calf Traits ####

IDYearDF %<>% 
  left_join(BirthWts %>% dplyr::select(Code, BirthWt), 
            by = c("Name" = "Code")) %>% 
  left_join(BirthWts %>% dplyr::select(Code, BirthWt), 
            by = c("Calf" = "Code"),
            suffix = c("", ".Calf")) %>% 
  left_join(BirthDates, by = c("Name" = "Code")) %>% 
  left_join(BirthDates, by = c("Calf" = "Code"),
            suffix = c("", ".Calf")) %>% 
  left_join(Individuals %>% dplyr::select(Calf = Code, Sex),
            suffix = c("", ".Calf"), by = c("Calf")) ->
  
  IDYearDF

# Subsequent reproduction ####

ReprodCovar <- c("BirthWt", "cBirthDate") %>% paste0(".Calf") %>% c("ReprodStatus")

IDYearDF %>% 
  mutate_at("Year", ~.x - 1) %>% 
  dplyr::select(Name, Year, all_of(ReprodCovar)) %>% 
  left_join(IDYearDF, ., suffix = c("", ".t2"), by = c("Name", "Year")) ->
  IDYearDF

IDYearDF %>% 
  mutate_at("Year", ~.x + 1) %>% 
  dplyr::select(Name, Year, all_of(ReprodCovar)) %>% 
  left_join(IDYearDF, ., suffix = c("", ".t0"), by = c("Name", "Year")) ->
  IDYearDF

IDYearDF %<>% mutate_at("ReprodStatus.t0", ~str_replace_all(.x, "Summer", "None"))

CurrentYear <- max(Censuses$DeerYear, na.rm = T)

IDYearDF %<>% 
  mutate(
    Pregnant = as.numeric(Hind == 1 & 
                            ReprodStatus.t2 %in% 
                            c("Summer.Yeld", "Winter.Yeld", "Milk", "Summer", "Winter"))
  )

#IDYearDF %>% 
#  mutate_at(vars(contains("ReprodStatus")), 
#            ~factor(.x, levels = c("Naive","True.Yeld","Summer.Yeld","Winter.Yeld","Milk"))) ->
#  
#  IDYearDF

IDYearDF %>% group_by(Name) %>% 
  summarise(Longevity = max(Age, na.rm = T)) %>% 
  left_join(IDYearDF, .) -> IDYearDF

IDYearDF %<>% mutate_at("Longevity", ~ifelse(DeathStatus == "D", .x, NA))

# Survival ####

HindStatus %>% 
  dplyr::select(Name = Calf, CalfSurvival = ReprodStatus) %>% 
  left_join(IDYearDF, .) ->
  IDYearDF

Censuses %>% 
  filter(Code %in% DeerNames) %>% 
  mutate_at("Date", ~lubridate::dmy(.x)) %>% 
  arrange(Date) %>% group_by(Code) %>% 
  slice(n()) %>% 
  dplyr::select(Name = Code, 
                Date, Day, Month, Year, DeerYear) %>% 
  rename_at(c("Date", "Day", "Month", "Year", "DeerYear"), 
            ~paste0("LastSeen.", .x)) -> 
  LastSeenDates

LastSeenDates %<>% 
  left_join(Individuals %>% dplyr::select(Code, DeathDay, DeathMonth, DeathYear), 
            by = c("Name" = "Code")) %>% 
  mutate_at("DeathDay", ~ifelse(is.na(.x), 01, .x)) %>% 
  mutate_at(c("DeathDay", "DeathMonth"), ~str_pad(.x, 2, "left", "0")) %>% 
  mutate(DeathDate = lubridate::ymd(paste0(DeathYear, DeathMonth, DeathDay, sep = "/"))) %>% 
  mutate_at("DeathYear", ~ifelse(as.numeric(DeathMonth) < 5, .x - 1, .x))

LastSeenDates %<>% 
  mutate_at("LastSeen.DeerYear", ~ifelse(DeathYear > .x, DeathYear, .x))

CurrentYear <- max(Censuses$DeerYear, na.rm = T)

LastSeenDates %>% 
  dplyr::select(Name, LastSeen.Date, LastSeen.Year, LastSeen.DeerYear) %>% 
  left_join(IDYearDF, .) ->
  IDYearDF

IDYearDF %>% 
  mutate(Survived0 = 1 - as.numeric(LastSeen.DeerYear == Year)) %>% 
  mutate(Survived1 = as.numeric(LastSeen.DeerYear > Year)) %>% 
  mutate(Survived2 = as.numeric(LastSeen.DeerYear > Year + 1)) %>% 
  mutate(Survived3 = as.numeric(LastSeen.DeerYear > Year + 1)) ->
  IDYearDF

IDYearDF %<>% 
  mutate_at(vars(contains("Survived")),
            ~ifelse(DeathStatus == "L", 1, .x))

IDYearDF %>% 
  mutate_at(
    "Survived0", ~ifelse(Year == CurrentYear, NA, .x)
  ) %>% 
  mutate_at(
    "Survived1", ~ifelse(Year == CurrentYear, NA, .x)
  ) %>% 
  mutate_at(
    "Survived2", ~ifelse(Year >= CurrentYear - 1, NA, .x)
  ) ->
  IDYearDF

if("Spring" %in% FocalSeason){
  
  IDYearDF$Survived0 <- IDYearDF$Survived1
  
}

IDYearDF %<>% 
  mutate(Shot = 1 - as.numeric(DeathType != "S" | is.na(DeathType)))

IDYearDF %<>% 
  mutate_at("Pregnant", 
            ~ifelse((Year == CurrentYear)|
                      (Survived1 == 0), NA, .x))

Deer <- IDYearDF

# Attaching Social Data ####

Deer %>% arrange(Year) %>% pull(Year) %>% unique %>% as.character -> FocalYears

FocalYears %<>% c(min(as.numeric(FocalYears))-1, .) %>% as.numeric

source("R/0a_Social Network Setup.R")

Deer %<>% left_join(FullSociality, by = c("Name", "Year"))

Deer %<>% mutate(MeshInclude = MeshInclude(E, N, 1355, 1384.777, 7997.5, 8050))

# Lifetime Density ####

Censuses %>% 
  filter(DeerYear %in% FocalYears) %>% 
  group_by(Code) %>% 
  summarise_at(c("Easting", "Northing"), ~mean(.x, na.rm = T)) ->
  LifetimeCentroids

SPDF <- SpatialPointsDataFrame(data = LifetimeCentroids[,c("Easting", "Northing")], 
                               coords = LifetimeCentroids[,c("Easting", "Northing")])

LifetimeKUDL <- kernelUD(SPDF, same4all = TRUE, grid = 500)

LifetimeKUDL %>% raster::raster() %>% raster::extract(Deer[,c("E", "N")]) ->
  
  Deer$LifetimeDensity

# Annual Density ####

Deer %>% arrange(Year) %>% pull(Year) %>% unique %>% as.character -> FocalYears

# FocalYears %<>% c(min(as.numeric(FocalYears))-1, .)

Censuses %>% 
  filter(DeerYear %in% FocalYears) %>% 
  group_by(Code, DeerYear) %>% 
  summarise_at(c("Easting", "Northing"), 
               ~mean(.x, na.rm = T)) %>% 
  rename(XCentroidAnnual = Easting, YCentroidAnnual = Northing) -> 
  
  AnnualCentroids

AnnualCentroids %<>% filter(DeerYear %in% FocalYears)

FocalYears %<>% intersect(AnnualCentroids$DeerYear) %>% as.character

SPDF <- SpatialPointsDataFrame(data = AnnualCentroids[,c("XCentroidAnnual", "YCentroidAnnual", "DeerYear")], 
                               coords = AnnualCentroids[,c("XCentroidAnnual", "YCentroidAnnual")])

SPDF <- SPDF[,"DeerYear"]

KUDL <- kernelUD(SPDF, same4all=TRUE, grid=500)

2:length(FocalYears) %>% lapply(function(a){
  
  print(FocalYears[a])
  
  DF <- Deer %>% filter(Year == FocalYears[a])
  
  KUDL2 <- KUDL[[FocalYears[a]]]
  
  KUDL2 %>% raster::raster() %>% raster::extract(DF[,c("E", "N")]) ->
    
    DF$AnnualDensity
  
  return(DF)
  
}) -> DensityList

DensityList %>% bind_rows -> Deer

Deer %<>% 
  group_by(Year) %>% 
  mutate_at(vars(contains("AnnualDensity")), 
            ~scales::rescale(.x, c(0, 1))) %>% 
  ungroup

# Sighting densities ####

if(0){
  
  Censuses2 <- Censuses %>% filter(DeerYear %in% FocalYears)
  
  SPDF <- SpatialPointsDataFrame(data = Censuses2[,c("Easting", "Northing")], 
                                 coords = Censuses2[,c("Easting", "Northing")])
  
  KUDL <- kernelUD(SPDF, same4all = TRUE, grid = 500)
  
  KUDL %>% raster::raster() %>% raster::extract(Deer[,c("E", "N")]) ->
    
    Deer$SightingDensity
  
  SPDF <- SpatialPointsDataFrame(data = Censuses2[,c("Easting", "Northing", "Year")], 
                                 coords = Censuses2[,c("Easting", "Northing")])
  
  SPDF <- SPDF[,"Year"]
  
  KUDL <- kernelUD(SPDF, same4all=TRUE, grid=500)
  
  1:length(FocalYears) %>% lapply(function(a){
    
    print(FocalYears[a])
    
    DF <- Deer %>% filter(Year == FocalYears[a])
    
    KUDL2 <- KUDL[[FocalYears[a]]]
    
    KUDL2 %>% raster::raster() %>% raster::extract(DF[,c("E", "N")]) ->
      
      DF$AnnualSightingDensity
    
    return(DF)
    
  }) -> DensityList
  
  DensityList %>% bind_rows -> Deer
  
}

# Adding centroid distances ####

Censuses %>% 
  group_by(Code) %>% 
  summarise(Lifetime.E = mean(Easting, na.rm = T),
            Lifetime.N = mean(Northing, na.rm = T)) %>% 
  left_join(Deer, ., by = c("Name" = "Code")) %>% 
  left_join(Deer %>% 
              mutate(Year2 = Year + 1) %>% 
              dplyr::select(Name, Year2, E.t0 = E, N.t0 = N) %>% unique, 
            by = c("Name", "Year" = "Year2")) ->
  
  Deer

Censuses %>% ungroup %>% 
  filter(Age<6) %>% 
  group_by(Code) %>% 
  summarise(Early.E = mean(Easting, na.rm = T),
            Early.N = mean(Northing, na.rm = T)) %>% 
  left_join(Deer, ., by = c("Name" = "Code")) -> 
  
  Deer

Deer %>% Pythagoreg(X = c("E", "E.t0"), Y = c("N", "N.t0")) -> 
  Deer$AnnualDistance

Deer %>% 
  arrange(Name, Year) %>% ungroup %>% 
  group_by(Name) %>% mutate(YearDiff = c(NA, diff(Year))) %>% 
  ungroup ->
  Deer

Deer %<>% 
  mutate_at("AnnualDistance", ~ifelse(is.na(YearDiff)|(YearDiff>1), NA, .x))

Deer %>% Pythagoreg(X = c("E", "Lifetime.E"), Y = c("N", "Lifetime.N")) -> 
  Deer$LifetimeDistance

Deer %>% Pythagoreg(X = c("E", "Early.E"), Y = c("N", "Early.N")) -> 
  Deer$EarlyDistance

UberCentroid <- Censuses %>% 
  group_by(Code, DeerYear) %>% 
  summarise_at(c("Easting", "Northing"), ~mean(.x, na.rm = T)) %>% 
  ungroup %>% 
  summarise_at(c("Easting", "Northing"), ~mean(.x, na.rm = T))

Deer %>% 
  mutate(Uber.E = UberCentroid$Easting, Uber.N = UberCentroid$Northing) %>% 
  Pythagoreg(X = c("E", "Uber.E"), Y = c("N", "Uber.N")) -> 
  Deer$UberDistance

# Adding Home range models ####

if(!file.exists(paste0("Data/", 
                       paste0(FocalSeason, collapse = "_"), 
                       "MovementLists.rds"))){
  
  source("R/0c_Home Range Setup")
  
}

if(file.exists(paste0("Data/", 
                      paste0(FocalSeason, collapse = "_"), 
                      "MovementLists.rds"))){
  
  MovementLists <- readRDS(paste0("Data/", 
                                  paste0(FocalSeason, collapse = "_"), 
                                  "MovementLists.rds"))
  
  MovementLists[[1]] %>% bind_rows(.id = "Name") %>% melt %>% 
    filter(!is.na(value)) %>% 
    rename(HRA = value) %>% 
    mutate(Year = variable %>% str_remove("X") %>% as.numeric) %>% 
    dplyr::select(Name, Year, HRA) -> Areas
  
  MovementLists[[2]][!map_lgl(MovementLists[[2]], ~all(is.na(.x)))] %>% 
    map(~data.frame(HRAShrink = AnnualChange(.x),
                    Year = rownames(.x)[2:nrow(.x)])) %>% 
    bind_rows(.id = "Name") -> Shrink
  
  Shrink %>% mutate_at("Year", as.numeric) %>% full_join(Areas) %>% 
    left_join(Deer, .) -> Deer
  
}

if(file.exists(paste0("Data/", 
                      paste0(FocalSeason, collapse = "_"), 
                      "MCPLists.rds"))){
  
  MCPLists <- readRDS(paste0("Data/", 
                             paste0(FocalSeason, collapse = "_"), 
                             "MCPLists.rds"))
  
  MCPLists[[1]] %>% bind_rows(.id = "Name") %>%
    dplyr::select(Name, Year, MCPArea) -> Areas
  
  MCPLists[[2]][!map_lgl(MCPLists[[2]], ~all(is.na(.x)))] %<>% 
    
    lapply(function(a){
      
      N <- nrow(a)
      
      New <- a %>% as.numeric %>% matrix(nrow = N, ncol = N)
      
      dimnames(New) <- list(rownames(a), rownames(a))
      
      return(New)
      
    })
  
  MCPLists[[2]][!map_lgl(MCPLists[[2]], ~all(is.na(.x)))] %>% 
    map(~data.frame(MCPShrink = AnnualChange(.x),
                    Year = rownames(.x)[2:nrow(.x)] %>% str_remove("X"))) %>%
    bind_rows(.id = "Name") -> Shrink
  
  Shrink %>% mutate_at("Year", as.numeric) %>% full_join(Areas) %>% 
    left_join(Deer, .) -> Deer
  
}

Deer %<>% 
  mutate_at(vars(matches("Shrink")), ~ifelse(is.na(YearDiff)|(YearDiff>1), NA, .x))

# Adding dead individual strength ####

Individuals %>% 
  filter(DeathType == "S") %>% 
  dplyr::select(Name = Code, DeathYear, DeathMonth, DeathType) %>% 
  mutate_at(2:3, ~as.numeric(as.character(.x))) %>% 
  #filter(DeathMonth < 05) %>% 
  mutate_at("DeathYear", ~ifelse(DeathMonth < 05, .x - 1, .x)) -> 
  DeadIndividuals

AllYears <- (min(Censuses$DeerYear, na.rm = TRUE):
               (max(Censuses$DeerYear, na.rm = T) - 1))

seq_along(AllYears)[-1]  %>% lapply(function(r){
  
  print(r)
  
  AllYears[-1][r] -> FocalYear
  
  DeadFocal <- 
    DeadIndividuals %>% 
    filter(DeathYear == FocalYear) %>% 
    pull(Name) %>% 
    as.character()
  
  Columns <- RutAMList[[paste0("X", FocalYear)]] %>% colnames %>% intersect(DeadFocal)
  
  if(length(Columns)>0){
    
    if(length(Columns)>1){
      
      RutAMList[[paste0("X", FocalYear)]][,Columns] %>% rowSums %>% as.data.frame() %>% 
        rownames_to_column("Name") %>% 
        rename(ShotStrength = 2) %>% 
        mutate(Year = FocalYear)
      
    }else{
      
      RutAMList[[paste0("X", FocalYear)]][,Columns] %>%
        as.data.frame() %>% 
        rownames_to_column("Name") %>% 
        rename(ShotStrength = 2) %>% 
        mutate(Year = FocalYear)
      
    }
    
  }else{
    
    if(is.null(RutAMList[[paste0("X", FocalYear)]])){
      
      NULL
      
    }else{
      
      RutAMList[[paste0("X", FocalYear)]] %>% rownames %>% data.frame(Name = ., ShotStrength = 0) %>% 
        mutate(Year = FocalYear)
      
    }
  }
}) %>% bind_rows -> ShotStrengths

Individuals %>% 
  dplyr::select(Name = Code, DeathYear, DeathMonth, DeathType) %>% 
  mutate_at(2:3, ~as.numeric(as.character(.x))) %>% 
  # filter(DeathMonth < 05) %>% 
  mutate_at("DeathYear", ~ifelse(DeathMonth < 05, .x - 1, .x)) -> 
  DeadIndividuals

AllYears <- (min(Censuses$DeerYear, na.rm = TRUE):(max(Censuses$DeerYear, na.rm = T) - 1))

seq_along(AllYears)[-1] %>% lapply(function(r){
  
  print(r)
  
  AllYears[-1][r] -> FocalYear
  
  DeadFocal <- DeadIndividuals %>% filter(DeathYear == FocalYear) %>% pull(Name) %>% as.character()
  
  Columns <- RutAMList[[paste0("X", FocalYear)]] %>% colnames %>% intersect(DeadFocal)
  
  if(length(Columns)>0){
    
    if(length(Columns)>1){
      
      RutAMList[[paste0("X", FocalYear)]][,Columns] %>% rowSums %>% as.data.frame() %>% 
        rownames_to_column("Name") %>% rename(DeadStrength = 2) %>% 
        mutate(Year = FocalYear)
      
    }else{
      
      RutAMList[[paste0("X", FocalYear)]][,Columns] %>%
        as.data.frame() %>% 
        rownames_to_column("Name") %>% rename(DeadStrength = 2) %>% 
        mutate(Year = FocalYear)
      
    }
    
  }else{
    
    if(is.null(RutAMList[[paste0("X", FocalYear)]])){
      
      NULL
      
    }else{
      
      RutAMList[[paste0("X", FocalYear)]] %>% rownames %>% data.frame(Name = ., DeadStrength = 0) %>% 
        mutate(Year = FocalYear)
      
    }
  }
  
}) %>% bind_rows -> DeadStrengths

DeadStrengths %>% full_join(ShotStrengths, by = c("Name", "Year")) %>% 
  mutate_at("Year", ~.x + 1) %>% 
  left_join(Deer, .) -> Deer

DeadStrengths %>% full_join(ShotStrengths, by = c("Name", "Year")) %>% 
  mutate_at("Year", ~.x + 2) %>% 
  left_join(Deer, ., by = c("Name", "Year"), suffix = c("", ".t2")) -> Deer

DeadStrengths %>% full_join(ShotStrengths, by = c("Name", "Year")) %>% 
  mutate_at("Year", ~.x + 3) %>% 
  left_join(Deer, ., by = c("Name", "Year"), suffix = c("", ".t3")) -> Deer

# Grase Type ####

Censuses %>% 
  ungroup %>%  
  rename(Name = Code) %>% 
  # mutate(Year = DeerYear) %>% 
  group_by(Name, Year) %>%
  summarise_at("GrazeType", ~.x %>% str_detect("^G.$") %>% Prev) %>% 
  ungroup -> GrazeDeer

Deer %<>% left_join(GrazeDeer, by = c("Name", "Year"))


# Done ####

Deer %<>% mutate_if(is.numeric, ~ifelse(.x == Inf, NA, .x))

Deer %<>% 
  ungroup 

FullDeer <- Deer

#Deer %<>% filter(Survived0 == 1)

# Adding mean and deviation ####

Deer %>% group_by(Name) %>% 
  dplyr::select(Name, #Year, 
                HRA, HRAShrink, MCPArea, MCPShrink, 
                contains("Distance"),
                contains("Density"), 
                SocResps[1:3], 
                GrazeType) %>% 
  mutate_all(~mean(.x, na.rm = T)) %>% 
  rename_all(~paste0(.x, ".Mean")) %>% 
  ungroup %>% Grelect(-Name.Mean) %>% 
  bind_cols(Deer, .) ->
  Deer

Deer %>% group_by(Name) %>% 
  dplyr::select(Name, #Year, 
                HRA, HRAShrink, MCPArea, MCPShrink, 
                contains("Distance"),
                contains("Density"), 
                SocResps[1:3],
                GrazeType) %>% 
  mutate_all(~.x - mean(.x, na.rm = T)) %>% 
  rename_all(~paste0(.x, ".Within")) %>% 
  ungroup %>% Grelect(-Name.Within) %>% 
  bind_cols(Deer, .) ->
  Deer

# Adding last year's measures ####

Deer %>% 
  filter(Survived0 == 1|is.na(Survived0)) %>% 
  #filter(Survived0 == 1) %>% 
  dplyr::select(Name, Year, 
                contains(c("HRA", "HRAShrink", "MCPArea", "MCPShrink")), 
                contains("Distance"),
                contains("Density"), 
                contains(SocResps[1:3]),
                contains("GrazeType")) %>% 
  mutate_at("Year", ~.x + 1) %>% 
  left_join(Deer, ., by = c("Name", "Year"), 
            suffix = c("", ".t0")) ->
  
  Deer

# Removing problem groups ####

Deer %<>% filter(Hind == 1)

#Deer %<>% filter(!ReprodStatus == "Naive") %>% 
#  mutate_at("ReprodStatus", 
#           ~factor(.x, levels = c("True.Yeld", "Summer.Yeld", "Winter.Yeld", "Milk")))

SurvivalDeer <- Deer

Deer %<>% 
  filter(Survived0 == 1|is.na(Survived0))

