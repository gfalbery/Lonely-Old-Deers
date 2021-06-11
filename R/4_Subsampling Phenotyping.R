
# 4_Subsampling Phenotyping ####

rm(list = ls())

library(tidyverse);library(reshape2);library(igraph); library(ggregplot); library(INLA); library(gsheet); library(magrittr)
library(readxl); library(adehabitatHR); library(rgeos)

FocalSeason <- c("Spring", "Rut")

# Setting up ####

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
  left_join(LBS[,c("Code", "LRS","LBS")], by = c("Name" = "Code"))

IDYearDF %>% 
  dplyr::select(Name, Year, ReprodStatus, Age) %>% 
  mutate_at(c("Year", "Age"), ~.x + 1) %>% 
  left_join(IDYearDF, ., by = c("MumCode" = "Name", "BirthYear" = "Year"),
            suffix = c("", ".Mum")) ->
  IDYearDF

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

CurrentYear <- max(Censuses$DeerYear, na.rm = T)

IDYearDF %<>% 
  mutate(
    Pregnant = as.numeric(Hind == 1 & 
                            ReprodStatus.t2 %in% 
                            c("Summer.Yeld", "Winter.Yeld", "Milk"))
  ) %>% 
  mutate_at("Pregnant", ~ifelse(Year == CurrentYear, NA, .x))

IDYearDF %>% 
  mutate_at(vars(contains("ReprodStatus")), 
            ~factor(.x, levels = c("Naive","True.Yeld","Summer.Yeld","Winter.Yeld","Milk"))) ->
  
  IDYearDF

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

CurrentYear <- max(Censuses$DeerYear, na.rm = T)

LastSeenDates %>% 
  dplyr::select(Name, LastSeen.Date, LastSeen.Year, LastSeen.DeerYear) %>% 
  left_join(IDYearDF, .) ->
  IDYearDF

IDYearDF %>% 
  mutate(Survived0 = 1 - as.numeric(LastSeen.Year == Year)) %>% 
  mutate(Survived1 = as.numeric(LastSeen.DeerYear > Year)) %>% 
  mutate(Survived2 = as.numeric(LastSeen.DeerYear > Year + 1)) %>% 
  mutate(Survived3 = as.numeric(LastSeen.DeerYear > Year + 1)) ->
  IDYearDF

IDYearDF %<>% mutate_at("Pregnant", ~ifelse(Survived1 == 1, .x, NA))

if("Spring" %in% FocalSeason){
  
  IDYearDF$Survived0 <- IDYearDF$Survived1
  
}

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

IDYearDF %<>% 
  mutate(Shot = 1 - as.numeric(DeathType != "S" | is.na(DeathType)))

NIterations <- 100

if(file.exists("Data/SubSampledCensuses.rds")) SubSampledCensuses <- readRDS("Data/SubSampledCensuses.rds") else{
  
  source("R/0d_Froy Home Range.R")
  
}

j <- "Intermediate" %>% list.files(pattern = "DeerDF") %>% 
  str_remove_all(".rds$") %>% 
  str_remove_all("DeerDF") %>% 
  as.numeric %>% max %>%  add(1)

for(j in j:NIterations){ # Loop ####
  
  print(j)
  
  Deer <- IDYearDF
  
  Deer %>% 
    arrange(Name, Year) %>% ungroup %>% 
    group_by(Name) %>% mutate(YearDiff = c(NA, diff(Year))) %>% 
    ungroup ->
    Deer
  
  # Attaching Social Data ####
  
  Deer %>% arrange(Year) %>% pull(Year) %>% unique %>% as.character -> FocalYears
  
  FocalYears %<>% c(min(as.numeric(FocalYears))-1, .) %>% as.numeric
  
  Root <- "Data"
  
  # Initial data import and cleaning ####
  
  Resps <- c("GroupSize", "Degree", "Strength", "Strength2", "Eigenvector", "Eigenvector2")
  
  Censuses <- SubSampledCensuses[[j]]
  
  Censuses %>% 
    ungroup %>% 
    separate(Date, "/", into = c("Day", "Month", "Year")) %>% 
    dplyr::select(c("Day", "Month", "Year")) %>% 
    mutate_all(as.numeric) ->
    Censuses[,c("Day", "Month", "Year")]
  
  Censuses %<>% ungroup %>% 
    mutate(Group = GroupDate %>% str_split(",") %>% map_chr(last)) %>% 
    mutate(DeerYear = ifelse(Month<5, Year - 1, Year)) %>% 
    mutate(Season = ifelse(Year == DeerYear, "Rut", "Spring")) %>% 
    mutate(GroupDate = paste(Date, Group, sep=",")) %>% 
    filter(!is.na(Code), !Code == "") %>% 
    dplyr::select(c("Date","Code","Easting","Northing","GroupSize","Year","DeerYear","Season","GroupDate"))
  
  Individuals <- read_xlsx(paste0(Root, "/Phenotypes/tblLife.xlsx"))
  Names <- read_xlsx(paste0(Root, "/Phenotypes/tblNames.xlsx"))
  
  Individuals <- merge(Individuals, Names, by="Code",all.x=TRUE)
  
  Individuals %<>% mutate_at(c("GivenName", "FamilyName"), as.character)
  
  Individuals[is.na(Individuals$GivenName),"GivenName"] <- Individuals[is.na(Individuals$GivenName),"FamilyName"]
  Individuals$Animal <- Individuals$GivenName
  colnames(Individuals)[colnames(Individuals)=="Birth.Date"]<-"BirthYear"
  
  Individuals$Sex<-cut(Individuals$Sex,breaks=c(0,1.5,2.5,3.5),labels=c("F","M","3"))
  
  for(x in which(colnames(Individuals)=="BirthDay"):which(colnames(Individuals)=="DeathYear")){
    Individuals[,x]<-factor(as.factor(Individuals[,x]),levels=c(0:10000,paste(0,1:10,sep="")))
  }
  
  for(x in which(colnames(Individuals)=="BirthDay"):which(colnames(Individuals)=="DeathYear")){
    Individuals[(sapply(Individuals[,x],function(y) nchar(substr(y,1,2)))==1)&!is.na(Individuals[,x]),x]<-paste(0,Individuals[(sapply(Individuals[,x],function(y) nchar(substr(y,1,2)))==1)&!is.na(Individuals[,x]),x],sep="")
  }
  
  Individuals$Birth.Date<-as.character(factor(with(Individuals,paste(BirthDay,BirthMonth,BirthYear,sep="/"))))
  Individuals$Death.Date<-as.character(factor(with(Individuals,paste(DeathDay,DeathMonth,DeathYear,sep="/"))))
  
  Individuals[Individuals$Birth.Date=="NA/NA/NA","Birth.Date"]<-NA
  Individuals[Individuals$Death.Date=="NA/NA/NA","Death.Date"]<-NA
  
  Individuals$BirthYear<-as.numeric(as.character(Individuals$BirthYear))
  Individuals$DeathYear<-as.numeric(as.character(Individuals$DeathYear))
  Individuals$Name = Individuals$Code
  
  Censuses <- merge(Censuses, Individuals[,c("Sex", "Name", "BirthYear")], 
                    by.x = c("Code"), by.y = c("Name"))
  
  Censuses$Age <- with(Censuses, DeerYear - BirthYear)
  Censuses$Hind <- with(Censuses, ifelse(Age > 2 & Sex == "F", "Y", "N"))
  
  Censuses %<>% arrange(lubridate::dmy(Date))
  
  Censuses %<>% filter(Hind == "Y")
  
  # to make annual records for all sightings ####
  
  #FocalSeason <- "Rut"
  
  Censuses %<>% filter(Season %in% FocalSeason)
  
  Censuses %>% arrange(DeerYear) %>% pull(Year) %>% unique %>% as.character -> FocalYears
  
  FocalYears %<>% #c(min(as.numeric(FocalYears))-1, .) %>% 
    as.numeric
  
  RutAMList <- longlist <- NetList <- list()
  
  x <- min(Censuses$DeerYear, na.rm = TRUE)
  
  for(x in (FocalYears)){
    
    print(x)
    
    Censuses2 <- 
      Censuses %>% filter(DeerYear == x) %>% droplevels
    
    if(nrow(Censuses2)>0){
      
      M <- with(Censuses2, table(Code, GroupDate))
      SocGraph <- graph.incidence(M, weighted = T)
      DeerProj <- bipartite.projection(SocGraph)$proj1
      AssMat <- DeerProj %>% get.adjacency(attr = "weight") %>% as.matrix 
      NObs <- diag(AssMat) <- table(Censuses2$Code)
      
      N <- nrow(AssMat)
      
      A <- matrix(rep(table(Censuses2$Code), N), N)
      B <- matrix(rep(table(Censuses2$Code), each = N), N)
      
      AM <- AssMat/(A + B - AssMat)
      
      diag(AssMat) <- diag(AM) <- 0
      RutAMList[[x-(min(Censuses$DeerYear,na.rm=TRUE)-1)]] <- AM
      
      DeerGraph <- graph_from_adjacency_matrix(AM, weighted = TRUE, mode = "undirected")
      
      Eigens <- data.frame(Name = rownames(AM),
                           Degree = colSums(AM>0), 
                           Strength = colSums(AM), 
                           Eigenvector = eigen_centrality(DeerGraph, scale=TRUE)$vector,
                           Eigenvector_Weighted = eigen_centrality(DeerGraph, weights = NA, scale=TRUE)$vector,
                           Betweenness = betweenness(DeerGraph),
                           # Closeness = closeness(DeerGraph),
                           Clustering = transitivity(DeerGraph, type = "local")
      ) %>%
        mutate(Strength_Mean = Strength/Degree) %>%
        mutate(Strength_Mean = ifelse(is.na(Strength_Mean), 0, Strength_Mean))
      
      Eigens <- Eigens[order(Eigens$Name),]
      
      Censuses2$EastingJ <- as.numeric (Censuses2$Easting + as.integer (runif (nrow (Censuses2),-1,1)))
      Censuses2$NorthingJ <- as.numeric (Censuses2$Northing + as.integer (runif (nrow (Censuses2),-1,1)))
      
      E <- with(Censuses2,tapply(Easting, Code, mean))
      N <- with(Censuses2,tapply(Northing, Code, mean))
      
      Subdf <- data.frame(Year = x,
                          Name = levels(Censuses2$Code),
                          NObs = c(NObs),
                          GroupSize = Censuses2 %>% 
                            group_by(Code) %>% 
                            summarise_at("GroupSize", ~mean(.x, na.rm = T)) %>% pull(GroupSize),
                          E = as.numeric(E),N = as.numeric(N),
                          RiverDistance = as.numeric(abs(1363-E))
      )
      
      rownames(Subdf) <- Subdf$Name
      
      longlist[[x-(min(Censuses$DeerYear, na.rm = TRUE) - 1)]] <- Subdf
      
      longlist[[x-(min(Censuses$DeerYear, na.rm = TRUE) - 1)]]$Reg6 <- 
        with(longlist[[x-(min(Censuses$DeerYear, na.rm = TRUE) - 1)]], LocToReg6(E, N))
      
      longlist[[x-(min(Censuses$DeerYear, na.rm = TRUE) - 1)]] <- 
        merge(longlist[[x-(min(Censuses$DeerYear, na.rm = TRUE) - 1)]], Eigens, by = "Name", all.X = TRUE)
      
    }
  }
  
  FullSociality <- bind_rows(longlist)
  
  FullSociality$Season <- paste0(FocalSeason, collapse = "_")
  
  FullSociality <- FullSociality %>% filter(!Year == 1992)
  
  SocResps <- c("GroupSize", "Degree",
                "Strength", "Strength_Mean",
                "Eigenvector", "Eigenvector_Weighted",
                "Betweenness", "Clustering")
  
  
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
  
  # Adding Home Ranges ####
  
  HindNames <- Deer %>% filter(Hind == 1) %>% 
    pull(Name) %>% unique %>% sort
  
  KUDLList <- HROList <- HRAList <- list()
  
  i <- 1
  
  print("Kernel!")
  
  for(i in i:length(HindNames)){
    
    FocalHind <- HindNames[i]
    
    cat(FocalHind)
    
    Censuses %>% filter(Code == FocalHind) ->
      
      Censuses2
    
    Censuses2 %>% group_by(DeerYear) %>% count %>% filter(n<6) %>% 
      anti_join(Censuses2, ., by = "DeerYear") ->
      Censuses2
    
    Censuses2 %>% mutate(
      
      EastingJ = Easting + runif(n(), -1, 1),
      NorthingJ = Northing + runif(n(), -1, 1)
      
    ) %>% filter(Hind == "Y") %>% 
      dplyr::select(EastingJ, NorthingJ, Code, DeerYear) %>% na.omit ->
      
      Censuses2
    
    Censuses2 %<>% arrange(DeerYear)
    
    Years <- Censuses2$DeerYear %>% unique %>% sort
    
    if(length(Years)>0){
      
      spdf <- SpatialPointsDataFrame(data = Censuses2, 
                                     coords = Censuses2[,c("EastingJ","NorthingJ")])
      
      spdf <- spdf[,"DeerYear"]
      
      kudl <- kernelUD(spdf, same4all = TRUE, grid = 500)
      
      HRAList[[FocalHind]] <- kernel.area(kudl, percent = 70)
      
      if(length(Years)>1){
        
        HRO <- kerneloverlaphr(kudl, percent = 75, method = "HR")
        
        HROList[[FocalHind]] <- HRO
        
      }else{
        
        HROList[[FocalHind]] <- NA
        
      }
    }
  }
  
  MovementLists <- list(
    
    HRAList,
    HROList
    
  )
  
  # Adding MCP ####
  
  MCPList <- MCPHROList <- MCPAreaList <- list()
  
  i <- 1
  
  print("MCP!")
  
  for(i in i:length(HindNames)){
    
    FocalHind <- HindNames[i]
    
    cat(FocalHind)
    
    Censuses %>% filter(Code == FocalHind) ->
      
      Censuses2
    
    Censuses2 %>% group_by(DeerYear) %>% count %>% filter(n<6) %>% 
      anti_join(Censuses2, ., by = "DeerYear") ->
      Censuses2
    
    Censuses2 %>% mutate(
      
      EastingJ = Easting + runif(n(), -1, 1),
      NorthingJ = Northing + runif(n(), -1, 1)
      
    ) %>% filter(Hind == "Y") %>% 
      dplyr::select(EastingJ, NorthingJ, Code, DeerYear) %>% na.omit ->
      
      Censuses2
    
    Censuses2 %<>% arrange(DeerYear)
    
    Years <- Censuses2$DeerYear %>% unique %>% sort
    
    if(length(Years)>0){
      
      Years %>% 
        lapply(function(a){
          
          Censuses2 %>% filter(DeerYear == a) %>% 
            dplyr::select(EastingJ, NorthingJ) %>% 
            SpatialPoints %>% mcp
          
        }) -> MCPList
      
      names(MCPList) <- paste0("X", Years)
      
      MCPList %>% map_dbl(~.x@data$area) ->
        MCPArea
      
      MCPAreaList[[FocalHind]] <- data.frame(MCPArea,
                                             Year = Years)
      
      if(length(Years)>1){
        
        Dyads <- expand.grid(Year1 = paste0("X", Years), Year2 = paste0("X", Years))
        
        Dyads %>% t %>% data.frame %>% 
          map(~MCPOverlap(MCPList, .x %>% unlist %>% as.character, Symmetrical = F)) %>% 
          matrix(nrow = length(Years)) ->
          HRO
        
        dimnames(HRO) <- list(paste0("X", Years), paste0("X", Years))
        
        MCPHROList[[FocalHind]] <- HRO
        
      }else{
        
        MCPHROList[[FocalHind]] <- NA
        
      }
    }
  }
  
  MCPLists <- list(
    
    MCPAreaList,
    MCPHROList
    
  )
  
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
  
  Deer %<>% 
    mutate_at(vars(matches("Shrink")), ~ifelse(is.na(YearDiff)|(YearDiff>1), NA, .x))
  
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
    
    Columns <- RutAMList[[r]] %>% colnames %>% intersect(DeadFocal)
    
    if(length(Columns)>0){
      
      if(length(Columns)>1){
        
        RutAMList[[r]][,Columns] %>% rowSums %>% as.data.frame() %>% 
          rownames_to_column("Name") %>% 
          rename(ShotStrength = 2) %>% 
          mutate(Year = FocalYear)
        
      }else{
        
        RutAMList[[r]][,Columns] %>%
          as.data.frame() %>% 
          rownames_to_column("Name") %>% 
          rename(ShotStrength = 2) %>% 
          mutate(Year = FocalYear)
        
      }
      
    }else{
      
      if(is.null(RutAMList[[r]])){
        
        NULL
        
      }else{
        
        RutAMList[[r]] %>% rownames %>% data.frame(Name = ., ShotStrength = 0) %>% 
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
    
    Columns <- RutAMList[[r]] %>% colnames %>% intersect(DeadFocal)
    
    if(length(Columns)>0){
      
      if(length(Columns)>1){
        
        RutAMList[[r]][,Columns] %>% rowSums %>% as.data.frame() %>% 
          rownames_to_column("Name") %>% rename(DeadStrength = 2) %>% 
          mutate(Year = FocalYear)
        
      }else{
        
        RutAMList[[r]][,Columns] %>%
          as.data.frame() %>% 
          rownames_to_column("Name") %>% rename(DeadStrength = 2) %>% 
          mutate(Year = FocalYear)
        
      }
      
    }else{
      
      if(is.null(RutAMList[[r]])){
        
        NULL
        
      }else{
        
        RutAMList[[r]] %>% rownames %>% data.frame(Name = ., DeadStrength = 0) %>% 
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
  
  # Done ####
  
  Deer %<>% mutate_if(is.numeric, ~ifelse(.x == Inf, NA, .x))
  
  Deer %<>% 
    ungroup 
  
  FullDeer <- Deer
  
  #Deer %<>% filter(Survived0 == 1)
  
  # Adding last year's measures ####
  
  Deer %>% 
    filter(Survived0 == 1) %>% 
    dplyr::select(Name, Year, 
                  HRA, HRAShrink, MCPArea, MCPShrink, 
                  contains("Distance"),
                  contains("Density"), 
                  SocResps[1:3]) %>% 
    mutate_at("Year", ~.x + 1) %>% 
    left_join(Deer, ., by = c("Name", "Year"), 
              suffix = c("", ".t0")) ->
    
    Deer
  
  # Removing problem groups ####
  
  Deer %<>% filter(Hind == 1)
  
  ReprodReplace <- c("None", "None", "Summer", "Winter", "Winter")
  
  names(ReprodReplace) <- c("Naive", "True.Yeld", "Summer.Yeld", "Winter.Yeld", "Milk")
  
  Deer %<>% mutate_at("ReprodStatus", ~.x %>% 
                        str_replace_all(c(ReprodReplace)))
  
  SurvivalDeer <- Deer
  
  # Deer %<>% filter(Survived0 == 1)
  
  saveRDS(Deer, file = paste0("Intermediate/DeerDF", j, ".rds"))
  
}
