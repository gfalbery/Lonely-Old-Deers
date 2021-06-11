
# 0c_Home Range Setup ####

library(tidyverse);library(reshape2);library(igraph); library(ggregplot); library(adehabitatHR); library(rgeos)

HindNames <- Deer %>% filter(Hind == 1) %>% 
  pull(Name) %>% unique %>% sort

KUDLList <- HROList <- HRAList <- list()

i <- 1

for(i in i:length(HindNames)){
  
  FocalHind <- HindNames[i]
  
  print(FocalHind)
  
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

MovementLists %>% 
  saveRDS(paste0("Data/", 
                 paste0(FocalSeason, collapse = "_"), 
                 "MovementLists.rds"))

# Adding MCP ####

MCPOverlap <- function(CPs, Dyad, Symmetrical = T){
  
  p1 <- CPs[[Dyad[1]]]
  p2 <- CPs[[Dyad[2]]]
  
  Intersect <- gIntersection(p1,p2)
  
  if(!is.null(Intersect)){
    
    Numerator <- gArea(Intersect)
    
    if(Symmetrical){
      
      Denominator <- gArea(p1) + gArea(p2) - Numerator
      
    }else{
      
      Denominator <- gArea(p1)
      
    }
    
    Overlap <- Numerator / Denominator
    
  }else Overlap <- 0
  
  return(Overlap)
  
}

MCPList <- MCPHROList <- MCPAreaList <- list()

i <- 1

for(i in i:length(HindNames)){
  
  FocalHind <- HindNames[i]
  
  print(FocalHind)
  
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

MovementLists <- list(
  
  MCPAreaList,
  MCPHROList
  
)

MovementLists %>% 
  saveRDS(paste0("Data/", 
                 paste0(FocalSeason, collapse = "_"), 
                 "MCPLists.rds"))
