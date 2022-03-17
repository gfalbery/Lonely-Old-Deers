# Subsampling Social and Spatial behvaiour Metrics

# rm(list = ls())

Root <- "Data"

# Initial data import and cleaning ####

library(tidyverse);library(reshape2);library(igraph); library(ggregplot); library(magrittr)

Resps <- c("GroupSize", "Degree", "Strength", "Strength2", "Eigenvector", "Eigenvector2")
Perm_Resps <- paste0("Perm_",Resps)

Censuses <- read.csv(paste0(Root, "/Behaviour/FullCensuses.csv"))

Censuses %>% separate(Date, "/", into = c("Day", "Month", "Year")) %>% 
  dplyr::select(c("Day", "Month", "Year")) %>% 
  mutate_all(as.numeric) ->
  Censuses[,c("Day", "Month", "Year")]

Censuses %<>% 
  mutate(DeerYear = ifelse(Month<5, Year - 1, Year)) %>% 
  mutate(Season = ifelse(Year == DeerYear, "Rut", "Spring")) %>% 
  mutate(GroupDate = paste(Date, Group, sep=",")) %>% 
  filter(!is.na(Code), !Code == "") %>% 
  dplyr::select(c("Date","Code","Easting","Northing","GroupSize","Year","DeerYear","Season","GroupDate", GrazeType))

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

Censuses %>% arrange(DeerYear) %>% pull(DeerYear) %>% unique %>% as.character -> FocalYears

FocalYears %<>% #c(min(as.numeric(FocalYears))-1, .) %>% 
  as.numeric

Records = 5

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
                        Name = unique(Censuses2$Code),
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

names(RutAMList) <- paste0("X", FocalYears)

FullSociality <- bind_rows(longlist)

FullSociality$Season <- paste0(FocalSeason, collapse = "_")

FullSociality <- FullSociality %>% filter(!Year == 1992)

SocResps <- c("GroupSize", "Degree",
              "Strength", "Strength_Mean",
              "Eigenvector", "Eigenvector_Weighted",
              "Betweenness", "Clustering")

Censuses %<>% filter(!is.na(Easting), !is.na(Northing))

# Graze ####

if(0){
  
  Censuses %>% filter(GrazeType %in% (Censuses$GrazeType %>% table %>% sort %>% rev %>%  extract(1:10) %>% names)) %>%     
    group_by(Easting, Northing, GrazeType) %>% count %>% ungroup %>%
    data.frame %>%  group_by(GrazeType) %>% 
    mutate_at("n", ~(.x/sum(.x))) %>% 
    ggplot(aes(Easting, Northing, fill = n)) + geom_tile() + coord_fixed() + facet_wrap(~GrazeType)
  
}