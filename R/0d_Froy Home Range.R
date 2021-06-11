
# Running Hannah's Home Range Area Code #####

IDYearDF %>% dplyr::select(Name, Year) %>% unique -> HindYears

Root <- "Data"

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

Censuses %<>% mutate(Month = substr(Date, 4, 5))

i <- 1

# Where an individual was observed more than once on a single census, only the first observation was retained. 

Censuses %>% group_by(Code, Date) %>% mutate(N = 1:n()) %>% filter(N == 1) -> Censuses2

# We considered only months where an individual was observed on three or more censuses, 

Censuses2 %<>% group_by(Code, Month, DeerYear) %>% 
  count %>% filter(n >= 3) %>% 
  semi_join(Censuses2, ., by = c("Code", "DeerYear", "Month")) %>% 
  semi_join(HindYears, by = c("Code" = "Name", "Year")) %>% 
  ungroup

# and randomly sampled 4 months without replacement. 
# Where possible, months were sampled from both summer–autumn and winter–spring (94% of individual–years).

Censuses2 %>% dplyr::select(Code, DeerYear) %>% unique -> HindYears2

# Iterate

NIterations <- 100

1:NIterations %>% map(function(i){
  
  print(i)
  
  FocalMonths <- Censuses2 %>% 
    dplyr::select(Code, DeerYear, Month) %>% unique %>% RandomSlice %>% 
    group_by(Code, DeerYear) %>% mutate(N = 1:n()) %>% filter(N <= 4)
  
  FocalMonths %>% group_by(Code, DeerYear) %>% 
    summarise_at("N", max) %>% filter(N == 4) %>% 
    semi_join(FocalMonths, ., by = c("Code", "DeerYear")) %>% 
    dplyr::select(1:3) ->
    FocalMonths
  
  # Three observations were then sampled randomly without replacement for each sampled month, 
  # result- ing in 12 locations for each individual in each year. 
  
  Censuses2 %>% 
    semi_join(FocalMonths, by = c("Code", "DeerYear", "Month")) %>% 
    RandomSlice %>% 
    group_by(Code, DeerYear, Month) %>% 
    mutate(N = 1:n()) %>% 
    filter(N <= 3) %>% return
  
}) -> SubSampledCensuses

SubSampledCensuses %>% saveRDS("Data/SubSampledCensuses.rds")

# We excluded the year of death for all individuals, so only years with complete census records were included. 

# Each subsampling strategy was run 100 times, resulting in 100 subsampled data sets for both deer and sheep.

if(0){
  
  i <- 1
  
  HRAList <- list()
  
  for(i in i:NIterations){
    
    print(i)
    
    Censuses3 <- SubSampledCensuses[[i]] %>% droplevels
    
    Censuses3 %<>% mutate(
      
      EastingJ = Easting + runif(n(), -5, 5),
      NorthingJ = Northing + runif(n(), -5, 5)
      
    )
    
    spdf <- SpatialPointsDataFrame(data = Censuses3, 
                                   coords = Censuses3[,c("EastingJ","NorthingJ")])
    
    spdf <- spdf[,"Code"]
    
    kudl <- kernelUD(spdf, same4all = TRUE, grid = 500)
    
    HRAList[[i]] <- kernel.area(kudl, percent = 70)
    
  }
  
  HRAList %>% saveRDS("Data/HRAListSubsampled.rds")
  
}