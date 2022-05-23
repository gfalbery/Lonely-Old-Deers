
# 00_Senescence Master Script ####

library(tidyverse);library(reshape2);library(igraph); library(ggregplot); 
library(INLA); library(gsheet); library(magrittr); library(readxl); library(adehabitatHR)

rm(list = ls())

# Putting it all together ####

SeasonList <- list("Rut", "Spring", 
                   c("Spring", "Rut"))

j <- 3

FocalSeason <- SeasonList[[j]]

AgeConcentrate <- c(3:6)#[c(4)]

RepAge <- 5

# for(j in 2:3){

# FocalSeason <- SeasonList[[j]]

source("R/0b_Phenotyping.R")

# for(RepAge in AgeConcentrate[2:3]){

print(paste0("Age: ", RepAge))

Deer %<>% filter(Age >= RepAge)
SurvivalDeer %<>% filter(Age >= RepAge)

N <- Deer %>% nrow

print(N)

source("R/1_Social Senescence Models.R")

# if("Spring" %in% FocalSeason){
#   
#   source("R/2_Spring Survival Models.R")
#   
# }else{
#   
#   source("R/2_Rut Survival Models.R")
#   
# }

# source("R/3_Social Senescence Figures.R")

file.rename("Output Files", paste0("Output Files_", paste0(FocalSeason, collapse = "_"), "_", RepAge))
# file.rename("Figures", paste0("Figures_", paste0(FocalSeason, collapse = "_"), "_", RepAge))

#   }
# }
