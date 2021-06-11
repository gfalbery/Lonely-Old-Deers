
# 3_Social Senescence Outputting ####

library(tidyverse); library(ggregplot); library(cowplot)

theme_set(theme_cowplot())

AgeConcentrate <- c(3:6)[c(1, 4)]

SeasonList <- list("Rut", "Spring", c("Spring", "Rut"))

SeasonList %>% map_chr(~paste0(.x, collapse = "_")) %>%
  paste0("Output Files_", ., "_") %>%
  paste0(rep(AgeConcentrate, length(SeasonList))) -> FileRoots

list.files(pattern = "Output Files") %>% setdiff("Output Files") -> FileRoots

FileRoots %>% map(~readRDS(paste0(.x, "/SurvivalModels.rds"))) -> SurvivalModels

SurvivalModels %>% map("FinalModel") %>% Efxplot(ModelNames = FileRoots)
SurvivalModels %>% map(c("Spatial", "Model")) %>% Efxplot(ModelNames = FileRoots)

FileRoots %>% map(~readRDS(paste0(.x, "/CombinedModels.rds"))) -> CombinedModels

CombinedModels %>% map("FinalModel") %>% Efxplot(ModelNames = FileRoots)

File <- FileRoots[[5]]

for(File in FileRoots){
  
  print(File)
  
  Model <- readRDS(paste0(File, "/CombinedModels.rds"))
  
  Model %<>% lapply(function(a){
    
    a$AllModels <- NULL
    
    a %>% return
    
  })
  
  Model %>% saveRDS(paste0(File, "/CombinedModels.rds"))
  
}

Model %>% map("FinalModel") %>% Efxplot +
  Model %>% map(c("Spatial", "Model")) %>% Efxplot

FileRoots %>% map(~readRDS(paste0(.x, "/SpatialModels3.rds"))) -> SpatialModels3

SpatialModels3 %>% map(~map(.x, c("FinalModel")) %>% Efxplot) %>% ArrangeCowplot()

FileRoots %>% map(~readRDS(paste0(.x, "/SpatialModels1.rds"))) -> SpatialModels1

SpatialModels1 %>% map(~map(.x, c("FinalModel")) %>% Efxplot) %>% ArrangeCowplot()
