
# 4b_Summarising Subsampling ####

library(tidyverse)

"Intermediate" %>% list.files(full.names = T) %>% gtools::mixedsort() %>% map(readRDS) ->
  DeerDFList


Groups <- DeerDFList[[1]] %>% dplyr::select(Name:YearDiff) %>% names

SumDF <- DeerDFList[1:length(DeerDFList)] %>%
  bind_rows %>% 
  group_by(Name, Year) %>% 
  summarise_if(is.numeric, ~mean(.x, na.rm = T))

Deer <- IDYearDF[,intersect(Groups, names(Deer))] %>% 
  full_join(SumDF %>% dplyr::select(-intersect(names(SumDF), setdiff(Groups, c("Name", "Year")))))

Deer %<>% filter(Hind == 1)

SurvivalDeer <- Deer

Deer %<>% filter(Survived0 == 1)


# Extra ####

DeerDFList %>% bind_rows(.id = "SubSampleRep") %>% 
  pivot_wider(names_from = "SubSampleRep") %>% names

CompDF <- DeerDFList[1:length(DeerDFList)] %>% map(~.x %>% arrange(Name, Year)) %>% bind_cols

CompDF %>% ggplot(aes(HRA1, HRA2)) + geom_point() + geom_smooth(method = lm)


SumDF <- DeerDFList[1:length(DeerDFList)] %>% map(~.x %>% arrange(Name, Year)) %>% bind_rows %>% 
  group_by(Name, Year) %>% summarise_if(is.numeric, mean)
