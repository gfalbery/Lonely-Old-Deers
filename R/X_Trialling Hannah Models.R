
# Trialling Hannah's Models ####

library(tidyverse)

HannahDF <- read.delim("C:/Users/gfalb/Downloads/Deer2.txt")

HannahDF$HRareaLastYr %<>% log

HannahDF %>% group_by(ID) %>% 
  mutate(MeanHRA = mean(HRareaLastYr)) %>% 
  mutate(WithinHRA = HRareaLastYr - MeanHRA) %>% 
  ungroup -> HannahDF

HannahDF %<>% mutate_at(c("Age", "MeanHRA", "WithinHRA", "Age"), ~c(scale(.x)))

IM1 <- INLAModelAdd(
  
  Response = "Survival",
  Data = HannahDF,
  Explanatory = c("MeanHRA", "WithinHRA", "Age", "Offspring", "OffspringLastYr"),
  #Add = AddCovar,
  Random = c("ID", "Year"), 
  RandomModel = rep("iid", 2),
  Family = "binomial"
  
)

IM2 <- INLAModelAdd(
  
  Response = "Survival",
  Data = HannahDF,
  Explanatory = c("MeanHRA", "Age", "Offspring", "OffspringLastYr"),
  Add = "WithinHRA",
  Random = c("ID", "Year"), 
  RandomModel = rep("iid", 2),
  Family = "binomial"
  
)

IM3 <- INLAModelAdd(
  
  Response = "Survival",
  Data = HannahDF,
  Explanatory = c("MeanHRA", "Age", "Offspring"),
  Add = "WithinHRA",
  Random = c("ID", "Year"), 
  RandomModel = rep("iid", 2),
  Family = "binomial"
  
)

IM4 <- INLAModelAdd(
  
  Response = "Survival",
  Data = HannahDF,
  Explanatory = c("Age", "Offspring", "OffspringLastYr"),
  Add = "WithinHRA",
  Random = c("ID", "Year"), 
  RandomModel = rep("iid", 2),
  Family = "binomial"
  
)

IM5 <- INLAModelAdd(
  
  Response = "Survival",
  Data = HannahDF,
  Explanatory = c("Age", "Offspring"),
  Add = "WithinHRA",
  Random = c("ID", "Year"), 
  RandomModel = rep("iid", 2),
  Family = "binomial"
  
)

IMList <- list(IM1, IM2, IM3, IM4, IM5)

IMList %>% map("FinalModel") %>% Efxplot

