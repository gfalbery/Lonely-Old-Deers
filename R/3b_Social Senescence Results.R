
# 3b_Social Senescence Results ####

library(RColorBrewer); library(ggregplot); library(tidyverse); library(colorspace);
library(GGally); library(cowplot); library(INLA); library(patchwork); library(magrittr)
library(MCMCglmm); library(magrittr)

theme_set(theme_cowplot() + 
            theme(strip.background = element_rect(fill = "white", colour = "dark grey")))

AlberPalettes <- c("YlGnBu","Reds","BuPu", "PiYG")
AlberColours <- sapply(AlberPalettes, function(a) RColorBrewer::brewer.pal(5, a)[4])
AlberColours[length(AlberColours)+1:2] <- RColorBrewer::brewer.pal(11, AlberPalettes[[4]])[c(2,10)]

AlberColours %<>% c(Pink = "#FD6396", Blue = "#3C78D8")

Resps = c("GroupSize", "Degree", "Strength")

RespLabels = c("Group Size", "Degree", "Strength")
names(RespLabels) <- Resps

# Import ####

# Figure 1: Social effects ####

SpocialList <- readRDS("Output Files/SocialModels1.rds")
SocialModels3 <- readRDS("Output Files/SocialModels2.rds")

SpocialData <- SpocialList %>% map("Data")

Covar <- c("ReprodStatus", "Age",
           "Year", "PopN", "NObs")

# Figure 2: Spatial Effects ####

SpatialModels2 <- readRDS("Output Files/SpatialModels1.rds")
SpatialModels3 <- readRDS("Output Files/SpatialModels2.rds")
SpatialModels4 <- readRDS("Output Files/SpatialModels3.rds")

SpatialList <- list(
  
  AnnualDensity = SpatialModels2$AnnualDensity) %>% 
  append(SpatialModels3) %>% append(SpatialModels4)

SpatialResps <-
  c("AnnualDensity") %>% 
  c("AnnualDistance", "LifetimeDistance", "EarlyDistance", "UberDistance") %>% 
  c("HRAShrink", "MCPShrink", "HRA", "MCPArea")

SpatialLabels <- c("Density", 
                   "Annual movement", 
                   "Lifetime centroid distance", 
                   "Natal centroid distance", 
                   "Pop'n centroid distance",
                   "Kernel overlap", "Polygon overlap",
                   "Kernel area", "Polygon area")

names(SpatialLabels) <- SpatialResps

Covar <- c("ReprodStatus", "Age",
           "Year", "PopN", "NObs")

CombinedModels <- readRDS("Output Files/CombinedModels3.rds")

# Results snippets ####

SocialModels1 <- readRDS("C:/Users/gfalb/Documents/Script/OldDeers/Output Files/SocialModels1.rds")
SocialModels2 <- readRDS("C:/Users/gfalb/Documents/Script/OldDeers/Output Files/SocialModels2.rds")

# Ageing 1 year came with 
# a reduction in average group size of 0.27 individuals (CI; P value; Figure 1A), 
# 0.66 fewer unique contacts (CI; P value; Figure 1B), 
# 0.05 weaker network connection strength (CI; P value; Figure 1C).

# the subset of individuals with known death year ( % of individuals; Model Set 2). 

SocialModels1[[1]]$Data %>% nrow
SocialModels2[[1]]$Data %>% nrow

3242/3581

# Longevity was positively associated with 
# degree centrality and connection strength(Effect; CI; P Value; Figure 1D). 

SocialModels2 %>% 
  map(3) %>% 
  map(~GetEstimates(.x, "Longevity"))

SocialModels2 %>% 
  map(3) %>% 
  map(~INLAPValue(.x, "Longevity"))

# observed senescence did not originate from 
# selective disappearance of more-social individuals (Effect; CI; P Value; Figure 1D).

SocialModels2 %>% 
  map(3) %>% 
  map(~INLAPValue(.x, "Age"))

# SPDE effect substantially improved the fit of our models (ΔDIC) 

SocialModels1 %>% 
  map(~.x$FinalModel$dic$dic - 
        .x$Spatial$Model$dic$dic)

# reduced age effect estimates in Model Set 1-2 (Maximum P Value) 

SocialModels1 %>% 
  map(c("Spatial", "Model")) %>% 
  map(~INLAPValue(.x, "Age"))


# and removed both significant longevity effects in Model Set 2 (Maximum P Value; Figure 1; See Supplementary Table 2 for full effect estimates). 

SocialModels2 %>% 
  map(4) %>% 
  map(~INLAPValue(.x, "Longevity"))


# Age was heavily spatially structured (ΔDIC): 



#  Older deer occurred in lower-density areas (Effect; CI; P Value; Figure 2A), 

SpatialList %>% 
  map("FinalModel") %>% 
  map(~tibble(Estimate = GetEstimates(.x, "Age"),
              P = unlist(INLAPValue(.x, "Age"))))

# further from the centre of the population (Effect; CI; P Value; Figure 2C), 

# and moved their annual centroids slightly less between years (Effect; CI; P Value; Figure 2B). 

# Older individuals also had smaller home ranges (Effect; CI; P Value; Figure 2D), 

# but this result did not hold when accounting for spatial autocorrelation in home range size (Effect; CI; P Value; Figure 2D). There was no age trend in the degree of overlap between consecutive annual home ranges (Effect; CI; P Value; Figure 2E)

SpatialList %>% 
  map(c("Spatial", "Model")) %>% 
  map(~tibble(Estimate = GetEstimates(.x, "Age"),
              P = unlist(INLAPValue(.x, "Age"))))

# individuals were less likely to be observed on high quality grazing (Effect; CI; P Value; Figure 2F).

GrazeModels1 <- readRDS("C:/Users/gfalb/Documents/Script/OldDeers/Output Files/GrazeModels1.rds")

GrazeModels1$FinalModel %>% 
  GetEstimates("Age")

GrazeModels1$FinalModel %>% 
  INLAPValue("Age")

# Longer lifespan was associated with those individuals that: 

"Output Files" %>% 
  list.files(pattern = "SelectiveSpatial", full.names = T) ->
  SpatialFiles

SpatialFiles %>% map(readRDS) -> 
  SelectiveSpatialList

SelectiveSpatialList %>% summary

SelectiveSpatialList2 <- 
  SelectiveSpatialList %>% 
  Unlist1 %>% map("AllModels") %>% 
  map(function(a) list(a[[1]], a[[2]][[1]], a[[2]][[2]], a[[3]][[1]])) 

SelectiveSpatialList2 %>% summary

#   inhabited areas of lower density (Supplementary Figure 1A); 
# moved further between annual centroids (Supplementary Figure 1B); 
# and lived further from the population centre (Supplementary Figure 1C).

SelectiveSpatialList2[c("AnnualDensity", 
                        "AnnualDistance",
                        "UberDistance")] %>% 
  map(~GetEstimates(.x[[3]], "Longevity"))

SelectiveSpatialList2[c("AnnualDensity", 
                        "AnnualDistance",
                        "UberDistance")] %>% 
  map(~INLAPValue(.x[[3]], "Longevity"))

# Individuals with larger home ranges had shorter lives only when we accounted for spatial autocorrelation (Supplementary Figure 1F), 

SelectiveSpatialList2$HRA[[4]] %>% 
  GetEstimates("Longevity")

SelectiveSpatialList2$HRA[[4]] %>% 
  INLAPValue("Longevity")

# controlling for spatial autocorrelation also removed the longevity effect of annual centroid distance (Supplementary Figure 1B). 

SelectiveSpatialList2$AnnualDistance[[4]] %>% 
  INLAPValue("Longevity")

# Neither home range overlap nor grazing quality were associated with longevity (Supplementary Figure 1D-E). 

SelectiveSpatialList2[c("HRAShrink", 
                        "GrazeType")] %>% 
  map(~INLAPValue(.x[[3]], "Longevity"))

# selective disappearance did not decrease or remove any age effects except that of local density (Supplementary Figure 1A).

SelectiveSpatialList2[c("AnnualDensity")] %>% 
  map(~INLAPValue(.x[[3]], "Age"))

# Additionally, social metrics were negatively associated with distance from the population centre (P Value<). 

CombinedModels %>% 
  map("FinalModel") %>% 
  map(~INLAPValue(.x, "UberDistance"))

# However, fitting these effects as explanatory variables in the GLMMs did not supplant the negative effects of ageing (P Value<; Supplementary Figure 2; Supplementary Table 4).

CombinedModels %>% 
  map("FinalModel") %>% 
  map(~INLAPValue(.x, "Age"))

# Including the SPDE effect alongside these three spatial covariates removed the significant associations of age with degree and strength (P Value >), 

CombinedModels %>% 
  map(c("Spatial", "Model")) %>% 
  map(~INLAPValue(.x, "Age"))

# but retained an association with group size (P Value =; Supplementary Figure 2B).

