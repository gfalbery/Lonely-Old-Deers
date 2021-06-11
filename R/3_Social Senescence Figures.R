
# X_Figures ####

{
  
  library(RColorBrewer); library(ggregplot); library(tidyverse); library(colorspace);
  library(GGally); library(cowplot); library(INLA); library(patchwork); library(magrittr)
  library(MCMCglmm); library(magrittr); library(scales)
  
  theme_set(theme_cowplot() + 
              theme(strip.background = element_rect(fill = "white", colour = "dark grey")))
  
  AlberPalettes <- c("YlGnBu","Reds","BuPu", "PiYG")
  AlberColours <- sapply(AlberPalettes, function(a) RColorBrewer::brewer.pal(5, a)[4])
  AlberColours[length(AlberColours)+1:2] <- RColorBrewer::brewer.pal(11, AlberPalettes[[4]])[c(2,10)]
  
  AlberColours %<>% c(Pink = "#FD6396", Blue = "#3C78D8")
  
  Resps = c("GroupSize", "Degree", "Strength")
  
  RespLabels = c("Group Size", "Degree", "Strength")
  names(RespLabels) <- Resps
  
}

# Figure 1: Social effects ####

SpocialList <- readRDS("Output Files/SocialModels1.rds")
SocialModels3 <- readRDS("Output Files/SocialModels2.rds")

SpocialData <- SpocialList %>% map("Data")

Covar <- c("ReprodStatus", "Age",
           "Year", "PopN", "NObs")

a <- Resps[1]

Figure1aList <- LabelYList <- 
  LabelList <- LabelXList <- 
  TransList <- UnTransList <- TransTransList <-
  YMeanList <- YSDList <- 
  list()

for(a in Resps){
  
  print(a)
  
  TestHinds <- SpocialData[[a]]
  SocMesh <- SpocialList[[a]]$Spatial$Mesh
  
  YSD <- 
    TestHinds[,paste0(a, ".Original")] %>% sd
  
  YMean <- 
    TestHinds[,paste0(a, ".Original")] %>% mean
  
  XSD <- 
    TestHinds[,paste0("Age", ".Original")] %>% sd
  
  XMean <- 
    TestHinds[,paste0("Age", ".Original")] %>% mean
  
  X = seq(min(TestHinds$Age), 
          max(TestHinds$Age), 
          length.out = nrow(TestHinds)) %>% c
  
  B2 = summary(SpocialList[[a]]$Spatial$Model)$fixed["Age","mean"]
  A2 = summary(SpocialList[[a]]$Spatial$Model)$fixed[,"mean"]%*%t(SpocialList[[a]]$Spatial$Model$model.matrix) %>% mean
  Y2 = A2 + B2*X
  
  SpocialList[[a]]$FinalModel %>% 
    INLAFit(TestHinds, Covar, NDraw = 100, Draw = T) %>% map_dbl(mean) -> Intercepts
  
  SpocialList[[a]]$FinalModel %>% 
    GetEstimates("Age", NDraw = 100, Draws = T) -> Slopes
  
  B = mean(Slopes)
  A = mean(Intercepts)
  Y = A + B*X
  
  1:length(Slopes) %>% map(~data.frame(X = X,
                                       Y = X*Slopes[[.x]] + Intercepts[[.x]]) %>% 
                             slice(1, n())) -> SlopeDF
  
  SlopeDF %<>% bind_rows(.id = "Rep")
  
  FitLine <- data.frame(
    Age = rep(X, 2),
    Y = c(Y, Y2),
    Model = rep(c("Base","SPDE"), each = length(X))
  )
  
  TestHinds$Y <- TestHinds[,a]
  
  Est <- (SpocialList[[a]]$FinalModel %>% 
            #GetEstimates(Variable = "Age") %>%
            GetEstimates(Variable = "Age", Mode = "Numeric") %>% 
            multiply_by(YSD) %>% divide_by(XSD) %>% unlist %>% 
            round(3)) 
  
  paste0(Est[[1]], " (", Est[[2]], ",", Est[[3]], ")") %>% 
    paste0("; P < ", SpocialList[[a]]$FinalModel %>% INLAPValue("Age")) -> 
    LabelList[[a]]
  
  LabelYList[[a]] <- max(TestHinds$Y + diff(range(TestHinds$Y))/10)
  
  LabelXList[[a]] <- median(X)
  
  LabelDf <- data.frame(
    
    Age = LabelXList[[a]],
    Y = LabelYList[[a]],
    Label = LabelList[[a]]
    
  )
  
  TransList[[a]] <- Trans <- function(x){
    
    (x + YMean/
       YSD)*YSD
    
  }
  
  UnTransList[[a]] <- UnTrans <- function(y){
    
    (y - YMean)/(YSD)
    
  }
  
  TransTransList[[a]] <- 
    Trans_trans <- 
    trans_new("Trans_trans", Trans, function(A) A)
  
  Figure1aList[[a]] <- 
    TestHinds %>% 
    RandomSlice %>% mutate(Group = factor(1:n())) %>%
    ggplot(aes(Age, Y)) + 
    geom_point(alpha = 0.1, aes(colour = Group)) + 
    geom_line(alpha = 0.05, data = SlopeDF, aes(X, group = Rep)) +
    scale_colour_discrete_sequential(palette = "Blues 3", 
                                     guide = F) +
    #geom_line(alpha = 0.1, data = SlopeDF2, aes(X, Y, group = Rep), 
    #          colour = "grey") +
    geom_line(data = FitLine, aes(lty = Model, group = Model), size = 1)  +
    labs(y = RespLabels[[a]], x = "Age") +
    geom_text(data = LabelDf, aes(label = Label), hjust = 0.5,
              colour = AlberColours[[1]], size = 3) +
    scale_x_continuous(breaks = sort(unique(TestHinds$Age))[1:5*5-4], 
                       labels = c(1:5)*5)# + 
  # scale_y_continuous(trans = Trans_trans, 
  #                    breaks = UnTrans(0:10*10), 
  #                    limits = c(UnTrans(0), NA))# %>% plot_grid()# %>% plot
  # 
}

Figure1aList[[1]] <- 
  Figure1aList[[1]] +
  scale_y_continuous(trans = 
                       trans_new("Trans_trans", 
                                 function(x){
                                   
                                   (x + SpocialData[[1]][,paste0("GroupSize", ".Original")] %>% mean/
                                      SpocialData[[1]][,paste0("GroupSize", ".Original")] %>% sd)*
                                     SpocialData[[1]][,paste0("GroupSize", ".Original")] %>% sd
                                   
                                 }, function(A) A), 
                     breaks = ((0:10*10) - SpocialData[[1]][,paste0("GroupSize", ".Original")] %>% mean)/(SpocialData[[1]][,paste0("GroupSize", ".Original")] %>% sd), 
                     limits = c(((0:10*10)[1] - SpocialData[[1]][,paste0("GroupSize", ".Original")] %>% mean)/(SpocialData[[1]][,paste0("GroupSize", ".Original")] %>% sd), NA))

Figure1aList[[2]] <- 
  Figure1aList[[2]] +
  scale_y_continuous(trans = 
                       trans_new("Trans_trans", 
                                 function(x){
                                   
                                   (x + SpocialData[[2]][,paste0("Degree", ".Original")] %>% mean/
                                      SpocialData[[2]][,paste0("Degree", ".Original")] %>% sd)*
                                     SpocialData[[2]][,paste0("Degree", ".Original")] %>% sd
                                   
                                 }, function(A) A), 
                     breaks = ((0:10*20) - SpocialData[[2]][,paste0("Degree", ".Original")] %>% mean)/(SpocialData[[2]][,paste0("Degree", ".Original")] %>% sd), 
                     limits = c(((0:10*10)[1] - SpocialData[[2]][,paste0("Degree", ".Original")] %>% mean)/(SpocialData[[2]][,paste0("Degree", ".Original")] %>% sd), NA))

Figure1aList[[3]] <- 
  Figure1aList[[3]] +
  scale_y_continuous(trans = 
                       trans_new("Trans_trans", 
                                 function(x){
                                   
                                   (x + SpocialData[[3]][,paste0("Strength", ".Original")] %>% mean/
                                      SpocialData[[3]][,paste0("Strength", ".Original")] %>% sd)*
                                     SpocialData[[3]][,paste0("Strength", ".Original")] %>% sd
                                   
                                 }, function(A) A), 
                     breaks = (((-5):15*2) - SpocialData[[3]][,paste0("Strength", ".Original")] %>% mean)/(SpocialData[[3]][,paste0("Strength", ".Original")] %>% sd), 
                     limits = c(((0:10*10)[1] - SpocialData[[3]][,paste0("Strength", ".Original")] %>% mean)/(SpocialData[[3]][,paste0("Strength", ".Original")] %>% sd), NA))

Resps %>% lapply(function(b){
  
  names(SocialModels3[[b]]) <- c("Base", "+ ID", "+ Longevity", "+ SPDE")
  
  SocialModels3[[b]] %>% map("summary.fixed") %>% lapply(function(a){
    
    a %>% rownames_to_column("Explanatory") %>% 
      rename(Lower = `0.025quant`, Upper = `0.975quant`) %>% 
      dplyr::select(Explanatory, Mean = mean, Lower, Upper) %>% 
      filter(Explanatory %in% c("Age", "Longevity"))
    
  }) %>% bind_rows(.id = "Model")
}) %>% bind_rows(.id = "Response") %>% 
  mutate_at("Response", ~RespLabels[as.numeric(.x)] %>% factor(levels = RespLabels)) %>%
  mutate_at("Model", ~factor(.x, levels = c("Base", "+ ID", "+ Longevity", "+ SPDE"))) %>% 
  ggplot(aes(Explanatory, Mean, colour = Model, group = Model)) + facet_grid(~Response, scales = "free") +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  geom_point(colour = "black", size = 3,
             position = position_dodge(w = 0.7)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                position = position_dodge(w = 0.7),
                width = 0.3) +
  #geom_errorbar(colour = "black", aes(ymin = X0.025quant, ymax = X0.975quant),
  #              position = position_dodge(w = 0.7),
  #              width = 0.3) +
  geom_point(position = position_dodge(w = 0.7), size = 2) +
  labs(x = NULL, y = "Effect estimate") +
  scale_colour_discrete_sequential(palette = AlberPalettes[[3]], nmax = 6, order = c(3:6)) +
  theme(legend.position = "right") ->
  Figure1b

(((Figure1aList[[1]] | Figure1aList[[2]] | Figure1aList[[3]]) + 
    plot_layout(guides = "collect")) /Figure1b) +
  #plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") +
  plot_layout(heights = c(2, 1.5)) +
  ggsave("Figures/Figure1.jpeg", 
         width = 250, height = 150, dpi = 600, units = "mm")

# Outputting Table ####

SpocialList %>% 
  map(c("FinalModel", "summary.fixed", data.frame, rownames_to_column)) %>% 
  bind_rows(.id = "Response") %>% 
  dplyr::select(Response, 
                Variable = rowname, Mean = mean, 
                Lower = `X0.025quant`, Upper = `X0.975quant`) %>% 
  mutate_if(is.numeric, ~round(.x, 3)) %>% 
  mutate(" " = c("", "*")[as.numeric(Lower*Upper > 0)+1]) %>% 
  cbind(
    
    SpocialList %>% 
      map(c("Spatial", "Model", "summary.fixed", data.frame, rownames_to_column)) %>% 
      bind_rows(.id = "Response") %>% 
      dplyr::select(Mean = mean, 
                    Lower = `X0.025quant`, Upper = `X0.975quant`) %>% 
      mutate_if(is.numeric, ~round(.x, 3)) %>% 
      rename_all(~paste0("SPDE", .x)) %>% 
      mutate(" " = c("", "*")[as.numeric(SPDELower*SPDEUpper > 0)+1])
    
  ) %>% write.csv("Output Files/SocialModelTable.csv", row.names = F)

# Figure 2: Spatial Effects ####

SpatialModels2 <- readRDS("Output Files/SpatialModels1.rds")
SpatialModels3 <- readRDS("Output Files/SpatialModels2.rds")
SpatialModels4 <- readRDS("Output Files/SpatialModels3.rds")
GrazeModels <- readRDS("Output Files/GrazeModels1.rds")

SpatialList <- list(AnnualDensity = SpatialModels2$AnnualDensity) %>% 
  append(SpatialModels3) %>% append(SpatialModels4) %>% 
  append(list(GrazeType = GrazeModels))

SpatialResps <-
  c("AnnualDensity") %>% 
  c("AnnualDistance", "LifetimeDistance", "EarlyDistance", "UberDistance") %>% 
  c("HRAShrink", "MCPShrink", "HRA", "MCPArea", "GrazeType")

SpatialLabels <- c("Density", 
                   "Annual centroid distance", 
                   "Lifetime centroid distance", 
                   "Natal centroid distance", 
                   "Pop'n centroid distance",
                   "Home range overlap", "Polygon overlap",
                   "Home range area", "Polygon area", "Grazing quality")

names(SpatialLabels) <- SpatialResps

Covar <- c("ReprodStatus", "Age",
           "Year", "PopN", "NObs")

Figure2aList <- LabelYList <- LabelList <- LabelXList <- list()

for(a in SpatialResps){
  
  print(a)
  
  TestHinds <- SpatialList[[a]]$Data
  SocMesh <- SpatialList[[a]]$Spatial$Mesh
  
  X = seq(min(TestHinds$Age), 
          max(TestHinds$Age), 
          length.out = nrow(TestHinds)) %>% c
  
  SpatialList[[a]]$FinalModel %>% 
    INLAFit(TestHinds, Covar, NDraw = 100, Draw = T) %>% 
    map_dbl(mean) -> 
    Intercepts
  
  SpatialList[[a]]$FinalModel %>% 
    GetEstimates("Age", NDraw = 100, Draws = T) -> 
    Slopes
  
  B = mean(Slopes)
  A = mean(Intercepts)
  Y = A + B*X
  
  B2 = summary(SpatialList[[a]]$Spatial$Model)$fixed["Age","mean"]
  A2 = A
  Y2 = A2 + B2*X
  
  1:length(Slopes) %>% map(~data.frame(X = X,
                                       Y = X*Slopes[[.x]] + Intercepts[[.x]]) %>% 
                             slice(1, n())) -> SlopeDF
  
  SlopeDF %<>% bind_rows(.id = "Rep")
  
  FitLine <- data.frame(
    Age = rep(X, 2),
    Y = c(Y, Y2),
    Model = rep(c("Base","SPDE"), each = length(X))
  )
  
  TestHinds$Y <- TestHinds[,a]
  
  P <- SpatialList[[a]]$FinalModel %>% INLAPValue("Age")
  
  if(P[[1]]>10^-4){
    
    P <- P[[1]] %>% round(4)
    
  }
  
  SpatialList[[a]]$FinalModel %>% 
    
    GetEstimates(Variable = "Age") %>%
    paste0("; P = ", P) -> 
    LabelList[[a]]
  
  LabelYList[[a]] <- max(TestHinds$Y + diff(range(TestHinds$Y))/10)
  
  LabelXList[[a]] <- median(X)
  
  LabelDf <- data.frame(
    
    Age = LabelXList[[a]],
    Y = LabelYList[[a]],
    Label = LabelList[[a]]
    
  )
  
  if(a %in% c("AnnualDensity", SpatialResps[str_detect(SpatialResps, "Distance")][2:4])){
    
    FitLine %<>% filter(Model == "Base")
    
  }
  
  Plot <- 
    TestHinds %>% RandomSlice %>% mutate(Group = factor(1:n())) %>%
    ggplot(aes(Age, Y)) + 
    geom_point(alpha = 0.1, aes(colour = Group)) + 
    geom_line(alpha = 0.05, data = SlopeDF, aes(X, group = Rep)) +
    scale_colour_discrete_sequential(palette = "Reds", 
                                     guide = F) +
    geom_line(data = FitLine, # %>% filter(Model == "Base"), 
              aes(lty = Model),
              size = 1)  +
    labs(y = SpatialLabels[[a]], x = "Age") +
    geom_text(data = LabelDf, aes(label = Label), hjust = 0.5,
              colour = AlberColours[[2]], size = 3) +
    scale_linetype_manual(limits = c("Base", "SPDE"), values = c(1, 2)) +
    scale_x_continuous(breaks = sort(unique(TestHinds$Age))[1:5*5 - 4], 
                       labels = c(1:5*5)) # %>% plot
  
  Figure2aList[[a]] <- Plot
  
}

# SpatialRespsOrder <- c(SpatialResps[c(1, 2, 4, 5, 8, 9, 6, 7)])

SpatialRespsOrder <- SpatialResps[c(1, 2, 5, 8, 6, 10)]

Figure2aList[SpatialRespsOrder] %>% ArrangeCowplot +
  plot_layout(guides = "collect") + plot_layout(nrow = 2) +
  plot_annotation(tag_levels = "A") +
  ggsave("Figures/Figure2.jpeg", 
         width = 220, height = 160, dpi = 600, units = "mm")

SpatialList[SpatialRespsOrder] %>% 
  map(c("FinalModel", "summary.fixed", data.frame, rownames_to_column)) %>% 
  bind_rows(.id = "Response") %>% 
  dplyr::select(Response, 
                Variable = rowname, Mean = mean, 
                Lower = `X0.025quant`, Upper = `X0.975quant`) %>% 
  mutate_if(is.numeric, ~round(.x, 3)) %>% 
  mutate(" " = c("", "*")[as.numeric(Lower*Upper > 0)+1]) %>% 
  full_join(
    
    SpatialList[SpatialRespsOrder] %>% 
      map(c("Spatial", "Model", "summary.fixed", data.frame, rownames_to_column)) %>% 
      bind_rows(.id = "Response") %>% 
      dplyr::select(Response, 
                    Variable = rowname, 
                    Mean = mean, 
                    Lower = `X0.025quant`, Upper = `X0.975quant`) %>% 
      mutate_if(is.numeric, ~round(.x, 3)) %>% 
      mutate_if(is.numeric, 
                ~ifelse(Response %in% c("AnnualDensity", "UberDistance"), NA, .x)) %>% 
      mutate_at("Variable", ~str_replace(.x, "Intercept", "(Intercept)")) %>% 
      rename_all(~paste0("SPDE", .x)) %>% 
      mutate(" " = c("", "*")[as.numeric(SPDELower*SPDEUpper > 0)+1]),
    
    by = c("Response" = "SPDEResponse", "Variable" = "SPDEVariable")
    
  ) %>% filter(!Variable %in% c("Age_AnnualDistance", "Age:AnnualDistance")) %>% 
  write.csv("Output Files/SpatialModelTable.csv", row.names = F)


# ~~~~~ Supplementary ####

# Spatial distribution of spatial stuff ####

SpatialRespsOrder[c(2, 4:6)] %>% 
  map(~ggField(SpatialList[[.x]]$Spatial$Model, Mesh = SpatialList[[.x]]$Spatial$Mesh) +
        labs(fill = SpatialLabels[[.x]]) +
        scale_fill_discrete_sequential(palette = "Mint",
                                       # rev = F,
                                       labels = RespLabels)) %>% 
  ArrangeCowplot() +
  plot_layout(nrow = 2)

ggField(SpatialList$UberDistance$Spatial$Model, 
        Mesh = SpatialList$UberDistance$Spatial$Mesh) +
  labs(fill = "Distance") +
  scale_fill_discrete_sequential(palette = "Mint",
                                 # rev = F,
                                 labels = RespLabels)

# Dead Friends Estimates ####

DemographicModels1 <- readRDS("Output Files/DemographicModels1.rds")

DemographyFX <- 
  DemographicModels1 %>% 
  map("FinalModel") %>% 
  Efxplot(PointOutline = T,
          ModelNames = Resps) +
  #scale_colour_manual(values = c(AlberColours[[1]], AlberColours[[2]], AlberColours[[3]]))
  scale_colour_discrete_sequential(palette = "Mint",
                                   labels = RespLabels)

DemographicModels1 %>% map(c("AllModels", 2, Efxplot)) %>% ArrangeCowplot()

DemographicModels1 %>% 
  map("dDIC") %>% map(DICTableGet) %>% 
  bind_rows(.id = "Response") %>% 
  write.csv("Figures/DemographyDIC.csv", row.names = F)

DemographicModels2 <- readRDS("Output Files/DemographicModels2.rds")

DemographicModels2 %>% map(c("AllModels", 2, Efxplot)) %>% ArrangeCowplot()

DemographyFX2 <- 
  DemographicModels2 %>% 
  map("FinalModel") %>% 
  Efxplot(PointOutline = T,
          ModelNames = Resps) +
  #scale_colour_manual(values = c(AlberColours[[1]], AlberColours[[2]], AlberColours[[3]]))
  scale_colour_discrete_sequential(palette = "Mint",
                                   labels = RespLabels)

DemographicModels2 %>% 
  map("dDIC") %>% map(DICTableGet) %>% 
  bind_rows(.id = "Response") %>% 
  write.csv("Figures/DemographyDIC.csv", row.names = F)

# Combined effects plot ####

DemographicModels2 %>% 
  map("AllModels") %>% map(2) %>% 
  map(~lapply(.x, function(a) a$summary.fixed)) %>% unlist(recursive = F) %>% 
  map(~.x %>% as.data.frame %>% rownames_to_column) %>% 
  bind_rows(.id = "Model") %>% 
  group_by(Model) %>% mutate(N = 1:n()) %>% filter(N == max(N)) %>% 
  separate(Model, "[.]", into = c("Response", "None", "Explanatory")) %>% 
  rename(Lower = `0.025quant`, Upper = `0.975quant`, Mean = mean) %>% 
  mutate_at("Explanatory", ~str_remove(.x, "Strength")) %>% 
  mutate_at("Response", ~factor(.x, levels = c("GroupSize", "Degree", "Strength"))) %>% 
  ggplot(aes(Explanatory, Mean, colour = Response, group = Response)) + 
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                width = 0.3,
                position = position_dodge(w = 0.5)) +
  labs(x = "Friends", y = "Estimate") +
  geom_point(colour = "black", size = 3, position = position_dodge(w = 0.5)) +
  geom_point(size = 2, position = position_dodge(w = 0.5)) +
  scale_colour_discrete_sequential(palette = "Mint", nmax = 5, 
                                   order = c(3:6-1),
                                   labels = RespLabels) + 
  ggsave("Figures/DeadFriends.jpeg", units = "mm", width = 120, height = 120, dpi = 300)


# Spocial Model Effects ####

CombinedModels <- readRDS("Output Files/CombinedModels3.rds")

CombnVar <- c("Age", "AnnualDensity", "UberDistance", "HRA", "Longevity")
CombnLabels <- c("Age", "Density", "Pop'n Distance", "HRA", "Longevity")

Resps <- c("GroupSize", "Degree", "Strength")

CombinedFXa <- 
  CombinedModels %>% 
  map("FinalModel") %>% 
  Efxplot(VarOrder = CombnVar, 
          VarNames = CombnVar, # %>% str_replace("MCPArea", "HRA"),
          PointOutline = T,
          ModelNames = Resps) +
  scale_x_discrete(limits = CombnVar %>% rev,
                   labels = CombnLabels %>% rev) +
  #scale_colour_manual(values = c(AlberColours[[1]], AlberColours[[2]], AlberColours[[3]]))
  scale_colour_discrete_sequential(palette = "Mint", nmax = 5, 
                                   order = c(3:6-1),
                                   labels = RespLabels)

CombinedFXb <- 
  CombinedModels %>% 
  map(c("Spatial", "Model")) %>% 
  Efxplot(VarOrder = CombnVar, 
          VarNames = CombnVar, # %>% str_replace("MCPArea", "HRA"),
          PointOutline = T,
          ModelNames = Resps) +
  scale_x_discrete(limits = CombnVar %>% rev,
                   labels = CombnLabels %>% rev) +
  #scale_colour_manual(values = c(AlberColours[[1]], AlberColours[[2]], AlberColours[[3]]))
  scale_colour_discrete_sequential(palette = "Mint", nmax = 5, 
                                   order = c(3:6-1),
                                   labels = RespLabels)

CombinedFXa + CombinedFXb +
  plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = "A") +
  ggsave("Figures/CombinedFX.jpeg", units = "mm", width = 200, height = 200, dpi = 300)

EffectCompare1 <- 
  SocialModels3 %>% 
  map(c(3)) %>% 
  map("summary.fixed") %>% 
  map(rownames_to_column) %>% 
  bind_rows(.id = "Response") %>% 
  bind_rows(
    CombinedModels %>% 
      map("FinalModel") %>% 
      map("summary.fixed") %>% 
      map(rownames_to_column) %>% 
      bind_rows(.id = "Response"),
    .id = "ModelSet"
  ) %>% 
  rename(Mean = mean,
         Lower = `0.025quant`,
         Upper = `0.975quant`) %>% 
  filter(rowname == "Age") %>% 
  ggplot(aes(Response, Mean, colour = ModelSet)) + 
  geom_hline(yintercept = 0, lty = 2, alpha = 0.3) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                position = position_dodge(w = 0.5),
                width = 0.3) +
  geom_point(position = position_dodge(w = 0.5)) +
  coord_flip()

EffectCompare2 <- 
  SocialModels3 %>% 
  map(c(4)) %>% 
  map("summary.fixed") %>% 
  map(rownames_to_column) %>% 
  bind_rows(.id = "Response") %>% 
  bind_rows(
    CombinedModels %>% 
      map(c("Spatial", "Model")) %>% 
      map("summary.fixed") %>% 
      map(rownames_to_column) %>% 
      bind_rows(.id = "Response"),
    .id = "ModelSet"
  ) %>% 
  rename(Mean = mean,
         Lower = `0.025quant`,
         Upper = `0.975quant`) %>% 
  filter(rowname == "Age") %>% 
  ggplot(aes(Response, Mean, colour = ModelSet)) + 
  geom_hline(yintercept = 0, lty = 2, alpha = 0.3) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                position = position_dodge(w = 0.5),
                width = 0.3) +
  geom_point(position = position_dodge(w = 0.5)) +
  coord_flip()

(EffectCompare + labs(y = "Effect",
                      x = "",
                      colour = "Model set") + 
    scale_colour_manual(labels = c("Base", "+ Spatial behaviours"),
                        limits = c(1, 2),
                        values = c(AlberColours[[1]], 
                                   AlberColours[[2]]))) + 
  (EffectCompare2 + labs(y = "Effect", 
                         colour = "Model set",
                         x = "") + 
  scale_x_discrete(labels = NULL) + 
    scale_colour_manual(labels = c("Base", "+ Spatial behaviours"),
                        limits = c(1, 2),
                        values = c(AlberColours[[1]], 
                                   AlberColours[[2]]))) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") +
  ggsave("Figures/CombinedFX2.jpeg", units = "mm", 
         width = 220, height = 150, 
         dpi = 300)

CombinedModels %>% map(c("dDIC", DICTableGet)) %>% 
  bind_rows(.id = "Response") %>% filter(!Round == 4) %>% 
  write.csv("Figures/SpocialDIC.csv", row.names = F)

#MultivariateModel <- readRDS("Output Files/MultivariateModel.rds")

#MultivariateModel %>% Efxplot

#PhenDF <- 
#  MultivariateModel %>% PhenCorr(3) %>% rownames_to_column() %>% 
#  mutate_at("rowname", ~str_remove_all(.x, "trait")) %>% 
#  separate(rowname, "[.]", into = c("Var1", "Var2", "Name"))

SpatialResps <- c("EarlyDistance", "HRA", "AnnualDensity", "GroupSize", "Degree", "Strength")

# PhenDF %>% rename(Var1 = Var2, Var2 = Var1) %>%
#  bind_rows(PhenDF) %>%
#  mutate(Label = glue::glue("{Mode}\n({LCI},{UCI})")) %>%
#  mutate_at(c("Var1", "Var2"), ~factor(.x, levels = SpatialResps)) %>%
#  ggplot(aes(Var1, Var2, fill = Mode)) +
#  geom_tile() +
#  geom_text(aes(label = Label)) +
#  scale_fill_continuous_diverging("Tropic") +
#  coord_fixed()

# Spatial selective disappearance ####

SpatialResps <-
  c("AnnualDensity") %>%
  c("AnnualDistance", "LifetimeDistance", "EarlyDistance", "UberDistance") %>%
  c("HRAShrink", "MCPShrink", "HRA", "MCPArea", "GrazeType")

SpatialLabels <- c("Density",
                   "Annual centroid distance",
                   "Lifetime centroid distance",
                   "Natal centroid distance",
                   "Pop'n centroid distance",
                   "Home range overlap", "Polygon overlap",
                   "Home range area", "Polygon area", "Grazing quality")

names(SpatialLabels) <- SpatialResps

SpatialRespsOrder <- SpatialResps[c(1, 2, 5, 8, 6, 10)]

SpatialLabels <- 
  SpatialLabels[SpatialRespsOrder]

SpatialLabels2 <- 
  glue::glue("{LETTERS[1:length(SpatialLabels)]}) {SpatialLabels}") %>% 
  as.character()

names(SpatialLabels2) <- names(SpatialLabels)

"Output Files" %>% 
  list.files(pattern = "SelectiveSpatial", full.names = T) ->
  SpatialFiles

SpatialFiles %>% map(readRDS) -> 
  SelectiveSpatialList

SelectiveSpatialList %>% 
  Unlist1 %>% map("AllModels") %>% 
  map(function(a) list(a[[1]], a[[2]][[1]], a[[2]][[2]], a[[3]][[1]])) %>% 
  #map(Efxplot) %>% ArrangeCowplot() +
  #plot_layout(guides = "collect")
  Unlist1 %>% 
  map(c("summary.fixed", as.data.frame, rownames_to_column)) %>% 
  bind_rows(.id = "Model") %>% 
  rename(Mean = mean, Lower = `0.025quant`, Upper = `0.975quant`) %>% 
  as.data.frame %>% filter(rowname %in% c("Age", "Longevity")) %>% 
  mutate(Response = substr(Model, 1, (nchar(Model) - 1))) %>% 
  mutate(Model = substr(Model, nchar(Model), nchar(Model))) ->
  SelectiveSpatialDF

SelectiveSpatialDF %>% 
  filter(Response %in% SpatialRespsOrder) %>% 
  filter(!Response == "LifetimeDistance") %>% 
  mutate_at("Response", ~factor(.x, levels = SpatialRespsOrder)) %>% 
  mutate_at("Model", ~factor(c("Base", "+ ID", "+ Longevity")[as.numeric(.x)], levels = c("Base", "+ ID", "+ Longevity", "+ SPDE"))) %>% 
  filter(!is.na(Model)) %>% 
  ggplot(aes(rowname, Mean, colour = Model, group = Model)) + 
  facet_wrap(~Response, ncol = 2, 
             scales = "free_y", labeller = as_labeller(SpatialLabels2)) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  geom_point(colour = "black", size = 3,
             position = position_dodge(w = 0.7)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                position = position_dodge(w = 0.7),
                width = 0.3) +
  #geom_errorbar(colour = "black", aes(ymin = X0.025quant, ymax = X0.975quant),
  #              position = position_dodge(w = 0.7),
  #              width = 0.3) +
  geom_point(position = position_dodge(w = 0.7), size = 2) +
  labs(x = NULL, y = "Effect estimate") +
  scale_colour_discrete_sequential(palette = AlberPalettes[[3]], nmax = 6, order = c(3:6)) +
  theme(legend.position = "right") ->
  SelectiveSpatialPlot

SelectiveSpatialPlot + 
  ggsave("Figures/SelectiveSpatialPlot.jpeg", units = "mm", width = 200, height = 200, dpi = 300)

# Adding SPDE ####

SelectiveSpatialList %>% 
  Unlist1 %>% map(c("Spatial", "Model")) %>% 
  map(c("summary.fixed", as.data.frame, rownames_to_column)) %>% 
  bind_rows(.id = "Model") %>% 
  rename(Mean = mean, Lower = `0.025quant`, Upper = `0.975quant`) %>% 
  as.data.frame %>% filter(rowname %in% c("Age", "Longevity")) %>% 
  rename(Response = Model) %>% 
  mutate(Model = "5") %>% 
  bind_rows(SelectiveSpatialDF, .) %>% 
  filter(Response %in% SpatialRespsOrder) %>% 
  filter(!Response == "LifetimeDistance") %>% 
  mutate_at("Model", ~factor(c("Base", "+ ID", "+ Longevity", "+ SPDE")[as.numeric(.x)], levels = c("Base", "+ ID", "+ Longevity", "+ SPDE"))) %>% 
  filter(!is.na(Model)) %>% 
  filter(!(Model == "+ SPDE" & Response %in% c("AnnualDensity", "UberDistance"))) %>% 
  mutate_at("Response", ~factor(.x, levels = SpatialRespsOrder)) %>% 
  ggplot(aes(rowname, Mean, colour = Model, group = Model)) + 
  facet_wrap(~Response, ncol = 2, 
             scales = "free_y", labeller = as_labeller(SpatialLabels2)) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  geom_point(colour = "black", size = 3,
             position = position_dodge(w = 0.7)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                position = position_dodge(w = 0.7),
                width = 0.3) +
  #geom_errorbar(colour = "black", aes(ymin = X0.025quant, ymax = X0.975quant),
  #              position = position_dodge(w = 0.7),
  #              width = 0.3) +
  geom_point(position = position_dodge(w = 0.7), size = 2) +
  labs(x = NULL, y = "Effect estimate") +
  scale_colour_discrete_sequential(palette = AlberPalettes[[3]], nmax = 6, order = c(3:6)) +
  theme(legend.position = "right") ->
  SelectiveSpatialPlot

SelectiveSpatialPlot + 
  ggsave("Figures/SelectiveSpatialPlot.jpeg", units = "mm", width = 200, height = 200, dpi = 300)

# ~~~~~~ Defunct ####
# Survival and pregnancy tables ####

SurvivalModelsA <- readRDS("Output Files/SurvivalModelsA.rds")
SurvivalModelsB <- readRDS("Output Files/SurvivalModelsB.rds")

SurvivalModelsA$dDIC %>% DICTableGet()

SurvivalModelsB$dDIC %>% DICTableGet()

PregnancyModelsA <- readRDS("Output Files/PregnancyModels.rds")
PregnancyModelsB <- readRDS("Output Files/PregnancyModelsB.rds")

PregnancyModelsA$dDIC %>% DICTableGet() %>% 
  bind_rows(
    
    PregnancyModelsB$dDIC %>% DICTableGet(),
    .id = "Response"
    
  )

SurvivalModelsB$FinalModel %>% Efxplot(VarOrder = AddCovar) +
  ggsave("Figures/SurvivalEffects.jpeg", units = "mm", width = 120, height = 100)

# Survival and parasite stuff ####

# Figure 3: Survival effects ####

SurvivalModelsA <- readRDS("Output Files/SurvivalModelsA.rds")
SurvivalModelsB <- readRDS("Output Files/SurvivalModelsB.rds")

SurvivalModels <- SurvivalModelsB

Figure3aList <- LabelYList <- LabelList <- LabelXList <- list()

SurvVarLabels <- c("Natal centroid distance\n(Within-individual deviation)", 
                   "Polygon area\n(Within-individual deviation)", 
                   "Density", "Degree")
names(SurvVarLabels) <- c("EarlyDistance.t0", "MCPArea.t0", "AnnualDensity.t0", "Degree.t0")

Covar <- c("ReprodStatus","ReprodStatus.t0", "Age",
           "Year", "PopN") %>% 
  #c("GroupSize", "Degree", "Strength") %>% 
  #c("AnnualDensity") %>% 
  c("AnnualDensity.t0")

for(a in c("AnnualDensity.t0")){
  
  print(a)
  
  TestHinds <- SurvivalModelsA$Data
  SocMesh <- SurvivalModelsA$Spatial$Mesh
  
  X = seq(min(TestHinds[,a]), 
          max(TestHinds[,a]), 
          length.out = nrow(TestHinds)) %>% c
  
  B = summary(SurvivalModelsA$FinalModel)$fixed[a,"mean"]
  A = summary(SurvivalModelsA$FinalModel)$fixed[,"mean"]%*%
    t(SurvivalModelsA$FinalModel$model.matrix) %>% mean
  Y = A + B*X
  
  SurvivalModelsA$FinalModel %>% 
    INLAFit(TestHinds, Covar, NDraw = 100, Draw = T) %>% map_dbl(mean) -> Intercepts
  
  SurvivalModelsA$FinalModel %>% 
    GetEstimates(a, NDraw = 100, Draws = T) -> Slopes
  
  1:length(Slopes) %>% map(~data.frame(X = X,
                                       Y = X*Slopes[[.x]] + Intercepts[[.x]])) ->
    
    SlopeDF
  
  SlopeDF %<>% bind_rows(.id = "Rep") %>% mutate_at("Y", logistic)
  
  FitLine <- data.frame(
    X = X,
    Y = logistic(Y)
  )
  
  TestHinds$Y <- TestHinds$Survived1
  
  TestHinds$X <- TestHinds[,a]
  
  P <- SurvivalModelsA$FinalModel %>% INLAPValue(a)
  
  if(P[[1]]>10^-4){
    
    P <- P[[1]] %>% round(4)
    
  }
  
  SurvivalModelsA$FinalModel %>% 
    
    GetEstimates(Variable = a) %>%
    paste0("; P = ", P) -> 
    LabelList[[a]]
  
  LabelYList[[a]] <- max(TestHinds$Y + diff(range(TestHinds$Y))/10)
  
  LabelXList[[a]] <- median(X)
  
  LabelDf <- data.frame(
    
    X = LabelXList[[a]],
    Y = LabelYList[[a]],
    Label = LabelList[[a]]
    
  )
  
  Figure3aList[[a]] <- 
    ggplot(data = TestHinds, aes(X, Y)) + 
    geom_point(alpha = 0.1, colour = AlberColours[["Pink"]]) + 
    geom_line(alpha = 0.05, data = SlopeDF, aes(X, Y, group = Rep)) +
    #geom_line(alpha = 0.1, data = SlopeDF2, aes(X, Y, group = Rep), 
    #          colour = "grey") +
    geom_line(data = FitLine, size = 1)  +
    labs(y = "Survived", x = SurvVarLabels[[a]]) +
    scale_y_continuous(limits = c(0, NA), breaks = c(0:5/5)) +
    geom_text(data = LabelDf, aes(label = Label), hjust = 0.5,
              colour = AlberColours[["Pink"]], size = 3)# %>% plot
  
}

Covar <- c("ReprodStatus","ReprodStatus.t0", "Age",
           "Year", "PopN") %>% 
  #c("GroupSize", "Degree", "Strength") %>% 
  #c("AnnualDensity") %>% 
  c("MCPArea.t0", "EarlyDistance.t0")

for(a in c("EarlyDistance.t0", "MCPArea.t0")){
  
  print(a)
  
  TestHinds <- SurvivalModels$Data
  SocMesh <- SurvivalModels$Spatial$Mesh
  
  X = seq(min(TestHinds[,a]), 
          max(TestHinds[,a]), 
          length.out = nrow(TestHinds)) %>% c
  
  B = summary(SurvivalModels$FinalModel)$fixed[a,"mean"]
  A = summary(SurvivalModels$FinalModel)$fixed[,"mean"]%*%
    t(SurvivalModels$FinalModel$model.matrix) %>% mean
  Y = A + B*X
  
  SurvivalModels$FinalModel %>% 
    INLAFit(TestHinds, Covar, NDraw = 100, Draw = T) %>% map_dbl(mean) -> Intercepts
  
  SurvivalModels$FinalModel %>% 
    GetEstimates(a, NDraw = 100, Draws = T) -> Slopes
  
  1:length(Slopes) %>% map(~data.frame(X = X,
                                       Y = X*Slopes[[.x]] + Intercepts[[.x]])) ->
    
    SlopeDF
  
  SlopeDF %<>% bind_rows(.id = "Rep") %>% mutate_at("Y", logistic)
  
  FitLine <- data.frame(
    X = X,
    Y = logistic(Y)
  )
  
  TestHinds$Y <- TestHinds$Survived1
  
  TestHinds$X <- TestHinds[,a]
  
  P <- SurvivalModels$FinalModel %>% INLAPValue(a)
  
  if(P[[1]]>10^-4){
    
    P <- P[[1]] %>% round(4)
    
  }
  
  SurvivalModels$FinalModel %>% 
    
    GetEstimates(Variable = a) %>%
    paste0("; P = ", P) -> 
    LabelList[[a]]
  
  LabelYList[[a]] <- max(TestHinds$Y + diff(range(TestHinds$Y))/10)
  
  LabelXList[[a]] <- median(X)
  
  LabelDf <- data.frame(
    
    X = LabelXList[[a]],
    Y = LabelYList[[a]],
    Label = LabelList[[a]]
    
  )
  
  Figure3aList[[a]] <- 
    ggplot(data = TestHinds, aes(X, Y)) + 
    geom_point(alpha = 0.1, colour = AlberColours[["Pink"]]) + 
    geom_line(alpha = 0.05, data = SlopeDF, aes(X, Y, group = Rep)) +
    #geom_line(alpha = 0.1, data = SlopeDF2, aes(X, Y, group = Rep), 
    #          colour = "grey") +
    geom_line(data = FitLine, size = 1)  +
    labs(y = "Survived", x = SurvVarLabels[[a]]) +
    scale_y_continuous(limits = c(0, NA), breaks = c(0:5/5)) +
    geom_text(data = LabelDf, aes(label = Label), hjust = 0.5,
              colour = AlberColours[["Pink"]], size = 3)# %>% plot
  
}

Figure3aList[[3]] <- Figure3aList[[3]] + scale_x_reverse()

Figure3aList %>% 
  ArrangeCowplot() +
  plot_layout(nrow = 1) +
  plot_annotation(tag_levels = "A") +
  ggsave("Figures/Figure3.jpeg", 
         width = 220, height = 80, 
         dpi = 600, units = "mm")


# Figure 4: Parasites ####

ParasiteModels <- readRDS("Output Files/ParasiteModels1.rds")

SpocialList <- ParasiteModels# %>% map("FinalModel")

ParasitePalettes <- c("PuRd")
names(ParasitePalettes) <- Parasites[1]

Parasites <- names(ParasiteModels)
names(ParasiteColours) <- Parasites

ParasiteLabels <- c("log(Strongyles + 1)",
                    "log(E.cervi + 1)",
                    "IgA OD")
names(ParasiteLabels) <- Parasites[c(1, 3, 4)]

Covar <- c("Season", "Age", "ReprodStatus", "fYear")

Figure4List <- LabelYList <- LabelList <- LabelXList <- list()

a = Parasites[1]

for(a in Parasites[c(1, 3)]){
  
  print(a)
  
  TestHinds <- ParasiteModels[[a]]$Data
  SocMesh <- SpocialList[[a]]$Spatial$Mesh
  
  X = seq(min(TestHinds$Degree.t0), 
          max(TestHinds$Degree.t0), 
          length.out = nrow(TestHinds)) %>% c
  
  SpocialList[[a]]$FinalModel %>% 
    INLAFit(TestHinds, Covar, NDraw = 100, Draw = T) %>% 
    map_dbl(mean) -> Intercepts
  
  SpocialList[[a]]$FinalModel %>% 
    GetEstimates("Degree.t0", NDraw = 100, Draws = T) -> 
    Slopes
  
  B = mean(Slopes)
  A = mean(Intercepts)
  Y = A + B*X
  
  B2 = summary(SpocialList[[a]]$Spatial$Model)$fixed["Degree.t0","mean"]
  A2 = A
  Y2 = A2 + B2*X
  
  1:length(Slopes) %>% 
    map(~data.frame(X = X,
                    Y = exp(X*Slopes[[.x]] + Intercepts[[.x]]))) -> 
    SlopeDF
  
  SlopeDF %<>% bind_rows(.id = "Rep")
  
  FitLine <- data.frame(
    Degree.t0 = rep(X, 2),
    Y = exp(c(Y, Y2)),
    Model = rep(c("Base","SPDE"), each = length(X))
  )
  
  TestHinds$Y <- TestHinds[,a]
  
  P <- SpocialList[[a]]$FinalModel %>% INLAPValue("Degree.t0")
  
  if(P[[1]]>10^-4){
    
    P <- P[[1]] %>% round(4)
    
  }
  
  SpocialList[[a]]$FinalModel %>% 
    
    GetEstimates(Variable = "Degree.t0") %>%
    paste0("; P = ", P) -> 
    LabelList[[a]]
  
  LabelYList[[a]] <- max(TestHinds$Y + diff(range(TestHinds$Y)))
  
  LabelXList[[a]] <- median(X)
  
  LabelDf <- data.frame(
    
    Degree.t0 = LabelXList[[a]],
    Y = LabelYList[[a]],
    Label = LabelList[[a]]
    
  )
  
  Figure4List[[a]] <- 
    TestHinds %>% RandomSlice %>% mutate(Group = factor(1:n())) %>%
    ggplot(aes(Degree.t0, log(Y+1))) + 
    geom_point(alpha = 0.3, aes(colour = Group)) + 
    geom_line(alpha = 0.05, data = SlopeDF, aes(X, group = Rep)) +
    scale_colour_discrete_sequential(palette = ParasitePalettes[[a]], 
                                     guide = F) +
    geom_line(data = FitLine, aes(lty = Model, group = Model), size = 1)  +
    labs(y = ParasiteLabels[[a]], x = "Degree") +
    geom_text(data = LabelDf, aes(label = Label), hjust = 0.5,
              colour = ParasiteColours[[a]], size = 3) +
    lims(y = c(0, NA))
  
}

EcLabel <- 
  expression(paste("log(", italic("E.cervi"), " + 1)"))

Figure4List[[2]] <- Figure4List[[2]] + labs(y = EcLabel)
Figure4List[[1]] <- Figure4List[[1]] + 
  scale_colour_discrete_sequential(palette = ParasitePalettes[[1]], 
                                   guide = F)

# Within-individual deviance ####

ParasiteModels <- readRDS("Output Files/ParasiteModels2.rds")

SpocialList <- ParasiteModels# %>% map("FinalModel")

ParasiteLabels <- c("log(Strongyles + 1)",
                    "log(E.cervi + 1)",
                    "IgA OD")
names(ParasiteLabels) <- Parasites[c(1, 3, 4)]

Covar <- c("Season", "Age", "ReprodStatus", "fYear")

Figure4bList <- LabelYList <- LabelList <- LabelXList <- list()

for(a in Parasites[c(1, 3)]){
  
  print(a)
  
  TestHinds <- ParasiteModels[[a]]$Data
  SocMesh <- SpocialList[[a]]$Spatial$Mesh
  
  X = seq(min(TestHinds$Degree.Within.t0), 
          max(TestHinds$Degree.Within.t0), 
          length.out = nrow(TestHinds)) %>% c
  
  SpocialList[[a]]$FinalModel %>% 
    INLAFit(TestHinds, Covar, NDraw = 100, Draw = T) %>% 
    map_dbl(mean) -> Intercepts
  
  SpocialList[[a]]$FinalModel %>% 
    GetEstimates("Degree.Within.t0", NDraw = 100, Draws = T) -> 
    Slopes
  
  B = mean(Slopes)
  A = mean(Intercepts)
  Y = A + B*X
  
  B2 = summary(SpocialList[[a]]$Spatial$Model)$fixed["Degree.Within.t0","mean"]
  A2 = A
  Y2 = A2 + B2*X
  
  1:length(Slopes) %>% 
    map(~data.frame(X = X,
                    Y = exp(X*Slopes[[.x]] + Intercepts[[.x]]))) -> 
    SlopeDF
  
  SlopeDF %<>% bind_rows(.id = "Rep")
  
  FitLine <- data.frame(
    Degree.Within.t0 = rep(X, 2),
    Y = exp(c(Y, Y2)),
    Model = rep(c("Base","SPDE"), each = length(X))
  )
  
  TestHinds$Y <- TestHinds[,a]
  
  P <- SpocialList[[a]]$FinalModel %>% INLAPValue("Degree.Within.t0")
  
  if(P[[1]]>10^-4){
    
    P <- P[[1]] %>% round(4)
    
  }
  
  SpocialList[[a]]$FinalModel %>% 
    
    GetEstimates(Variable = "Degree.Within.t0") %>%
    paste0("; P = ", P) -> 
    LabelList[[a]]
  
  LabelYList[[a]] <- max(TestHinds$Y + diff(range(TestHinds$Y)))
  
  LabelXList[[a]] <- median(X)
  
  LabelDf <- data.frame(
    
    Degree.Within.t0 = LabelXList[[a]],
    Y = LabelYList[[a]],
    Label = LabelList[[a]]
    
  )
  
  Figure4bList[[a]] <- 
    TestHinds %>% RandomSlice %>% mutate(Group = factor(1:n())) %>%
    ggplot(aes(Degree.Within.t0, log(Y+1))) + 
    geom_point(alpha = 0.3, aes(colour = Group)) + 
    geom_line(alpha = 0.05, data = SlopeDF, aes(X, group = Rep)) +
    scale_colour_discrete_sequential(palette = ParasitePalettes[[a]], 
                                     guide = F) +
    geom_line(data = FitLine, aes(lty = Model, group = Model), size = 1)  +
    labs(y = ParasiteLabels[[a]], x = "Degree\n(Within-individual deviation)") +
    geom_text(data = LabelDf, aes(label = Label), hjust = 0.5,
              colour = ParasiteColours[[a]], size = 3) +
    lims(y = c(0, NA)) + scale_x_reverse()
  
}

EcLabel <- 
  expression(paste("log(", italic("E.cervi"), " + 1)"))

Figure4bList[[2]] <- Figure4bList[[2]] + labs(y = EcLabel)
Figure4bList[[1]] <- Figure4bList[[1]] + 
  scale_colour_discrete_sequential(palette = ParasitePalettes[[1]], 
                                   guide = F)

((Figure4List %>% 
    ArrangeCowplot()) /
    (Figure4bList %>% ArrangeCowplot())) +
  plot_layout(guides = "collect") +
  plot_layout(nrow = 2) +
  plot_annotation(tag_levels = "A") +
  ggsave("Figures/Figure4.jpeg", 
         width = 200, height = 200, 
         dpi = 600, units = "mm")

