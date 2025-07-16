
library(readxl)
library(readr)
library(tidyverse)

#### Plots for seedlots planting 2026 (fall 2025)
# harvest order
harvest_order_6565 <- read_csv("data/harvest_order_6565.csv") %>% # havest order can be downloaded from T3 if there is a spacial layout
  select(plot_name,harvest_order,plot_number)

# phenotype data from T3
Cornell_WinterOatPeaIntercrop_2024_Ithaca <- read_csv("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca.csv")

# meta data with combo inf0 and T3 observationUnitName used as unique ID
Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta <- read_excel("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta.xlsx")



colnames(Cornell_WinterOatPeaIntercrop_2024_Ithaca)

# select phenotype columns and join with harvest order and metadata
df <- Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  filter(observationLevel == "plot") %>% 
  select(observationUnitName,germplasmName,`Grain weight - g|CO_350:0005123`,`Pea grain weight - g|COMP:0000009`) %>% 
  left_join(harvest_order_6565, by = join_by("observationUnitName" == "plot_name")) %>% 
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta, by = join_by("observationUnitName" == "plot_name") ) %>% 
  select(observationUnitName,germplasmName, `Grain weight - g|CO_350:0005123`,`Pea grain weight - g|COMP:0000009`,harvest_order,plot_number,inter_crop) %>% 
  arrange(harvest_order)

# create list of intercrop in prior harvested plot
prior_crop <- df %>% 
  mutate(prior_harvest = harvest_order + 1) %>% 
  mutate(prior_crop = inter_crop) %>% 
  select(inter_crop, harvest_order, prior_harvest,prior_crop) %>%  
  select(  prior_harvest,prior_crop) 

seedlots<- df %>% 
  left_join(prior_crop, by = join_by("harvest_order" == "prior_harvest"), keep = TRUE) %>% 
  mutate(prior_crop = if_else(plot_number == 168 | plot_number == 166| plot_number == 338|plot_number == 324,"barley",prior_crop )) %>% # adjust the prior crop for the few plots that had barley guard rows before them.
  mutate(prior_crop = if_else(germplasmName == "Winter Hayden", "pea", prior_crop)) %>% # Winter hayden was not planted plots were pea monocrop
  mutate(prior_rank = if_else(prior_crop == "barley",0,    #add rank value for prior intercop plot type, barley is lowest
                              if_else(prior_crop == "barley-pea",1,
                                      if_else(prior_crop == "oat",2,
                                              if_else(prior_crop == "oat-pea", 3,
                                                      if_else(prior_crop == "pea", 4, -10))))))%>% 
  arrange(germplasmName,-prior_rank) %>% 
  group_by(germplasmName) %>% 
  filter(prior_rank == max(prior_rank, na.rm=TRUE)) %>% # select plots within germplasm name with highest rank
  filter(`Grain weight - g|CO_350:0005123` == max(`Grain weight - g|CO_350:0005123`, na.rm=TRUE)) %>%  # if rank value is equal, choose plot with greater seed. 
  arrange(plot_number) %>% # put them in numeric order for pulling
  filter(germplasmName != "Winter Hayden") %>% # remove plots with no oats actually planted
  filter(germplasmName != "NO_OATS_PLANTED")  # remove plots with no oats actually planted


seedlots


write.csv(seedlots,"./data/2024_WOP_seedlots_for_2026_planting.csv", row.names = F)






##plots with Barley guard rows before. 
168,166,338,324 
