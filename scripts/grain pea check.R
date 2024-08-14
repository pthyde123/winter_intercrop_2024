
library(tidyverse)

library(readr)
Cornell_WinterOatPeaIntercrop_2024_Ithaca <- read_csv("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca.csv")


library(readxl)
Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta <- read_excel("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta.xlsx")




Cornell_WinterOatPeaIntercrop_2024_Ithaca
Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta


colnames(Cornell_WinterOatPeaIntercrop_2024_Ithaca)


####  check for peas in grain mono  ####

pea_check <- Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta,join_by(observationUnitDbId == plot_id) ) %>% 
  select(observationLevel,observationUnitName,observationUnitDbId,inter_crop ,`Grain yield - g/m2|CO_350:0000260`,`Pea grain yield - g/m2|COMP:0000049`) %>% 
  filter(observationLevel == "plot") %>% 
  mutate("check" = if_else((inter_crop == "oat" | inter_crop == "barley"),"grain","pea")) %>% 
  filter(check == "grain") 
 

hist(pea_check$`Pea grain yield - g/m2|COMP:0000049`) ###  looks like two grain plots have a lot of pea
 
  
Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta,join_by(observationUnitDbId == plot_id) ) %>% 
  select(observationLevel,observationUnitName,observationUnitDbId,inter_crop ,`Grain yield - g/m2|CO_350:0000260`,`Pea grain yield - g/m2|COMP:0000049`) %>% 
  filter(observationLevel == "plot") %>% 
  mutate("check" = if_else((inter_crop == "oat" | inter_crop == "barley"),"grain","pea")) %>% 
  filter(check == "pea") %>% 
  filter(`Pea grain yield - g/m2|COMP:0000049` > 5)   #### plots 232 and 319 look like they are reversed




####  check for grain in pea mono  ####


grain_check <- Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta,join_by(observationUnitDbId == plot_id) ) %>% 
  select(observationLevel,observationUnitName,observationUnitDbId,inter_crop ,`Grain yield - g/m2|CO_350:0000260`,`Pea grain yield - g/m2|COMP:0000049`) %>% 
  filter(observationLevel == "plot") %>% 
  mutate("check" = if_else((inter_crop == "oat" | inter_crop == "barley"),"grain","pea")) %>% 
  filter(check == "pea") 


hist(grain_check$`Grain yield - g/m2|CO_350:0000260`) 


Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta,join_by(observationUnitDbId == plot_id) ) %>% 
  select(observationLevel,observationUnitName,observationUnitDbId,inter_crop ,`Grain yield - g/m2|CO_350:0000260`,`Pea grain yield - g/m2|COMP:0000049`) %>% 
  filter(observationLevel == "plot") %>% 
  mutate("check" = if_else((inter_crop == "oat" | inter_crop == "barley"),"grain","pea")) %>% 
  filter(check == "grain") %>% 
  filter(`Pea grain yield - g/m2|COMP:0000049` > 20)   

#####


Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta,join_by(observationUnitDbId == plot_id) ) %>% 
  select(observationLevel,observationUnitName,observationUnitDbId,inter_crop ,`Grain yield - g/m2|CO_350:0000260`,`Pea grain yield - g/m2|COMP:0000049`) %>% 
  filter(observationLevel == "plot") %>% 
  filter(inter_crop == "pea") %>% 
  print(n = 36)   ###  164, 353, 312  have a high quantity of grain in the pea plot
 

#####  Pea yield in sole oat / barley plots


Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta,join_by(observationUnitDbId == plot_id) ) %>% 
  select(observationLevel,observationUnitName,observationUnitDbId,inter_crop ,`Grain yield - g/m2|CO_350:0000260`,`Pea grain yield - g/m2|COMP:0000049`,grain, pea) %>% 
  filter(observationLevel == "plot") %>% 
  filter(grain == "1" & pea== "0") %>% 
  print(n=171)


Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta,join_by(observationUnitDbId == plot_id) ) %>% 
  select(observationLevel,observationUnitName,observationUnitDbId,inter_crop ,`Grain yield - g/m2|CO_350:0000260`,`Pea grain yield - g/m2|COMP:0000049`,grain, pea) %>% 
  filter(observationLevel == "plot") %>% 
  filter(grain == "1" & pea== "0") %>% 
  filter(`Pea grain yield - g/m2|COMP:0000049` > 10)



#####  Oats yield in sole pea plots


Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta,join_by(observationUnitDbId == plot_id) ) %>% 
  select(observationLevel,observationUnitName,observationUnitDbId,inter_crop ,`Grain yield - g/m2|CO_350:0000260`,`Pea grain yield - g/m2|COMP:0000049`,grain, pea) %>% 
  filter(observationLevel == "plot") %>% 
  filter(grain == "0" & pea== "1") %>% 
  print(n=36)


Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta,join_by(observationUnitDbId == plot_id) ) %>% 
  select(observationLevel,observationUnitName,observationUnitDbId,inter_crop ,`Grain yield - g/m2|CO_350:0000260`,`Pea grain yield - g/m2|COMP:0000049`,grain, pea) %>% 
  filter(observationLevel == "plot") %>% 
  filter(grain == "0" & pea== "1") %>%  
  filter(`Grain yield - g/m2|CO_350:0000260` > 10)  ###  164, 353, 312  have a high quantity of grain in the pea plot

