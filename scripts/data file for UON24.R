library(tidyverse)

library(readr)
Cornell_WinterOatPeaIntercrop_2024_Ithaca <- read_csv("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca.csv")


library(readxl)
Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta <- read_excel("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta.xlsx")




Cornell_WinterOatPeaIntercrop_2024_Ithaca
Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta


colnames(Cornell_WinterOatPeaIntercrop_2024_Ithaca)

colnames(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta)




Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta,join_by(observationUnitDbId == plot_id) ) %>% 
  filter(source == "UON") %>% 
  filter(inter_crop == "oat") %>% 
  rowwise() %>% 
  mutate("PLOT_YLD" = sum(`Grain yield - g/m2|CO_350:0000260`,`Pea grain yield - g/m2|COMP:0000049`,na.rm=TRUE)) %>% 
  select(observationLevel,observationUnitName,observationUnitDbId,inter_crop ,source,
         colNumber,rowNumber,replicate,plotNumber,germplasmName,
         PLOT_YLD,
         `Lodging severity - 0-9 Rating|CO_350:0005007`,
         `Freeze damage severity - 0-9 Rating|CO_350:0005001`,
         `Winter survival - percent|CO_350:0000170`) %>% 
  arrange(plotNumber) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)


Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta,join_by(observationUnitDbId == plot_id) ) %>% 
  filter(source == "UON") %>% 
  filter(inter_crop == "oat") %>% 
  rowwise() %>% 
  mutate("PLOT_YLD" = sum(`Grain yield - g/m2|CO_350:0000260`,`Pea grain yield - g/m2|COMP:0000049`,na.rm=TRUE)) %>% 
  select(germplasmName,
         PLOT_YLD,
         `Lodging severity - 0-9 Rating|CO_350:0005007`,
         `Freeze damage severity - 0-9 Rating|CO_350:0005001`,
         `Winter survival - percent|CO_350:0000170`) %>% 
  group_by(germplasmName) %>% 
  summarise_at(vars(PLOT_YLD:`Winter survival - percent|CO_350:0000170`), mean, na.rm = TRUE) %>%
  arrange(germplasmName) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)

