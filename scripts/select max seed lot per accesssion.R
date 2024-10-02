
library(tidyverse)
library(readr)



Cornell_WinterOatPeaIntercrop_2024_Ithaca <- read_csv("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca.csv")


colnames(Cornell_WinterOatPeaIntercrop_2024_Ithaca)

  
  
Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
    select(observationUnitName,plotNumber,observationLevel,germplasmName,`Grain weight - g|CO_350:0005123`) %>% 
    filter(observationLevel == "plot")  %>%
    group_by(germplasmName) %>% 
    filter(`Grain weight - g|CO_350:0005123` == max(`Grain weight - g|CO_350:0005123`,na.rm=TRUE)) %>% 
    arrange(plotNumber) %>% 
    filter(germplasmName != "NO_OATS_PLANTED") %>% 
    filter(germplasmName != "Winter Hayden") %>% 
    write.csv('output/winter_2024_accession_max_seedlots.csv')
  
  





