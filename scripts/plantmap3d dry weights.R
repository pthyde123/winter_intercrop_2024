

library(readxl)
library(tidyverse)


plot_data <- read_excel("data/2024_winter_data.xlsx", 
                        sheet = "2024_winter_plot")


subplot_data <- read_excel("data/2024_winter_data.xlsx", 
                           sheet = "2024_winter_subplot")



meta_data <- plot_data %>% 
  select(plot_id,plot_number,barley_accession,pea_accession,accession,crop,biomass_1_sampled,biomass_2_planned)




### data for PlantMap3d T1

subplot_data %>% 
  select(subplot_id,biomass_t1,Oat,Pea,Other) %>% 
  filter(biomass_t1 == 1) %>% 
  replace(is.na(.), 0) %>% 
  filter(! subplot_id %in% c(716365,717185)) %>%    #### subplots with missing oat weights add them in later 
  pivot_longer(Oat:Other, names_to = "Species", values_to = "Dry Wt (g)" ) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)





### data for PlantMap3d T2

library(readr)
library(readxl)

Cornell_WinterOatPeaIntercrop_2024_Ithaca <- read_csv("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca.csv")
colnames(Cornell_WinterOatPeaIntercrop_2024_Ithaca)

Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta <- read_excel("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta.xlsx")

colnames(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta)




Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  select("observationLevel","plotNumber","observationUnitDbId","Above ground dry biomass - g|day 168|COMP:0000064" ,
         "Pea above ground dry biomass - g|day 168|COMP:0000061","Weed above ground dry biomass - g|day 168|COMP:0000066") %>% 
  filter(observationLevel == "subplot") %>% 
  filter(!is.na(`Above ground dry biomass - g|day 168|COMP:0000064`)) %>% 
  rename("Oat" = "Above ground dry biomass - g|day 168|COMP:0000064") %>% 
  rename("Pea" = "Pea above ground dry biomass - g|day 168|COMP:0000061") %>% 
  rename("Other" = "Weed above ground dry biomass - g|day 168|COMP:0000066") %>% 
  rename("Plot ID" = "observationUnitDbId") %>%     
  pivot_longer(Oat:Other, names_to = "Species", values_to = "Dry Wt (g)" ) %>% 
  select(`Plot ID`,Species, "Dry Wt (g)" ) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE, col.names = FALSE)






