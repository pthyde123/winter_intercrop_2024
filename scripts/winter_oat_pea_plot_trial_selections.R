
library(tidyverse)
library(readr)




library(readxl)
Cornell_WinterOatFounders_2024_Headrow_phenotypes <- read_excel("data/Cornell_WinterOatFounders_2024_Headrow_phenotypes.xlsx")


OatTraitBLUPs <-read_csv("data/selind_ListOfAccessionsPlantFall2024.csv")



winter_2024_accession_max_seedlots <- read_csv("output/winter_2024_accession_max_seedlots.csv")


#####  Select top 80% by SelIndex

keepers = round(nrow(OatTraitBLUPs)*.77)  ## number to keep, dropped from .8 to .77 to get final to 72 plots, even number.




plot_trial <- OatTraitBLUPs %>% 
  mutate("germplasmName" = Oat) %>% 
  select(germplasmName,selIndex) %>% 
  arrange(selIndex) %>% 
  slice_max(selIndex, n = keepers) %>% 
  mutate("plots_to_plant" = 4) %>% 
  left_join(winter_2024_accession_max_seedlots,join_by(germplasmName)) %>% ### add in seed lot weights
  select(observationUnitName,germplasmName,`Grain weight - g|CO_350:0005123`,plots_to_plant,selIndex) %>% 
  print( n = nrow(OatTraitBLUPs))



colnames(Cornell_WinterOatFounders_2024_Headrow_phenotypes)

headrow_trial <- Cornell_WinterOatFounders_2024_Headrow_phenotypes %>% 
  select(observationUnitName,"germplasmName",`Grain weight - g|CO_350:0005123`) %>% 
  filter(`Grain weight - g|CO_350:0005123` > 86) %>%    ### selected by enough seed for 2 plots (81g) plus a little extra to save
  mutate("plots_to_plant" = 2) %>% 
  print( n = nrow(Cornell_WinterOatFounders_2024_Headrow_phenotypes))

mono_pea<- tibble(germplasmName = "Blaze")

bind_rows(plot_trial,headrow_trial,mono_pea) %>% 
  write.csv('output/winter_oat_pea_plot_trial_selections.csv')


