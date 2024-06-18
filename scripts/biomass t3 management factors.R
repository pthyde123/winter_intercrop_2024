


library(readxl)
library(tidyverse)


plot_data <- read_excel("data/2024_winter_data.xlsx", 
                        sheet = "2024_winter_plot")


subplot_data <- read_excel("data/2024_winter_data.xlsx", 
                           sheet = "2024_winter_subplot")



meta_data <- plot_data %>% 
  select(plot_id,plot_number,barley_accession,pea_accession,accession,crop,biomass_1_sampled,biomass_2_planned)

### creating biomass harvest managment factors for t3 

subplot_data %>% 
  left_join(meta_data, join_by(plot_number)) %>% ### watch out this puts the plot-subsampe for all subplots 
  select(subplot_id,subplot_number,biomass_1_sampled,biomass_2_planned) %>% 
  mutate(biomass_t1 = if_else(subplot_number == biomass_1_sampled, 1, NA)) %>% 
  mutate(biomass_t2 = if_else(subplot_number == biomass_2_planned, 1, NA)) %>% 
  select(subplot_id,subplot_number,biomass_t1,biomass_t2) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)








