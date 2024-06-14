



library(readxl)
library(tidyverse)


plot_data <- read_excel("data/2024_winter_data.xlsx", 
                                sheet = "2024_winter_plot")


subplot_data <- read_excel("data/2024_winter_data.xlsx", 
                        sheet = "2024_winter_subplot")



meta_data <- plot_data %>% 
  select(plot_id,plot_number,barley_accession,pea_accession,accession,crop,biomass_1_sampled,biomass_2_planned)


subplot_data %>% 
  mutate(t2_canopy_cm = pmax(`Plant Height - cm-6.13.24`,`Pea Plant Height - cm-6.13.24`)) %>% 
  select(block_number,subplot_id,plot_number,subplot_number,t2_canopy_cm) %>% 
  left_join(meta_data, join_by(plot_number)) %>% 
  filter(!is.na(biomass_2_planned)) %>% 
  filter(subplot_number == biomass_2_planned) %>% 
  filter(crop != "Barley") %>%
  group_by(block_number) %>% 
  summarise(median = median(t2_canopy_cm),mean = mean(t2_canopy_cm)) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)


hist <- subplot_data %>% 
  mutate(t2_canopy_cm = pmax(`Plant Height - cm-6.13.24`,`Pea Plant Height - cm-6.13.24`)) %>% 
  select(block_number,subplot_id,plot_number,subplot_number,t2_canopy_cm) %>% 
  left_join(meta_data, join_by(plot_number)) %>% 
  filter(!is.na(biomass_2_planned)) %>% 
  filter(subplot_number == biomass_2_planned) %>% 
  filter(crop != "Barley")

  
ggplot(hist, aes(x=t2_canopy_cm)) + 
  geom_histogram(binwidth=3)+
  facet_wrap(~block_number)






