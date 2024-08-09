

library(readxl)
library(tidyverse)


plot_data <- Diversity_Plot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Diversity_Plot_2024.xls")


subplot_data <- Diversity_SubPlot_2024 <- read_excel("~/Intercrop JLJ/2024_spring/Data/Diversity_SubPlot_2024.xls")


meta_data <- plot_data %>% 
  select(plot_id,plot_number,accession_name)

subplot_data %>% 
  mutate(`pea_height_8-2-24`= as.numeric(`pea_height_8-2-24`)) %>% 
  mutate(t4_canopy_cm = as.numeric(pmax(`oat_height_8-2-24`,`pea_height_8-2-24`,na.rm=TRUE))) %>% 
  select(block_number,subplot_id,plot_number,subplot_number,t4_canopy_cm,`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T4`,`oat_height_8-2-24`,`pea_height_8-2-24`) %>% 
  left_join(meta_data, join_by(plot_number)) %>% 
  filter(!is.na(`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T4`)) %>% 
  print(n=300)



subplot_data %>% 
  mutate(t4_canopy_cm = as.numeric(pmax(`oat_height_8-2-24`,`pea_height_8-2-24`,na.rm=TRUE))) %>% 
  select(block_number,subplot_id,plot_number,subplot_number,t4_canopy_cm,`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T4`,`oat_height_8-2-24`,`pea_height_8-2-24`) %>% 
  left_join(meta_data, join_by(plot_number)) %>% 
  filter(!is.na(`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T4`)) %>% 
  group_by(block_number) %>% 
  summarise(median = median(t4_canopy_cm),mean = mean(t4_canopy_cm)) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)


hist <- subplot_data %>% 
  mutate(`pea_height_7-15-24`= as.numeric(`pea_height_7-15-24`)) %>% 
  mutate(t3_canopy_cm = as.numeric(pmax(`oat_height_7-15-24`,`pea_height_7-15-24`))) %>% 
  select(block_number,subplot_id,plot_number,subplot_number,t3_canopy_cm,`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T3`) %>% 
  left_join(meta_data, join_by(plot_number)) %>% 
  filter(!is.na(`ManagementFactor:SpringOatPeaIntercrop_2024_NY_biomass_T3`))


ggplot(hist, aes(x=t3_canopy_cm)) + 
  geom_histogram(binwidth=3)+
  facet_wrap(~block_number)

