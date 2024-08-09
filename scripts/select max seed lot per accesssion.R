
library(tidyverse)
library(readxl)

X2024_winter_data <- read_excel("Intercrop JLJ/2024_winter/2024_winter_intercrop/data/2024_winter_data.xlsx", 
                                sheet = "2024_winter_subplot")




X2024_winter_data %>% 
  select(plot_number,subplot_number,subplot_id,accession_name,`Grain weight - g			|CO_350:0005123`) %>%
  mutate("grain_weight" = as.numeric(`Grain weight - g			|CO_350:0005123`)) %>% 
  group_by(accession_name) %>% 
  filter(grain_weight == max(grain_weight,na.rm=TRUE)) %>% 
  write.table("clipboard", sep="\t", row.names=FALSE)
