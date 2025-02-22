### generate QGIS plugin plot boundary rows and columns for META data file

library(tidyverse)
library(readxl)


QGIS_plot_boundary_map <- read_excel("data/QGIS_plot_boundary_map.xlsx")

### this map has the t3 plot numbers with the row column designations assigned using plotBoundary in QGIS
### guard / fill plots are plot number in the 900'S

QGIS_plot_boundary_map


QGIS_plot_boundary_map %>% 
  pivot_longer(!row, names_to = "column", values_to = "plot") %>% 
  mutate(prow = row) %>% 
  mutate(pcol = column) %>% 
  mutate(plot_number = plot) %>% 
  select(plot_number,	prow,pcol) %>% 
  filter(!is.na(plot_number)) %>% 
  slice(rep(1:n(), each = 3)) %>% 
  mutate(grow = 1) %>% 
  mutate(gcol = rep(1:3,times=392)) %>% 
  mutate(subplot = rep(3:1,times=392))

  write.csv("output/plot_number_to_plot_boundary.csv",row.names=FALSE)
  








