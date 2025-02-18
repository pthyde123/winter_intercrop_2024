library("dplyr")                                                 
library("plyr")                                                  
library("readr")   
library("readxl") 


### Import spectral data 5-12-24 ####
spectral_data <- list.files(path = "data/5-12-24_subplot",     
                       pattern = "*.xlsx", 
                       full.names = TRUE) %>%  
  lapply(read_excel) %>%                                            
  bind_cols() %>% 
  select(1:4,contains("mean")) %>% 
  rename(prow = prow...1) %>% 
  rename(pcol = pcol...2) %>%
  rename(grow = grow...3) %>%
  rename(gcol = gcol...4) %>% 
  mutate(pg_id = str_c(prow,"_",pcol,"_",grow,"_",gcol))


spectral_data 

### boundary, grid, plot meta 
plot_number_to_plot_boundary <- read_csv("output/plot_number_to_plot_boundary.csv") 

plot_number_to_plot_boundary <- plot_number_to_plot_boundary %>% 
  mutate(plot_subplot = str_c(plot_number,"_",subplot)) %>% 
  mutate(pg_id = str_c(prow,"_",pcol,"_",grow,"_",gcol))

plot_number_to_plot_boundary


###

df <- spectral_data %>% 
  left_join(plot_number_to_plot_boundary,join_by(pg_id) ) %>% 
  select(plot_number,subplot,plot_subplot,contains("mean")) %>% 
  filter(plot_number < 800) %>% 
  arrange(plot_number,subplot)




spectral_T3 <- Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  
  mutate(plot_name = gsub("\\_subplot_.*", "", observationUnitName)) %>% 
  
  filter(observationLevel == "subplot") %>% 
  mutate(subplot_number = str_sub(observationUnitName,-1)) %>% ## DANGER this only works if you filter to subplot first
  
  mutate(plot_subplot = str_c(plotNumber,"_",subplot_number)) %>% 
  
  left_join(df,join_by(plot_subplot)) %>% 
  
  select(observationUnitName,observationUnitDbId,plot_number,subplot,plot_subplot,contains("mean"))


write.csv(spectral_T3,"output/spectral_5-12-24_data.csv",row.names=FALSE)



###############################################################################


### Import spectral data 6-14-24 ####
spectral_data <- list.files(path = "data/6-14-24_subplot",     
                            pattern = "*.xlsx", 
                            full.names = TRUE) %>%  
  lapply(read_excel) %>%                                            
  bind_cols() %>% 
  select(1:4,contains("mean")) %>% 
  dplyr::rename(prow = prow...1) %>% 
  dplyr::rename(pcol = pcol...2) %>%
  dplyr::rename(grow = grow...3) %>%
  dplyr::rename(gcol = gcol...4) %>% 
  mutate(pg_id = str_c(prow,"_",pcol,"_",grow,"_",gcol))


spectral_data 

### boundary, grid, plot meta 
plot_number_to_plot_boundary <- read_csv("output/plot_number_to_plot_boundary.csv") 

plot_number_to_plot_boundary <- plot_number_to_plot_boundary %>% 
  mutate(plot_subplot = str_c(plot_number,"_",subplot)) %>% 
  mutate(pg_id = str_c(prow,"_",pcol,"_",grow,"_",gcol))

plot_number_to_plot_boundary


###

df <- spectral_data %>% 
  left_join(plot_number_to_plot_boundary,join_by(pg_id) ) %>% 
  select(plot_number,subplot,plot_subplot,contains("mean")) %>% 
  filter(plot_number < 800) %>% ### remove guard plots
  arrange(plot_number,subplot)




spectral_T3 <- Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  
  mutate(plot_name = gsub("\\_subplot_.*", "", observationUnitName)) %>% 
  
  filter(observationLevel == "subplot") %>% 
  mutate(subplot_number = str_sub(observationUnitName,-1)) %>% ## DANGER this only works if you filter to subplot first
  
  mutate(plot_subplot = str_c(plotNumber,"_",subplot_number)) %>% 
  
  left_join(df,join_by(plot_subplot)) %>% 
  
  select(observationUnitName,observationUnitDbId,plot_number,subplot,plot_subplot,contains("mean"))


write.csv(spectral_T3,"output/spectral_6-14-24_data.csv",row.names=FALSE)


