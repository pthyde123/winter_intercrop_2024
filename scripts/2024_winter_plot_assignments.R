
library(tidyverse)
library(readr)

#import randomization
design <- read_csv("data/WinterOatDesign.csv")

#remove KY plots, keeping NY plot
ny_design <- design %>% 
  filter(state == "NY") 

#add in column of sequential numbers identifying plot pairs
ny_design <- mutate(ny_design, "pair" = seq(1:nrow(ny_design)))

#create dataframe with intercrop identified as oat-pea
oat_pea <- ny_design %>% 
  mutate("inter_crop" = ("oat-pea"))

#create dataframe with intercrop identified as oat
oat <- ny_design %>% 
  mutate("inter_crop" = ("oat"))

#combine oat and oat_pea dataframes, now all pairs are represented twice
all_pairs <- bind_rows(oat_pea,oat)

#create string of random numbers between 0-1 the same length as the total number of plots
rand <- runif(n = nrow(all_pairs), min = 0, max = 1)

#add random numbers to the plan, sort by pair and random number, add in plot numbers
plot_design <- all_pairs %>% 
  mutate("rand" = rand) %>% 
  arrange(pair,rand) %>% 
  mutate("plot" = seq(1:nrow(all_pairs))) 


#clean up the interplot name to indicate sole pea and barley
plot_design <- plot_design %>% 
  mutate(inter_crop = if_else(plot_design$crop == "Pea", "pea",plot_design$inter_crop)) %>% 
  mutate("inter_crop" = if_else(crop == "Barley", 
                           str_replace(plot_design$inter_crop,"oat","barley"),
                           plot_design$inter_crop )) 

#select and organize columens and save as csv
plot_design %>% 
  select(plot,pair,block,accession,state,crop,source,inter_crop) %>% 
  write.csv("output/2024_winter_oat_pea_plot_design")
  
  
  

