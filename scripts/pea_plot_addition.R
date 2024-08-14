
library(tidyverse)
library(readr)


###############################

#import old plot assignments

old_plots <- read_csv("data/2024_winter_oat_pea_plot_design.csv")



####adding in extra 18 plots 9 pairs of sole pea in order to have nice square blocks

### create and save random number to add to pair numbers
set.seed(1)
rand <- runif(n = 9, min = 0, max = 20)


pea_plot_add_in <- old_plots %>% 
  filter(inter_crop == "pea") %>%   # keep only pea pairs 
  distinct(block, .keep_all = TRUE) %>%   # remove duplicate pairs
  mutate(base_pair = seq(0, 160, by = 20)) %>% #create base pair number column 
  mutate(pair_rand = rand) %>%  #create random number column 
  mutate(base_pair_rand = base_pair + pair_rand) %>% #add random number to base pair
  mutate(pair = base_pair_rand) %>%  #rename base pair as pair 
  mutate(plot = 9999) %>% # create dummy plot numbers
  select(plot,pair,block,accession,state,crop,source,inter_crop) %>% 
  mutate(count = (2)) %>% # add count column
  uncount(count) # use count column to duplicate all rows. 


plots <- bind_rows(pea_plot_add_in,old_plots) %>% # combine old plots and new pea plots
  arrange(pair)  # sort by pairs


plots <-  plots %>% mutate("plot_2" = seq(1:nrow(plots))) %>%  #add new plot numbers
  mutate(old_plot = plot) %>% 
  mutate(plot= plot_2) %>% 
  select(plot:inter_crop,old_plot)


#save as csv
plots %>% 
  write.csv("output/2024_winter_oat_pea_plot_design_2.csv",row.names = F)
