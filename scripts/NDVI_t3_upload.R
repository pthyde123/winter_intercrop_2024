


library(readxl)
library(readr)
library(tidyverse)


### NDVI calculations are done in in QGIS and saved as a file for each flight date.
#data is summarized by subplot (1,2,3) mean and std are uploaded to T3 but other summary stats are saved
NDVI_10_17_23 <- read_excel("data/NDVI_10-17-23.xlsx")
NDVI_11_16_23 <- read_excel("data/NDVI_11-16-23.xlsx")
NDVI_04_01_24 <- read_excel("data/NDVI_04-01-24.xlsx")
NDVI_04_20_24 <- read_excel("data/NDVI_04-20-24.xlsx")
NDVI_05_12_24 <- read_excel("data/NDVI_05-12-24.xlsx")
NDVI_06_14_24 <- read_excel("data/NDVI_06-14-24.xlsx")
NDVI_07_08_24 <- read_excel("data/NDVI_07-08-24.xlsx")

# plot_number_to_plot_boundary file is used to convert from the plot_subplot number in T3 and
# and the prow, pcol, grow, gcol disignations in the plot boundary and plot grid plugin in QGIS
# this file is made with script "plot number and plot_boundary meta.R"
plot_number_to_plot_boundary <- read_csv("output/plot_number_to_plot_boundary.csv")

# the "template" file is downloaded from T3, "Upload Data Files" / Phenotyping Spreadsheets
# if you have a list of traits you can easily add the list to the phenotyping spread sheet

NDVI_template <- read_excel("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca_t3_NDVI_template.xlsx")


NDVI_template <- NDVI_template %>% select(-notes) # you can select no notes column in t3, I forgot to so I'm removing it




### combine the plot id's,  mean and std data for all flight dates

ndvi <- bind_cols(
  
  NDVI_10_17_23[,1:4],  # column 1-4 are the plot boundary and plot grid designations
  
  NDVI_10_17_23[,7],    # column 7 is the NDVI mean for the subplot
  NDVI_11_16_23[,7],
  NDVI_04_01_24[,7],
  NDVI_04_20_24[,7],
  NDVI_05_12_24[,7],
  NDVI_06_14_24[,7],
  NDVI_07_08_24[,7],
  
  NDVI_10_17_23[,9],  # column 9 is the NDVI std for the subplot
  NDVI_11_16_23[,9],
  NDVI_04_01_24[,9],
  NDVI_04_20_24[,9],
  NDVI_05_12_24[,9],
  NDVI_06_14_24[,9],
  NDVI_07_08_24[,9]
)




ndvi <- ndvi %>% 
  mutate(pg_id = str_c(prow,"_",pcol,"_",grow,"_",gcol)) %>% 
  
  
  left_join(plot_number_to_plot_boundary %>% 
              mutate(pg_id = str_c(prow,"_",pcol,"_",grow,"_",gcol)) %>% 
              select(plot_number,subplot,pg_id))




ndvi <- ndvi %>% 
  relocate(plot_number,subplot,pg_id) %>% 
  filter(plot_number < 800) %>% 
  mutate(plot_subplot = str_c("PLOT_",plot_number,"_subplot_",subplot)) %>% 
  relocate(plot_subplot)



observationunit_name <- NDVI_template %>% 
  mutate(plot_subplot = gsub(".*Ithaca-", "", observationunit_name)) %>% 
  select(observationunit_name,plot_subplot)


df <- ndvi %>% 
  left_join(observationunit_name) %>% 
  select(observationunit_name,`NDVI_10-17-23_mean`:`NDVI_07-08-24_stdev` )


colnames(NDVI_template)
colnames(df)


colnames(df)<-colnames(NDVI_template)


subplot_2 <- df %>% 
  mutate(subplot = gsub(".*_subplot_", "", observationunit_name)) %>% 
  relocate(subplot) %>% 
  filter(subplot == 2) %>% 
  select(!subplot)


write.csv(subplot_2,"output/Cornell_WinterOatPeaIntercrop_2024_Ithaca_t3_NDVI_upload.csv",row.names = FALSE)









