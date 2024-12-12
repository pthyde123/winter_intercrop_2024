# Header:
#   The first row (header) should contain the following:
# 
#   "seedlot_name","accession_name","operator_name","amount","weight_gram","description","box_name","quality","source"
# 
#   Required fields:
# seedlot_name (must be unique)
# accession_name (must exist in the database. the accession_name is the unique identifier for the named genotype)
# operator_name (the name of the person who oversaw the inventory process. can be any name.)
# amount (number of seeds in seedlot. can be provided in conjunction with weight_gram. must provide a value for amount or weight_gram or both.)
# AND/OR
# weight_gram (weight in grams of seedlot. can be provided in conjunction with amount. must provide a value for amount or weight_gram or both.)
# box_name (the box name that the seed is located in. can be any name.)
# Optional fields:
#   description (information about why this seedlot is being added)
# quality (status of the seedlot, for example "ok", "moldy", "insect damage" etc.
#          source (an alternate source, such as a plot, subplot, or plant identifier from which the seed was collected)
 



library(tidyverse)
library(readr)


# Import the trial you want to make the seed lot from 
trial <- Cornell_WinterOatFounders_2024_GH_phenotypes <- read_csv("data/Cornell_WinterOatFounders_2024_GH_phenotypes.csv")

operator_name = "Peter Hyde"
description = "Greenhouse single plant self seed produced from genotyped plants of the WOF population"
box_name = "box"
quality = "good"

        
seedlot_header <- c("seedlot_name","accession_name","operator_name","amount","weight_gram","description","box_name","quality","source")


# format of the seedlot name
# {accession_name}-{trial_name}-PLOT_{plot_number}


trial %>% 
  select(studyName,observationUnitName,germplasmName,plotNumber,`Grain weight - g|CO_350:0005123`,) %>% 
  
  mutate(seedlot_name = str_c(germplasmName,"-",studyName,"-PLOT_",plotNumber)) %>% 
  mutate(accession_name = germplasmName) %>% 
  mutate(operator_name = operator_name) %>% 
  mutate(amount = "") %>% 
  mutate(weight_gram = `Grain weight - g|CO_350:0005123`) %>% 
  mutate(description = description) %>% 
  mutate(box_name = box_name) %>% 
  mutate(quality = quality) %>% 
  mutate(source = observationUnitName) %>% 
  
  select(all_of(seedlot_header))















