







# Header:
#   The first row (header) should contain the following:
# 
# 
# 
#   Required fields:
#   seedlot_name (must be unique)
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
# 

library(tidyverse)
library(readr)
Cornell_WinterOatFounders_2024_GH_phenotypes <- read_csv("data/Cornell_WinterOatFounders_2024_GH_phenotypes.csv")

         
seedlot_header <- c("seedlot_name","accession_name","operator_name","amount","weight_gram","description","box_name","quality","source")



Cornell_WinterOatFounders_2024_GH_phenotypes %>% 
  select(germplasmName,plotNumber,`Grain weight - g|CO_350:0005123`)
