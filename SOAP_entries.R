


library(readr)
library(dplyr)

SOAP <- read_csv("data/2024_SOAP_Entries.csv")



## the 26 entries that were repeated in 2024
## should they be repeated again

SOAP %>% 
  group_by(Name) %>% 
  filter(n()>1) %>% 
  distinct(Entry) %>% 
  print(n=26)





