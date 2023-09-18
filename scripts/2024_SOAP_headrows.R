

library(readr)
library(tidyverse)


SOAP <- read_csv("data/2024_SOAP_Entries.csv")


# Block 1 randomization
SOAP_1 <- SOAP %>% 
  filter(Entry != 186) %>% 
  mutate (block = 1)
  
set.seed(1)
rand_1 <- runif(n = nrow(SOAP_1), min = 0, max = 1) 

SOAP_1 <- SOAP_1 %>% 
  mutate("rand" = rand_1)


# Block 2 randomization
SOAP_2 <- SOAP %>% 
  filter(Entry != 186) %>% 
  mutate (block = 2)

set.seed(2)
rand_2 <- runif(n = nrow(SOAP_2), min = 0, max = 1) 

SOAP_2 <- SOAP_2 %>% 
  mutate("rand" = rand_2)

# Combine blocks 1 and 2, arrange by random number, 

SOAP_headrows_2024 <- rbind(SOAP_1,SOAP_2) %>% 
  arrange(block,rand) %>% 
  mutate("plot" = seq(1:960)) %>%
  select(plot,block,Entry,Name,Pedigree)

SOAP_headrows_2024 %>% 
  write.csv("output/SOAP_headrows_2024.csv",row.names = F)







