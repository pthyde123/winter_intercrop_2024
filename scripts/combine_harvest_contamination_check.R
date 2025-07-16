


library(readxl)
library(readr)
library(tidyverse)
harvest_order_6565 <- read_csv("data/harvest_order_6565.csv") %>% 
  select(plot_name,harvest_order,plot_number)

Cornell_WinterOatPeaIntercrop_2024_Ithaca <- read_csv("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca.csv")

Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta <- read_excel("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta.xlsx")



colnames(Cornell_WinterOatPeaIntercrop_2024_Ithaca)


df <- Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  
  filter(germplasmName != "Winter Hayden") %>%  # remove winter hayden, it was not planted, plots are pea only

  filter(observationLevel == "plot") %>% 
  select(observationUnitName,`Pea grain weight - g|COMP:0000009`,`Grain weight - g|CO_350:0005123`) %>% 
  left_join(harvest_order_6565, by = join_by("observationUnitName" == "plot_name")) %>% 
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta, by = join_by("observationUnitName" == "plot_name") ) %>% 
  filter (inter_crop %in% c("oat","pea", "oat-pea")) %>% 
  select(observationUnitName,`Pea grain weight - g|COMP:0000009`, `Grain weight - g|CO_350:0005123`,harvest_order,plot_number,inter_crop) %>% 
  arrange(harvest_order)


prior_crop <- df %>% 
  mutate(prior_harvest = harvest_order + 1) %>% 
  select(inter_crop, prior_harvest) %>% 
  rename("prior_crop" = "inter_crop")

##### oat in pea #####
pea_cont <- df %>% 
  left_join(prior_crop, by = join_by("harvest_order" == "prior_harvest"), keep = TRUE) %>% 
  filter(inter_crop != "oat-pea") %>% 
  filter(inter_crop == "pea") %>% 
  filter(!is.na(prior_crop)) %>% 
  filter(inter_crop != prior_crop) %>% 
  replace(is.na(.), 0) %>% 
  mutate(percent_contamination = (`Grain weight - g|CO_350:0005123`/ `Pea grain weight - g|COMP:0000009`)*100)

hist(pea_cont$percent_contamination)                

mean(pea_cont$percent_contamination)
  


##### pea in oat #####
oat_cont <- df %>% 
  left_join(prior_crop, by = join_by("harvest_order" == "prior_harvest"), keep = TRUE) %>% 
  filter(inter_crop != "oat-pea") %>% 
  filter(inter_crop == "oat") %>% 
  filter(!is.na(prior_crop)) %>% 
  filter(inter_crop != prior_crop) %>% 
  replace(is.na(.), 0) %>% 
  mutate(percent_contamination = (`Pea grain weight - g|COMP:0000009` / `Grain weight - g|CO_350:0005123`)*100) 

hist(oat_cont$percent_contamination)                

mean(oat_cont$percent_contamination)


##############

map <- Cornell_WinterOatPeaIntercrop_2024_Ithaca  %>% 
  filter(observationLevel == "plot") %>% 
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta, by = join_by("observationUnitName" == "plot_name") )%>% 
  select(colNumber,rowNumber,observationUnitName,inter_crop)
  
library(RColorBrewer) 
map %>% 
  ggplot(aes(x=colNumber,y=rowNumber,fill= inter_crop))+
  geom_tile(color = "white",
            lwd = 0.5,
            linetype = 1)+
  xlab("")+
  ylab("")+
  scale_fill_brewer(palette="Set1")+
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line=element_blank())


                                  