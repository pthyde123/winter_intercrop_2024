library(tidyverse)

library(readr)
Cornell_WinterOatPeaIntercrop_2024_Ithaca <- read_csv("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca.csv")


library(readxl)
Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta <- read_excel("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta.xlsx")



Cornell_WinterOatPeaIntercrop_2024_Ithaca
Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta


colnames(Cornell_WinterOatPeaIntercrop_2024_Ithaca)

colnames(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta)





plot_data <- Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  mutate("plot_name" = if_else(observationLevel == "subplot",str_sub(observationUnitName,end = -11), observationUnitName)) %>% 
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta,join_by(plot_name) ) %>% 
  rowwise() %>% 
  mutate(pea_yield = if_else(inter_crop == "pea",
                             sum(`Grain yield - g/m2|CO_350:0000260`,`Pea grain yield - g/m2|COMP:0000049`,na.rm=TRUE),`Pea grain yield - g/m2|COMP:0000049` )) %>% 
  mutate(oat_yield = if_else(inter_crop == "oat",
                             sum(`Grain yield - g/m2|CO_350:0000260`,`Pea grain yield - g/m2|COMP:0000049`,na.rm=TRUE),`Grain yield - g/m2|CO_350:0000260` )) %>% 
  mutate(pea_yield = if_else(inter_crop == "oat", 0 , pea_yield )) %>% 
  mutate(oat_yield = if_else(inter_crop == "pea", 0 , oat_yield )) %>% 
  select(observationLevel,observationUnitDbId,inter_crop ,source,
         replicate,plotNumber,germplasmName, inter_crop,
         oat_yield,
         pea_yield,
         `Lodging severity - 0-9 Rating|CO_350:0005007`,
         `Freeze damage severity - 0-9 Rating|CO_350:0005001`,
         `Winter survival - percent|CO_350:0000170`) %>% 
  filter(observationLevel == "plot") %>% 
  filter(source != "Barley") %>% 
  filter(source != "Martens") %>% 
  mutate("total_yield" = oat_yield+pea_yield)




library(RColorBrewer)

####  Yield Summary  ####

plot_data %>% 
    ggplot(aes(inter_crop,total_yield))+
      geom_boxplot() +
      geom_jitter(aes(inter_crop,total_yield,color=source),width = 0.25,size=4)+
      xlab("")+
      ylab("Total Grain g")+
      scale_color_brewer(palette="Paired")+
      theme_classic()+
      theme(axis.text.x=element_text(angle = 90, hjust = 1))+
      theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
      theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))


plot_data %>% 
  ggplot(aes(inter_crop,oat_yield))+
  geom_boxplot() +
  geom_jitter(aes(inter_crop,oat_yield,color=source),width = 0.25,size=4)+
  xlab("")+
  ylab("Oat Yield g")+
  scale_color_brewer(palette="Paired")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))


plot_data %>% 
  ggplot(aes(inter_crop,pea_yield))+
  geom_boxplot() +
  geom_jitter(aes(inter_crop,pea_yield,color=source),width = 0.25,size=4)+
  xlab("")+
  ylab("Pea Yield g")+
  scale_color_brewer(palette="Paired")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))


plot_data %>% 
  select(inter_crop,plotNumber, oat_yield, pea_yield) %>% 
  pivot_longer(oat_yield:pea_yield, names_to = "Crop", values_to = "grams") %>% 
  filter(grams != 0) %>% 
    ggplot(aes(inter_crop,grams))+
      geom_boxplot() +
      geom_jitter(aes(inter_crop,grams,color=Crop),width = 0.25,size=4)+
      xlab("")+
      ylab("Grams")+
      scale_color_brewer(palette="Paired")+
      theme_classic()+
      theme(axis.text.x=element_text(angle = 90, hjust = 1))+
      theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
      theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
      theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))

###
 

plot_data %>%
  group_by(inter_crop) %>% 
  summarise_at(c("oat_yield", "pea_yield"), mean, na.rm=TRUE)


plot_data %>%
  filter(inter_crop == "oat") %>% 
  group_by(germplasmName) %>% 
  summarise(oat_y = mean(oat_yield)) %>% 
  mutate(oat_mean = 227) %>% 
  mutate(oat_dif = oat_y-oat_mean) %>% 
  arrange(-oat_dif)


plot_data %>%
  filter(inter_crop == "oat-pea") %>% 
  group_by(germplasmName) %>% 
  summarise(oat_y = mean(oat_yield)) %>% 
  mutate(oat_mean = 227) %>% 
  mutate(oat_dif = oat_y-oat_mean) %>% 
  arrange(-oat_dif)



plot_data %>%
  group_by(germplasmName, inter_crop) %>% 
  summarise(oat_y = mean(oat_yield)) %>% 
  pivot_wider(names_from = inter_crop, values_from = oat_y) %>% 
  mutate(oat_dif = (`oat-pea`- oat)) %>% 
  arrange(-oat_dif) %>% 
  print(n=95)
  







###  Freeze Damage Summary  ###


plot_data %>%
  filter(source != "Pea") %>% 
  ggplot(aes(inter_crop,`Freeze damage severity - 0-9 Rating|CO_350:0005001`))+
  geom_boxplot() +
  geom_jitter(aes(inter_crop,`Freeze damage severity - 0-9 Rating|CO_350:0005001`,color=source),width = 0.25,size=4)+
  xlab("")+
  ylab("Freeze Damage")+
  scale_color_brewer(palette="Paired")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))+
  facet_wrap(~source)




###  Lodging Summary  ###

plot_data %>%
  ggplot(aes(inter_crop,`Lodging severity - 0-9 Rating|CO_350:0005007`))+
  geom_boxplot() +
  geom_jitter(aes(inter_crop,`Lodging severity - 0-9 Rating|CO_350:0005007`,color=source),width = 0.25,size=4)+
  xlab("")+
  ylab("Lodging")+
  scale_color_brewer(palette="Paired")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))+
  facet_wrap(~source)




#### Biomass


Cornell_WinterOatPeaIntercrop_2024_Ithaca
Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta





plot_data <- Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  mutate("plot_name" = if_else(observationLevel == "subplot",str_sub(observationUnitName,end = -11), observationUnitName)) %>% 
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta,join_by(plot_name) ) %>% 
  rowwise() %>% 
  mutate(pea_yield = if_else(inter_crop == "pea",
                             sum(`Grain yield - g/m2|CO_350:0000260`,`Pea grain yield - g/m2|COMP:0000049`,na.rm=TRUE),`Pea grain yield - g/m2|COMP:0000049` )) %>% 
  mutate(oat_yield = if_else(inter_crop == "oat",
                             sum(`Grain yield - g/m2|CO_350:0000260`,`Pea grain yield - g/m2|COMP:0000049`,na.rm=TRUE),`Grain yield - g/m2|CO_350:0000260` )) %>% 
  mutate(pea_yield = if_else(inter_crop == "oat", 0 , pea_yield )) %>% 
  mutate(oat_yield = if_else(inter_crop == "pea", 0 , oat_yield )) %>% 
  select(observationLevel,observationUnitDbId,inter_crop ,source,
         replicate,plotNumber,germplasmName, inter_crop,
         oat_yield,
         pea_yield,
         `Lodging severity - 0-9 Rating|CO_350:0005007`,
         `Freeze damage severity - 0-9 Rating|CO_350:0005001`,
         `Winter survival - percent|CO_350:0000170`) %>% 
  filter(observationLevel == "plot") %>% 
  filter(source != "Barley") %>% 
  filter(source != "Martens") %>% 
  mutate("total_yield" = oat_yield+pea_yield)



colnames(Cornell_WinterOatPeaIntercrop_2024_Ithaca)

colnames(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta)


df <- Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  select("observationUnitName","observationUnitDbId","observationLevel","Above ground dry biomass - g|day 136|COMP:0000063",
         "Pea above ground dry biomass - g|day 136|COMP:0000060",
         "Weed above ground dry biomass - g|day 136|COMP:0000065",
         "Above ground dry biomass - g|day 168|COMP:0000064",
         "Pea above ground dry biomass - g|day 168|COMP:0000061",
         "Weed above ground dry biomass - g|day 168|COMP:0000066",
         'Grain yield - g/m2|CO_350:0000260',
         'Pea grain yield - g/m2|COMP:0000049',
         "replicate",
         plotNumber,
         germplasmName,
         `Lodging severity - 0-9 Rating|CO_350:0005007`,
         `Freeze damage severity - 0-9 Rating|CO_350:0005001`,
         `Winter survival - percent|CO_350:0000170`
         ) %>% 
  filter(!is.na(`Above ground dry biomass - g|day 136|COMP:0000063` )) %>% 
  mutate("plot_name" = if_else(observationLevel == "subplot",str_sub(observationUnitName,end = -11), observationUnitName)) %>% 
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta,join_by(plot_name) ) %>% 
  rowwise() %>% 
  mutate(pea_yield = if_else(inter_crop == "pea",
                             sum(`Grain yield - g/m2|CO_350:0000260`,`Pea grain yield - g/m2|COMP:0000049`,na.rm=TRUE),`Pea grain yield - g/m2|COMP:0000049` )) %>% 
  mutate(oat_yield = if_else(inter_crop == "oat",
                             sum(`Grain yield - g/m2|CO_350:0000260`,`Pea grain yield - g/m2|COMP:0000049`,na.rm=TRUE),`Grain yield - g/m2|CO_350:0000260` )) %>% 
  mutate(pea_yield = if_else(inter_crop == "oat", 0 , pea_yield )) %>% 
  mutate(oat_yield = if_else(inter_crop == "pea", 0 , oat_yield )) %>% 
  select(observationLevel,observationUnitDbId,inter_crop ,source,
         replicate,plotNumber,germplasmName, inter_crop,
         oat_yield,
         pea_yield,
         `Lodging severity - 0-9 Rating|CO_350:0005007`,
         `Freeze damage severity - 0-9 Rating|CO_350:0005001`,
         `Winter survival - percent|CO_350:0000170`,
         "Pea above ground dry biomass - g|day 136|COMP:0000060",
         "Weed above ground dry biomass - g|day 136|COMP:0000065",
         "Above ground dry biomass - g|day 168|COMP:0000064",) %>% 
  filter(source != "Barley") %>% 
  filter(source != "Martens") %>% 
  mutate("total_yield" = oat_yield+pea_yield)






