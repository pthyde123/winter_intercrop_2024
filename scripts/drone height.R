
library(tidyverse)
library(readr)
library(readxl)

## T3 plot-subplot data
Cornell_WinterOatPeaIntercrop_2024_Ithaca <- read_csv("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca.csv")

colnames(Cornell_WinterOatPeaIntercrop_2024_Ithaca)

## QGIS plot boundary id
plot_number_to_plot_boundary <- read_csv("output/plot_number_to_plot_boundary.csv")


### metadata #inter_crop
Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta <- read_excel("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta.xlsx")

meta <- Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta %>% 
  mutate(plotNumber = as.numeric(str_sub(plot_name,48,-1))) %>% 
  select(plotNumber, inter_crop, crop, source, pea_accession)


###QGIS height output
X5_12_24_subplot_height <- read_csv("data/5-12-24_subplot_height.csv")

height_ExG01_5_12_12 <- read_excel("data/height_ExG01_5-12-12.xlsx") # doesnt work that well

### drone flight Julian day 132

####
df1 <- Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  select("germplasmName","plotNumber","observationLevel","observationUnitName","Pea plant height - cm|day 134|COMP:0000062","Plant height - cm|day 134|COMP:0000054") %>% 
  filter(observationLevel == "subplot") %>% 
  mutate(subplot = str_sub(observationUnitName,-1)) %>% 
  mutate(plot_subplot = (str_c(plotNumber,"_",subplot))) %>% 
  left_join(meta, by = join_by(plotNumber == plotNumber))


df2 <- plot_number_to_plot_boundary %>% 
  filter(plot_number < 500) %>%  # remove guard plots, they are 900's
  mutate(plot_subplot = str_c(plot_number,"_",subplot)) %>% 
  mutate(prow_pcol_grow_gcol = str_c(prow,"_",pcol,"_",grow,"_",gcol))


df3<-left_join(df1,df2, by = join_by(plot_subplot)) %>% 
  mutate(height = pmax(`Pea plant height - cm|day 134|COMP:0000062`,`Plant height - cm|day 134|COMP:0000054`))

####
df4 <- X5_12_24_subplot_height %>% 
  mutate(prow_pcol_grow_gcol = str_c(prow,"_",pcol,"_",grow,"_",gcol)) %>% 
  select(prow_pcol_grow_gcol,`5-12-24_mean`)

 

df_5 <- left_join(df3,df4, by= join_by(prow_pcol_grow_gcol)) %>% 
  select(height,`5-12-24_mean`,"germplasmName",inter_crop,crop) %>% 
  mutate(drone_height = `5-12-24_mean` * 100) %>% 
  filter(height < 100) %>% 
  mutate(height_dif = height - drone_height) %>% 
  filter(crop != "Barley")


  
hist(df_5$height_dif)


plot(df_5$height,df_5$drone_height)


####
df_5 %>% 
ggplot(aes(height,drone_height,color=inter_crop))+
  geom_point() +
  geom_smooth(se = FALSE, method = lm)+
  xlab("Ruler Height")+
  ylab("Drone Height")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))



lm <- lm(height ~ (drone_height),
           data = df_5)

summary(lm)


