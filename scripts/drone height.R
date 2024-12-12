

library(readr)
Cornell_WinterOatPeaIntercrop_2024_Ithaca <- read_csv("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca.csv")

colnames(Cornell_WinterOatPeaIntercrop_2024_Ithaca)
library(readxl)
library(readr)


plot_number_to_plot_boundary <- read_csv("output/plot_number_to_plot_boundary.csv")

Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta <- read_excel("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta.xlsx")






julian day 132

df1 <- Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  select("germplasmName","plotNumber","observationLevel","observationUnitName","Pea plant height - cm|day 134|COMP:0000062","Plant height - cm|day 134|COMP:0000054") %>% 
  filter(observationLevel == "subplot") %>% 
  mutate(subplot = str_sub(observationUnitName,-1)) %>% 
  mutate(plot_subplot = (str_c(plotNumber,"_",subplot)))

          
  
  


df2 <- plot_number_to_plot_boundary %>% 
  filter(plot_number < 500) %>% 
  mutate(plot_subplot = str_c(plot_number,"_",subplot)) %>% 
  mutate(prow_pcol_grow_gcol = str_c(prow,"_",pcol,"_",grow,"_",gcol))


df3<-left_join(df1,df2, by = join_by(plot_subplot)) %>% 
  mutate(height = pmax(`Pea plant height - cm|day 134|COMP:0000062`,`Plant height - cm|day 134|COMP:0000054`))




X5_12_24_subplot_height <- read_csv("data/5-12-24_subplot_height.csv")


df4 <- X5_12_24_subplot_height %>% 
  mutate(prow_pcol_grow_gcol = str_c(prow,"_",pcol,"_",grow,"_",gcol)) %>% 
  select(prow_pcol_grow_gcol,`5-12-24_mean`)
  

df_5 <- left_join(df3,df4, by= join_by(prow_pcol_grow_gcol)) %>% 
  select(height,`5-12-24_mean`,"germplasmName") %>% 
  mutate(drone_height = `5-12-24_mean` * 100) %>% 
  filter(height < 100) %>% 
  mutate(height_dif = height - drone_height)
  filter(germplasmName != "NO_OATS_PLANTED")

hist(df_5$height_dif)



plot(df_5$height,df_5$drone_height)



df_5 %>% 
  filter(height_dif > 25) %>% 
  print(n = 77)


Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta %>% 
  print(n=378)


