



library(readxl)
library(readr)
library(tidyverse)



NDVI_10_17_23 <- read_excel("data/NDVI_10-17-23.xlsx")
NDVI_11_16_23 <- read_excel("data/NDVI_11-16-23.xlsx")
NDVI_04_01_24 <- read_excel("data/NDVI_04-01-24.xlsx")
NDVI_04_20_24 <- read_excel("data/NDVI_04-20-24.xlsx")
NDVI_05_12_24 <- read_excel("data/NDVI_05-12-24.xlsx")
NDVI_06_14_24 <- read_excel("data/NDVI_06-14-24.xlsx")
NDVI_07_08_24 <- read_excel("data/NDVI_07-08-24.xlsx")


plot_number_to_plot_boundary <- read_csv("output/plot_number_to_plot_boundary.csv")




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






######


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

#####


ndvi_s2 <- ndvi %>% 
  filter(gcol == 2)

plot_data %>% 
  select(plotNumber,germplasmName,replicate,inter_crop,oat_yield,pea_yield,total_yield) %>% 
  left_join(ndvi_s2, by=join_by(plotNumber == plot_number)) %>% 
  mutate(cNDVI = rowSums(across(`NDVI_10-17-23_mean`:`NDVI_07-08-24_mean`))) %>% 
  
  ggplot(aes(cNDVI,total_yield,color=inter_crop))+
  geom_point() +
  geom_smooth(se = FALSE, method = lm)+
  xlab("cNDVI")+
  ylab("Total Grain g")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))






plot_data %>% 
  select(plotNumber,germplasmName,replicate,inter_crop,oat_yield,pea_yield,total_yield) %>% 
  left_join(ndvi_s2, by=join_by(plotNumber == plot_number)) %>% 
  mutate(cNDVI = rowSums(across(`NDVI_10-17-23_mean`:`NDVI_07-08-24_mean`))) %>% 
  
  ggplot(aes(`NDVI_06-14-24_mean`,total_yield,color=inter_crop))+
  geom_point() +
  geom_smooth(se = FALSE, method = lm)+
  xlab("NDVI_06-14-24")+
  ylab("Total Grain g")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))


#### linear modeling





model_data <- plot_data %>% 
  select(plotNumber,germplasmName,replicate,inter_crop,oat_yield,pea_yield,total_yield) %>% 
  left_join(ndvi_s2, by=join_by(plotNumber == plot_number)) %>% 
  mutate(cNDVI = rowSums(across(`NDVI_10-17-23_mean`:`NDVI_07-08-24_mean`)))


model_data <- model_data %>% 
  select(germplasmName,replicate,inter_crop,total_yield,`NDVI_10-17-23_mean`:cNDVI) %>% 
  rename(block = replicate) %>% 
  filter(inter_crop == "oat-pea")




yield <- lme4::lmer(total_yield ~ (1|germplasmName) + (1|block),
           data = model_data)


df_trait <- as.data.frame(VarCorr(yield))
vg_trait <- df_trait[df_trait$grp == "germplasmName","vcov"]
ve_trait <- df_trait[df_trait$grp == "Residual", "vcov"]
h2_trait <- vg_trait/(vg_trait + ve_trait/(2))

h2_yield <- h2_trait

h2_yield








ndvi <- lme4::lmer(`NDVI_06-14-24_mean` ~ (1|germplasmName) + (1|block),
                    data = model_data)

df_trait <- as.data.frame(VarCorr(ndvi))
vg_trait <- df_trait[df_trait$grp == "germplasmName","vcov"]
ve_trait <- df_trait[df_trait$grp == "Residual", "vcov"]

h2_trait <- vg_trait/(vg_trait + ve_trait/(2))

h2_ndvi <- h2_trait

h2_ndvi






GC_yield <- ranef(yield)$germplasmName

GC_ndvi <- ranef(ndvi)$germplasmName



GC_yield <- GC_yield %>%
  rownames_to_column(var = "germplasmName")%>% 
  rename(GC_yield = "(Intercept)")


GC_ndvi <- GC_ndvi %>% 
  rownames_to_column(var = "germplasmName")%>% 
  rename(GC_ndvi = "(Intercept)")

 


model_data %>% 
  select(germplasmName,`NDVI_06-14-24_mean`) %>% 
  group_by(germplasmName) %>% 
  summarise(mean_ndvi = mean(`NDVI_06-14-24_mean`)) %>% 
  left_join(GC_yield) %>% 
    ggplot(aes(mean_ndvi,GC_yield))+
    geom_point() +
    geom_smooth(se = FALSE, method = lm)+
    xlab("NDVI_06-14-24")+
    ylab("GC Yield")+
    scale_color_brewer(palette="Dark2")+
    theme_classic()+
    theme(axis.text.x=element_text(angle = 90, hjust = 1))+
    theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
    theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
    theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))
  


model_data %>% 
  select(germplasmName,`total_yield`) %>% 
  group_by(germplasmName) %>% 
  summarise(mean_yield = mean(`total_yield`)) %>% 
  left_join(GC_yield) %>% 
  ggplot(aes(mean_yield,GC_yield))+
  geom_point() +
  geom_smooth(se = FALSE, method = lm)+
  xlab("Total Yield")+
  ylab("GC Yield")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))

 

model_data %>% 
  select(germplasmName,`cNDVI`) %>% 
  group_by(germplasmName) %>% 
  summarise(mean_cNDVI = mean(cNDVI)) %>% 
  left_join(GC_yield) %>% 
  ggplot(aes(mean_cNDVI,GC_yield))+
  geom_point() +
  geom_smooth(se = FALSE, method = lm)+
  xlab("cNDVI")+
  ylab("GC Yield")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))








geno_yield %>% 
  left_join(geno_ndvi) %>%
  ggplot(aes(ndvi,yield))+
  geom_point() +
  geom_smooth(se = FALSE, method = lm)+
  xlab("NDVI_06-14-24")+
  ylab("Total Grain g")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))
  








r.squaredGLMM(yield)











### BLUP


selind_ListOfAccessionsPlantFall2024 <- read_csv("data/selind_ListOfAccessionsPlantFall2024.csv")




blup <- selind_ListOfAccessionsPlantFall2024 %>% 
  select(Oat,OatGrain_Inter,PeaGrain_Inter,OatGrain_Mono)



df<- plot_data %>% 
  select(plotNumber,germplasmName,replicate,inter_crop,oat_yield,pea_yield,total_yield) %>% 
  left_join(ndvi_s2, by=join_by(plotNumber == plot_number)) %>% 
  mutate(cNDVI = rowSums(across(`NDVI_10-17-23_mean`:`NDVI_07-08-24_mean`))) %>% 
  select(germplasmName,cNDVI,oat_yield,inter_crop) %>% 
  filter(inter_crop=="oat") %>% 
  group_by(germplasmName) %>% 
  summarise_at(c("cNDVI", "oat_yield"), mean, na.rm = TRUE)


df %>% 
  left_join(blup, join_by("germplasmName" == "Oat")) %>% 
  
  ggplot(aes(cNDVI,OatGrain_Mono))+
  geom_point() +
  geom_smooth(se = TRUE, method = lm)+
  xlab("cNDVI")+
  ylab("Oat BLUP")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))



df<- plot_data %>% 
  select(plotNumber,germplasmName,replicate,inter_crop,oat_yield,pea_yield,total_yield) %>% 
  left_join(ndvi_s2, by=join_by(plotNumber == plot_number)) %>% 
  mutate(cNDVI = rowSums(across(`NDVI_10-17-23_mean`:`NDVI_07-08-24_mean`))) %>% 
  select(germplasmName,cNDVI,oat_yield,inter_crop,total_yield) %>% 
  filter(inter_crop=="oat-pea") %>% 
  group_by(germplasmName) %>% 
  summarise_at(c("cNDVI", "oat_yield","total_yield"), mean, na.rm = TRUE)


df %>% 
  left_join(blup, join_by("germplasmName" == "Oat")) %>% 
  mutate(totalgrain_inter = OatGrain_Inter+PeaGrain_Inter) %>% 
  ggplot(aes(cNDVI,OatGrain_Inter))+
  geom_point() +
  geom_smooth(se = TRUE, method = lm)+
  xlab("cNDVI")+
  ylab("Oat BLUP")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))



##heritability

rep_2 <- plot_data %>% 
  group_by(germplasmName) %>% 
  count() %>% 
  filter(n == 4)


h2_data <- plot_data %>% 
  select(plotNumber,germplasmName,replicate,inter_crop,oat_yield,pea_yield,total_yield) %>% 
  left_join(ndvi_s2, by=join_by(plotNumber == plot_number)) %>% 
  mutate(cNDVI = rowSums(across(`NDVI_10-17-23_mean`:`NDVI_07-08-24_mean`))) %>% 
  rename("block" = "replicate") %>% 
  filter(germplasmName %in% c(rep_2$germplasmName)) %>% 
  filter(inter_crop == "oat")


h2_data


trait <- `NDVI_06-14-24_mean`


lm_trait <- lme4::lmer(total_yield ~ (1|germplasmName) + (1|block),
                       data = h2_data)


#lm_trait <- lme4::lmer(cNDVI ~ (1|block),
                    #   data = h2_data)



df_trait <- as.data.frame(VarCorr(lm_trait))
vg_trait <- df_trait[df_trait$grp == "germplasmName","vcov"]
ve_trait <- df_trait[df_trait$grp == "Residual", "vcov"]

h2_trait <- vg_trait/(vg_trait + ve_trait/(2))



h2_trait








######ndvi over time


ndvi_s2.1 <- ndvi_s2 %>% 
  filter(gcol == 2) %>% 
  select(plot_number,`NDVI_10-17-23_mean`:`NDVI_07-08-24_mean`)

df <- plot_data %>% 
  select(plotNumber,germplasmName,replicate,inter_crop,oat_yield,pea_yield,total_yield,`Freeze damage severity - 0-9 Rating|CO_350:0005001`) %>% 
  left_join(ndvi_s2.1, by=join_by(plotNumber == plot_number)) 


dap_names <- c("plotNumber","germplasmName","replicate",
               "inter_crop","oat_yield","pea_yield","total_yield","freeze",
               13,43,180,199,221,254,278)

names = c(colnames(df))




colnames(df) <- dap_names


df %>% 
  pivot_longer(`13`:`278`, names_to = "dap", values_to = "ndvi" ) %>% 
  mutate(dap = as.numeric(dap)) %>% 
  ggplot(aes(dap,ndvi,color=inter_crop))+
  geom_jitter() +
  geom_smooth(se = TRUE, method = loess,span=.1)+
  xlab("DAP")+
  ylab("NDVI")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))





df %>% 
  pivot_longer(`13`:`278`, names_to = "dap", values_to = "ndvi" ) %>% 
  mutate(dap = as.numeric(dap)) %>% 
  filter(dap == 43| dap == 180) %>% 
  ggplot(aes(dap,ndvi,color=inter_crop))+
  geom_point() +
  geom_smooth(se = TRUE, method = lm)+
  xlab("DAP")+
  ylab("NDVI")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))




df %>% 
  pivot_longer(`13`:`278`, names_to = "dap", values_to = "ndvi" ) %>% 
  mutate(dap = as.numeric(dap)) %>% 
  filter(dap == 43| dap == 180) %>%
  filter(inter_crop == "oat") %>% 
  ggplot(aes(dap,ndvi,color=germplasmName))+
  geom_point() +
  geom_smooth(se = TRUE, method = lm)+
  xlab("DAP")+
  ylab("NDVI")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))



df %>% 
  mutate(winter_change = `180` - `43`) %>% 
  filter(inter_crop == "oat") %>% 
  ggplot(aes(germplasmName,winter_change))+
  geom_boxplot()+
  xlab("Accession")+
  ylab("NDVI Winter Change")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text( color = "black", size = 12))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))
 


 
  
df %>%   
    left_join(plot_data, by = "plotNumber") %>% 
  mutate(winter_change = `180` - `43`) %>% 
  filter(inter_crop.x == "oat") %>% 
  mutate(freeze_damage = `Freeze damage severity - 0-9 Rating|CO_350:0005001`) %>% 
  ggplot(aes(`Freeze damage severity - 0-9 Rating|CO_350:0005001`, winter_change))+
    geom_point()+
  geom_smooth()+
  ylab("NDVI Winter Change")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text( color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))





###### heritability  

library(lme4)


rep_geno_2 <- df %>%   
  left_join(plot_data, by = "plotNumber") %>% 
  mutate(winter_change = `180` - `43`) %>% 
  filter(inter_crop.x == "oat") %>% 
  select(germplasmName.x, winter_change, replicate.x) %>% 
  group_by(germplasmName.x) %>% 
  count() %>% 
  filter(n == 2)





h2_data <- df %>%   
  left_join(plot_data, by = "plotNumber") %>% 
  mutate(winter_change = `180` - `43`) %>% 
  filter(inter_crop.x == "oat") %>% 
  select(germplasmName.x, winter_change, replicate.x,freeze) %>% 
  filter(germplasmName.x %in% c(rep_geno_2$germplasmName.x)) %>% 
  rename(germplasmName = germplasmName.x) %>% 
  rename(block = replicate.x)












h2_data


lm_trait <- lme4::lmer(winter_change ~ (1|germplasmName) + (1|block),
                       data = h2_data)


df_trait <- as.data.frame(VarCorr(lm_trait))
vg_trait <- df_trait[df_trait$grp == "germplasmName","vcov"]
ve_trait <- df_trait[df_trait$grp == "Residual", "vcov"]

h2_trait <- vg_trait/(vg_trait + ve_trait/(2))

h2_trait





h2_data %>% 
  arrange(desc(winter_change)) %>% 
  group_by(germplasmName) %>% 
  summarise(mean = mean(winter_change)) %>% 
 
  arrange(desc(mean)) %>% 
  print(n= nrow(h2_data))



h2_data


lm_trait <- lme4::lmer(freeze ~ (1|germplasmName) + (1|block),
                       data = h2_data)


df_trait <- as.data.frame(VarCorr(lm_trait))
vg_trait <- df_trait[df_trait$grp == "germplasmName","vcov"]
ve_trait <- df_trait[df_trait$grp == "Residual", "vcov"]

h2_trait <- vg_trait/(vg_trait + ve_trait/(2))

h2_trait







`Freeze damage severity - 0-9 Rating|CO_350:0005001`






290 320 92 111 133 166 190

-75
-45

277

13,43,180,199,221,254,278



