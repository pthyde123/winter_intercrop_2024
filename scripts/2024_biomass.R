

library(readr)
Cornell_WinterOatPeaIntercrop_2024_Ithaca <- read_csv("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca.csv")

colnames(Cornell_WinterOatPeaIntercrop_2024_Ithaca)
library(readxl)
library(readr)



#### import the data sets ####
plot_number_to_plot_boundary <- read_csv("output/plot_number_to_plot_boundary.csv") 

plot_number_to_plot_boundary <- plot_number_to_plot_boundary %>% 
  mutate(plot_subplot = str_c(plot_number,"_",subplot)) %>% 
  mutate(pg_id = str_c(prow,"_",pcol,"_",grow,"_",gcol))
  
  

Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta <- read_excel("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta.xlsx")



NDVI_05_12_24 <- read_excel("data/NDVI_05-12-24.xlsx") %>% 
  mutate(pg_id = str_c(prow,"_",pcol,"_",grow,"_",gcol))


X5_12_24_subplot_height <- read_csv("data/5-12-24_subplot_height.csv") %>% 
  mutate(pg_id = str_c(prow,"_",pcol,"_",grow,"_",gcol))



#### create full data set ####

subplot_data <- Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  
  mutate(plot_name = gsub("\\_subplot_.*", "", observationUnitName)) %>% 
  
  filter(observationLevel == "subplot") %>% 
  mutate(subplot_number = str_sub(observationUnitName,-1)) %>% ## DANGER this only works if you filter to subplot first
  
  mutate(plot_subplot = str_c(plotNumber,"_",subplot_number)) %>% 
  
  left_join(plot_number_to_plot_boundary,join_by(plot_subplot)) %>% 
  
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta, join_by(plot_name)) %>% 
  
  left_join(NDVI_05_12_24, join_by(pg_id)) %>% 
  
  left_join(X5_12_24_subplot_height, join_by(pg_id))


#### the full data set ####

subplot_data


#### initial summary graphs ####
subplot_data %>% 
  arrange(plotNumber) %>% 
  
  select(plot_name,plot_subplot, "observationUnitName" ,"germplasmName","plotNumber",observationLevel,inter_crop, source,
       `Above ground dry biomass - g|day 136|COMP:0000063`,
        `Weed above ground dry biomass - g|day 136|COMP:0000065`,
        `Pea above ground dry biomass - g|day 136|COMP:0000060`,
        "Pea plant height - cm|day 134|COMP:0000062",
       `NDVI_05-12-24_mean`) %>% 
 
    filter(!is.na(`Above ground dry biomass - g|day 136|COMP:0000063`)) %>% 

    mutate("total_biomass" = `Above ground dry biomass - g|day 136|COMP:0000063`+
           `Weed above ground dry biomass - g|day 136|COMP:0000065`+
           `Pea above ground dry biomass - g|day 136|COMP:0000060` ) %>% 

    filter(inter_crop == "oat-pea") %>% 
  
  ggplot(aes(`NDVI_05-12-24_mean`,total_biomass,color=inter_crop))+
  geom_point() +
  geom_smooth(se = FALSE, method = lm)+
  xlab("NDVI")+
  ylab("Total Biomass (g)")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))





subplot_data %>% 
  arrange(plotNumber) %>% 
  
  select(plot_name,plot_subplot, "observationUnitName" ,"germplasmName","plotNumber",observationLevel,inter_crop, source,
         `Above ground dry biomass - g|day 136|COMP:0000063`,
         `Weed above ground dry biomass - g|day 136|COMP:0000065`,
         `Pea above ground dry biomass - g|day 136|COMP:0000060`,
         "Pea plant height - cm|day 134|COMP:0000062",
         `NDVI_05-12-24_mean`) %>% 
  
  filter(!is.na(`Above ground dry biomass - g|day 136|COMP:0000063`)) %>% 
  
  filter(source != "Barley") %>% 
  filter(source != "Martens") %>% 
  
  mutate("total_biomass" = `Above ground dry biomass - g|day 136|COMP:0000063`+
           `Weed above ground dry biomass - g|day 136|COMP:0000065`+
           `Pea above ground dry biomass - g|day 136|COMP:0000060` ) %>% 
  
  select(inter_crop,`NDVI_05-12-24_mean`,`Above ground dry biomass - g|day 136|COMP:0000063`,
    `Pea above ground dry biomass - g|day 136|COMP:0000060`) %>% 
  pivot_longer(`Above ground dry biomass - g|day 136|COMP:0000063`:`Pea above ground dry biomass - g|day 136|COMP:0000060`,
               values_to = "biomass", names_to = "crop") %>% 
  
  
  
  mutate(crop = substr(crop, 1,1   )) %>% 
  
  ggplot(aes(`NDVI_05-12-24_mean`,biomass,color=inter_crop))+
  geom_point() +
  geom_smooth(se = FALSE, method = lm)+
  xlab("NDVI")+
  ylab("Total Biomass (g)")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))








subplot_data %>% 
  arrange(plotNumber) %>% 
  
  select(plot_name,plot_subplot, "observationUnitName" ,"germplasmName","plotNumber",observationLevel,inter_crop, source,
         `Above ground dry biomass - g|day 136|COMP:0000063`,
         `Weed above ground dry biomass - g|day 136|COMP:0000065`,
         `Pea above ground dry biomass - g|day 136|COMP:0000060`,
         "Pea plant height - cm|day 134|COMP:0000062",
         `NDVI_05-12-24_mean`) %>% 
  
  filter(!is.na(`Above ground dry biomass - g|day 136|COMP:0000063`)) %>% 
  
  filter(source != "Barley") %>% 
  filter(source != "Martens") %>% 
  
  mutate("total_biomass" = `Above ground dry biomass - g|day 136|COMP:0000063`+
           `Weed above ground dry biomass - g|day 136|COMP:0000065`+
           `Pea above ground dry biomass - g|day 136|COMP:0000060` ) %>% 
  
  select(inter_crop,`NDVI_05-12-24_mean`,`Above ground dry biomass - g|day 136|COMP:0000063`,
         `Pea above ground dry biomass - g|day 136|COMP:0000060`) %>% 
  pivot_longer(`Above ground dry biomass - g|day 136|COMP:0000063`:`Pea above ground dry biomass - g|day 136|COMP:0000060`,
               values_to = "biomass", names_to = "crop") %>% 
  
  mutate(crop = substr(crop, 1,1   )) %>%
  
  filter(inter_crop == "oat-pea") %>% 
  
  ggplot(aes(`NDVI_05-12-24_mean`,biomass,color=crop))+
  geom_point() +
  geom_smooth(se = FALSE, method = lm)+
  xlab("NDVI")+
  ylab("Total Biomass (g)")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))
 










subplot_data %>% 
  arrange(plotNumber) %>% 
  
  select(plot_name,plot_subplot, "observationUnitName" ,"germplasmName","plotNumber",observationLevel,inter_crop, source,
         `Above ground dry biomass - g|day 136|COMP:0000063`,
         `Weed above ground dry biomass - g|day 136|COMP:0000065`,
         `Pea above ground dry biomass - g|day 136|COMP:0000060`,
         "Pea plant height - cm|day 134|COMP:0000062",
         `NDVI_05-12-24_mean`,`Pea plant height - cm|day 134|COMP:0000062`) %>% 
  
  filter(!is.na(`Above ground dry biomass - g|day 136|COMP:0000063`)) %>% 
  
  filter(source != "Barley") %>% 
  filter(source != "Martens") %>% 
  
  mutate("total_biomass" = `Above ground dry biomass - g|day 136|COMP:0000063`+
           `Weed above ground dry biomass - g|day 136|COMP:0000065`+
           `Pea above ground dry biomass - g|day 136|COMP:0000060` ) %>% 
  
  select(inter_crop,`NDVI_05-12-24_mean`,`Above ground dry biomass - g|day 136|COMP:0000063`,
         `Pea above ground dry biomass - g|day 136|COMP:0000060`,total_biomass) %>% 
 
  
  ggplot(aes(`NDVI_05-12-24_mean`,total_biomass,color=inter_crop))+
  geom_point() +
  geom_smooth(se = FALSE, method = lm)+
  xlab("NDVI")+
  ylab("Total Biomass (g)")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))



  
  
subplot_data %>% 
  arrange(plotNumber) %>% 
  
  select(plot_name,plot_subplot, "observationUnitName" ,"germplasmName","plotNumber",observationLevel,inter_crop, source,
         `Above ground dry biomass - g|day 136|COMP:0000063`,
         `Weed above ground dry biomass - g|day 136|COMP:0000065`,
         `Pea above ground dry biomass - g|day 136|COMP:0000060`,
         "Pea plant height - cm|day 134|COMP:0000062",
         `NDVI_05-12-24_mean`,`Pea plant height - cm|day 134|COMP:0000062`) %>% 
  
  filter(!is.na(`Above ground dry biomass - g|day 136|COMP:0000063`)) %>% 
  
  filter(source != "Barley") %>% 
  filter(source != "Martens") %>% 
  
  mutate("total_biomass" = `Above ground dry biomass - g|day 136|COMP:0000063`+
           `Weed above ground dry biomass - g|day 136|COMP:0000065`+
           `Pea above ground dry biomass - g|day 136|COMP:0000060` ) %>% 
  
  select(inter_crop,`NDVI_05-12-24_mean`,`Above ground dry biomass - g|day 136|COMP:0000063`,
         `Pea above ground dry biomass - g|day 136|COMP:0000060`,total_biomass) %>% 
  
  filter(inter_crop == "oat-pea") %>% 
  
  ggplot(aes(`NDVI_05-12-24_mean`,`total_biomass`,color=inter_crop))+
  geom_point() +
  geom_smooth(se = FALSE, method = lm)+
  xlab("NDVI")+
  ylab("Total Biomass (g)")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))




#### RANDOM FOREST ####
library(randomForest)

subplot_data
colnames(subplot_data)

subplot_data %>%
  filter(inter_crop %in% c("oat","pea","oat-pea")) %>% 
  
  select(plot_name,plot_subplot, "observationUnitName" ,"germplasmName","plotNumber",observationLevel,inter_crop, source,
       `Above ground dry biomass - g|day 136|COMP:0000063`,
       `Weed above ground dry biomass - g|day 136|COMP:0000065`,
       `Pea above ground dry biomass - g|day 136|COMP:0000060`,
       "Pea plant height - cm|day 134|COMP:0000062",
       `NDVI_05-12-24_mean`,`Pea plant height - cm|day 134|COMP:0000062`,
       "5-12-24_mean","Plant height - cm|day 134|COMP:0000054")


RF_data <- subplot_data %>%
  filter(inter_crop %in% c("oat-pea")) %>% 
  
  select("germplasmName",inter_crop,
         `Above ground dry biomass - g|day 136|COMP:0000063`,
         `Pea above ground dry biomass - g|day 136|COMP:0000060`,
         "Pea plant height - cm|day 134|COMP:0000062",
         `NDVI_05-12-24_mean`,`Pea plant height - cm|day 134|COMP:0000062`,
         "Plant height - cm|day 134|COMP:0000054") %>% 
  rename(oat_biomass = `Above ground dry biomass - g|day 136|COMP:0000063`) %>% 
  rename(pea_biomass = `Pea above ground dry biomass - g|day 136|COMP:0000060`) %>% 
  rename(oat_height = "Plant height - cm|day 134|COMP:0000054") %>% 
  rename(pea_height = "Pea plant height - cm|day 134|COMP:0000062") %>% 
  rename(NDVI = "NDVI_05-12-24_mean") %>% 
  select(oat_biomass,oat_height,`NDVI`,pea_biomass,pea_height) %>% 
  mutate(total_biomass = oat_biomass+pea_biomass) %>% 
  filter(!is.na(oat_biomass))

data <- as.data.frame(RF_data)

data$germplasmName <- as.factor(data$germplasmName)
data$inter_crop <- as.factor(data$inter_crop)
data$oat_biomass <- as.factor(data$oat_biomass)
data$pea_biomass <- as.factor(data$pea_biomass)
data$pea_height <- as.factor(data$pea_height)
data$NDVI <- as.factor(data$NDVI)
data$oat_height <- as.factor(data$oat_height)


str(data)


set.seed(99)



### second try at random forest using another tutorial


data <- as.data.frame(RF_data)


data <-data %>% 
  select(oat_biomass,pea_biomass,oat_height,pea_height,NDVI)

str(data)

# Create features and target
X <- data %>% 
  select(oat_height,pea_height,NDVI)
y <- data$oat_biomass


library(caret)
# Split data into training and test sets
index <- createDataPartition(y, p=0.75, list=FALSE)
X_train <- X[ index, ]
X_test <- X[-index, ]
y_train <- y[index]
y_test<-y[-index]

# Train the model 
regr <- randomForest(x = X_train, y = y_train , maxnodes = 10, ntree = 10)

predictions <- predict(regr, X_test)
result <- X_test
result['biomass'] <- y_test
result['prediction']<-  predictions
head(result)

ggplot(  ) + 
  geom_point( aes(x = X_test$NDVI, y = y_test, color = 'red', alpha = 0.5) ) + 
  geom_point( aes(x = X_test$NDVI , y = predictions, color = 'blue',  alpha = 0.5)) + 
  labs(x = "NDVI", y = "Biomass", color = "", alpha = 'Transperency') +
  scale_color_manual(labels = c( "Predicted", "Real"), values = c("blue", "red")) 

library(Metrics)
print(paste0('MAE: ' , mae(y_test,predictions) ))
print(paste0('MSE: ' ,caret::postResample(predictions , y_test)['RMSE']^2 ))
print(paste0('R2: ' ,caret::postResample(predictions , y_test)['Rsquared'] ))



