
library(readr)
Cornell_WinterOatPeaIntercrop_2024_Ithaca <- read_csv("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca.csv")

colnames(Cornell_WinterOatPeaIntercrop_2024_Ithaca)
library(readxl)
library(readr)



#### import the data sets ####

spectral_5_12_24_data <- read_csv("output/spectral_5-12-24_data.csv")


Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta <- read_excel("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta.xlsx")



#### create full data set ####

subplot_data <- Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  
  mutate(plot_name = gsub("\\_subplot_.*", "", observationUnitName)) %>% 
  
  filter(observationLevel == "subplot") %>% 
  mutate(subplot_number = str_sub(observationUnitName,-1)) %>% ## DANGER this only works if you filter to subplot first
  
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta, join_by(plot_name)) %>% 
  
  left_join(spectral_5_12_24_data, join_by(observationUnitName)) %>%   
  
  filter(germplasmName != "Winter Hayden")


#### the full data set ####

subplot_data


#####

colnames(subplot_data)

model_data <- subplot_data %>%
  filter(inter_crop %in% c("oat","pea","oat-pea")) %>% 
  
  select(plot_name,plot_subplot,subplot, "observationUnitName" ,"germplasmName",
         "plotNumber",observationLevel,blockNumber,inter_crop, source,
         `Above ground dry biomass - g|day 136|COMP:0000063`,
         `Weed above ground dry biomass - g|day 136|COMP:0000065`,
         `Pea above ground dry biomass - g|day 136|COMP:0000060`,
         `Pea plant height - cm|day 134|COMP:0000062`,
         `Plant height - cm|day 134|COMP:0000054`,
         contains("mean")) %>% 
  rename(oat_biomass = `Above ground dry biomass - g|day 136|COMP:0000063`) %>% 
  rename(pea_biomass = `Pea above ground dry biomass - g|day 136|COMP:0000060`) %>% 
  rename(oat_height = "Plant height - cm|day 134|COMP:0000054") %>% 
  rename(pea_height = "Pea plant height - cm|day 134|COMP:0000062") %>% 
  as.data.frame()

colnames(model_data)


##training data set, the plots with biomass samples

training_data <- model_data %>% 
  select(inter_crop,oat_biomass,pea_biomass,oat_height,pea_height,contains("mean"))%>% 
  filter(!is.na(oat_biomass)) %>% 
  filter(!is.na(pea_biomass)) %>% 
  filter(inter_crop %in% c("oat-pea","oat"))%>% 
  select(-pea_biomass) %>% 
  filter(oat_biomass > 10)

str(training_data)

training_data %>% 
  filter(oat_biomass<5)


##### best method I have found  way https://rpubs.com/GChirinos/Tuning_Random_Forest

library(tidymodels)
library(ranger)

str(training_data) 

# Split 80% / 20%
set.seed(123)
split <- initial_split(data = training_data, prop = 0.80)

# Training set
train_set <- training(split)

# Testing set
test_set <- testing(split)

# make the recipe,ie the model, it can also have data transformations
recipe <- 
  recipe(oat_biomass ~ ., data = train_set)


#Apart from partitioning the data into training and test sets, 
#the training set will be partitioned into 5 data subsets, 
#this allows having a variety of partitions to train and measure 
#the result of the hyperparameter adjustment process.
set.seed(123)
folds <- vfold_cv(train_set, v = 5)


# Random forest spec
rf_spec <- rand_forest(
  mtry = tune(), trees = tune(), min_n = tune()) %>%
  set_engine("ranger", num.threads = 7, importance = "impurity") %>%
  set_mode("regression")

# Random forest workflow
rf_Wf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(rf_spec)

# Space-filling designs grid
rf_grid <- 
  grid_space_filling(
    min_n(), 
    mtry(range = c(4, 9)), 
    trees(), 
    size = 80)

# Lets use seven cores for this process
doParallel::registerDoParallel(cores = 7)

set.seed(123)
tune_res <- 
  rf_Wf %>% 
  tune_grid(
    resamples = folds, grid = rf_grid, 
    metrics = metric_set(rmse, mae, rsq)
  )


# finalize model set up
final_rf <- rf_Wf %>% 
  finalize_workflow(
    show_best(x = tune_res, metric = "rmse", n = 1)
  )


final_rs <-
  final_rf %>%
  last_fit(
    split, metrics = metric_set(rmse, mae, rsq)
  )


final_rs %>% 
  collect_metrics()


library(vip)

final_rs %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 20)  ###


final_rs$.metrics

as.data.frame(final_rs$.predictions) %>% 
ggplot(aes(oat_biomass,.pred))+
  geom_point() +
  geom_smooth(se = FALSE, method = lm)+
  xlab("Observed")+
  ylab("Predicted")+
  scale_color_brewer(palette="Dark2")+
  theme_classic()+
  theme(axis.text.x=element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 16))+
  theme(axis.text.y = element_text(face = "bold", color = "black", size = 16))


## prediction, out of sample 
## prediction data set, out of sample, subplot 2

prediction_data <- model_data %>% 
  filter(subplot == 2) %>% 
  select(observationUnitName,inter_crop,oat_height,pea_height,contains("mean")) %>% 
  filter(inter_crop == c("oat-pea"))

str(prediction_data)

prediction_data


final_wf <- final_rs %>%
  extract_workflow()

final_wf

pred <- predict(final_wf, prediction_data[,-1] )

biomass_predictions <- bind_cols(prediction_data,pred) %>% 
  select(observationUnitName,.pred) %>% 
  rename(pred_biomass = .pred)


####
library(lme4) 

model_factors <- subplot_data %>% 
  select(observationUnitName,blockNumber,germplasmName) %>% 
  mutate(blockNumber = as.character(blockNumber))

df <- biomass_predictions %>% 
  left_join(model_factors)


biomass <- lme4::lmer(`pred_biomass` ~ (1|germplasmName) + (1|blockNumber),
                   data = df)


df_trait <- as.data.frame(VarCorr(biomass))
vg_trait <- df_trait[df_trait$grp == "germplasmName","vcov"]

ve_trait <- df_trait[df_trait$grp == "Residual", "vcov"]

h2_trait <- vg_trait/(vg_trait + ve_trait/(4))

h2_trait






