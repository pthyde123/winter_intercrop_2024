
library(readr)
library(readxl)
library(readr)
library(tidyverse)

#### import the data sets ####

# T3 data file trial name "Cornell_WinterOatPeaIntercrop_2024_Ithaca"
Cornell_WinterOatPeaIntercrop_2024_Ithaca <- read_csv("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca.csv")
colnames(Cornell_WinterOatPeaIntercrop_2024_Ithaca)

# spectral, subplot averages
spectral_6_14_24_data <- read_csv("output/spectral_6-14-24_data.csv")
colnames(spectral_6_14_24_data)

# plot meta data, not on T3
Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta <- read_excel("data/Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta.xlsx")
colnames(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta)


#### create full data set ####

subplot_data <- Cornell_WinterOatPeaIntercrop_2024_Ithaca %>% 
  
  mutate(plot_name = gsub("\\_subplot_.*", "", observationUnitName)) %>% # get plot name without subplot number for joining
  
  filter(observationLevel == "subplot") %>% # keep only subplot level data, most of our data is subplot level
  mutate(subplot_number = str_sub(observationUnitName,-1)) %>% # DANGER this only works if you filter to subplot first
  
  left_join(Cornell_WinterOatPeaIntercrop_2024_Ithaca_plot_meta, join_by(plot_name)) %>% # join T3 data with meta, need inter_crop designation, not found on T3 
  
  left_join(spectral_6_14_24_data, join_by(observationUnitName)) %>% # join with spectral, subplot averages  
  
  filter(germplasmName != "Winter Hayden") # remove winter hayden, it was not planted, plots are pea only


#### the full data set ####

subplot_data

subplot_data %>% 
  dplyr::group_by(germplasmName) %>% 
  dplyr::count()   # the number of subplots per genotype, divide by 3 to get replicates per genotype

str(subplot_data)

#ID oat biomass outliers  
subplot_data %>%
  select(inter_crop, observationUnitName,`Above ground dry biomass - g|day 168|COMP:0000064`) %>% 
  filter(inter_crop == "oat") %>% 
  filter(`Above ground dry biomass - g|day 168|COMP:0000064` < 10)
## there are oat subplots with 0 biomass, removed below 

subplot_data %>%
  select(inter_crop, observationUnitName,`Above ground dry biomass - g|day 168|COMP:0000064`) %>% 
  filter(inter_crop == "oat-pea") %>% 
  filter(`Above ground dry biomass - g|day 168|COMP:0000064` < 10)



#ID pea height outlier
subplot_data %>%
  select(inter_crop, observationUnitName,`Above ground dry biomass - g|day 168|COMP:0000064`,`Pea plant height - cm|day 164|COMP:0000040`) %>% 
  filter(inter_crop == "oat") %>% 
  filter(`Pea plant height - cm|day 164|COMP:0000040` > 5) 




#### formatting subplot data for model ####

colnames(subplot_data)

model_data <- subplot_data %>%
  filter(inter_crop %in% c("oat","pea","oat-pea")) %>% # keep only oat accessions, remove barley
  
  select(plot_name,plot_subplot,subplot, "observationUnitName" ,"germplasmName",
         "plotNumber",observationLevel,blockNumber,inter_crop, source,
         `Above ground dry biomass - g|day 168|COMP:0000064`,
         `Weed above ground dry biomass - g|day 168|COMP:0000066`,
         `Pea above ground dry biomass - g|day 168|COMP:0000061`,
         `Pea plant height - cm|day 164|COMP:0000040`,
         `Plant height - cm|day 164|COMP:0000023`,
         contains("mean")) %>% # selecting columns with "mean" in them keeps all the spectral subplot means data
  
  dplyr::rename(oat_biomass = `Above ground dry biomass - g|day 168|COMP:0000064`) %>% 
  dplyr::rename(pea_biomass = `Pea above ground dry biomass - g|day 168|COMP:0000061`) %>% 
  dplyr::rename(oat_height = "Plant height - cm|day 164|COMP:0000023") %>% 
  dplyr::rename(pea_height = "Pea plant height - cm|day 164|COMP:0000040") %>% 
  dplyr::filter(observationUnitName != "Cornell_WinterOatPeaIntercrop_2024_Ithaca-PLOT_182_subplot_1") %>% # remove outlier
  dplyr::filter(observationUnitName != "Cornell_WinterOatPeaIntercrop_2024_Ithaca-PLOT_181_subplot_1") %>% # remove outlier
  as.data.frame() 



summary(model_data)

#### create training data set ####

# want the subplots with biomass samples
# keep only the columns wanted in model
# this model is for oat and oat_pea inter_crop 

training_data <- model_data %>% 
  select(oat_biomass,inter_crop,pea_biomass,oat_height,pea_height,contains("mean"))%>% 
  filter(!is.na(oat_biomass)) %>% 
  filter(!is.na(pea_biomass)) %>% 
  filter(inter_crop %in% c("oat-pea","oat")) %>% 
  select(-pea_biomass) # remove pea_biomass from oat model 


str(training_data)

summary(training_data)

#### create the model #### 
# useful tutorial https://rpubs.com/GChirinos/Tuning_Random_Forest

library(tidymodels)
library(ranger)

# set seed
runif(1)*10000
seed <- 2051

# training data
str(training_data)

# Split training data 80% / 20%
set.seed(seed)
split <- initial_split(data = training_data, prop = 0.80)

# Training set
train_set <- training(split)

# Testing set
test_set <- testing(split)

# make the recipe,ie the model, it can also have data transformations
recipe <- 
  recipe(oat_biomass ~ ., data = train_set)



#### Hyperparameter #### 

#adjustments are included in rf_spec, tune()

#Apart from partitioning the data into training and test sets, 
#the training set will be partitioned into 5 data subsets, 
#this allows having a variety of partitions to train and measure 
#the result of the hyperparameter adjustment process.
set.seed(seed)
folds <- vfold_cv(train_set, v = 5)

# Random forest model spec
rf_spec <- rand_forest(
  mtry = tune(), trees = tune(), min_n = tune()) %>% # parameters will be calculated and fine tuned below.
  set_engine("ranger", num.threads = 7, importance = "impurity") %>%  # using ranger package for random forest 
  set_mode("regression") # this is a regression 

# Random forest workflow
rf_Wf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(rf_spec)

# Space-filling designs grid
rf_grid <- 
  grid_space_filling(
    min_n(), 
    mtry(range = c(4, 10)), # limit the range to what should be good.
    trees(range = c(50, 3000)), 
    size = 100) # why 100?  looks ok, graphs(autoplot(tune_res)) show variability curve  


# use seven cores for this process
doParallel::registerDoParallel(cores = 7) # this will speed up the processing, not entirely needed for this operation.

set.seed(seed)

tune_res <- 
  rf_Wf %>% 
  tune_grid(
    resamples = folds, grid = rf_grid, 
    metrics = metric_set(rmse, mae, rsq)
  )


autoplot(tune_res) 

tune_res$.metrics


# finalize model set up
final_rf <- rf_Wf %>% 
  finalize_workflow(
    show_best(x = tune_res, metric = "rmse", n = 1) # optimized to reduce rmse, can pick other metric_set(rmse, mae, rsq) 
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
  vip(num_features = 20)  


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



#### Use Model to Predict Biomass ####
## prediction, out of sample 
## prediction data set, out of sample, subplot 2

full_data <- subplot_data %>%
  filter(inter_crop %in% c("oat","pea","oat-pea")) %>% # keep only oat accessions, remove barley
  
  select(plot_name,plot_subplot,subplot, "observationUnitName" ,"germplasmName",
         "plotNumber",observationLevel,blockNumber,inter_crop, source,
         `Above ground dry biomass - g|day 168|COMP:0000064`,
         `Weed above ground dry biomass - g|day 168|COMP:0000066`,
         `Pea above ground dry biomass - g|day 168|COMP:0000061`,
         `Pea plant height - cm|day 164|COMP:0000040`,
         `Plant height - cm|day 164|COMP:0000023`,
         contains("mean")) %>% # selecting columns with "mean" in them keeps all the spectral subplot means data
  
  dplyr::rename(oat_biomass = `Above ground dry biomass - g|day 168|COMP:0000064`) %>% 
  dplyr::rename(pea_biomass = `Pea above ground dry biomass - g|day 168|COMP:0000061`) %>% 
  dplyr::rename(oat_height = "Plant height - cm|day 164|COMP:0000023") %>% 
  dplyr::rename(pea_height = "Pea plant height - cm|day 164|COMP:0000040")




four_reps <- full_data %>% 
  dplyr::group_by(germplasmName) %>% 
  dplyr::count() %>% 
  filter(n==12)# make a list of genotypes with 4 reps, don't use 2 rep genotypes


prediction_data <- full_data %>% 
  filter(subplot == 2) %>% # only make estimates using subplot 2
  filter(germplasmName %in%  c(four_reps$germplasmName)) %>% # remove 2 rep genotypes
  select(observationUnitName,inter_crop,oat_height,pea_height,contains("mean")) %>% 
  filter(inter_crop %in% c("oat-pea","oat"))  


str(prediction_data)

prediction_data


final_wf <- final_rs %>%
  extract_workflow()

final_wf # this is the workflow model with the best paramaters 

pred <- predict(final_wf, prediction_data[,-1] ) # remove observationUnitName it isnt part of the model

biomass_predictions <- bind_cols(prediction_data,pred) %>% 
  select(observationUnitName,.pred) %>% 
  dplyr::rename(pred_biomass = .pred)


#### Heritability of Biomass estimates ####
library(lme4) 

lm_model_factors <- subplot_data %>% 
  select(observationUnitName,blockNumber,germplasmName) %>% 
  mutate(blockNumber = as.character(blockNumber))

df <- biomass_predictions %>% 
  left_join(lm_model_factors) %>% 
  mutate(blockNumber = as.factor(blockNumber)) %>% 
  mutate(germplasmName = as.factor(germplasmName))


str(df)

df %>% 
  dplyr::group_by(blockNumber) %>% 
  dplyr::count() %>% 
  arrange(n) %>% 
  print(n=63)


biomass <- lme4::lmer(pred_biomass ~ (1|germplasmName) + (1|blockNumber),
                      data = df)


biomass <- lme4::lmer(pred_biomass ~ (1|germplasmName),
                      data = df)


df_trait <- as.data.frame(VarCorr(biomass))

vg_trait <- df_trait[df_trait$grp == "germplasmName","vcov"]

ve_trait <- df_trait[df_trait$grp == "Residual", "vcov"]

h2_trait <- vg_trait/(vg_trait + ve_trait/(4))

h2_trait


