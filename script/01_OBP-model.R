#load some packages#
#install.packages('tidymodels')
#install.packages('tidyverse')
library(tidymodels)
library(tidyverse)
tidymodels_prefer(quiet = FALSE)

#load some data
data <- "1jXGfMCmPkutX7sXgyk1YZIryD_iFPa6J" # google file ID
df <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", data)) #https://stackoverflow.com/questions/33135060/read-csv-file-hosted-on-google-drive
past <- read_csv("data/batting_2018.csv") #Load Fangraphs 2018 season data

#Clean the data
names(df)
str(df)
df <- as.data.frame(apply(df, 2, function(x) gsub("%",'', x, fixed = FALSE))) #
#remove the percent sign - https://stackoverflow.com/questions/48953295/replace-a-specific-strings-from-multiple-columns-in-a-dataframe 

#Change character to numeric for proper categories
names <- df %>% select(playerid:Team)
df <- df %>%
  select(-c(playerid:Team)) %>% 
  mutate_if(is.character,as.numeric) %>%
  bind_cols(names) %>% 
  select(playerid:Team,everything())

past <- past %>%  select(-Team) #Set up previous season data for joining by removing team column.

#Find 2018 mean OBP and PA. This will be used impute data for players with no previous season data
means <- past %>%
  summarise(obp_mean = weighted.mean(OBP,PA),
            PA_mean = weighted.mean(PA,OBP))
means
mean_obp = means[,1]
mean_PA = means[,2]

#Set up the data for modeling
d <- df %>% select(-c(playerid:Team,FullSeason_OBP))

#Keep biographical data
save <- df %>% select(playerid:Team,FullSeason_OBP,MarApr_PA) %>% 
  left_join(past,by=c("Name"="Name")) %>% 
  mutate(PA = replace_na(PA,mean_PA),
         OBP = replace_na(OBP,mean_obp)) %>% 
  rename(PA_2018=PA,OBP_2018=OBP)

save <- as.data.frame(save)


#For a problem like this, I would typically split into train and test, but given that there is no test set this is commented out.
#Data splitting
# df_split <- initial_split(d, prop = .7,strata=MarApr_PA) # Last two weeks
# df_split
# 
# df_train <- training(df_split)
# df_test  <- testing(df_split)
# c(training = nrow(df_train), testing = nrow(df_test))


spec_lm <- linear_reg() %>% set_engine("lm") #create an engine for standard linear regression
spec_glmnet <- linear_reg(penalty = tune(),mixture=tune()) %>% set_engine("glmnet") #create an engine for regularized regression

#Create a 'recipe' for the model
df_rec <- 
  recipe(MarApr_OBP ~ ., 
         data = d) %>% 
  step_zv(all_predictors()) %>% #remove any zero variance variables
  step_normalize(all_numeric_predictors()) %>% #center and scale numeric data
  step_corr(all_numeric_predictors(), threshold = 0.9) %>% #remove highly correlated variables
  step_pca(all_numeric_predictors()) #combine data into components, ie, do PCA

#Build out workflows for both the linear and regularized regression
lm_wflow <- 
  workflow() %>% #set up a workflow
  add_model(spec_lm) %>% #add the model type - linear regression
  add_recipe(df_rec) #add the model and feature engineering

glmnet_wflow <- 
  workflow() %>% 
  add_model(spec_glmnet) %>% #add the regularized regression model type
  add_recipe(df_rec) 

#Fit the linear regression
lm_fit <- lm_wflow %>% fit(d)

#Tune the regularized regression
glmnet_wflow %>% parameters()
set.seed(2)

cv_splits <- rsample::vfold_cv(d)

glmn_set <- parameters(penalty(range = c(-5,1), trans = log10_trans()),
                       mixture())

glmn_grid <- 
  grid_regular(glmn_set, levels = c(7, 5))

ctrl <- control_grid(save_pred = TRUE, verbose = TRUE)

glmn_tune <- 
  tune_grid(glmnet_wflow,
            resamples = cv_splits,
            grid = glmn_grid,
            metrics = metric_set(rmse),
            control = ctrl)

best_glmn <- select_best(glmn_tune, metric = "rmse")
best_glmn

#final glmnet model and fit
glmnet_final <- 
  glmnet_wflow %>%
  finalize_workflow(best_glmn) %>%
  fit(data = d)

#Linear Regression Prediction
d_predict_lm <- stats::predict(lm_fit,new_data = d) %>% 
  rename(.lmpred=.pred)

#GLM Net Prediction
d_predict <- stats::predict(glmnet_final,new_data = d) %>% 
  rename(.glmpred=.pred) %>% 
  bind_cols(save) %>% bind_cols(d_predict_lm)


d_predict$PA_2018 <- as.numeric(d_predict$PA_2018) #convert columns from list to numeric. 
d_predict$OBP_2018 <- as.numeric(d_predict$OBP_2018)
d_predict

rmse(d_predict,.lmpred,FullSeason_OBP)
rmse(d_predict,.glmpred,FullSeason_OBP) #the glm model on its own is barely better than the lm
rsq(d_predict,truth = FullSeason_OBP, estimate=.glmpred)
rsq(d_predict,truth = FullSeason_OBP, estimate=.lmpred)
#Combine 2019 predictions with previous season weighted by plate appearances with last season's PA decayed to 50% of its original PA
decay = .5
final <- d_predict %>% 
  mutate(glm_pred=(.glmpred*MarApr_PA)+(OBP_2018*(PA_2018*decay)),pa=MarApr_PA+(PA_2018*decay)) %>% 
  mutate(prediction=glm_pred/pa) %>%
  select(playerid,Name,Team,prediction,FullSeason_OBP) %>% 
  mutate(residual=prediction-FullSeason_OBP)

rmse(final,prediction,FullSeason_OBP) #rmse = 30 obp points
rsq(final,truth = FullSeason_OBP, estimate=prediction) #rsq = .414

final %>% write_csv('export/predictions.csv')



