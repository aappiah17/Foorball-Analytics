# Read the rds data
#load packages and helper functions
source("Packages and helper functions.R")
all_shots_years <- readRDS("Allshots.rds") # data scraped from understat
# Define functions for computing estimates 
distance <- function(x_pos, y_pos){
  x_meters <- 95.4
  y_meters <- 76.25
  
  x_shift <- (100 - x_pos)*x_meters/100
  y_shift <- abs(50 - y_pos)*y_meters/100
  
  distance <- sqrt(x_shift*x_shift + y_shift*y_shift)
}

goal_angle <- function(x_pos, y_pos){
  x_meters <- 95.4
  y_meters <- 76.25
  
  x_shift <- (100 - x_pos)*x_meters/100
  y_shift <- (50 - y_pos)*y_meters/100
  
  angle <- atan((7.32*x_shift)/(x_shift*x_shift + y_shift*y_shift - (7.32/2)*(7.32/2)))
  angle <- ifelse(angle < 0, angle + pi, angle)
  
  angle_degrees <- angle*180/pi
}

# Exploratory data analysis
glimpse(all_shots_years)


# Define function for summaries
sum_var <- function(var){
  all_shots_years %>%
    group_by({{var}})%>%
    tally() %>%
    mutate(prop=n/sum(n))
}

# See type shots by year
sum_var(year)
# See results of shots 
sum_var(result)
# See types of shots 
sum_var(shotType)
# See situation 
sum_var(situation)
# last action
check<-sum_var(lastAction)

all_shots_years %>%
  group_by(result)%>%
  tally()
# Data preparation
# exclude own goals
shots_ml <- all_shots_years %>% filter(result !="OwnGoal")%>%
  # create a goal variable
  mutate(is_goal=ifelse(result=="Goal","Goal","No Goal"))%>%
  rowwise() %>%
  mutate(distance = distance(X, Y)) %>% # distance from goal mid-point
  mutate(angle = goal_angle(X, Y), is_goal=factor(is_goal, levels=c("Goal","No Goal"))) %>% # based on available goal mouth
  #  ungroup()%>%
  # group_by(match_id)%>%
  #arrange(minute, .by_group=T)%>%
  #dplyr::mutate(h_score=lag((is_goal=="Goal")*(h_a=="h"), default = 0),
  #a_score=lag((is_goal=="Goal")*(h_a=="a"), default = 0),
  #game_state=ifelse(h_a=="h",h_score-a_score,a_score-h_score))%>%
  select(-h_team, -a_team, -player_assisted, -date, -h_goals, -a_goals, -result) %>% na.omit()


# Start modelling
set.seed(1991) #My birthyear

shots_split <- initial_split(shots_ml, prob=0.8, strata = is_goal)
shots_train <- training(shots_split)
shots_test <- testing(shots_split)
shot_fold <- vfold_cv(shots_train)


shots_ml%>% filter(is.na(lastAction))%>%
  tally()
# Shots recipe
shots_recipe <-
  recipe(is_goal~X+Y+distance+angle+shotType+situation+lastAction, data = shots_train)%>%
  step_normalize(all_numeric()) %>%
  #step_string2factor(all_nominal()) %>%
  step_other(lastAction, threshold = 0.03) %>%
  step_dummy(all_nominal_predictors(),one_hot = T)%>%
  step_corr(all_numeric_predictors(), threshold = 0.7, method = "spearman") 


# Logistic regression
lr_model <- logistic_reg()%>%
  set_engine("glm")

# Put everything together in a workflow
shots_wf <-
  workflow()%>%
  add_recipe(shots_recipe)%>%
  add_model(lr_model)

# Fit the model
tic()
shots_fit <- shots_wf %>% 
  fit_resamples(resamples=shot_fold, control=control_resamples(save_pred = TRUE))
toc()

# Evaluate the model
collect_metrics (shots_fit)

# Last fit 
shot_fit_1 <- last_fit(shots_wf, shots_split)
collect_metrics(shot_fit_1)

# Collect predictions and bind to the test data
collect_predictions(shot_fit_1) %>% select(xg_AA=.pred_1, is_goal)%>%
  bind_cols(shots_test%>%select(xG))

collect_predictions(shot_fit_1)%>%
  conf_mat(is_goal,.pred_class)
# Sensitivity and specificity

collect_predictions(shot_fit_1)%>%
  sens(is_goal,.pred_class)
collect_predictions(shot_fit_1)%>%
  spec(is_goal,.pred_class)

shot_fit_1 %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 20)

# Train random forest
# Specify rf 

rf_spec <-
  rand_forest(mtry = tune(),
              trees = 1000,
              min_n = tune()) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance="permutation")

rf_workflow <-
  workflow() %>%
  add_recipe(shots_recipe) %>%
  add_model(rf_spec)

set.seed(1991)
tic()
rf_tune <-
  tune_grid(rf_workflow,
            resamples = shot_fold,
            grid=20
  )
toc()
# Evaluate models 
rf_tune %>% collect_metrics()

# Show best model 
## Based on accuracy
rf_tune %>% show_best("accuracy")
## Based on roc auc
rf_tune %>% show_best("roc_auc")

# Finalize model
rf_spec <- finalize_model(rf_spec, rf_tune %>% select_best("roc_auc"))
final_rf <- workflow()%>%
  add_model(rf_spec)%>%
  add_recipe(shots_recipe)

# Last fit
shots_rf_fit<-last_fit(rf_workflow, shots_split)
shots_rf_fit%>% collect_metrics()

shots_rf_fit %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 20)

# Check final metrics
shots_rf_fit %>% collect_metrics()
collect_predictions(shots_rf_fit)%>%
  conf_mat(is_goal,.pred_class)
# Sensitivity and specificity

collect_predictions(shots_rf_fit)%>%
  sens(is_goal,.pred_class)
collect_predictions(shots_rf_fit)%>%
  spec(is_goal,.pred_class)

# Train the model with xGBoost
# Model with xgboost
xgboost_model<-
  boost_tree(
    mode = "classification",
    trees = 1000,
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
  ) %>%
  set_engine('xgboost')

xgboost_params <-
  parameters(
    min_n(),
    tree_depth(),
    learn_rate(),
    loss_reduction()
  )
xgboost_grid <- grid_max_entropy(
  xgboost_params,
  size = 10
)

# minimal tuning
xgboost_model<-
  boost_tree(
  ) %>%
  set_engine('xgboost')%>%
  set_mode("classification")

# Wf
shot_model <- workflow()%>%
  add_model(xgboost_model)%>%
  add_recipe(shots_recipe)

tic()
shot_tuned <- 
  shot_model %>%
  fit_resamples(
    resamples = shot_fold, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  ) 

toc()

# Show best model 
## Based on accuracy
shot_tuned %>% show_best("accuracy")
## Based on roc auc
shot_tuned %>% show_best("roc_auc")

# Finalize model
shot_tuned <- finalize_model(xgboost_model, shot_tuned %>% select_best("roc_auc"))
final_xgboost <- workflow()%>%
  add_model(xgboost_model)%>%
  add_recipe(shots_recipe)

# Last fit
shots_xg_fit<-last_fit(final_xgboost, shots_split)

# Model Eval
shots_xg_fit%>% collect_metrics()

collect_predictions(shots_xg_fit)%>%
  conf_mat(is_goal,.pred_class)
# Sensitivity and specificity

collect_predictions(shots_xg_fit)%>%
  sens(is_goal,.pred_class)
collect_predictions(shots_xg_fit)%>%
  spec(is_goal,.pred_class)

shots_xg_fit %>% 
  pluck(".workflow", 1) %>%   
  extract_fit_parsnip() %>% 
  vip(num_features = 20)

# Fit final model
final_shot_model <- final_xgboost %>% fit(shots_ml)

saveRDS(final_shot_model,"Goal model/xg_model.rds")


# More parameter tuning
tic()
doParallel::registerDoParallel()
xgboost_tuned <- tune_grid(
  object = soccer_xg,
  resamples = shot_fold,
  grid = xgboost_grid,
  metrics = metric_set(yardstick::accuracy, roc_auc, mn_log_loss),
  control = control_grid(verbose = TRUE)
)

toc()
# Evaluate models 
shot_tuned %>% collect_metrics()

# Show best model 
## Based on accuracy
xgboost_tuned %>% show_best("accuracy")
## Based on roc auc
xgboost_tuned %>% show_best("roc_auc")

# Finalize model
xgboost_model <- finalize_model(xgboost_model, xgboost_tuned %>% select_best("roc_auc"))
final_xgboost <- workflow()%>%
  add_model(xgboost_model)%>%
  add_recipe(shots_recipe)

# Last fit
shots_xg_fit<-last_fit(final_xgboost, shots_split)
shots_xg_fit%>% collect_metrics()

shots_xg_fit %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 20)

# Check final metrics
collect_predictions(shots_xg_fit)%>%
  conf_mat(is_goal,.pred_class)
# Sensitivity and specificity

collect_predictions(shots_xg_fit)%>%
  sens(is_goal,.pred_class)
collect_predictions(shots_xg_fit)%>%
  spec(is_goal,.pred_class)
# Collect predictions and bind to the test data
AA_pred_xg<-collect_predictions(shots_xg_fit) %>% select(xg_AA=.pred_Goal, .pred_class)%>%
  bind_cols(shots_test)
pen<-AA_pred_xg %>% filter(situation=="Penalty")

AA_pred_xg %>%
  group_by(year)%>%
  summarise(xg_AA=sum(xg_AA), xg_under=sum(xG))%>%
  arrange(-xg_under)%>%
  slice(1:20)
# Model with no penalties
final_xg_model <- final_xgboost %>% fit(shots_ml)
saveRDS(final_xg_model, "AA_xGModel.rds")