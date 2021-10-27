# Load packages and helper functions
source("Packages and helper functions.R")
leagues_20_events <- readRDS("leagues_20_events.rds") # pass data from whoscored used here


# Clean passing data and create some additional variables
passing_data <-  leagues_20_events%>%
  filter(type=="Pass") %>%
  select(minute,second,h_a,team_name,x,y,end_x,end_y,match_id,player_id,player_name,outcome_type,qualifiers,satisfied_events_types,pass_accurate,short_pass_inaccurate,short_pass_accurate,
         pass_corner,pass_corner_accurate,pass_corner_inaccurate,pass_freekick,pass_back,pass_forward,pass_left,pass_right,
         pass_long_ball_accurate,pass_long_ball_inaccurate,pass_right_foot,pass_left_foot,pass_head,
         pass_cross_accurate,pass_cross_inaccurate,pass_through_ball_accurate,pass_through_ball_inaccurate,throw_in,
         defensive_third,mid_third,final_third,epv,league_name)%>%
  # Calculate passing distance
  mutate(passing_distance=round(distance_function(x,y,end_x,end_y),1), pass_angle=atan2(end_y*0.68-y*0.68,end_x*1.05-x*1.05),
         pass_angle =round(pass_angle*180/pi/15)*15,initial_dist_to_goal=round(distance_function(x,y,105,34),1),
         final_dist_to_goal=round(distance_function(end_x,end_y,105,34),1),
         is_progressive=ifelse((final_dist_to_goal/initial_dist_to_goal)<0.75,T,F),
         # Change location data to international standard
         across(c(x,end_x),.fns = ~round((.*105/100),1)),
         across(c(y,end_y),.fns = ~round((.*68/100),1)))%>%
  relocate(c(passing_distance,pass_angle),.after=end_y) %>%
  mutate(pass_body_part=case_when(pass_right_foot==T~"Right Foot",
                                  pass_left_foot==T~"Left Foot",
                                  pass_head==T~"Head"),
         pass_field_area=case_when(defensive_third==T~"Defensive Third",
                                   mid_third==T~"Mid Third",
                                   final_third==T~"Final Third"),
         pass_direction=case_when(pass_back==T~"Back Pass",
                                  pass_forward==T~"Forward Pass"),
         through_ball=ifelse(pass_through_ball_accurate==T|pass_through_ball_inaccurate, T,F),
         cross=ifelse(pass_cross_accurate==T|pass_cross_inaccurate==T,TRUE,FALSE),
         long_ball=ifelse(pass_long_ball_accurate==T|pass_long_ball_inaccurate==T,T,F)
  )%>%
  relocate(pass_body_part,.after=pass_head)%>%
  relocate(pass_direction,.after=pass_forward)


# Select variables for modelling 
passing_model_df <- passing_data %>% 
  select(player_id,player_name,h_a,end_x,end_y,passing_distance,pass_angle,pass_field_area,outcome_type,pass_corner,pass_freekick,pass_direction,through_ball,cross,long_ball,
         throw_in,pass_head)%>%
  mutate(outcome_type=factor(outcome_type, levels = c("Unsuccessful","Successful")),
         h_a=factor(h_a,levels = c("h","a")))

# Start modelling
set.seed(1991) #My birthyear
# Split data
pass_split <- initial_split(passing_model_df, prob=0.80)
pass_train <- training(pass_split)
pass_test <- testing(pass_split)
pass_fold <- vfold_cv(pass_train)



# Prep recipe
pass_recipe <-
  recipe(outcome_type~., data = pass_train)%>%
  update_role(player_name, player_id,new_role = "id variable") %>%
  step_impute_bag(pass_direction)%>%
  recipes::step_normalize(all_numeric_predictors()) %>%
  recipes::step_dummy(all_nominal_predictors(),one_hot = T)%>%
  # recipes::step_nzv(all_nominal_predictors())%>%
  step_corr(all_numeric_predictors(), threshold = 0.7, method = "spearman") 


# Specify logistic regression
lr_model <- logistic_reg()%>%
  set_engine("glm")

# Put everything together in a workflow

pass_wf <-
  workflow()%>%
  add_recipe(pass_recipe)%>%
  add_model(lr_model)
# Fit the model
tic()
pass_fit <- pass_wf %>% 
  tune::fit_resamples(resamples=pass_fold, control=control_resamples(save_pred = TRUE))
toc()

# Evaluate the model
collect_metrics (pass_fit)

# Last fit 
pass_fit_1 <- last_fit(pass_wf, pass_split)
collect_metrics(pass_fit_1)

# Collect predictions and bind to the test data
pass_pred <-collect_predictions(pass_fit_1) %>% select(predicted_pass_outcome=.pred_class)%>%
  bind_cols(pass_test)

pass_pred %>% filter(outcome_type=="Successful")%>%
  group_by(player_name)%>%
  tally()%>%
  arrange(-n)

pass_pred %>% filter(outcome_type=="Successful")%>%
  group_by(player_name)%>%
  tally()%>%
  arrange(-n)
collect_predictions(pass_fit_1)%>%
  conf_mat(outcome_type,.pred_class)
# Sensitivity and specificity

collect_predictions(pass_fit_1)%>%
  sens(outcome_type,.pred_class)
collect_predictions(pass_fit_1)%>%
  spec(outcome_type,.pred_class)

collect_predictions(pass_fit_1)%>%
  metrics(outcome_type,.pred_class,.pred_class)

pass_fit_1 %>% 
  pluck(".workflow", 1) %>%   
  extract_fit_parsnip() %>% 
  vip(num_features = 20)

# Fit xGBoost
# Set model parameters
# Minimal tuning 

xgboost_model<-
  boost_tree(
  ) %>%
  set_engine('xgboost')%>%
  set_mode("classification")

# Wf
pass_model <- workflow()%>%
  add_model(xgboost_model)%>%
  add_recipe(pass_recipe)

tic()
xgboost_tuned <- 
  pass_model %>%
  fit_resamples(
    resamples = pass_fold, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  ) 

toc()


# More parameter tuning

xgboost_model<-
  boost_tree(
    mode = "classification",
    trees = 100,
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

# xG Wf
pass_model <- workflow()%>%
  add_model(xgboost_model)%>%
  add_recipe(pass_recipe)

tic()
doParallel::registerDoParallel()
xgboost_tuned <- tune_grid(
  object = pass_model,
  resamples = pass_fold,
  grid = xgboost_grid,
  metrics = metric_set(yardstick::accuracy, roc_auc, mn_log_loss),
  control = control_grid(verbose = TRUE)
)

toc()
# Evaluate models 
xgboost_tuned %>% collect_metrics()

# Show best model 
## Based on accuracy
xgboost_tuned %>% show_best("accuracy")
## Based on roc auc
xgboost_tuned %>% show_best("roc_auc")

# Finalize model
xgboost_model <- finalize_model(xgboost_model, xgboost_tuned %>% select_best("roc_auc"))
final_xgboost <- workflow()%>%
  add_model(xgboost_model)%>%
  add_recipe(pass_recipe)

# Last fit
pass_fit<-last_fit(final_xgboost, pass_split)
pass_fit%>% collect_metrics()

pass_fit %>% 
  pluck(".workflow", 1) %>%   
  extract_fit_parsnip() %>% 
  vip(num_features = 20)

# Check final metrics
collect_predictions(pass_fit)%>%
  conf_mat(outcome_type,.pred_class)
# Sensitivity and specificity

collect_predictions(pass_fit)%>%
  sens(outcome_type,.pred_class)
collect_predictions(pass_fit)%>%
  spec(outcome_type,.pred_class)
# Save the model
tic()
final_pass_model <- final_xgboost %>% fit(passing_model_df)
toc()

saveRDS(final_pass_model,"Pass model/pass_comp_model.rds")
