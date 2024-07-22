#--------------------------------------------------------------------------#
# Project: POCFluxPred
# Script purpose: Fit a RF model to predict POC flux from POC concentration profile and sediment trap depth.
# Date: 18/07/2024
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#

source("utils.R")


## Data preparation ----
#--------------------------------------------------------------------------#
message("Data preparation ----")

## Read data
# Load data to fit the model
load("data/00.df_all.Rdata")

## Transform
# Log-transform POC flux and store it in another column
df_all <- df_all %>% 
  mutate(log_poc_flux = log10(poc_flux), .after = poc_flux)

## Assign variable roles
resp_var <- c("log_poc_flux", "poc_flux")
meta_vars <- c("lon", "lat")
exp_vars <- df_all %>% select(depth_trap, poc_0:poc_1000) %>% colnames()
# Use both meta and explain vars
#exp_vars <- c(meta_vars, exp_vars)

## Data split
set.seed(seed)
# Stratified CV on deciles of response variable
nb_folds <- 5
folds <- nested_cv(
    df_all,
    outside = vfold_cv(v = nb_folds, strata = log_poc_flux, breaks = 4),
    inside = vfold_cv(v = nb_folds, strata = log_poc_flux, breaks = 4)
  ) %>%
  mutate(cv_type = "stratified")


## Model definition ----
#--------------------------------------------------------------------------#
message("Model definition ----")

# Define a Random Forest model with hyperparameters to tune
rf_spec <- rand_forest(
  trees = tune(),
  mtry = tune(),
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger")

# Generate formula from list of explanatory variables
#TODO: include lon and lat?
rf_form <- as.formula(paste("log_poc_flux ~ ", paste(c("poc_flux", exp_vars), collapse = " + "), sep = ""))

# Define one grid for all folds
set.seed(seed)
rf_grid <- grid_latin_hypercube(
  trees(range = c(50, 1000)),
  mtry(range = c(3, 37)),
  min_n(),
  size = 10
)


## Model fitting ----
#--------------------------------------------------------------------------#
message("Model fitting ----")

res <- pbmclapply(1:nrow(folds), function(i){
  
  ## Get fold
  x <- folds[i,]
  
  ## Train and test sets
  df_train <- analysis(x$splits[[1]]) %>% as_tibble()
  df_test <- assessment(x$splits[[1]]) %>% as_tibble()
  
  ## Recipe
  rf_rec <- recipe(rf_form, data = df_train) %>%
    update_role(poc_flux, new_role = "untransformed outcome")
  
  ## Workflow
  rf_wflow <- workflow() %>%
    add_recipe(rf_rec) %>%
    add_model(rf_spec)
  
  ## Gridsearch
  set.seed(seed)
  rf_res <- tune_grid(
    rf_wflow,
    resamples = x$inner_resamples[[1]],
    grid = rf_grid,
    metrics = metric_set(rmse),
    control = control_grid(save_pred = TRUE)
  )
  best_params <- select_best(rf_res, metric = "rmse")
  
  ## Final fit
  final_rf <- finalize_workflow(
    rf_wflow,
    best_params
  )
  final_res <- fit(final_rf, df_train)
  
  ## Save model to use later
  save(final_res, file = paste0("models/01.fold_", str_pad(i, width = 2, pad = "0"), ".rda"))
  
  ## Prediction on outer folds
  preds <- predict(final_res, new_data = df_test) %>%
    bind_cols(df_test %>% select(log_poc_flux))
  
  ## Model explainer
  # Select only predictors
  vip_train <- rf_rec %>% prep() %>% bake(new_data = NULL, all_predictors())
  
  # Explainer
  rf_explain <- DALEXtra::explain_tidymodels(
    model = extract_fit_parsnip(final_res),
    data = vip_train,
    y = df_train %>%  pull(log_poc_flux),
    verbose = FALSE
  )
  
  # Variable importance
  full_vip <- DALEX::model_parts(rf_explain) %>%
    bind_rows() %>%
    filter(variable != "_baseline_")
  
  # CP profiles for all variables
  cp_profiles <- lapply(exp_vars, function(my_var){
    DALEX::model_profile(explainer = rf_explain, variables = my_var)$cp_profiles %>% as_tibble()
  }) %>%
    bind_rows()

  ## Return results
  return(tibble(
    resp = resp_var[2],
    season = "0",
    cv_type = x$cv_type,
    fold = x$id,
    preds = list(preds),
    importance = list(full_vip),
    cp_profiles = list(cp_profiles)
  ))
}, mc.cores = 5, ignore.interactive = TRUE) %>%
  bind_rows()


## Save results ----
#--------------------------------------------------------------------------#
save(res, file = "data/01.predictions.Rdata")
