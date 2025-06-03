#--------------------------------------------------------------------------#
# Project: POCFluxPred
# Script purpose: Function to train a model given a configuration path
# Date: 02/06/2025
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#

source(here::here("utils.R"))

run_experiment <- function(config_path) {
  
  ## Config
  message("Reading configuration")
  # Read configuration file
  config <- yaml::read_yaml(config_path)
  # Get base dir for saving models and results
  base_dir <- fs::path_dir(config_path)
  # Create dir for models
  # For models, first delete the previous results
  models_dir <- fs::path(base_dir, "models")
  if (fs::dir_exists(models_dir)) {fs::dir_delete(models_dir)}
  fs::dir_create(models_dir)
  # Create dir for results
  results_dir <- fs::path(base_dir, "results")
  fs::dir_create(results_dir)
  # Seed
  set.seed(config$seed)
  
  ## Data preparation
  message("Preparing data")
  # Read dataset
  data <- read_csv(config$dataset, show_col_types = FALSE)
  
  # Log-transform POC_flux
  data <- data %>% mutate(log_poc_flux = log10(poc_flux), .after = poc_flux)
  
  # Duplicate coordinates (essential if using lon and lat as predictors)
  data <- data %>% mutate(x = lon, y = lat, .before = lon)
  
  
  ## Data split
  # Transform dataframe to sf object for spatial CV.
  data_sf <- sf::st_as_sf(data, coords = c("x", "y"), crs = 4326)
  
  # Generate folds

  folds <- bind_rows(
    # Stratified CV on deciles of response variable
    nested_cv(
      data,
      outside = vfold_cv(v = config$nested_cv$n_outer_folds, strata = log_poc_flux, breaks = config$nested_cv$n_breaks_strat),
      inside = vfold_cv(v = config$nested_cv$n_inner_folds, strata = log_poc_flux, breaks = config$nested_cv$n_breaks_strat)) %>%
      mutate(cv_type = "stratified"),
    # Spatial CV
    nested_cv(
      data_sf,
      outside = spatialsample::spatial_block_cv(v = config$nested_cv$n_outer_folds),
      inside = spatialsample::spatial_block_cv(v = config$nested_cv$n_inner_folds)) %>%
      mutate(cv_type = "spatial")
  )
  
  ## Model definition
  message("Preparing model")
  # Define a xgboost model with hyperparameters to tune
  xgb_spec <- boost_tree(
    trees = tune(),
    tree_depth = tune(),
    min_n = tune(),
    learn_rate = tune(),
    sample_size = 0.5
  ) %>%
    set_mode("regression") %>%
    set_engine("xgboost")
  
  # Generate formula from list of explanatory variables
  xgb_form <- as.formula(paste("log_poc_flux ~ ", paste(c("poc_flux", config$exp_vars), collapse = " + "), sep = ""))
  
  # Define one grid for all folds
  xgb_grid <- grid_space_filling(
    trees(),
    learn_rate(),
    tree_depth(),
    min_n(),
    size = config$grid_size
  )
  
  ## Model fitting
  message("Fitting model")
  res_xgb <- pbmcapply::pbmclapply(1:nrow(folds), function(i){
    
    ## Get fold
    x <- folds[i,]
    
    ## Train and test sets
    df_train <- analysis(x$splits[[1]])
    df_test <- assessment(x$splits[[1]])
    
    # If df_train is of type sf
    if (any(class(df_train) == "sf")) {
      # Extract coordinates and add them to original tibble
      df_train <- sf::st_drop_geometry(df_train)  
      df_test <- sf::st_drop_geometry(df_test)
      
    } else {
      # Otherwise, just convert to tibble
      df_train <- as_tibble(df_train) %>% select(-c(x, y))
      df_test <- as_tibble(df_test) %>% select(-c(x, y))
    }
    
    ## Recipe
    xgb_rec <- recipe(xgb_form, data = df_train) %>%
      update_role(poc_flux, new_role = "untransformed outcome")
    
    ## Workflow
    xgb_wflow <- workflow() %>%
      add_recipe(xgb_rec) %>%
      add_model(xgb_spec)
    
    ## Gridsearch
    xgb_res <- tune_grid(
      xgb_wflow,
      resamples = x$inner_resamples[[1]],
      grid = xgb_grid,
      metrics = metric_set(rmse),
      control = control_grid(save_pred = TRUE)
    )
    best_params <- select_best(xgb_res, metric = "rmse")
    
    ## Final fit
    final_xgb <- finalize_workflow(
      xgb_wflow,
      best_params
    )
    final_res <- fit(final_xgb, df_train)
    
    # Save model for later use
    saveRDS(final_res, file = fs::path(models_dir, paste0("xgb_fold_", str_pad(i, width = 2, pad = "0"), ".rds")))
    

    ## Prediction on outer folds
    preds <- bind_cols(
      df_test,
      predict(final_res, new_data = df_test)
    )
    
    ## Model explainer
    # Select only predictors
    vip_train <- xgb_rec %>% prep() %>% bake(new_data = NULL, all_predictors())
    
    # Explainer
    xgb_explain <- DALEXtra::explain_tidymodels(
      model = extract_fit_parsnip(final_res),
      data = vip_train,
      y = df_train %>%  pull(log_poc_flux),
      verbose = FALSE
    )
    
    # Variable importance
    full_vip <- DALEX::model_parts(xgb_explain) %>%
      bind_rows() %>%
      filter(variable != "_baseline_")
    
    # CP profiles for all variables
    cp_profiles <- lapply(config$exp_vars, function(my_var){
      DALEX::model_profile(explainer = xgb_explain, variables = my_var)$cp_profiles %>% as_tibble()
    }) %>%
      bind_rows()
    
    ## Return results
    return(tibble(
      resp = config$resp_var[2],
      cv_type = x$cv_type,
      fold = x$id,
      preds = list(preds),
      importance = list(full_vip),
      cp_profiles = list(cp_profiles)
    ))
  }, mc.cores = 8, ignore.interactive = TRUE) %>%
    bind_rows()
  
  ## Save results
  message("Saving results")
  saveRDS(res_xgb, fs::path(results_dir, "res_xgb.rds"))
}