#--------------------------------------------------------------------------#
# Project: POCFluxPred
# Script purpose: Predict new POC flux
# Date: 22/07/2024
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#

source("utils.R")

## Load data ----
#--------------------------------------------------------------------------#
load("data/00.df_all.Rdata")

# Subsample some profiles
#TODO consider doing this not randomly
df_sub <- df_poc %>% 
  drop_na() %>% 
  slice_sample(n = 49)


## Generate predictions ----
#--------------------------------------------------------------------------#
## List saved models
files <- list.files("models", full.names = TRUE)

new_preds <- lapply(1:length(files), function(i){
  # Load model
  file <- files[i]
  load(file)
  
  # Predict flux at 100 and 1000 m for all pixels
  flux_100 <- augment(
    final_res, 
    new_data = df_poc %>% 
      mutate(
        poc_flux = NA,
        depth_trap = 100,
        #.before = lon
      ) %>% 
      select(lon, lat, poc_flux, depth_trap, everything()) %>% 
      drop_na(poc_0)
  ) %>% 
    select(-poc_flux) %>% 
    rename(pred_log_poc_flux = .pred) %>% 
    mutate(pred_poc_flux = 10^(pred_log_poc_flux), .after = pred_log_poc_flux) %>% 
    select(lon, lat, depth = depth_trap, pred_poc_flux)
  
  flux_1000 <- augment(
    final_res, 
    new_data = df_poc %>% 
      mutate(
        poc_flux = NA,
        depth_trap = 1000,
        #.before = lon
      ) %>% 
      select(lon, lat, poc_flux, depth_trap, everything()) %>% 
      drop_na(poc_0)
  ) %>% 
    select(-poc_flux) %>% 
    rename(pred_log_poc_flux = .pred) %>% 
    mutate(pred_poc_flux = 10^(pred_log_poc_flux), .after = pred_log_poc_flux) %>% 
    select(lon, lat, depth = depth_trap, pred_poc_flux)
  
  # Predict entire profiles
  flux_prof <- augment(
    final_res, 
    new_data = crossing(
      depth_trap = seq(50, 1000, by = 50),
      df_sub
    ) %>% 
      mutate(poc_flux = NA) %>% 
      select(lon, lat, poc_flux, depth_trap, everything())
  ) %>% 
    select(-poc_flux) %>% 
    rename(pred_log_poc_flux = .pred) %>% 
    mutate(pred_poc_flux = 10^(pred_log_poc_flux), .after = pred_log_poc_flux) %>% 
    select(lon, lat, depth = depth_trap, pred_poc_flux)
  
  ## Store results together
  new_preds <- flux_100 %>% mutate(type = "flux_100") %>% 
    bind_rows(flux_1000 %>% mutate(type = "flux_1000")) %>% 
    bind_rows(flux_prof %>% mutate(type = "flux_prof")) %>% 
    mutate(fold = as.character(i), .before = lon)
  
  ## Return results
  return(new_preds)
}) %>% 
  bind_rows()


## Compute mean and sd across folds ----
#--------------------------------------------------------------------------#
new_preds <- new_preds %>% 
  group_by(lon, lat, depth, type) %>% 
  summarise(
    avg_poc_flux = mean(pred_poc_flux, na.rm = TRUE),
    sd_poc_flux = sd(pred_poc_flux, na.rm = TRUE)
  ) %>% 
  ungroup()


## Save ----
#--------------------------------------------------------------------------#
save(new_preds, file = "data/03.new_preds.Rdata")




