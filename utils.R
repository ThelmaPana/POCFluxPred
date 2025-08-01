## Loading packages ----
#--------------------------------------------------------------------------#

library(tidyverse)
library(tidymodels)
library(castr)
library(lubridate)
library(ncdf4)
library(parallel)
library(pbmcapply)
library(cmocean)
library(Hmisc)
library(abind)
library(ggtext)
library(terra)
library(furrr)
library(future.apply)
library(future)
library(vegan)
library(ggrepel)
library(corrplot)
library(sf)
library(spatialsample)
library(fields)
library(chroma)
library(R.matlab)
library(fs)
library(yaml)
library(glue)
library(knitr)
library(optparse)

## Settings ----
#--------------------------------------------------------------------------#
# Number of cores for parallel computing
n_cores <- 8

seed <- 12

# Whether to download WOA data
download_woa <- TRUE
# Location of WOA data
woa_dir <- "/Users/thepan/Documents/Data/OCExport/woa" # where it is stored
woa_loc <- "data/raw/woa" # where to write simlinks within the project

# Max depth to compute clines from WOA
max_depth_woa <- 500 # compromise between coverage and computation cost

# Max depth of the layers we consider for predictors
surface_bottom <- 10
meso_top <- 200
meso_bottom <- 1000
# bathy is anything below meso

## World map ----
#--------------------------------------------------------------------------#
# For plots
world <- fortify(map_data('world', wrap = c(-180, 180))) %>% rename(lon = long)

# To compute inland points
coast <- read_csv(here::here("data/raw/gshhg_world_c.csv"), show_col_types = FALSE)

## Plots ----
#--------------------------------------------------------------------------#

# ggplot theme
theme_set(theme_minimal())


## Colour palettes ----
#--------------------------------------------------------------------------#

## Colour palettes for image.plot
col_temp   <- cmocean("thermal")(100)
col_sal    <- cmocean("haline")(100)
col_dens   <- cmocean("dense")(100)
col_oxy    <- brewer_colors(100, "Blues")
col_aou    <- brewer_colors(100, "Purples")
col_nit    <- cmocean("tempo")(100)
col_phos   <- brewer_colors(100, "BuPu")
col_sil    <- brewer_colors(100, "PuBu")
col_chl    <- cmocean("algae")(100)
col_irr    <- cmocean("solar")(100)
col_depth  <- cmocean("deep")(100)
col_alk    <- brewer_colors(100, "RdPu")
col_bbp    <- cmocean("turbid")(100)
col_npp    <- brewer_colors(100, "YlGn")
col_psd    <- brewer_colors(100, "Greens")
col_fmicro <- brewer_colors(100, "Greys")
col_dic    <- brewer_colors(100, "YlOrBr")
col_pic    <- brewer_colors(100, "OrRd")
col_poc    <- cmocean("matter")(100)
col_misc   <- viridis_colors(100)


## Colour palettes for ggplot
div_pal <- scale_colour_gradient2(low = "#4575b4", mid = "#ffffbf", high = "#d73027") # diverging palette centered at 0


## Compute percentage of NA in a vector ----
#--------------------------------------------------------------------------#
#' Compute percentage of NA in a vector
#' @param x input vector
#' @return number in [0,1] = the percentage of NA; returns 1 is x is empty.
percent_na <- function(x) {
  if (length(x) == 0) {
    res <- 1
  } else {
    res <- sum(is.na(x)) / length(x)
  }
  return(res)
}


## Read mat files from Cael ----
#--------------------------------------------------------------------------#
#' Read mat file and extract variable of interest from Cael climatology
#'
#' @param file path to .mat file
#' @param var name of variable to extract
#' @param yearly whether to compute yearly average, if `FALSE` monthly data is returned
#'
#' @return an array
#'
#' @examples read_clim(file = "data/raw/clim4cael.mat", var = "cafes")
read_clim <- function(file, var, yearly = TRUE){
  # Read file
  clim <- readMat(file)
  
  # Extract variable of interest
  data <- clim[[var]]
  
  # Swap lon and lat dimensions
  data <- aperm(data, c(2,1,3))
  # Reorder lat
  data <- data[, ncol(data):1,]
  
  # Compute yearly from monthly
  if (yearly){
    data <- apply(data, c(1,2), mean, na.rm = TRUE)
  }
  return(data)
}

## Plot a nice ggplot map ----
#--------------------------------------------------------------------------#
#' Plot a map, whether raster or points
#'
#' Generate a ggplot raster map from a dataframe and a variable name.
#' Palette can be inferred from the name of the variable or provided in the arguments.
#' If no palette is found, it defaults to viridis.
#' Land is plotted by default but this can be changed in arguments.
#'
#' @param df a dataframe, must contain at least 3 columns: `lon`, `lat` and var to plot
#' @param var a character, name of variable to plot
#' @param type a character, defining type of map (raster or points)
#' @param land a boolean, whether to plot land or not
#' @param palette a filling palette, will be generated if none is
#'
#' @return a ggplot object
#'
#' @examples ggmap(df, var = "temperature", type = "raster")
ggmap <- function(df, var, type = c("raster", "point"), land = TRUE, palette = NULL) {
  ## Check args
  # df is a dataframe containing "lon", "lat" and var
  checkmate::assert_data_frame(df)
  checkmate::assert_names(names(df), must.include = c("lon", "lat", var))
  # var is a character
  checkmate::assert_character(var)
  # type is either "raster" or "point"
  checkmate::assert_choice(type, c("raster", "point"))
  # land is a boolean
  checkmate::assert_logical(land)
  # palette is a palette or NULL
  checkmate::assert_multi_class(palette, c("ScaleContinuous", "Scale", "NULL"))
  
  ## Palettes
  # To look for palette, remove "_mean" in case we are working with annual climatology values
  var_pal <- str_remove(var, "_mean")
  # Remove also anything after "."
  var_pal <- str_split(var_pal, "\\.")[[1]][1]
  
  # If no palette is supplied, generate one
  if(is.null(palette)){
    
    # List of palettes for common variables
    pals <- tribble(
      ~vars, ~raster, ~point,
      "temperature", scale_fill_cmocean(name = "thermal", na.value = NA),                    scale_colour_cmocean(name = "thermal", na.value = NA),
      "salinity",    scale_fill_cmocean(name = "haline", na.value = NA),                     scale_colour_cmocean(name = "haline", na.value = NA),
      "density",     scale_fill_cmocean(name = "dense", na.value = NA),                      scale_colour_cmocean(name = "dense", na.value = NA),
      "oxygen",      scale_fill_distiller(palette = "Blues", na.value = NA, direction = 1),  scale_colour_distiller(palette = "Blues", na.value = NA, direction = 1),
      "nitrate",     scale_fill_cmocean(name = "tempo", na.value = NA),                      scale_colour_cmocean(name = "tempo", na.value = NA),
      "phosphate",   scale_fill_distiller(palette = "BuPu", na.value = NA, direction = 1),   scale_colour_distiller(palette = "BuPu", na.value = NA, direction = 1),
      "silicate",    scale_fill_distiller(palette = "PuBu", na.value = NA, direction = 1),   scale_colour_distiller(palette = "PuBu", na.value = NA, direction = 1),
      "chl",         scale_fill_cmocean(name = "algae", na.value = NA),                      scale_colour_cmocean(name = "algae", na.value = NA),
      "par",         scale_fill_cmocean(name = "solar", na.value = NA),                      scale_colour_cmocean(name = "solar", na.value = NA),
      "mld",         scale_fill_cmocean(name = "deep", na.value = NA),                       scale_colour_cmocean(name = "deep", na.value = NA),
      "mld_argo",    scale_fill_cmocean(name = "deep", na.value = NA),                       scale_colour_cmocean(name = "deep", na.value = NA),
      "thermo",      scale_fill_cmocean(name = "deep", na.value = NA),                       scale_colour_cmocean(name = "deep", na.value = NA),
      "pycno",       scale_fill_cmocean(name = "deep", na.value = NA),                       scale_colour_cmocean(name = "deep", na.value = NA),
      "z_eu",        scale_fill_cmocean(name = "deep", na.value = NA),                       scale_colour_cmocean(name = "deep", na.value = NA),
      "s_cline",     scale_fill_cmocean(name = "deep", na.value = NA),                       scale_colour_cmocean(name = "deep", na.value = NA),
      "p_cline",     scale_fill_cmocean(name = "deep", na.value = NA),                       scale_colour_cmocean(name = "deep", na.value = NA),
      "n_cline",     scale_fill_cmocean(name = "deep", na.value = NA),                       scale_colour_cmocean(name = "deep", na.value = NA),
      "alkalinity",  scale_fill_distiller(palette = "PuRd", na.value = NA, direction = 1),   scale_colour_distiller(palette = "PuRd", na.value = NA, direction = 1),
      "bbp",         scale_fill_cmocean(name = "turbid", na.value = NA),                     scale_colour_cmocean(name = "turbid", na.value = NA),
      "npp",         scale_fill_distiller(palette = "YlGn", na.value = NA, direction = 1),   scale_colour_distiller(palette = "YlGn", na.value = NA, direction = 1),
      "psd",         scale_fill_distiller(palette = "Greens", na.value = NA, direction = 1), scale_colour_distiller(palette = "Greens", na.value = NA, direction = 1),
      "fmicro",      scale_fill_distiller(palette = "Greys", na.value = NA, direction = 1),  scale_colour_distiller(palette = "Greys", na.value = NA, direction = 1),
      "dic",         scale_fill_distiller(palette = "YlOrBr", na.value = NA, direction = 1), scale_colour_distiller(palette = "YlOrBr", na.value = NA, direction = 1),
      "pic",         scale_fill_distiller(palette = "OrRd", na.value = NA, direction = 1),   scale_colour_distiller(palette = "OrRd", na.value = NA, direction = 1),
      "poc",         scale_fill_cmocean(name = "matter", na.value = NA),                     scale_colour_cmocean(name = "matter", na.value = NA)
    )
    
    # Find the matching palette for variable to plot
    pal <- pals %>% filter(vars == var_pal) %>% pull(all_of(type))
    
    # If no palette is found, use viridis
    if (length(pal) == 0 & type == "raster"){
      pal <- scale_fill_viridis_c(na.value = NA)
    } else if (length(pal) == 0 & type == "point"){
      pal <- scale_colour_viridis_c(na.value = NA)
    }
    
  } else { # If palette is supplied, use it!
    pal <- palette
  }
  
  ## Plot
  # Base plot
  p <- ggplot(df) +
    coord_quickmap(expand = FALSE) +
    theme_minimal()
  
  # add raster or point layer
  if (type == "raster"){
    p <- p + geom_raster(aes(x = lon, y = lat, fill = .data[[var]]), na.rm = TRUE)
  } else {
    p <- p + geom_point(aes(x = lon, y = lat, colour = .data[[var]]), na.rm = TRUE, size = 0.5)
  }
  
  # add land if required
  if (land){p <- p + geom_polygon(data = world, aes(x = lon, y = lat, group = group), fill = "gray")}
  
  # add palette
  p <- p + pal
  
  ## Return plot
  return(p)
}
