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


## World map ----
#--------------------------------------------------------------------------#
# For plots
world <- fortify(map_data('world', wrap = c(-180, 180))) %>% rename(lon = long)

## Default values ----
#--------------------------------------------------------------------------#
# Number of cores for parallel computing
n_cores <- 8

# GGplot theme
theme_set(theme_minimal())

seed <- 12
