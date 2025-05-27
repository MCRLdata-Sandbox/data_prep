## This script will explore algorithmically cleaning up the salinity data, and 
## potentially using data to impute gaps
##
## 2025-05-23
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Set up environment
source("scripts/0_setup.R")


# 2. Read in data --------------------------------------------------------------

## Read in data
sal_raw <- read_csv("data/inputs/mcrl_data/MCRLdata_240501_250501_L0.csv") %>% 
  dplyr::select(time_pst, contains("salinity"))
