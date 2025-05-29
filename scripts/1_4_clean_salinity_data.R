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
require(oce) # Package for despiking


# 2. Read in data --------------------------------------------------------------

## Read in data
sal_raw <- read_csv("data/inputs/mcrl_data/MCRLdata_240501_250501_L0.csv") %>% 
  dplyr::select(time_pst, contains("salinity"))


# 3. Initial plots -------------------------------------------------------------

## First, let's look at our time-series
ggplot(sal_raw, aes(time_pst, salinity_ppt)) + geom_line()

## Lots of errors, let's remove those
ggplot(sal_raw %>% filter(qc_salinity == 0), aes(time_pst, salinity_ppt)) + 
  geom_line()


# 4. Clean up salinity ---------------------------------------------------------

sal_despike <- sal_raw %>% 
  filter(qc_salinity == 0) %>% 
  mutate(sal_ppt_despike1 = despike(salinity_ppt))

# ggplot(sal_despike, aes(sal_ppt_despike1, sal_ppt_despike2)) + 
#   geom_point()
# 
# plot_grid(#ggplot(sal_despike, aes(time_pst, salinity_ppt)) + geom_line(),
#   ggplot(sal_despike, aes(time_pst, sal_ppt_despike1)) + geom_line(), 
#   ggplot(sal_despike, aes(time_pst, sal_ppt_despike2)) + geom_line(), 
#   ncol = 1)
       
          
# 5. Write out -----------------------------------------------------------------

