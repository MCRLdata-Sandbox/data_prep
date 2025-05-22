## This script provides general setup for all other scripts, including loading
## a set of general-use packages, establishing functions used in multiple scripts
## and other general formatting things

## Clean your environment
rm(list = ls())

## Load packages
## If you've never installed the pacman package, uncomment the line below and run once
## install.packages("pacman")
require(pacman)
p_load(tidyverse, # keep your data workflows tidy
       ncdf4, # interact with netCDF files
       parsedate, # robust handling of datetimes
       cowplot, # arrange multiple plots
       janitor,  # clean_names()
       hms) # as_hms()

## Set ggplot theme
theme_set(theme_bw())


# 2. Functions -----------------------------------------------------------------

assign_season <- function(data){
    data %>% 
      mutate(date = as_date(time_pst)) %>% 
      mutate(month = month(date, label = T), 
             month_num = month(date, label = F)) %>% 
      mutate(season = case_when(month_num %in% c(3, 4, 5) ~ "1. Spring", 
                                month_num %in% c(6, 7, 8) ~ "2. Summer", 
                                month_num %in% c(9, 10, 11) ~ "3. Fall", 
                                month_num %in% c(12, 1, 2) ~ "4. Winter"))
}





