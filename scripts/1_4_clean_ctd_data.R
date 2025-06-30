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
ctd_raw <- read_csv("data/inputs/mcrl_data/MCRLdata_240501_250501_L0.csv") %>% 
  dplyr::select(time_pst, contains("salinity"), contains("temp")) %>% 
  dplyr::select(-contains("airtemp")) %>% 
  group_by(time_pst) %>% 
  summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))


### Salinity ###

# 3. Initial salinity plots ----------------------------------------------------

## First, let's look at our time-series
ggplot(ctd_raw, aes(time_pst, salinity_ppt)) + geom_line()

## Lots of errors, let's remove those
ggplot(ctd_raw %>% filter(qc_salinity == 0), aes(time_pst, salinity_ppt)) + 
  geom_line()


# 4. Clean up salinity ---------------------------------------------------------

sal_despike <- ctd_raw %>% 
  filter(qc_salinity == 0) %>% 
  filter(salinity_ppt > 29) %>% 
  mutate(sal_ppt_despike = despike(salinity_ppt)) 

## It's not beautiful, but good enough for pulling summary statistics I think
ggplot(sal_despike, aes(time_pst)) + 
  geom_line(aes(y = salinity_ppt), color = "gray") + 
  geom_line(aes(y = sal_ppt_despike), color = "blue")
    


### Water Temp ###
## This is going to be a problem child...

# 5. Initial temperature plots -------------------------------------------------

## First, let's look at our time-series
ggplot(ctd_raw, aes(time_pst, temp_deg_c)) + 
  geom_line(color = "gray") + 
  geom_point(data = ctd_raw %>% filter(qc_temp != 0), color = "red", alpha = 0.5)

## Lots of errors, let's remove those
ggplot(ctd_raw %>% filter(qc_temp == 0), aes(time_pst, temp_deg_c)) + 
  geom_line()

# 6. Clean up temperatuer ------------------------------------------------------

## Although I don't like doing this, on a small project with many different things
## to get to, I'm going to ignore temperature data once it starts getting wonky
## since the goal currently is to calculate statistics
temp_clean <- ctd_raw %>% 
  filter(qc_temp == 0) %>% 
  filter(temp_deg_c > 5) %>% ## removes 1 point in early 2024
  filter(temp_deg_c < 20) %>% ## removes 1 point in fall 2022
  filter(time_pst < "2024-05-01")

## That looks better
ggplot(temp_clean, aes(time_pst, temp_deg_c)) + 
  geom_line(color = "gray") 

# 7. Bind data -----------------------------------------------------------------

ctd_clean <- full_join(temp_clean %>% dplyr::select(time_pst, temp_deg_c), 
                       sal_despike %>% 
                         rename("salinity_psu_clean" = sal_ppt_despike) %>% 
                         dplyr::select(time_pst, salinity_psu_clean), 
                       by = "time_pst")


# 8. Write out -----------------------------------------------------------------

write_csv(ctd_clean, "data/outputs/L1/250630_ctd_water_temp_salinity_L1.csv")



