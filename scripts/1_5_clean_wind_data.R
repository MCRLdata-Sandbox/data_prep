## This script creates wind roses to understand wind directions and speeds
##
## 2025-05-15
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Set up environment
source("scripts/0_setup.R")
p_load(openair)


# 2. Read in data --------------------------------------------------------------

wind_raw <- read_csv("data/inputs/mcrl_data/MCRLdata_240501_250501_L0.csv") %>% 
  assign_season() %>% 
  dplyr::select(time_pst, contains("wind"))


# 3. Initial plots -------------------------------------------------------------

## Wind direction - nice and clean, with no clear flags
ggplot(wind_raw, aes(time_pst, winddir_deg_from)) + 
  geom_line(color = "gray") + 
  geom_point(data = wind_raw %>% filter(qc_winddir != 0), color = "red", alpha = 0.5)

## Windspeed - same
ggplot(wind_raw, aes(time_pst, windspeed_avg_m_s)) + 
  geom_line(color = "gray") + 
  geom_point(data = wind_raw %>% filter(qc_windspeed_avg != 0), color = "red", alpha = 0.5)

## Windspeed max - same
ggplot(wind_raw, aes(time_pst, windspeed_max_m_s)) + 
  geom_line(color = "gray") + 
  geom_point(data = wind_raw %>% filter(qc_windspeed_max != 0), color = "red", alpha = 0.5)


# 4. Write out -----------------------------------------------------------------

wind_final <- wind_raw %>% 
  dplyr::select(!contains("qc_")) %>% 
  drop_na()

write_csv(wind_final, "data/outputs/L1/250630_windspeed_L1.csv")




