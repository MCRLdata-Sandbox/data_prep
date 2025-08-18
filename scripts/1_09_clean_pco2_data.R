## This script cleans up pCO2 measurements to L1 
##
## 2025-07-28
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Set up environment
source("scripts/0_setup.R")
p_load(tictoc)


# 2. Read in data --------------------------------------------------------------

## Read in data
co2_raw <- read_csv("data/inputs/mcrl_data/MCRLdata_240501_250501_L0.csv") %>% 
  dplyr::select(time_pst, contains("pco2"), contains("p_co2")) %>% 
  group_by(time_pst) %>% 
  summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

co2_drop_na <- co2_raw %>% drop_na(p_co2_in_water_ppm)

n_flags <- nrow(co2_drop_na %>% drop_na(p_co2_in_water_ppm) %>% filter(qc_pco2_water != 0))

## No flagged points, and we don't necessarily have a reason to not believe any 
## of these values...
ggplot(co2_drop_na, aes(time_pst, p_co2_in_water_ppm)) + 
  geom_line(color = "gray") + 
  geom_point(data = co2_drop_na %>% filter(qc_pco2_water != 0), color = "red", alpha = 0.5) + 
  ggtitle(paste(n_flags, "flagged points"))

## No flags for atmospheric either... 
ggplot(co2_drop_na, aes(time_pst, p_co2_in_air_ppm)) + 
  geom_line(color = "gray") + 
  geom_point(data = co2_drop_na %>% filter(qc_pco2_air != 0), color = "red", alpha = 0.5) + 
  ggtitle(paste(n_flags, "flagged points"))


# 3. Write out -----------------------------------------------------------------

write_csv(co2_drop_na, "data/outputs/L1/250815_pco2_L1.csv")



