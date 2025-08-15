## This script will explore algorithmically cleaning up the temperature data, and 
## potentially using data to impute gaps
##
## 2025-05-15
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Set up environment
source("scripts/0_setup.R")

p_load(devtools)
#devtools::install_github("mikejohnson51/climateR")
#devtools::install_github("mikejohnson51/AOI")

p_load(AOI, climateR, ggtext)


# 2. Read in data --------------------------------------------------------------

## Read in data
temp_raw <- read_csv("data/inputs/mcrl_data/MCRLdata_240501_250501_L0.csv") %>% 
  dplyr::select(time_pst, contains("airtemp"))


# 3. Initial plots -------------------------------------------------------------

## First, let's look at our time-series - there are no flags, and patterns are
## pretty consistent between years with no clear outliers
ggplot(temp_raw, aes(time_pst, airtemp_avg_deg_c)) + 
  geom_line(color = "gray") + 
  geom_point(data = temp_raw %>% filter(qc_airtemp_avg != 0), color = "red", alpha = 0.5)


# 4. Merge air temperature sources ---------------------------------------------

## Pull in data from an external source which we can use to validate our dataset
## and potentially also gapfill (eventually)
sequim_temps_raw <- aoi_ext("Sequim", units = "km", bbox = TRUE) %>% 
  getGridMET(AOI = .,
             varname   = c("tmmn", "tmmx"),
             startDate = "2021-05-01", 
             endDate = "2025-05-01") %>% 
  as_tibble()

kelvin_to_c = 273.15

sequim_temps <- sequim_temps_raw %>% 
  mutate(doy = yday(as_date(date))) %>% 
  group_by(doy) %>% 
  summarize(gridmet_temp_c_min = mean(tmmn - kelvin_to_c, na.rm = T), 
            gridmet_temp_c_max = mean(tmmx - kelvin_to_c, na.rm = T)) 

temp_binned <- temp_raw %>% 
  filter(qc_airtemp_avg == 0) %>% 
  mutate(doy = yday(as_date(time_pst))) %>% 
  ungroup() %>% 
  group_by(doy) %>% 
  summarize(airtemp_min = min(airtemp_avg_deg_c, na.rm = T), 
            airtemp_max = max(airtemp_avg_deg_c, na.rm = T)) %>% 
  select(doy, contains("airtemp")) %>% 
  full_join(sequim_temps, by = "doy") 

plot_grid(ggplot(temp_binned, aes(airtemp_min, gridmet_temp_c_min)) + 
            geom_point(color = "gray") + geom_abline(slope = 1, intercept = 0), 
          ggplot(temp_binned, aes(airtemp_max, gridmet_temp_c_max)) + 
            geom_point(color = "gray") + geom_abline(slope = 1, intercept = 0), 
          nrow = 1)

## This confirms that our data generally fall within the range expected based on
## gridded model data
ggplot() + 
  geom_line(data = temp_raw, aes(time_pst, airtemp_avg_deg_c), color = "gray") + 
  geom_point(data = sequim_temps_raw, aes(date, y = tmmn-273.15), alpha = 0.1, color = "red") +  
  geom_point(data = sequim_temps_raw, aes(date, y = tmmx-273.15), alpha = 0.1, color = "blue") 

temp_final <- temp_raw %>% 
  filter(qc_airtemp_avg == 0) %>% 
  select(-contains("qc_"))

write_csv(temp_final, "data/outputs/L1/250630_air_temp_L1.csv")


