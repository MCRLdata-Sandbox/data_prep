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
  dplyr::select(time_pst, contains("temp"))


# 3. Initial plots -------------------------------------------------------------

## First, let's look at our time-series
plot_grid(ggplot(temp_raw, aes(time_pst, temp_deg_c)) + geom_line(), 
          ggplot(temp_raw, aes(time_pst, airtemp_avg_deg_c)) + geom_line(), 
          ncol = 1)

## Air temperature looks pretty good, water temperature is... really bad.
plot_grid(ggplot(temp_raw %>% filter(qc_temp == 0), aes(time_pst, temp_deg_c)) + geom_line(), 
          ggplot(temp_raw %>% filter(qc_airtemp_avg == 0), aes(time_pst, airtemp_avg_deg_c)) + geom_line(), 
          ncol = 1)

temp_raw %>% 
  filter(qc_temp == 0) %>% 
  filter(qc_airtemp_avg == 0) %>% 
  ggplot(aes(airtemp_avg_deg_c, temp_deg_c)) + 
  geom_point()

temp_raw %>% 
  filter(qc_airtemp_avg == 0) %>% 
  ggplot(aes(time_pst, airtemp_avg_deg_c)) + 
  geom_line()


temp_raw %>% 
  filter(qc_temp == 0) %>% 
  filter(as_date(time_pst) == "2025-01-04") %>% 
  ggplot(aes(time_pst, temp_deg_c)) + 
  geom_line()


# 4. Clean up air temperatures -------------------------------------------------

## I don't think I want to tackle water temps right now, they appear to be the
## messiest dataset, and there's not a simple way to clean that I currently see.
## I wonder if we are also getting temp from other sensors.... 
sequim_temps_raw <- aoi_ext("Sequim", units = "km", bbox = TRUE) %>% 
  getGridMET(AOI = .,
             varname   = c("tmmn", "tmmx"),
             startDate = "2021-05-01", 
             endDate = "2025-05-01") %>% 
  as_tibble()

sequim_temps <- sequim_temps_raw %>% 
  mutate(doy = yday(as_date(date))) %>% 
  group_by(doy) %>% 
  summarize(gridmet_temp_c_min = mean(tmmn-273.15, na.rm = T), 
            gridmet_temp_c_max = mean(tmmx-273.15, na.rm = T)) 

air_temp_raw <- temp_raw %>% 
  filter(qc_airtemp_avg == 0) %>% 
  mutate(doy = yday(as_date(time_pst))) %>% 
  ungroup() %>% 
  group_by(doy) %>% 
  summarize(airtemp_min = min(airtemp_avg_deg_c, na.rm = T), 
            airtemp_max = max(airtemp_avg_deg_c, na.rm = T)) %>% 
  select(doy, contains("airtemp")) %>% 
  full_join(sequim_temps, by = "doy") 

plot_grid(ggplot(air_temp_raw, aes(airtemp_min, gridmet_temp_c_min)) + 
            geom_point(color = "gray") + geom_abline(slope = 1, intercept = 0), 
          ggplot(air_temp_raw, aes(airtemp_max, gridmet_temp_c_max)) + 
            geom_point(color = "gray") + geom_abline(slope = 1, intercept = 0), 
          nrow = 1)


air_temp_avgs <- air_temp_raw %>% 
  mutate(airtemp_mean = (airtemp_max + airtemp_min)/2, 
         gridmet_temp_c_mean = (gridmet_temp_c_max + gridmet_temp_c_min)/2)


p1 <- ggplot(data = air_temp_avgs, aes(x = doy)) + 
  geom_ribbon(aes(ymin = gridmet_temp_c_min, ymax = gridmet_temp_c_max), alpha = 0.2, fill = "red") + 
  geom_smooth(aes(y = gridmet_temp_c_mean), se = F, color = "red") + 
  geom_ribbon(aes(ymin = airtemp_min, ymax = airtemp_max), alpha = 0.2, fill = "blue") + 
  geom_smooth(aes(y = airtemp_mean), se = F, color = "blue") + 
  labs(x = "Day of Year", 
       y = "Air temperature (C)", 
       title = "<span style='color:black;'>Seasonal air temperatures for </span><span style='color:blue;'>MRCL</span> and <span style='color:red;'>GridMET</span>") +
  theme(plot.title = element_textbox_simple(size = 12, face = "bold", margin = margin(10, 0, 10, 0)))


p2 <- temp_raw %>% 
  filter(qc_airtemp_avg == 0) %>% 
  assign_season() %>% 
  mutate(tod = hour(as_hms(time_pst - hours(8)))) %>% 
  group_by(season, tod) %>% 
  summarize(mean = mean(airtemp_avg_deg_c, na.rm = T)) %>% 
  ggplot(aes(tod, mean, color = season)) + 
  geom_line(lwd = 3, alpha = 0.3) + 
  geom_line(lwd = 1) + 
  scale_color_viridis_d() + 
  labs(x = "Hour of Day", y = "Average Air temperature (C)", title = "Diurnal air temperatures by season") + 
  theme(plot.title = element_textbox_simple(size = 12, face = "bold", margin = margin(10, 0, 10, 0)))


plot_grid(p1, p2, nrow = 1)
ggsave("figures/air_temperature.png", width = 11, height = 4)

## Stats for seasonality
temp_raw %>% 
  filter(qc_airtemp_avg == 0) %>% 
  assign_season() %>%
  group_by(date) %>% 
  mutate(min = min(airtemp_avg_deg_c), 
         max = max(airtemp_avg_deg_c), 
         range = max - min) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  summarize(mean(range))
  
temp_raw %>% 
  filter(qc_airtemp_avg == 0) %>% 
  assign_season() %>% 
  mutate(tod = hour(as_hms(time_pst - hours(8)))) %>% 
  group_by(season, tod) %>% 
  summarize(mean = mean(airtemp_avg_deg_c, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  summarize(tod[which.min(mean)])

