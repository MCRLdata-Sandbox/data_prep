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

df <- read_csv("data/mcrl_data/MCRLdata_240501_250501_L0.csv") %>% 
  assign_season()


# 2. Create plots --------------------------------------------------------------

df %>% 
  filter(season == "1. Spring")


create_wind_rose <- function(data, selected_season, title){
  data %>% 
    filter(season == selected_season) %>% 
    windRose(., ws = "windspeed_avg_m_s", wd = "winddir_deg_from", key.position = "bottom", main = title)
}

p0 <- windRose(df, ws = "windspeed_avg_m_s", wd = "winddir_deg_from", key.position = "bottom", main = "Annual")
p1 <- create_wind_rose(df, "1. Spring", "Spring (Mar-May)")
p2 <- create_wind_rose(df, "2. Summer", "Summer (Jun-Aug)")
p3 <- create_wind_rose(df, "3. Fall", "Fall (Sep-Nov)")
p4 <- create_wind_rose(df, "4. Winter", "Winter (Dec-Feb)")

save_windrose_png <- function(plot_name, name){
  png(filename = paste0("figures/wind_plots/", name, ".png"))
  plot(plot_name)
  dev.off()
}

save_windrose_png(p0, "annual")
save_windrose_png(p1, "spring")
save_windrose_png(p2, "summer")
save_windrose_png(p3, "fall")
save_windrose_png(p4, "winter")





