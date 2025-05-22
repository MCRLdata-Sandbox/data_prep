## This script will rely on algorithmic QC, either from flagging from the MCRLdata
## system or from turnkey R solutions like tsrobprep. There may be some light
## additional QC after, depending how the data look
##
## 2025-05-15
## Peter Regier
##
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## Set up environment
source("scripts/0_setup.R")


# 2. Read in data --------------------------------------------------------------

df_raw <- read_csv("data/mcrl_data/MCRLdata_240501_250501_L0.csv")


# 3. tidegauge water level QC --------------------------------------------------

tide <- df_raw %>% 
  dplyr::select(time_pst, contains("water_level")) %>% 
  drop_na() 

## Raw data - clearly some issues
ggplot(tide, aes(time_pst, water_level_m_navd88)) + 
  geom_line(color = "gray") +
  geom_point(data = tide %>% filter(qc_water_level != 0), color = "red", alpha = 0.1)

## Let's remove flagged data and replot
tide %>% 
  filter(qc_water_level == 0) %>% 
  ggplot(aes(time_pst, water_level_m_navd88)) + 
  geom_line(color = "gray")

## There are still issues, particularly late 2021/early 2022/late 2024
## Let's try out a new timeseries QC package and see if it can ID these issues
p_load(tsrobprep) # potential

x <- tide %>% 
  filter(qc_water_level == 0) 

wl_outliers <- detect_outliers(x$water_level_m_navd88, S = c(48, 96))


  






