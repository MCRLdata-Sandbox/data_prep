## This script creates interannual seawater chemistry envelopes for relevant 
## parameters in order to provide simple baseline information to seawater
## users at MCRL
##
## Peter Regier
## 2025-04-25

# 1. Setup ---------------------------------------------------------------------

## Load packages using pacman
require(pacman) ## If you have not installed pacman, run install.packages("pacman")
p_load(tidyverse, # tidy workflows
       readxl, # read xlsx files
       cowplot, #combine plots
       rtide,
       HL, #extract high and low tides
       oce, #despiking
       janitor) #clean column names

## Set ggplot theme
theme_set(theme_bw())


# 2. Read in data --------------------------------------------------------------

## 1. Read in tidal depth
depth_raw <- readxl::read_xlsx("data/mcrl_data/MCRLdata-All-20240901-20250218.xlsx", 
                               sheet = "Tide Gauge") %>% 
  clean_names() %>%  #make column names easier to read
  filter(qc_water_level == 0) %>% 
  mutate(time_utc = round_date(time_utc, "5 minutes"))

ggplot(depth_raw, aes(time_utc, water_level_m_navd88)) + geom_line()

## There are still errors around November. For now, let's scrub them
depth_qc <- depth_raw %>% 
  filter(time_utc < "2024-10-25" | 
           time_utc > "2024-11-06")

ggplot(depth_qc, aes(time_utc, water_level_m_navd88)) + geom_line()


## 2. Read in CTD data
ctd_raw <- readxl::read_xlsx("data/mcrl_data/MCRLdata-All-20240901-20250218.xlsx", 
                             sheet = "CTD") %>% 
  clean_names() %>%  #make column names easier to read
mutate(time_utc = round_date(time_utc, "5 minutes"))

## View data
ctd_raw

## Look at temperature
ggplot(ctd_raw, aes(time_utc, salinity_ppt)) + geom_line()

## We need to get rid of errors, starting with using QC flags
unique(ctd_raw$qc_salinity)

## Let's trim by all QC flags
ctd_qc <- ctd_raw %>% 
  filter(qc_temp == 0 & 
         qc_do == 0 & 
         qc_salinity == 0) %>% 
  mutate(sal_despike = despike(salinity_ppt), 
         temp_despike = temp_deg_c) 

## Look at data again
ctd_qc %>% 
  ggplot(aes(time_utc)) + 
  geom_line(aes(y = salinity_ppt), color = "gray") + 
  geom_line(aes(y = sal_despike, color = "blue"))

df <- inner_join(depth_qc, ctd_qc, by = "time_utc") %>% 
  dplyr::select(-contains("_qc")) %>% 
  mutate(month = month(time_utc))
  
plot_grid(ggplot(df, aes(time_utc, water_level_m_navd88)) + geom_line(), 
          ggplot(df, aes(time_utc, salinity_ppt)) + geom_line(),
          ncol = 1)

rtide_heights <- rtide::tide_height('Port Angeles', 
                        from = as.Date('2024-09-10'), 
                        to = as.Date('2025-01-01'), 
                        minutes = 10, tz ='UTC') %>% 
  clean_names() %>% 
  rename("time_utc" = "date_time", 
         "rtide_height_m" = tide_height)

df2 <- df %>% 
  inner_join(rtide_heights, by = "time_utc") %>% 
  mutate(rtide_lag = lag(rtide_height_m, 3)) %>% 
  drop_na()

df2 %>% 
  ggplot(aes(water_level_m_navd88, rtide_lag)) + 
  geom_point(alpha = 0.5, color = "gray") + 
  geom_abline(intercept = 0, slope = 1)

ccf(df2$water_level_m_navd88, df2$rtide_lag)






