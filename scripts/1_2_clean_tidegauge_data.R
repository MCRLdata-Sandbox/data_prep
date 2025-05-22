## This script will explore algorithmically cleaning up the tidal data, and 
## potentially using nearby tidal data to impute gaps
##
## 2025-05-15
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Set up environment
source("scripts/0_setup.R")
p_load(rtide, tictoc, VulnToolkit, plotly)

# 2. Read in data --------------------------------------------------------------

## Read in data
tide_raw <- read_csv("data/inputs/mcrl_data/MCRLdata_240501_250501_L0.csv") %>% 
  dplyr::select(time_pst, contains("water_level"))


## Plot the raw data
ggplot(tide_raw, aes(time_pst, water_level_m_navd88)) + 
  geom_line(color = "gray") +
  geom_point(data = tide_raw %>% filter(qc_water_level != 0), color = "red", alpha = 0.1)


# 3. Clean flags ---------------------------------------------------------------

## Clean data based on existing flags
tide_clean_flags <- tide_raw %>% 
  filter(qc_water_level == 0)

## 
ggplot(tide_clean_flags, aes(time_pst, water_level_m_navd88)) + 
  geom_line(color = "gray")


# 4. Read in nearby tidal data -------------------------------------------------

# tic("read in PA tides") #1143s (19 minutes!)
# rtide_raw <- rtide::tide_height('Port Angeles', 
#                                     from = as.Date('2021-05-01'), 
#                                     to = as.Date('2025-05-01'), 
#                                     minutes = 5, tz ='Etc/GMT+8') %>% 
#   clean_names() %>% 
#   rename("time_utc" = "date_time", 
#          "rtide_height_m" = tide_height)
# toc()


# 5. Compare tides -------------------------------------------------------------

# rtide <- rtide_raw %>% 
#   dplyr::select(-station) %>% 
#   rename("time_pst" = time_utc) # Labeled UTC, but tz set above
#write_csv(rtide, "data/mcrl_data/250515_rtide_port_angeles.csv")
rtide <- read_csv("data/inputs/mcrl_data/250515_rtide_port_angeles.csv")

ccf_check <- inner_join(rtide, tide_clean_flags) 

## There's a three-way tie for 8, 9, and 10, so we'll pick 9
ccf(ccf_check$water_level_m_navd88, ccf_check$rtide_height_m)

comparison <- full_join(rtide, tide_clean_flags) %>% 
  mutate(rtide_lagged = lag(rtide_height_m, 9)) %>% 
  mutate(residual = rtide_lagged - water_level_m_navd88)

## Check that lagging worked correctly
plot_grid(ggplot(comparison, aes(water_level_m_navd88, rtide_height_m)) + 
            geom_point(alpha = 0.2) + ggtitle("no lag"), 
          ggplot(comparison, aes(water_level_m_navd88, rtide_lagged)) + 
            geom_point(alpha = 0.2) + ggtitle("lag"), 
          nrow = 1)

plot_grid(ggplot(comparison, aes(x = time_pst, water_level_m_navd88)) + 
            geom_line(color = "gray"), 
          ggplot(comparison %>% filter(abs(residual) < 1), aes(x = time_pst, water_level_m_navd88)) + 
            geom_line(color = "blue"), 
          ncol = 1)

# 6. Remove initial data which still look wonky and write out as L1 ------------

tide_clean <- comparison %>% 
  filter(abs(residual) < 1) %>% 
  dplyr::select(time_pst, water_level_m_navd88) %>% 
  filter(time_pst > "2021-09-01")

ggplot(tide_clean, aes(time_pst, water_level_m_navd88)) + 
  geom_line(alpha = 0.5)

high_and_low_tides <- HL(tide_clean$water_level_m_navd88, 
   tide_clean$time_pst) %>% 
  as_tibble() %>% 
  rename("time_pst" = time, 
         "water_level_m_navd88" = level)

tide_final <- left_join(tide_clean, high_and_low_tides,
                        by = c("time_pst", "water_level_m_navd88"))

p1 <- ggplot(tide_final, aes(time_pst, water_level_m_navd88)) + 
  geom_line() + 
  geom_point(data = tide_final %>% filter(tide == "H"), color = "red")

ggplotly(p1)

write_csv(tide_final, "data/ready_to_use/L1/250515_tidegauge_L1.csv")

