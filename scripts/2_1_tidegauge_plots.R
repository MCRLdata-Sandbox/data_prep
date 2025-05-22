## This script explores tidal, diurnal, seasonal, and annual patterns in water 
## level data from the cleaned-up tidegauge dataset
##
## 2025-05-15
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Set up environment
source("scripts/0_setup.R")

df <- read_csv("data/ready_to_use/L1/250515_tidegauge_L1.csv") %>% 
  assign_season()


# 2. Basic plot - tidal variation by time of day and month ---------------------

p1 <- df %>% 
  mutate(tod = hour(as_hms(time_pst))) %>% 
  group_by(date, tod) %>% 
  summarize(season = first(season), 
            month = first(month),
            water_level_m_navd88 = mean(water_level_m_navd88)) %>% 
  mutate(year = year(date)) %>% 
  ggplot(aes(tod, water_level_m_navd88)) + 
  geom_line(aes(group = date, color = as.factor(year)), alpha = 0.4) + 
  geom_vline(xintercept = 12, linetype = "dashed") + 
  geom_smooth(color = "black") + 
  facet_wrap(~month) + 
  scale_color_viridis_d() + 
  labs(x = "Hour of day", y = "Water level (m NAVD88)", color = "Year")
#ggsave("figures/tidal_depth_by_month_and_tod.png", width = 12, height = 8)


# 3. A more complicated version of that plot -----------------------------------

# Calculate smoothed values and find min/max hour per month
extreme_df <- df %>% 
  mutate(tod = hour(as_hms(time_pst))) %>%
  group_by(date, tod) %>% 
  summarize(season = first(season), 
            month = first(month),
            water_level_m_navd88 = mean(water_level_m_navd88)) %>% 
  mutate(year = year(date)) %>% 
  ungroup() %>%
  group_by(month, tod) %>%
  summarize(smoothed_value = mean(water_level_m_navd88)) %>%
  group_by(month) %>%
  summarize(min_tod = tod[which.min(smoothed_value)],
            min_value = min(smoothed_value),
            max_tod = tod[which.max(smoothed_value)],
            max_value = max(smoothed_value)) %>%
  ungroup()

# Plot with geom_point at min/max
df %>% 
  mutate(tod = hour(as_hms(time_pst))) %>%
  group_by(date, tod) %>% 
  summarize(season = first(season), 
            month = first(month),
            water_level_m_navd88 = mean(water_level_m_navd88)) %>% 
  mutate(year = year(date)) %>% 
  ggplot(aes(tod, water_level_m_navd88)) + 
  #geom_line(aes(group = date, color = as.factor(year)), alpha = 0.4) + 
  geom_line(aes(group = date), color = "gray", alpha = 0.4) + 
  geom_smooth(color = "black") + 
  facet_wrap(~month) + 
  #scale_color_viridis_d() + 
  labs(x = "Hour of day", y = "Water level (m NAVD88)", color = "Year") +
  geom_point(data = extreme_df, aes(x = max_tod, y = max_value), color = "white", size = 3) +
  geom_point(data = extreme_df, aes(x = min_tod, y = min_value), color = "white", size = 3) +
  geom_point(data = extreme_df, aes(x = max_tod, y = max_value), color = "red", size = 2.5) +
  geom_point(data = extreme_df, aes(x = min_tod, y = min_value), color = "blue", size = 2.5)
ggsave("figures/tidal_depth_by_month_and_tod.png", width = 10, height = 7)


## Make a plot that shows two weeks or a month of really nice tidal data
start_date = as_date("2024-03-20")

x <- df %>% 
  filter(date > start_date & 
           date < start_date + days(30)) 

ggplot(x, aes(time_pst, y = water_level_m_navd88)) + 
  geom_line(color = "blue") + 
  labs(x = "", y = "Water Level (m NAVD88)")
ggsave("figures/250515_tide_example.png", width = 6, height = 4)



## Make some summary stats that present average HH, average LL, and seasonal ranges


