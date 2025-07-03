## This script cleans up water velocities measured by the ADCP to L1 
##
## 2025-07-03
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Set up environment
source("scripts/0_setup.R")
p_load(changepoint, 
       plotly)


# 2. Read in data --------------------------------------------------------------

## Read in data
adcp_raw <- read_csv("data/inputs/mcrl_data/MCRLdata_240501_250501_L0.csv") %>% 
  dplyr::select(time_pst, max_velocity_m_s, maxu_qc) %>% 
  group_by(time_pst) %>% 
  drop_na(max_velocity_m_s) %>% 
  summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) 

## Things look pretty good, and no flags to clean up
ggplot(adcp_raw, aes(time_pst, max_velocity_m_s)) +
  geom_line(color = "gray") + 
  geom_point(data = adcp_raw %>% filter(maxu_qc != 0), color = "red", alpha = 0.3)

## Confirm no flag issues
adcp_raw %>% filter(maxu_qc != 0) #n = 0


# 3. Clean up ------------------------------------------------------------------

## The only thing is a baseline shift in 2024. Let's see if we can isolate that 
## chunk, and potentially shift it down

## First, isolate the event
x <- adcp_raw %>% 
  filter(time_pst > "2024-03-15" & 
           time_pst < "2024-06-01")

## Plotting, it's clear that there is a shift
ggplot(x, aes(time_pst, max_velocity_m_s)) +
  geom_line()

## First, let's determine what a sensible minimum would be for ~1 month before and 
## ~1 month after the shift
pre_min <- min(adcp_raw %>% 
  filter(time_pst > "2024-03-01" & 
           time_pst < "2024-04-01") %>% 
  pull(max_velocity_m_s))

post_min <- min(adcp_raw %>% 
                  filter(time_pst > "2024-05-15" & 
                           time_pst < "2024-06-15") %>% 
                  pull(max_velocity_m_s))

## This is our target for a sensible minimum for the shifted time-series
proper_min = (pre_min + post_min) / 2


## Second, let's isolate the shifted portion using changepoint analysis
# Apply changepoint detection algorithm
change_result <- cpt.mean(x$max_velocity_m_s, method = "BinSeg")

# Extract changepoint locations
change_points <- cpts(change_result)

shift_start <- x$time_pst[[change_points[1]]]
shift_end <- x$time_pst[[change_points[2]]]

## Confirm points are correctly identified
x_labeled <- x %>% 
  mutate(needs_offset = case_when(time_pst > shift_start & time_pst < shift_end ~ "yes", 
                                  TRUE ~ "no")) 

ggplot(x_labeled, aes(time_pst, y = max_velocity_m_s)) + 
  geom_line(color = "gray") + 
  geom_line(data = x_labeled %>% filter(needs_offset == "yes"), color = "red", alpha = 0.5)

## shift_start looks good, shift_end does not. It was a good try. Let's do this
## manually since it's just ID'ing two points
rm(shift_start, shift_end)

p_start <- x %>% 
  filter(time_pst > "2024-04-01" & 
           time_pst < "2024-04-15") %>% 
  ggplot(aes(time_pst, max_velocity_m_s)) + 
  geom_line() + 
  geom_point(alpha = 0.3)

## There is a very clear jump between 2024-04-03 14:30 and 16:30
ggplotly(p_start)


## Now for shift_end
p_end <- x %>% 
  filter(time_pst > "2024-05-01" & 
           time_pst < "2024-05-15") %>% 
  ggplot(aes(time_pst, max_velocity_m_s)) + 
  geom_line() + 
  geom_point(alpha = 0.3)

## I'm thinking the issue is 2024-05-07 18:00, I'm thinking that needs to be 
## moved down
ggplotly(p_end)

shift_start <- as.POSIXct("2024-04-03 14:30:00", tz = "UTC") ## Use >, not >=
shift_end <- as.POSIXct("2024-05-07 18:00:00", tz = "UTC") ## Use <=, not <

current_min = min(adcp_raw %>% 
               filter(time_pst > shift_start & 
                                   time_pst <= shift_end) %>% 
               pull(max_velocity_m_s))

## We want to offset everything by the difference between the expected (proper_min)
## and the actual (current_min)
offset = current_min - proper_min


adcp_shift <- adcp_raw %>% 
  mutate(needs_offset = case_when(time_pst > shift_start & time_pst <= shift_end ~ "yes", 
                                  TRUE ~ "no")) %>% 
  mutate(max_velocity_m_s_cor = ifelse(needs_offset == "yes", 
         max_velocity_m_s - offset, 
         max_velocity_m_s))

y <- adcp_shift %>% 
  filter(time_pst > "2024-03-15" & 
           time_pst < "2024-05-15")

## Here are the data prior to fixing the baseline
p1 <- ggplot(y, aes(time_pst, y = max_velocity_m_s)) + 
  geom_line(color = "gray") + 
  geom_line(data = y %>% filter(needs_offset == "yes"), color = "red", alpha = 0.5) + 
  ggtitle("Data before baseline correction")

## And here's the data after baseline correction
## Here are the data prior to fixing the baseline
p2 <- ggplot(y, aes(time_pst, y = max_velocity_m_s_cor)) + 
  geom_line(color = "gray") + 
  ggtitle("Data after baseline correction")

## here they are together
plot_grid(p1, p2, ncol = 1)
ggsave("figures/250703_adcp_baseline_correction.png", width = 6, height = 6)


# 4. Finalize dataset and write out --------------------------------------------

adcp_clean <- adcp_shift %>% 
  dplyr::select(-c(maxu_qc, needs_offset, max_velocity_m_s)) %>% 
  rename("max_velocity_m_s" = max_velocity_m_s_cor)

write_csv(adcp_clean, "data/outputs/L1/250703_adcp_velocity_L1.csv")






