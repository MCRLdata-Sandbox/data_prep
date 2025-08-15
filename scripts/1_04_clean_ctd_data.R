## This script will explore algorithmically cleaning up the salinity data, and 
## potentially using data to impute gaps
##
## 2025-05-23
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Set up environment
source("scripts/0_setup.R")
p_load(tictoc, 
        noaaoceans,
        oce) # Package for despiking


# 2. Read in data --------------------------------------------------------------

## Read in data
ctd_raw <- read_csv("data/inputs/mcrl_data/MCRLdata_240501_250501_L0.csv") %>% 
  dplyr::select(time_pst, contains("salinity"), contains("temp")) %>% 
  dplyr::select(-contains("airtemp")) %>% 
  group_by(time_pst) %>% 
  summarize(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))


### Salinity ###

# 3. Initial salinity plots ----------------------------------------------------

n_flags <- nrow(ctd_raw %>% drop_na(salinity_ppt) %>% filter(qc_salinity != 0))

## First, let's look at our time-series
ggplot(ctd_raw, aes(time_pst, salinity_ppt)) + 
  geom_line(color = "gray") + 
  geom_point(data = ctd_raw %>% filter(qc_salinity != 0), color = "red", alpha = 0.5) + 
  ggtitle(paste(n_flags, "flagged points"))

## Lots of errors, let's remove those
ggplot(ctd_raw %>% filter(qc_salinity == 0), aes(time_pst, salinity_ppt)) + 
  geom_line()


# 4. Clean up salinity ---------------------------------------------------------

sal_despike <- ctd_raw %>% 
  filter(qc_salinity == 0) %>% 
  filter(salinity_ppt > 29) %>% 
  mutate(sal_ppt_despike = despike(salinity_ppt)) 

## It's not beautiful, but good enough for pulling summary statistics I think
ggplot(sal_despike, aes(time_pst)) + 
  geom_line(aes(y = salinity_ppt), color = "gray") + 
  geom_line(aes(y = sal_ppt_despike), color = "blue", alpha = 0.5)
    


### Water Temp ###
## This is going to be a problem child...

# 5. Initial temperature plots -------------------------------------------------

## First, let's look at our time-series
ggplot(ctd_raw, aes(time_pst, temp_deg_c)) + 
  geom_line(color = "gray") + 
  geom_point(data = ctd_raw %>% filter(qc_temp != 0), color = "red", alpha = 0.5)

## Lots of errors, let's remove those
ggplot(ctd_raw %>% filter(qc_temp == 0), aes(time_pst, temp_deg_c)) + 
  geom_line()


# 6. Clean up temperature ------------------------------------------------------

## Although I don't like doing this, on a small project with many different things
## to get to, I'm going to ignore temperature data once it starts getting wonky
## since the goal currently is to calculate statistics
temp_clean_harsh <- ctd_raw %>% 
  filter(qc_temp == 0) %>% 
  filter(temp_deg_c > 5) %>% ## removes 1 point in early 2024
  filter(temp_deg_c < 20) %>% ## removes 1 point in fall 2022
  filter(time_pst < "2024-05-01")

temp_clean <- ctd_raw %>% 
  filter(qc_temp == 0) %>% 
  filter(temp_deg_c > 5) %>% ## removes 1 point in early 2024
  filter(temp_deg_c < 20)

## That looks better, but we really do lose too much by trimming
ggplot(temp_clean_harsh, aes(time_pst, temp_deg_c)) + 
  geom_line(color = "gray") 


# 7. External water temp data --------------------------------------------------

## Similar to the tidal and air temp data, let's find some external data and
## see if we can find a proxy that will let us effectively clean/gapfill

## Using the noaaoceans package, let's find nearby water temp sensors
pa_id <- list_coops_stations() %>% 
  filter(station_state == 'WA' & water_temp == '1') %>% 
  as_tibble() %>% 
  filter(grepl("Port Angeles", station_names)) %>% 
  pull(station_id)

## PA is closest, let's use that

## we can only pull 365 days at a time: 
start_date <- as.Date("2021-05-01")
end_date <- as.Date("2025-08-10")
interval_days <- 31

# Generate a sequence of start dates
date_sequence <- seq(start_date, end_date, by = interval_days)

# Create an empty data frame to store results
all_chunks <- list()

# Loop to query data in chunks
for (i in seq_along(date_sequence)) {
  tic(sprintf("Pulling data chunk %d", i))
  
  # Define chunk start and end dates
  chunk_start <- date_sequence[i]
  chunk_end <- min(chunk_start + interval_days - 1, end_date)
  
  # Convert dates to yyyymmdd format
  chunk_start_formatted <- format(chunk_start, "%Y%m%d")
  chunk_end_formatted <- format(chunk_end, "%Y%m%d")
  
  # Query data for the specific chunk
  water_temps_chunk <- noaaoceans::query_coops_data(
    station_id = pa_id,
    start_date = chunk_start_formatted,
    end_date = chunk_end_formatted,
    time_zone = "gmt",
    data_product = "water_temperature") #,
    #interval = "h")
  
  # Store the chunk in the list
  all_chunks[[i]] <- water_temps_chunk
  
  toc()
}

pa_water_temps_raw <- bind_rows(all_chunks)

pa_water_temps_raw %>% 
  ggplot(aes(parsedate::parse_date(t), v)) + geom_point()

pa_water_temps <- pa_water_temps_raw %>% 
  as_tibble() %>% 
  mutate(time_gmt = parsedate::parse_date(t)) %>% 
  mutate(time_pst = with_tz(time_gmt, tzone = 'Etc/GMT+8'))


test <- full_join(temp_clean, 
                  pa_water_temps, 
                  by = "time_pst") %>% 
  mutate(v = as.numeric(v), 
         pa_temp_deg_c = (v - 32) * 5/9)

## This is definitely interesting, and has potential for future work. For now, I 
## think this might be a little too much. Let's try something simpler
ggplot(test, aes(lead(pa_temp_deg_c, 15), temp_deg_c)) + 
  geom_point(color = "gray") + 
  geom_smooth(method = "lm")


# 8. Use derivatives -----------------------------------------------------------

## We can use a derivative to filter out large changes between consecutive values
## which are likely errors since water temperature should not change rapidly. Let's
## experimentally determine that threshold: 
x <- temp_clean %>% 
  mutate(delta_t = temp_deg_c - lag(temp_deg_c))

plot_grid(ggplot(x, aes(time_pst, temp_deg_c)) + 
            geom_line(color = "gray"), 
          ggplot(x, aes(time_pst, delta_t)) + 
            geom_line(color = "red"), 
          ncol = 1)

## Plot 
colors = PNWColors::pnw_palette("Sunset", n = 4)

ggplot(x, aes(time_pst, y = temp_deg_c)) + 
  geom_line(color = "gray80") + 
  geom_point(data = x %>% filter(abs(delta_t) > 0.5), color = colors[4]) +
  geom_point(data = x %>% filter(abs(delta_t) > 1), color = colors[3]) +
  geom_point(data = x %>% filter(abs(delta_t) > 2), color = colors[2]) +
  geom_point(data = x %>% filter(abs(delta_t) > 3), color = colors[1])

## Visually, 3 takes out many of the spikes, but 2 seems to take out most of the
## spikes. A delta of 1 seems to take out a fairly large number of points within
## normal-looking patterns, so we'll use 2 as our threshold
delta_threshold = 2

x %>% filter(abs(delta_t) > delta_threshold) ## this threshold scrubs 165 of 247k (so < 1%)

## Values are also spread across years fairly evenly (though 2024 is notable)
x %>% filter(abs(delta_t) > delta_threshold) %>% group_by(year(time_pst)) %>% summarize(n())

temp_final <- temp_clean %>% 
  mutate(delta_t = temp_deg_c - lag(temp_deg_c)) %>% 
  filter(abs(delta_t) < delta_threshold)

## Final plot - this looks pretty reasonable
ggplot(temp_final, aes(time_pst, temp_deg_c)) + geom_line()

# 7. Bind data -----------------------------------------------------------------

ctd_clean <- full_join(temp_clean %>% dplyr::select(time_pst, temp_deg_c), 
                       sal_despike %>% 
                         rename("salinity_psu_clean" = sal_ppt_despike) %>% 
                         dplyr::select(time_pst, salinity_psu_clean), 
                       by = "time_pst")


# 8. Write out -----------------------------------------------------------------

write_csv(ctd_clean, "data/outputs/L1/250630_ctd_water_temp_salinity_L1.csv")



