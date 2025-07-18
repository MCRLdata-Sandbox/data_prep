## Prepare data for analysis and plotting that's pulled from AWS using Athena 
## Current datasets: tidegauge, ctd, ph, met, cdom, co2 
## calculating natural variability envelopes
##
## 2025-05-14
## Peter Regier
## 
# ########### #
# ########### #


# 1. Setup ---------------------------------------------------------------------

## Set up environment
source("scripts/0_setup.R")


# 2. Discover files ------------------------------------------------------------

## We have several files downloaded as csvs directly from Athena. In an effort to
## make things as plug-and-play as possible, I'm going to add extra steps to ID
## which files are which and read them in, hopefully programmatically
all_files <- list.files("data/inputs/mcrl_data/athena/210501_250501", 
                        full.names = T)

what_files_do_i_have <- function(file){
  headers <- read_csv(file) %>% 
    # we aren't interested in time or qc, just data cols
    dplyr::select(-c(contains("time"), contains("qc_"))) %>%  
    # make things easier to read, w/ no special characters to potential cause issues
    clean_names() 
  
  paste(colnames(headers), collapse = ", ")
}

file_vars <- all_files %>% 
  map(what_files_do_i_have)


# 3. Read in files -------------------------------------------------------------

## I suspect there's a cleaner way to do this, but it's likely most efficient to
## label files when you're downloading.

## Helper function to find the string you're looking for
which_file <- function(var_string){
  which(sapply(file_vars, grepl, pattern = var_string))[[1]]
}

## One more helper function to clean up column names, round datetime, and convert time to PST
read_csv_formatted <- function(file)(
  read_csv(file) %>% 
    clean_names() %>% 
    mutate(time_utc = round_date(force_tz(time_utc, tzone = "UTC"), unit = "5 min")) %>% 
    mutate(time_pst = with_tz(time_utc, tzone = "Etc/GMT+8"))# %>%  # Forcing to PST
    # mutate(year = year(time_pst), 
    #        quarter = quarter(time_pst, fiscal_start = 3),
    #        month = month(time_pst), 
    #        doy = yday(time_pst), 
    #        hour_of_day = hour(time_pst))
)


## Let's read in our datasets
adcp <- read_csv_formatted(all_files[[which_file("velocity")]]) 
cdom <- read_csv_formatted(all_files[[which_file("cdom")]]) 
co2 <- read_csv_formatted(all_files[[which_file("p_co2")]]) 
ctd <- read_csv_formatted(all_files[[which_file("salinity")]])
ph <- read_csv_formatted(all_files[[which_file("temperature, ph")]]) ## this is bad regex, need to isolate ph
met <- read_csv_formatted(all_files[[which_file("airtemp")]])
tidegauge <- read_csv_formatted(all_files[[which_file("water_level")]])

plot_grid(ggplot(tidegauge %>% filter(qc_water_level == 0), aes(time_pst, water_level_m_navd88)) + 
            geom_line(), 
          ggplot(adcp %>% filter(maxu_qc == 0), aes(time_pst, max_velocity_m_s)) + 
            geom_line(), 
          ggplot(ctd %>% filter(qc_salinity == 0), aes(time_pst, salinity_ppt)) + 
            geom_line(), 
          ggplot(ph %>% filter(qc_ph == 0), aes(time_pst, ph)) + 
            geom_line(), 
          ggplot(co2 %>% filter(qc_pco2_water == 0), aes(time_pst, p_co2_in_water_ppm)) + 
            geom_line(), 
          ncol = 1)

## There are doubles, but we will deal with those later
# Count occurrences of each `time_pst`
count_time_pst <- tidegauge %>%
  group_by(time_pst) %>%
  tally(name = "count")

# Filter rows where `time_pst` occurs more than once
duplicates_tidegauge <- tidegauge %>%
  inner_join(count_time_pst, by = "time_pst") %>%
  filter(count > 1)


## Finally, merge into a single dataframe, selecting parameters we expect to be of 
## most interest
df <- full_join(tidegauge %>% dplyr::select(-time_utc), 
                ctd %>% dplyr::select(-time_utc), by = "time_pst") %>% 
  full_join(adcp %>% dplyr::select(-time_utc), by = "time_pst") %>% 
  full_join(ph %>% dplyr::select(-time_utc), by = "time_pst") %>% 
  full_join(cdom %>% dplyr::select(-c(time_utc)), by = "time_pst") %>% 
  full_join(co2 %>% dplyr::select(-time_utc), by = "time_pst") %>% 
  full_join(met %>% dplyr::select(-time_utc), by = "time_pst") 

write_csv(df, "data/inputs/mcrl_data/MCRLdata_240501_250501_L0.csv")


## Make L0 plots for parameters

l0_plot <- function(var, flag){
  
  var_name <- deparse(substitute(var))
  
  x <- df %>% 
    drop_na({{var}})
  
  ggplot(x, aes(time_pst, {{var}})) + 
    geom_line(color = "gray") + 
    geom_point(data = x %>% filter({{flag}} != 0), color = "red", alpha = 0.3)
  
  ggsave(paste0("figures/L0_plots/L0_", var_name, ".png"), 
         width = 6, height = 3.5)
}

## WL - tide gauge
l0_plot(water_level_m_navd88, qc_water_level)

## Water temp
l0_plot(temp_deg_c, qc_temp)

## Salinity
l0_plot(salinity_ppt, qc_salinity)

## DO
l0_plot(do_mg_l, qc_do)

## Velocity
l0_plot(max_velocity_m_s, maxu_qc)

## Air temp
l0_plot(airtemp_avg_deg_c, qc_airtemp_avg)

## pH
l0_plot(ph, qc_ph)

## Chla
l0_plot(chlorophyll_mg_l, qc_chlorophyll)

## CDOM
l0_plot(cdom_ppb, qc_cdom)

## CO2
l0_plot(p_co2_in_water_ppm, qc_pco2_water)

## CO2
l0_plot(p_co2_in_water_ppm, qc_pco2_water)

