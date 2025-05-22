## This script takes in test netCDF files downloaded manually from AWS S3 to learn
## about data structure, develop code for reading and formatting, and hopefully 
## lay the bedrock useful for directly pulling info from S3
## Steps for this script: 
### 1. Read in data manually
### 2. Set up a function and test it on a couple
### 3. Figure out how to generalize function to other datasets

# QC flags
# 0 = good
# 1 = missing values
# 2 = too low
# 4 = too high
# 8 = spike detection/non-real data


# 0. Setup ---------------------------------------------------------------------

## Set up environment
source("scripts/0_setup.R")


# 1. Read in tide gauge data for comparison ------------------------------------

tide_athena <- read_csv("data/ready_to_use/L1/250515_tidegauge_L1.csv")


# 2. Manually read in a tide gauge dataset -------------------------------------

## Reads it in, not a normal file structure I'm used to looking at...
## Pretty opaque as an R object
tide_nc <- nc_open("data/inputs/mcrl_data/netcdf/test/tide_gauge.mcrl_pier-ysi_nile_1-5min.a1.20230801.nc")

## This is what chat told me to do for pulling which variables are in the file
variable_names <- sapply(tide_nc$var, function(x) x$name)

## We know the file is not in GMT, because that's baked in: 
tide_nc$is_GMT

## We really want to pull three things: 1) time, 2) water level, 3) any extra QC

## Time - this gets us time, but not in a usable unit, cause it's nanoseconds since 
## a specific time, that's buried elsewhere
raw_times <- ncvar_get(tide_nc, "time")

## This isn't very dynamic, but at least gets us the units
tide_nc$var$water_level$dim[[1]]$units

## Let's see if we can conver that to something usable
## str_remove scrubs text, parse_date converts to POSIXct - NOTE TZ is UTC!!!! 
## Above says that's wrong, though unclear what timezone actually is at this point
origin <- parsedate::parse_date(str_remove(tide_nc$var$water_level$dim[[1]]$units, "nanoseconds since "))

## Now we're getting somewhere, here are times
times <- tibble(raw_times_ns = raw_times) %>% 
  mutate(raw_times_s = raw_times_ns / 1e+9) %>% 
  mutate(time_pst = round_date(origin + seconds(raw_times_s), unit = "5 min"))

## water level
df <- times %>% 
  mutate(water_level_m_navd88 = ncvar_get(tide_nc, "water_level"), 
         water_level_qc = ncvar_get(tide_nc, "qc_water_level"), 
         water_level_smooth = ncvar_get(tide_nc, "water_level_smooth")) %>% 
  mutate(is_smooth = ifelse(abs(water_level_m_navd88 - water_level_smooth) < 0.01, "no", "smoothed"))


# 3. Compare to see if we have things lined up ---------------------------------

## This looks right, which is lovely! It also looks like these data would be PDT,
## but are potentially kept in PST
tide_athena %>% 
  filter(as_date(time_pst) == "2023-08-01") %>% 
  full_join(df, by = c("time_pst" = "time_pst")) %>% 
  ggplot(aes(water_level_m_navd88.x, water_level_m_navd88.y)) + 
  geom_point()


# 4. Time to figure out how to pull QC -----------------------------------------

ggplot(df, aes(time_pst, water_level_m_navd88)) + 
  geom_line() + 
  geom_point(data = df %>% filter(is_smooth == "smoothed"))



