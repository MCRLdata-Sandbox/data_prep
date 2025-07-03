## This script explores carbonate chemistry, nutrients, and CTD cast data collected
## By Jess and Tristen for Jess's LDRD
##

require(pacman)

p_load(tidyverse, 
       seacarb,
       janitor,
       readxl)

df <- readxl::read_xlsx("data/inputs/Master_LDRDData_ODV_110424.xlsx") %>% 
  clean_names()

df_clean_nas <-  df %>% 
  mutate(across(where(is.numeric), ~na_if(., -999))) 

# 2. Outlier-based cleaning decisions ------------------------------------------

## We will trim one TA point > 2300, pretty clear, isolated outlier
ggplot(df_clean_nas, aes(ta_umol_kg_20)) + geom_boxplot()

## Two potential outliers below 1925, unclear whether to scrub
ggplot(df_clean_nas, aes(dic_umol_kg_20_c)) + geom_boxplot()

## These all look fine so no cleaning necessary
ggplot(df_clean_nas, aes(temperature_celsius)) + geom_boxplot()
ggplot(df_clean_nas, aes(salinity_practical_salinity_scale)) + geom_boxplot()
ggplot(df_clean_nas, aes(sound_velocity_meters_per_second)) + geom_boxplot()
ggplot(df_clean_nas, aes(si_oh4)) + geom_boxplot()

## Suspect data - not enough metadata to effectively QC
ggplot(df_clean_nas, aes(nh4)) + geom_boxplot()
ggplot(df_clean_nas, aes(no3)) + geom_boxplot()
ggplot(df_clean_nas, aes(po4)) + geom_boxplot()


df_filter <- df_clean_nas %>% 
  mutate(tidal_name = str_to_title(tidal_name)) %>% 
  drop_na(ta_umol_kg_20) %>% #remove rows that don't include TA (priority var)
  filter(ta_umol_kg_20 < 2300) #remove one outlier

write_csv(df_filter, "data/outputs/L1/250701_carbonate_chem_data_Cross.csv")
