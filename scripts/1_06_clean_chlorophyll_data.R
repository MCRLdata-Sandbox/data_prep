## This script will explore algorithmically cleaning up the chlorophyll data
##
## 2025-06-04
## Peter Regier
##
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Set up environment
source("scripts/0_setup.R")
p_load(plotly, oce)


# 2. Read in data --------------------------------------------------------------

pull_in_data <- function(variable){
  df <- read_csv("data/inputs/mcrl_data/MCRLdata_240501_250501_L0.csv") %>% 
    dplyr::select(time_pst, contains(variable)) %>% 
    drop_na()
  
  return(df)
}

chla_raw <- pull_in_data("chlorophyll")

ggplot(chla_raw, aes(time_pst, chlorophyll_mg_l)) + 
  geom_line(color = "gray") +
  geom_point(data = chla_raw %>% filter(qc_chlorophyll != 0), color = "red", alpha = 0.1)


# 3. Clean flags ---------------------------------------------------------------

## Clean data based on existing flags
chla_clean_flags <- chla_raw %>% 
  filter(qc_chlorophyll == 0) %>% 
  mutate(chla_despike = despike(chlorophyll_mg_l))

## Standard plot, but it's not super easy to look at because of the crazy values
ggplot(chla_clean_flags, aes(time_pst, chlorophyll_mg_l)) + 
  geom_line(color = "gray") + 
  geom_line(aes(y = chla_despike), color = "red", alpha = 0.5)

ggplot(chla_clean_flags, aes(time_pst, chlorophyll_mg_l)) + 
  geom_point(color = "blue", alpha = 0.1)

x <- chla_clean_flags %>% 
  filter(time_pst > "2022-01-01") %>% 
  filter(time_pst < "2022-02-01") %>% 
  mutate(chla_despike = despike(chlorophyll_mg_l))

ggplot(x, aes(x = time_pst)) + 
  geom_line(aes(y = chlorophyll_mg_l), color = "gray") + 
  geom_line(aes(y = chla_despike), color = "red", alpha = 0.5)

p1 <- x %>% 
  ggplot(aes(time_pst, chla_despike)) + 
  geom_line(color = "blue")

ggplotly(p1)

## Let's look at some common-sense patterns: 
### 1) Are values higher in summer? 
### 2) Do values fluctuate daily? 
### 3) Do values relate to air temperature? 

chla_labeled <- chla_clean_flags %>% 
  assign_season() %>% 
  mutate(year = year(time_pst))

## This is not super helpful, we see wht looks like a reasonable seasonal pattern
## in 2024, but the other years seem kinda random.
ggplot(chla_labeled, aes(as.factor(month_num), chlorophyll_mg_l)) + 
  geom_boxplot() + 
  facet_wrap(~year, nrow = 1)
  
ggplot(chla_labeled, )










