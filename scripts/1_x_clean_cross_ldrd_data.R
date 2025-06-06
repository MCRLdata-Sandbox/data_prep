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

df_filter <- df %>% 
  mutate(across(where(is.numeric), ~na_if(., -999))) %>% 
  mutate(tidal_name = str_to_title(tidal_name)) %>% 
  drop_na(ta_umol_kg_20) %>% 
  filter(ta_umol_kg_20 < 2300) 

b = summary(lm(ta_umol_kg_20~salinity_practical_salinity_scale, data = df_filter))[[4]][1,1]
m = summary(lm(ta_umol_kg_20~salinity_practical_salinity_scale, data = df_filter))[[4]][2,1]
r2 = summary(lm(ta_umol_kg_20~salinity_practical_salinity_scale, data = df_filter))[[9]]

df_filter %>% 
  ggplot(aes(salinity_practical_salinity_scale, ta_umol_kg_20)) + 
  geom_point(color = "black", size = 3, aes(shape = station_name)) + 
  geom_point(size = 2.2, aes(color = tidal_name, shape = station_name)) + 
  #geom_smooth(se = F, color = "black") + 
  geom_smooth(method = "lm", se = F, color = "black", linetype = "dashed") + 
  scale_color_viridis_d() + 
  labs(x = "Salinity (PSU)", y = "Total alkalinity (umol/kg)", 
       color = "Tide", shape = "Location", 
       title = "1 value removed (TA = 2301)") + 
  annotate(geom = "text", x = 31, y = 2190, label = paste0("R2=", round(r2, 2))) + 
  annotate(geom = "text", x = 31, y = 2200, label = paste0("TA=", round(m, 0), "*S+", round(b, 0))) 
ggsave("figures/250606_sal_vs_ta.png", width = 5, height = 4)


