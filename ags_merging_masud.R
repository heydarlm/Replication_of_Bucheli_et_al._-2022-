# =========================================================================================================
# 1. Loading packages 
# =========================================================================================================

library(tidyverse)
# =========================================================================================================
# 2. Specifications, loading and preparing data
# =========================================================================================================

coordinates <- read_csv('data/geolocn/coordinates_csv.csv')
dt_final_raw <- read_csv('data/yield/dt_final.csv')

# =========================================================================================================
# 3. Adding municipality data to the first exact replication data
# =========================================================================================================
#in order to add municipality data into collected yield data, we find closest municipality
#centroids to the farm coordinates and merge them

# Creating coordinate and ags dataframe
coord_dt <- coordinates %>% 
  select(AGS, coords.x1,coords.x2) %>% 
  set_names(c('ags','lon.y','lat.y')) %>% 
  select(ags,lat.y,lon.y)

# Creating dataframe from required columns to keep distinct rows
dt_final <- dt_final_raw %>% 
  select(referenznummer,grp_farm,standort.breite,standort.laenge) %>% 
  set_names(c('referenznummer','grp_farm','lat.x','lon.x')) %>% 
  distinct() %>% 
  mutate(ind=1:nrow(.)) %>%
  select(referenznummer,ind, grp_farm, lat.x, lon.x)
  

# Splitting all values into lists as data frame
list_ind <- dt_final %>% group_split(ind) 
dt_all <- tibble()

# We try to find min coordinate difference between farms and
# AGSs and take the min difference AGS as farm's AGS
for(i in 1:length(list_ind)){
  print(i)
  
  dt_tmp1 <- list_ind[[i]]
  dt_tmp2 <- bind_cols(dt_tmp1,coord_dt) %>% 
    mutate(coord_diff=abs(lon.x-lon.y)+abs(lat.x-lat.y)) %>% 
    group_by(ind) %>% 
    filter(coord_diff==min(coord_diff))
  
  # Storing values
  dt_all <- bind_rows(dt_all,dt_tmp2)
}


# Bringing AGS information for each farm back and save
dt_all %>%
  select(referenznummer, ags) %>% 
  full_join(dt_final_raw, by = 'referenznummer') %>%
  dplyr::group_by(referenznummer, fruchtart) %>%
  dplyr::mutate(year_d = row_number()) %>%
  ungroup() %>% 
  select(referenznummer, ags, fruchtart, year, year_d, value) %>% 
  set_names(c('ID', 'AGS', 'fruchtart', 'Year', 'year_d', 'Yield'))%>%
  write.csv('dt_final_d2.csv', row.names = FALSE)





