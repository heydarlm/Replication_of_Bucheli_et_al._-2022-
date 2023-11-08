# =========================================================================================================
# 1. Loading packages 
# =========================================================================================================

library(raster)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(tidyr)
library(tidyverse)

# =========================================================================================================
# 2. Specifications, loading and preparing data
# =========================================================================================================

# -----------------------------------------
# Yield Data
# -----------------------------------------
dt_final <- read.csv('data/yield/dt_final_d2.csv')

# Separating crops
wheat_raw <- dt_final %>% 
  dplyr::filter(fruchtart=="Winterweizen") %>% 
  dplyr::select(-fruchtart)

rapeseed_raw <- dt_final %>% 
  dplyr::filter(fruchtart=="Raps") %>% 
  dplyr::select(-fruchtart)

# Checking wheat raw and rapeseed raw
lapply(list(wheat_raw,rapeseed_raw),head)

# -----------------------------------------
# Municipality Location
# -----------------------------------------

geolocshp <- readOGR("data/geolocn/coordinates_shape_transformed.shp")
geoloc <- read.csv('data/geolocn/coordinates_csv.csv')
geoloc <- geoloc[c('AGS','coords.x1','coords.x2')]
names(geoloc)[2:3] <- c('lon','lat')

# Merging yield data with municipality coordinates
wheat_mrg <- merge(wheat_raw,geoloc,by='AGS')
rapeseed_mrg <- merge(rapeseed_raw,geoloc,by='AGS')

# Checking for missing joins
sapply(wheat_mrg, function(d) sum(is.na(d)))
sapply(rapeseed_mrg, function(d) sum(is.na(d)))

# Selecting required columns
wheat_sub <- wheat_mrg[c('ID','lon','lat','Yield','Year')]
rapeseed_sub <- rapeseed_mrg[c('ID','lon','lat','Yield','Year')]


wheat_farms <-
  wheat_sub %>% 
  select(lat,lon) %>% 
  mutate(type='wheat')

raps_farms<-
  rapeseed_sub %>% 
  select(lat,lon) %>% 
  mutate(type='rapeseed')


######################################

germany <- ne_countries(scale = "medium", returnclass = "sf", country = "Germany")
germany <- st_transform(germany, crs = 4326)

german_states <- getData("GADM", country = "DEU", level = 1)
german_states <- st_as_sf(german_states)
german_states <- st_transform(german_states, crs = 4326)

bind_rows(wheat_farms,raps_farms) %>% 
  ggplot() +
  geom_sf(data = germany) +
  geom_sf(data = german_states, fill = "antiquewhite", color = "black", lwd = 0.5) +
  geom_point(aes(x = lon,
                 y = lat,
                 shape = type,
                 color=type),
             size = 3,
             alpha=0.5) +
  coord_sf(xlim = c(10, 16), ylim = c(50.1, 54.5), expand = FALSE)+
  ggtitle("Eastern Germany farms") +
  xlab("Longitude") +
  ylab("Latitude")+
  theme_minimal()+
  scale_colour_manual(name = "Crop Type",
                      values = c("blue", "blue"))+
  scale_shape_manual(name = "Crop Type",values = c(3,4))+
  theme(legend.position = "bottom",legend.title = element_blank())







