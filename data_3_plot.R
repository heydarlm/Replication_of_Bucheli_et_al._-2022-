library(raster)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(tidyr)
library(tidyverse)

#######################
#data preperation
####################
# -----------------------------------------
# Yield Data
# -----------------------------------------
barley_raw <- read.csv('data/yield/WB0015_Masud.csv')
rapeseed_raw <- read.csv('data/yield/WR0015_Masud.csv')

#ID "2019028" farm from barley and "2018007", "2020031" have been ommitted because they had values equal to 0
barley_raw <- subset(barley_raw, ID != "2019028")
rapeseed_raw <- rapeseed_raw[!(rapeseed_raw$ID %in% c("2018007", "2020031")), ]

lapply(list(barley_raw,rapeseed_raw),head)

# -----------------------------------------
# Municipality Location
# -----------------------------------------

geolocshp <- readOGR("data/geolocn/coordinates_shape_transformed.shp")
geoloc <- read.csv('data/geolocn/coordinates_csv.csv')
geoloc <- geoloc[c('AGS','coords.x1','coords.x2')]
names(geoloc)[2:3] <- c('lon','lat')

# Merging yield data with municipality coordinates
barley_mrg <- merge(barley_raw,geoloc,by='AGS')
rapeseed_mrg <- merge(rapeseed_raw,geoloc,by='AGS')

# Checking for missing joins
sapply(barley_mrg, function(d) sum(is.na(d)))
sapply(rapeseed_mrg, function(d) sum(is.na(d)))

# Selecting required columns
barley_farms <- barley_mrg[c('ID','lon','lat','Yield','Year')]
raps_farms <- rapeseed_mrg[c('ID','lon','lat','Yield','Year')]


barley_farms <-
  barley_farms%>%
  select(lat,lon) %>% 
  mutate(type='barley')

raps_farms<-
  raps_farms %>% 
  select(lat,lon) %>% 
  mutate(type='rapeseed')


######################################

germany <- ne_countries(scale = "medium", returnclass = "sf", country = "Germany")
germany <- st_transform(germany, crs = 4326)

german_states <- getData("GADM", country = "DEU", level = 1)
german_states <- st_as_sf(german_states)
german_states <- st_transform(german_states, crs = 4326)

bind_rows(barley_farms,raps_farms) %>% 
  ggplot() +
  geom_sf(data = germany) +
  geom_sf(data = german_states, fill = "antiquewhite", color = "black", lwd = 0.5) +
  geom_point(aes(x = lon,
                 y = lat,
                 shape = type,
                 color=type),
             size = 3,
             alpha=1) +
  coord_sf(xlim = c(11.8, 15.5), ylim = c(50.1, 51.8), expand = FALSE)+
  ggtitle("Saxony farms") +
  xlab("Longitude") +
  ylab("Latitude")+
  theme_minimal()+
  scale_colour_manual(name = "Crop Type",
                      values = c("blue", "blue"))+
  scale_shape_manual(name = "Crop Type",values = c(3, 4))+
  theme(legend.position = "bottom",legend.title = element_blank())







