
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
dt_final <- read_csv('data/yield/dt_final.csv')



wheat_farms <-
  dt_final %>% 
  filter(fruchtart=="Winterweizen",grp_farm=="0") %>% 
  select(standort.breite,standort.laenge) %>% 
  mutate(type='wheat')

raps_farms<-
  dt_final %>% 
  filter(fruchtart=="Raps",grp_farm=="0") %>% 
  select(standort.breite,standort.laenge) %>% 
  mutate(type='rapeseed')


all_farms<-
  dt_final %>% 
  filter(!grp_farm=="0") %>% 
  select(standort.breite,standort.laenge) %>% 
  mutate(type='wheat and rapeseed')

######################################

germany <- ne_countries(scale = "medium", returnclass = "sf", country = "Germany")
germany <- st_transform(germany, crs = 4326)

german_states <- getData("GADM", country = "DEU", level = 1)
german_states <- st_as_sf(german_states)
german_states <- st_transform(german_states, crs = 4326)

bind_rows(wheat_farms,raps_farms,all_farms) %>% 
  ggplot() +
  geom_sf(data = germany) +
  geom_sf(data = german_states, fill = "antiquewhite", color = "black", lwd = 0.5) +
  geom_point(aes(x = standort.laenge,
                 y = standort.breite,
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
                      values = c("blue", "blue",'blue'))+
  scale_shape_manual(name = "Crop Type",values = c(3, 4, 8))+
  theme(legend.position = "bottom",legend.title = element_blank())




 
  

