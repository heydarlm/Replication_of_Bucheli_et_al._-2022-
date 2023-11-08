# =========================================================================================================
# 1. Loading packages 
# =========================================================================================================

library(raster)
library(ncdf4)
library(reshape2)
library(rgdal)
library(raster)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(lubridate)
library(plyr)
library(esmisc)
library(sandwich)
library(MASS)
library(e1071)


# =========================================================================================================
# 2. Specifications, loading and preparing data
# =========================================================================================================

# -----------------------------------------
# Yield Data
# -----------------------------------------
dt_final <- read.csv('data/yield/dt_final_d2.csv')

# Separating crops
wheat_raw <- dt_final %>% 
  dplyr::filter(fruchtart=='Winterweizen') %>% 
  dplyr::select(-fruchtart)

rapeseed_raw <- dt_final %>% 
  dplyr::filter(fruchtart=='Raps') %>% 
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

# Transforming to wider form
wheat <- reshape2::dcast(wheat_sub, ID+lon+lat~Year,value.var = 'Yield') 
rapeseed <- reshape2::dcast(rapeseed_sub, ID+lon+lat~Year,value.var = 'Yield')

wheat$ID <- NULL
rapeseed$ID <- NULL

lapply(list(wheat,rapeseed),head)
farm_yields <- list(wheat,rapeseed)

# Creating coordinate object for yield data
wheat_coord <- wheat[,1:2]
rapeseed_coord <- rapeseed[,1:2]
coordinates(wheat_coord) <- c("lon", "lat")
coordinates(rapeseed_coord) <- c("lon", "lat")

# Setting proper CRS for the coordinate system
proj4string(wheat_coord) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
proj4string(rapeseed_coord) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
farm_coordinates <- list(wheat_coord,rapeseed_coord)

# -----------------------------------------
# Temperature data
# -----------------------------------------

# Define start & end date of panel (pick first and last day of year, respectively)
# First part 2000-2010
start_panel <- as.Date("2000-01-01", format ="%Y-%m-%d")
end_panel   <- as.Date("2010-12-31", format="%Y-%m-%d")

Tmin <- stack("data/weather/tn_ens_mean_0.1deg_reg_1995-2010_v27.0e.nc")
Tmax <- stack("data/weather/tx_ens_mean_0.1deg_reg_1995-2010_v27.0e.nc")


# Subset the historical temperature data w.r.t. yield panel
# Each layer in Tmin & Tmax is a year.
first_layer_date <- as.Date(substring(Tmin[[1]]@data@names,2), format = "%Y.%m.%d")
last_layer_date  <- as.Date(substring(Tmin[[dim(Tmin)[3]]]@data@names,2), format = "%Y.%m.%d")
seq_layers_dates <- seq(as.Date(first_layer_date), as.Date(last_layer_date), "days")
sub_layers_panel <- which(seq_layers_dates >= start_panel & seq_layers_dates <= end_panel)

sub_Tmin <- subset(Tmin, sub_layers_panel)
sub_Tmax <- subset(Tmax, sub_layers_panel)

# Remove large dataset from global environment
rm(Tmin,Tmax, first_layer_date,last_layer_date)


# -----------------------------------------
# Match farm locations with weather data
# -----------------------------------------

# Tmin
Tmin_farm_1 <- list()
for (k in 1:2){
  temp <- as.data.frame(matrix(NA,ncol= length(seq_layers_dates[sub_layers_panel]), nrow=nrow(farm_yields[[k]])))
  colnames(temp) <- seq_layers_dates[sub_layers_panel]
  
  for (t in 1:length(sub_layers_panel)){
    temp[1:nrow(farm_yields[[k]]),t] <- raster::extract(sub_Tmin[[t]], farm_coordinates[[k]])
  }
  Tmin_farm_1[[k]] <- temp 
  rm(temp)
}


# Tmax
Tmax_farm_1 <- list()
for (k in 1:2){
  temp <- as.data.frame(matrix(NA,ncol= length(seq_layers_dates[sub_layers_panel]), nrow=nrow(farm_yields[[k]])))
  colnames(temp) <- seq_layers_dates[sub_layers_panel]
  
  for (t in 1:length(sub_layers_panel)){
    temp[1:nrow(farm_yields[[k]]),t] <- raster::extract(sub_Tmax[[t]], farm_coordinates[[k]])
  }
  Tmax_farm_1[[k]] <- temp 
  rm(temp)
}

# Remove large dataset from global environment
rm(sub_Tmin,sub_Tmax)


# Second Part 2011-2018
start_panel <- as.Date("2011-01-01", format ="%Y-%m-%d")
end_panel   <- as.Date("2018-12-31", format="%Y-%m-%d")

Tmin <- stack("data/weather/tn_ens_mean_0.1deg_reg_2011-2022_v27.0e.nc")
Tmax <- stack("data/weather/tx_ens_mean_0.1deg_reg_2011-2022_v27.0e.nc")



# Subset the historical temperature data w.r.t. yield panel
# Each layer in Tmin & Tmax is a year.
first_layer_date <- as.Date(substring(Tmin[[1]]@data@names,2), format = "%Y.%m.%d")
last_layer_date  <- as.Date(substring(Tmin[[dim(Tmin)[3]]]@data@names,2), format = "%Y.%m.%d")
seq_layers_dates <- seq(as.Date(first_layer_date), as.Date(last_layer_date), "days")
sub_layers_panel <- which(seq_layers_dates >= start_panel & seq_layers_dates <= end_panel)

sub_Tmin <- subset(Tmin, sub_layers_panel)
sub_Tmax <- subset(Tmax, sub_layers_panel)

# Remove large dataset from global environment
rm(Tmin,Tmax, first_layer_date,last_layer_date)


# -----------------------------------------
# Match farm locations with weather data
# -----------------------------------------

# Tmin
Tmin_farm_2 <- list()
for (k in 1:2){
  temp <- as.data.frame(matrix(NA,ncol= length(seq_layers_dates[sub_layers_panel]), nrow=nrow(farm_yields[[k]])))
  colnames(temp) <- seq_layers_dates[sub_layers_panel]
  
  for (t in 1:length(sub_layers_panel)){
    temp[1:nrow(farm_yields[[k]]),t] <- raster::extract(sub_Tmin[[t]], farm_coordinates[[k]])
  }
  Tmin_farm_2[[k]] <- temp 
  rm(temp)
}


# Tmax
Tmax_farm_2 <- list()
for (k in 1:2){
  temp <- as.data.frame(matrix(NA,ncol= length(seq_layers_dates[sub_layers_panel]), nrow=nrow(farm_yields[[k]])))
  colnames(temp) <- seq_layers_dates[sub_layers_panel]
  
  for (t in 1:length(sub_layers_panel)){
    temp[1:nrow(farm_yields[[k]]),t] <- raster::extract(sub_Tmax[[t]], farm_coordinates[[k]])
  }
  Tmax_farm_2[[k]] <- temp 
  rm(temp)
}

# Remove large dataset from global environment
rm(sub_Tmin,sub_Tmax)


# Combine both parts
Tmin_farm <- list()
for(i in 1:2){
  Tmin_farm[[i]] <- cbind(Tmin_farm_1[[i]],Tmin_farm_2[[i]])
}

Tmax_farm <- list()
for(i in 1:2){
  Tmax_farm[[i]] <- cbind(Tmax_farm_1[[i]],Tmax_farm_2[[i]])
}


# -----------------------------------------
# Preparing Phenology Data
# -----------------------------------------

# Defining CRS
crs <- proj4string(geolocshp)
print(crs)

# Extract the coordinates from the SpatialPointsDataFrame object
coords <- coordinates(geolocshp)

# Extract the longitude and latitude coordinates
lons <- coords[,1]
lats <- coords[,2]

# Specify the CRS that you want to transform the object to
crs_target <- "+proj=longlat +datum=WGS84 +no_defs"

# Use spTransform() to transform the object to the new CRS
shape_transformed <- spTransform(geolocshp, CRS(crs_target))

# Extract the coordinates from the SpatialPointsDataFrame object
coords_transformed <- coordinates(shape_transformed)

# Extract the longitude and latitude coordinates
lons_trans <- coords_transformed[,1]
lats_trans <- coords_transformed[,2]
shape_data <- as.data.frame(shape_transformed)
shape_data$longitude <- NULL
shape_data$latitude <- NULL
plot(shape_transformed)


# wheat Beginning
years <- 2000:2018
list_dt <- list()

# Rounding is used for tif files to determine the dates
for(i in 1:length(years)){
  
  file_nm <- paste0("data/tif/DOY_202-15_",years[i],'.tif')
  DOY_tmp <- raster(file_nm)
  raster_crs <- proj4string(DOY_tmp)
  shape_raster_crs <- spTransform(shape_transformed, CRS(raster_crs))
  extracted_tmp <- raster::extract(DOY_tmp,shape_raster_crs,df = TRUE)[,2]
  extracted_tmp <- as.Date(round(extracted_tmp),
                           origin=paste0(years[i],"-01-01"))-1
  list_dt[[i]] <- data.frame(extracted_tmp)
}

# Binding list_dt as data.frame
wheat_start <- do.call(cbind,list_dt)
names(wheat_start) <- paste0('start_',2000:2018)

# wheat Ending
list_dt <- list()

# Rounding is used for tif files to determine the dates
for(i in 1:length(years)){
  
  file_nm <- paste0("data/tif/DOY_202-19_",years[i],'.tif')
  DOY_tmp <- raster(file_nm)
  raster_crs <- proj4string(DOY_tmp)
  shape_raster_crs <- spTransform(shape_transformed, CRS(raster_crs))
  extracted_tmp <- raster::extract(DOY_tmp,shape_raster_crs,df = TRUE)[,2]
  extracted_tmp <- as.Date(round(extracted_tmp),
                           origin=paste0(years[i],"-01-01"))-1
  list_dt[[i]] <- data.frame(extracted_tmp)
}

# Binding list_dt as data.frame
wheat_end <- do.call(cbind,list_dt)
names(wheat_end) <- paste0('end_',years)


# Rapeseed Beginning
list_dt <- list()

# Rounding is used for tif files to determine the dates
for(i in 1:length(years)){
  
  file_nm <- paste0("data/tif/DOY_205-17_",years[i],'.tif')
  DOY_tmp <- raster(file_nm)
  raster_crs <- proj4string(DOY_tmp)
  shape_raster_crs <- spTransform(shape_transformed, CRS(raster_crs))
  extracted_tmp <- raster::extract(DOY_tmp,shape_raster_crs,df = TRUE)[,2]
  extracted_tmp <- as.Date(round(extracted_tmp),
                           origin=paste0(years[i],"-01-01"))-1
  list_dt[[i]] <- data.frame(extracted_tmp)
}

# Binding list_dt as data.frame
rapeseed_start <- do.call(cbind,list_dt)
names(rapeseed_start) <- paste0('start_',years)


# Rapeseed Ending
list_dt <- list()

# Rounding is used for tif files to determine the dates
for(i in 1:length(years)){
  
  file_nm <- paste0("data/tif/DOY_205-22_",years[i],'.tif')
  DOY_tmp <- raster(file_nm)
  raster_crs <- proj4string(DOY_tmp)
  shape_raster_crs <- spTransform(shape_transformed, CRS(raster_crs))
  extracted_tmp <- raster::extract(DOY_tmp,shape_raster_crs,df = TRUE)[,2]
  extracted_tmp <- as.Date(round(extracted_tmp),
                           origin=paste0(years[i],"-01-01"))-1
  list_dt[[i]] <- data.frame(extracted_tmp)
}

# Binding list_dt as data.frame
rapeseed_end <- do.call(cbind,list_dt)
names(rapeseed_end) <- paste0('end_',years)


# rapeseed: start = bud formation, end = ripeness
PhenoFarm_rapeseed_start <- rapeseed_start
PhenoFarm_rapeseed_end   <- rapeseed_end
PhenoFarm_wheat_start <- wheat_start
PhenoFarm_wheat_end   <- wheat_end

# ==============================================================================================================
# 3. Calculation of hourly temperature exposure during the risk period 
# ==============================================================================================================

# -----------------------------------------
# Winter wheat
# -----------------------------------------

# Hourly steps in unit radian
# By 2pi/24 to set the hour steps, and add/substract 2*pi*1/48 to estimate at the middle of the hour
# First seq() is for first half-day; second seq() for second half-day
radian_observations <- c(seq(((-0.5*pi)+(2*pi/48)),(0.5*pi-(2*pi/48)), by=(2*pi/24)), seq((( 0.5*pi)+(2*pi/48)),(1.5*pi-(2*pi/48)), by=(2*pi/24)))
last_year <- 2018

# List for each farm: farm_wheat_exposure [[i]] [[t]] with i == farm, t == year
farm_wheat_exposure <- list()

for (i in 1:nrow(farm_yields[[1]])){
  # Temperature exposure of farm i n years t; each element of in "years_exposure" is a year of farm i.
  years_exposure <- list()
  
  for (y in 1:(last_year - 2000 + 1)){
    start_risk <- as.Date(PhenoFarm_wheat_start[i,y], formate = "%Y-%m-%d")
    # End the risk period before entrance into new growth phase (-1)
    end_risk   <- as.Date(PhenoFarm_wheat_end [i,y], formate = "%Y-%m-%d")-1
    
    # Create data frame for hourly temperature readings
    daily_temperature_readings            <- as.data.frame(matrix(NA, nrow= as.numeric(end_risk - start_risk +1), ncol=24))
    row.names(daily_temperature_readings) <- seq(start_risk, end_risk,1)
    colnames(daily_temperature_readings)  <- seq(0,23,1)
    
    for (d in 1:nrow(daily_temperature_readings)){
      
      # Get the relevant temperature data of this day
      Tmin_t  <- Tmin_farm[[1]][i,as.character(row.names(daily_temperature_readings)[d])]
      Tmin_t1 <- Tmin_farm[[1]][i,as.character(as.Date(row.names(daily_temperature_readings)[d])+1)]
      Tmax_t  <- Tmax_farm[[1]][i,as.character(row.names(daily_temperature_readings)[d])]
      
      # Get amplitude of first half (fh) and second half (sh) of the day
      amplitude_fh <- (Tmax_t - Tmin_t) / 2
      amplitude_sh <- (Tmax_t - Tmin_t1)/ 2
      
      # Get daily average of first half and second half of the day
      Tmean_fh <- (Tmax_t + Tmin_t) / 2
      Tmean_sh <- (Tmax_t + Tmin_t1)/ 2
      
      # Calculate the hourly temperature of the first half of the day (first sine curve)
      # Daily observation starts 30' after Tmin
      
      # Get hourly temperatures for the first half of the day (first sine curve)
      for (fh in 1:12){
        daily_temperature_readings[d,fh] <- Tmean_fh + amplitude_fh * sin(radian_observations[fh])
      }
      
      # Get hourly temperatures for the second half of the day (second sine curve)
      for (sh in 1:12){
        daily_temperature_readings[d,12+sh] <- Tmean_sh + amplitude_sh * sin(radian_observations[12+sh])
      }
    }
    
    years_exposure[[y]] <- daily_temperature_readings
    rm(daily_temperature_readings)
    years_exposure[[y]]$date<-seq(start_risk, end_risk,1)
  }
  farm_wheat_exposure[[i]] <- years_exposure 
  rm(years_exposure)
}


# -----------------------------------------
# Winter Rapeseed
# -----------------------------------------

# List for each farm: farm_rapeseed_exposure [[i]] [[t]] with i == farm, t == year
farm_rapeseed_exposure <- list()

for (i in 1:nrow(farm_yields[[2]])){
  # Temperature exposure of farm i n years t; each element of in "years_exposure" is a year of farm i.
  years_exposure <- list()
  
  for (y in 1:(last_year - 2000 + 1)){
    start_risk <- as.Date(PhenoFarm_rapeseed_start[i,y], formate = "%Y-%m-%d")
    # End the risk period before entrance into new growth phase (-1)
    end_risk   <- as.Date(PhenoFarm_rapeseed_end [i,y], formate = "%Y-%m-%d")-1
    
    # Create data frame for hourly temperature readings
    daily_temperature_readings            <- as.data.frame(matrix(NA, nrow= as.numeric(end_risk - start_risk +1), ncol=24))
    row.names(daily_temperature_readings) <- seq(start_risk, end_risk,1)
    colnames(daily_temperature_readings)  <- seq(0,23,1)
    
    for (d in 1:nrow(daily_temperature_readings)){
      
      # Get the relevant temperature data of this day
      Tmin_t  <- Tmin_farm[[2]][i,as.character(row.names(daily_temperature_readings)[d])]
      Tmin_t1 <- Tmin_farm[[2]][i,as.character(as.Date(row.names(daily_temperature_readings)[d])+1)]
      Tmax_t  <- Tmax_farm[[2]][i,as.character(row.names(daily_temperature_readings)[d])]
      
      # Get amplitude of first half (fh) and second half (sh) of the day
      amplitude_fh <- (Tmax_t - Tmin_t) / 2
      amplitude_sh <- (Tmax_t - Tmin_t1)/ 2
      
      # Get daily average of first half and second half of the day
      Tmean_fh <- (Tmax_t + Tmin_t) / 2
      Tmean_sh <- (Tmax_t + Tmin_t1)/ 2
      
      # Calculate the hourly temperature of the first half of the day (first sine curve)
      # Daily observation starts 30' after Tmin
      
      # Get hourly temperatures for the first half of the day (first sine curve)
      for (fh in 1:12){
        daily_temperature_readings[d,fh] <- Tmean_fh + amplitude_fh * sin(radian_observations[fh])
      }
      
      # Get hourly temperatures for the second half of the day (second sine curve)
      for (sh in 1:12){
        daily_temperature_readings[d,12+sh] <- Tmean_sh + amplitude_sh * sin(radian_observations[12+sh])
      }
    }
    
    years_exposure[[y]] <- daily_temperature_readings
    rm(daily_temperature_readings)
    years_exposure[[y]]$date <- seq(start_risk, end_risk,1)
  }
  farm_rapeseed_exposure[[i]] <- years_exposure 
  rm(years_exposure)
}



# ==============================================================================================================
# 4. Restructuring data
# ==============================================================================================================

# The following steps facilitate data handling in insurance calibration

# -----------------------------------------
# Winter wheat
# -----------------------------------------

# wheat yields
wheat_yield_melted           <- reshape2::melt(cbind(farm_yields[[1]][,-c(1:2)],id=1:nrow(farm_yields[[1]])),id="id")
colnames(wheat_yield_melted) <- c("Farm","year","yield")
wheat_yield_melted$year      <- as.numeric(as.character(wheat_yield_melted$year))

# Temperature exposure wheat
temp_wheat_exposure1           <- rbindlist(lapply(farm_wheat_exposure, rbindlist),idcol=T)
temp_wheat_exposure2           <- reshape2::melt(temp_wheat_exposure1, id=c(".id","date"))
colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
temp_wheat_exposure2$year      <- year(temp_wheat_exposure2$Date)

# -----------------------------------------
# winter rapeseed
# -----------------------------------------

# rapeseed yields
rapeseed_yield_melted           <-reshape2::melt(cbind(farm_yields[[2]][,-c(1:2)],id=1:nrow(farm_yields[[2]])),id="id")
colnames(rapeseed_yield_melted) <-c("Farm","year","yield")
rapeseed_yield_melted$year      <-as.numeric(as.character(rapeseed_yield_melted$year))

# Temperature exposure rapeseed
temp_rapeseed_exposure1         <- rbindlist(lapply(farm_rapeseed_exposure, rbindlist),idcol=T)
temp_rapeseed_exposure2          <- reshape2::melt(temp_rapeseed_exposure1, id=c(".id","date"))
colnames(temp_rapeseed_exposure2) <- c("Farm","Date","Hour", "Temperature")
temp_rapeseed_exposure2$year<- year(temp_rapeseed_exposure2$Date)


# ==============================================================================================================
# 5. Expected utility model
# ==============================================================================================================

# Coefficients of constant relative risk aversion
# 0.5 == slightly risk-averse, 2 == moderately risk-averse, 4 == extremely risk-averse
alpha <- 2

# Power utility function
utility_function <- function(alpha,revenue){
  
  if(alpha == 1){
    log(revenue)
  } else {
    ((revenue^(1 - alpha)) / (1 - alpha))
  }
}

# Inverse utility function (to derive certainty equivalents)
inverse_utility_function <- function(alpha,eu){
  
  if(alpha == 1){
    exp(eu)
  } else {
    (eu*(1-alpha))^(1/(1-alpha))
  }
}


# ==============================================================================================================
# 6. Expected utility model: Uninsured status (base-line scenario)
# ==============================================================================================================

# Step 1: Detrending yields 
# Step 2: Calculate expected utility
# Note that we report revenue in dt/ha and not on monetary terms.

# -----------------------------------------
# Winter wheat
# -----------------------------------------

lin_trend_wheat     <- rlm(yield ~ year , data = wheat_yield_melted, method="M")
quadr_trend_wheat  <- rlm(yield ~ year + I(year^2) , data=wheat_yield_melted, method="M")

# Use Akaike information criterion (AIC) to identify more appropriate trend
AIC(lin_trend_wheat)
AIC(quadr_trend_wheat)

# AIC favors quadratic trend for farm-individual winter wheat yields (lower value)
# Calculation of detrended yields using a quadratic trend
detr_wheat_yields <- as.data.frame(matrix(NA, nrow=nrow(farm_yields[[1]]), ncol=length(seq(2000,last_year,1))))
colnames(detr_wheat_yields) <- seq(2000,last_year,1)

for (i in 1:nrow(detr_wheat_yields)){  
  for (t in 1:length(seq(2000,last_year,1))){
    cat('\014')
    cat(i)
    detr_wheat_yields[i,t] <- ((coef(quadr_trend_wheat)[1] +last_year * coef(quadr_trend_wheat)[2] + (last_year^2) * coef(quadr_trend_wheat)[3])) - ((coef(quadr_trend_wheat)[1] + as.numeric(colnames(detr_wheat_yields)[t]) * coef(quadr_trend_wheat)[2] + (as.numeric(colnames(detr_wheat_yields)[t]))^2 * coef(quadr_trend_wheat)[3])) +  farm_yields[[1]][i,colnames(detr_wheat_yields)[t]]
    
  }
}

# Utility of uninsured status
utility_wheat_uninsured <- as.data.frame(matrix(NA, nrow=nrow(farm_yields[[1]]), ncol = length(seq(2000,last_year,1))))
for (i in 1:nrow(farm_yields[[1]])){
  for (t in 1:length(seq(2000,last_year,1))){
    utility_wheat_uninsured[i,t] <- utility_function(alpha = alpha, revenue = detr_wheat_yields[i,t])
  }
}

# Expected utility statistics
summary_EU_wheat_uninsured <- as.data.frame(matrix(NA, nrow=nrow(farm_yields[[1]]),ncol=4))
colnames(summary_EU_wheat_uninsured) <- c("EU", "Expected Revenue", "CE", "Risk Premium")

for (i in 1:nrow(farm_yields[[1]])){
  
  # Expected utility
  summary_EU_wheat_uninsured [i,1] <- mean(as.numeric(utility_wheat_uninsured[i,]), na.rm=T)
  
  # Expected Revenue
  summary_EU_wheat_uninsured [i,2] <- mean(as.numeric(detr_wheat_yields[i,]), na.rm=T)
  
  # Certainty equivalent
  summary_EU_wheat_uninsured [i,3] <- inverse_utility_function(alpha = alpha, eu = as.numeric(summary_EU_wheat_uninsured[i,1])) 
  
  # Risk Premium
  summary_EU_wheat_uninsured [i,4] <- summary_EU_wheat_uninsured [i,2] - summary_EU_wheat_uninsured [i,3] 
}

# -----------------------------------------
# rapeseed
# -----------------------------------------

lin_trend_rapeseed   <- rlm(yield ~ year , data = rapeseed_yield_melted, method="M")
quadr_trend_rapeseed <- rlm(yield ~ year + I(year^2) , data=rapeseed_yield_melted, method="M")

# Use Akaike information criterion (AIC) to identify more appropriate trend
AIC(lin_trend_rapeseed)
AIC(quadr_trend_rapeseed)

# AIC favors quadratic trend for farm-individual rapeseed yields (lower value)
# Calculation of detrended yields using a quadratic trend

detr_rapeseed_yields <- as.data.frame(matrix(NA, nrow=nrow(farm_yields[[2]]), ncol=length(seq(2000,last_year,1))))
colnames(detr_rapeseed_yields) <- seq(2000,last_year,1)

for (i in 1:nrow(detr_rapeseed_yields)){  
  for (t in 1:length(seq(2000,last_year,1))){
    
    detr_rapeseed_yields[i,t] <- detr_rapeseed_yields[i,t] <- ((coef(quadr_trend_rapeseed)[1] +last_year * coef(quadr_trend_rapeseed)[2] + (last_year^2) * coef(quadr_trend_rapeseed)[3]))  - ((coef(quadr_trend_rapeseed)[1] + as.numeric(colnames(detr_rapeseed_yields)[t]) * coef(quadr_trend_rapeseed)[2] + (as.numeric(colnames(detr_rapeseed_yields)[t]))^2 * coef(quadr_trend_rapeseed)[3])) +  farm_yields[[2]][i,colnames(detr_rapeseed_yields)[t]]
    
  }
}

# Utility of uninsured status
utility_rapeseed_uninsured <- as.data.frame(matrix(NA, nrow=nrow(farm_yields[[2]]), ncol = length(seq(2000,last_year,1))))
for (i in 1:nrow(farm_yields[[2]])){
  for (t in 1:length(seq(2000,last_year,1))){
    utility_rapeseed_uninsured[i,t] <- utility_function(alpha = alpha, revenue = detr_rapeseed_yields[i,t])
  }
}

# Expected utility statistics
summary_EU_rapeseed_uninsured <- as.data.frame(matrix(NA, nrow=nrow(farm_yields[[2]]),ncol=4))
colnames(summary_EU_rapeseed_uninsured) <- c("EU", "Expected Revenue", "CE", "Risk Premium")

for (i in 1:nrow(farm_yields[[2]])){
  
  # Expected utility
  summary_EU_rapeseed_uninsured [i,1] <- mean(as.numeric(utility_rapeseed_uninsured[i,]), na.rm=T)
  
  # Expected Revenue
  summary_EU_rapeseed_uninsured [i,2] <- mean(as.numeric(detr_rapeseed_yields[i,]), na.rm=T)
  
  # Certainty equivalent
  summary_EU_rapeseed_uninsured [i,3] <- inverse_utility_function(alpha = alpha, eu = as.numeric(summary_EU_rapeseed_uninsured[i,1])) 
  
  # Risk Premium
  summary_EU_rapeseed_uninsured [i,4] <- summary_EU_rapeseed_uninsured [i,2] - summary_EU_rapeseed_uninsured [i,3] 
}



# ==============================================================================================================
# 7. Winter wheat: out-of-sample calibration and testing for three knots
# ==============================================================================================================

# -------------------------------------------
# Definition of knots (out-of-sample)
# -------------------------------------------

# List with knots for each farm and model: [[i]] == farm; [knot,model]
list_knot_locations_kn3_wheat <- list()

for (i in 1:nrow(farm_yields[[1]])){
  # Leave-out data from farm i
  temp_sample <- temp_wheat_exposure2[which(temp_wheat_exposure2$Farm != i),]
  temp_knot_df <- as.data.frame(matrix(NA, nrow=3, ncol=3))
  row.names(temp_knot_df) <- c("knot1", "knot2", "knot3")
  colnames(temp_knot_df)  <- c("Best fit","Equal", "Quantile") 
  
  # Model 1: Best fit
  
  # lower and upper bound define lowest and highest knot location
  lower_bound <- ceiling(quantile(temp_sample$Temperature,0.05, type=1))
  upper_bound <- floor(quantile(temp_sample$Temperature,0.95, type=1))
  temp_range <- seq(lower_bound, upper_bound,1) 
  kn3_combi <- combn(temp_range,3, simplify = T)
  
  # Minimum space between 2 knots
  required_space <- 5
  
  # Get all knot combinations
  differences <- matrix(NA, nrow=3, ncol=ncol(kn3_combi))
  differences[1,] <- abs(kn3_combi[1,] - kn3_combi[2,])
  differences[2,] <- abs(kn3_combi[1,] - kn3_combi[3,])
  differences[3,] <- abs(kn3_combi[2,] - kn3_combi[3,])
  knots_3_combi_good <- kn3_combi[,which(differences[1,] >= required_space & differences[2,] >= required_space & differences[3,] >= required_space)]
  
  # vector containing residual sum of squares (RSS)
  RSS_3knots <- vector(length=ncol(knots_3_combi_good ))
  
  # Get the RSS for each model
  for (c in 1:ncol(knots_3_combi_good)){
    
    # Get new time series and aggregate hourly values  
    temp1 <- as.matrix(rcspline.eval(temp_sample$Temperature,knots= knots_3_combi_good[,c], inclx=T))
    temp2 <- cbind(temp_sample,temp1)
    
    temp3<-temp2 %>%
      group_by(Farm,year)%>%
      summarise_at(vars(x,V2),sum)
    
    # Match yearly values with yearly yields
    sub_wheat_yield_melted <- wheat_yield_melted[which(wheat_yield_melted$Farm != i),]
    temp4 <- join(sub_wheat_yield_melted,temp3, by=c("Farm", "year"))
    
    # Run regression
    temp_reg <- lm(yield ~ x + V2 + year + I(year^2) + as.factor(Farm)-1, data = temp4)
    RSS_3knots[c] <- sum(resid(temp_reg)^2)
    
    rm(temp_reg,temp4,sub_wheat_yield_melted,temp3,temp2,temp1)
    
    print(paste(round(c/ncol(knots_3_combi_good)*100,2),"% for farm ",i, round(i/nrow(farm_yields[[1]])*100,2)))
    
  }
  
  # Best number of knots
  temp_knot_df[,1] <- knots_3_combi_good[,which.min(RSS_3knots)]
  rm(RSS_3knots, knots_3_combi_good, differences, kn3_combi,temp_range,upper_bound, lower_bound)
  
  
  # Model 2: equally spaced
  temp_knot_df[,2] <- c(min(temp_sample$Temperature) + ((max(temp_sample$Temperature)-min(temp_sample$Temperature)) / 4),
                        min(temp_sample$Temperature) + 2* ((max(temp_sample$Temperature)-min(temp_sample$Temperature)) / 4),
                        min(temp_sample$Temperature) + 3* ((max(temp_sample$Temperature)-min(temp_sample$Temperature)) / 4))
  
  
  
  # Model 3: 10%, 50%, 90% quantile
  temp_knot_df[,3] <- c(quantile(temp_sample$Temperature,0.1, type=1),
                        quantile(temp_sample$Temperature,0.5, type=1),
                        quantile(temp_sample$Temperature,0.9, type=1))
  
  
  
  list_knot_locations_kn3_wheat[[i]] <-  temp_knot_df
  rm(temp_sample, temp_knot_df)
  print(i / nrow(farm_yields[[1]]))
}

#save.image('env_until_733_d2last.RData')

# Testing whether model with best fit is better than linear model
# Leave-out data from farm i

# 1 = cubic spline is superior to linear model
comparison_vector <- vector()

for (i in 1:nrow(farm_yields[[1]])){
  temp_sample <- temp_wheat_exposure2[which(temp_wheat_exposure2$Farm != i),]
  print(i)
  
  # The RCS model
  # Get new time series and aggregate hourly values  
  temp1 <- as.matrix(rcspline.eval(temp_sample$Temperature,knots= list_knot_locations_kn3_wheat[[i]][,1], inclx=T))
  temp2 <- cbind(temp_sample,temp1)
  
  temp3<-temp2 %>%
    group_by(Farm,year)%>%
    summarise_at(vars(x,V2),sum)
  
  # Match yearly values with yearly yields
  sub_wheat_yield_melted <- wheat_yield_melted[which(wheat_yield_melted$Farm != i),]
  temp4 <- join(sub_wheat_yield_melted,temp3, by=c("Farm", "year"))
  
  # Run cubic model (restricted cubic spline model)
  temp_cub_reg <- lm(yield ~ x + V2 + year + I(year^2) + as.factor(Farm)-1, data = temp4)
  
  # Run linear model
  temp_lin_reg <- lm(yield ~ x + year + I(year^2) + as.factor(Farm)-1, data = temp4)
  
  AIC_lin <- AIC(temp_lin_reg)
  AIC_cub <- AIC(temp_cub_reg)
  
  if (AIC_cub < AIC_lin){
    comparison_vector[i] <- 1
  } else {comparison_vector[i] <- 0}
  
  rm(temp_sample, temp1, temp2, temp3,sub_wheat_yield_melted,temp_cub_reg, temp_lin_reg,AIC_lin, AIC_cub)
}

# Check 
comparison_vector
rm(comparison_vector)


# -------------------------------------------
# Out-of-sample calibration: leave-out-farm i
# -------------------------------------------

# Yields together with new hourly temperature time series: [[i]] == farm; [[m]] == new time series for knot specification m
list_wheat_matrix_modelkn3_aggregated <- list() # [[i]][[m]]

# New time series of hourly temperature exposures: [[i]] == farm; [[m]] == new time series for knot specification m
list_wheat_matrix_modelkn3_hourly <- list() # [[i]][[m]]

for (i in 1:nrow(farm_yields[[1]])){
  temp_hourly <- list() #[[m]]
  temp_aggregated <- list() # [[m]]
  print(i)
  
  for (m in 1:ncol(list_knot_locations_kn3_wheat[[i]])){
    print(paste0('m:',m))
    # Get new time series for RCS   
    temp1 <- as.matrix(rcspline.eval(temp_wheat_exposure2$Temperature,knots= list_knot_locations_kn3_wheat[[i]][,m], inclx=T))
    temp2 <- cbind(temp_wheat_exposure2, temp1)
    
    # Aggregate to yearly values
    temp3 <- temp2 %>%
      group_by(Farm,year)%>%
      summarise_at(vars(x,V2),sum)
    
    temp_hourly[[m]]     <- temp2
    temp_aggregated[[m]] <- join(wheat_yield_melted,temp3, by=c("Farm", "year"))
    
    rm(temp1,temp2,temp3)
  }
  list_wheat_matrix_modelkn3_aggregated[[i]] <- temp_aggregated
  list_wheat_matrix_modelkn3_hourly[[i]] <- temp_hourly
  
  rm(temp_aggregated, temp_hourly)
}


# Now, we have all data together (yields and temperature time series)
# We continue with out-of-sample regression. See the paper for more details.

# List with model outputs. [[i]] == farm i; [[m]] == knot specification m
list_farm_wheat_panelmodelkn3_modeloutputs <- list()

for (i in 1:nrow(farm_yields[[1]])){
  print(i)
  # Contains the m models of farm i.
  list_farm_wheat_panelmodelkn3_models.of.farm <- list() 
  for (m in 1:ncol(list_knot_locations_kn3_wheat[[i]])){
    print(paste0('m ',m))
    # Aggregated data without farm i (leave-out-farm i for out-of-sample training)
    temp1 <- subset(list_wheat_matrix_modelkn3_aggregated[[i]][[m]], list_wheat_matrix_modelkn3_aggregated[[i]][[m]] [,"Farm"] != i)
    
    # Run panel regression
    temp_reg <- lm(yield ~ x + V2 + year + I(year^2) + as.factor(Farm)-1, data = temp1)
    list_farm_wheat_panelmodelkn3_models.of.farm[[m]] <- temp_reg
    
    rm(temp1,temp_reg)
    
  }
  
  list_farm_wheat_panelmodelkn3_modeloutputs [[i]] <- list_farm_wheat_panelmodelkn3_models.of.farm   
  rm(list_farm_wheat_panelmodelkn3_models.of.farm)
}


# Calculation of hourly effects (marginal temperature effect estimated at farm i)
list_farm_wheat_hourly_effect_kn3 <- list() # [[farm]] [[model]][]

for (i in 1:nrow(farm_yields[[1]])){
  print(i)  
  temp_list <- list()
  for (m in 1:ncol(list_knot_locations_kn3_wheat[[i]])){
    
    print(paste0('m ',m))
    # Subset of farm i
    temp1 <- which(list_wheat_matrix_modelkn3_hourly[[i]][[m]][,"Farm"] == i) 
    temp2 <- list_wheat_matrix_modelkn3_hourly[[i]][[m]][temp1,]  
    
    # Calculate temperature effect
    temp2$effect <- as.matrix(temp2[,6:7]) %*% coef(list_farm_wheat_panelmodelkn3_modeloutputs [[i]][[m]])[1:2]
    
    temp_list[[m]] <- temp2  
    rm(temp1,temp2)  
  }
  list_farm_wheat_hourly_effect_kn3[[i]] <- temp_list
  rm(temp_list)
}

# -------------------------------------------
# Historical payouts & premium
# -------------------------------------------

# Different strike level temperatures
strike_temperature <- c(seq(13,36,by=1))

# Historical payouts with following structure: [[i]] == of farm i, [[m]] with model specification m, 
# [[m]] contains a data.frame with [strike level, year]
list_farm_wheat_payouts_kn3 <- list() 
for (i in 1:nrow(farm_yields[[1]])){
  
  # temp list for specification[[m]]
  temp_list <- list() 
  for (m in 1:ncol(list_knot_locations_kn3_wheat[[i]])){
    
    temp_model <- list_farm_wheat_hourly_effect_kn3 [[i]] [[m]]
    
    # DF for payouts
    temp_df <- as.data.frame(matrix(NA, nrow=length(strike_temperature), ncol=length(seq(2000,last_year,1))))
    row.names(temp_df) <- strike_temperature 
    colnames(temp_df) <- seq(2000,last_year,1)
    
    for (t in 1:ncol(temp_df)){
      print(paste('i:',i,' m:',m,' t:',t))
      # Subset of year
      sub_model_df <- subset(temp_model, temp_model$year == as.numeric(colnames(temp_df)[t]))
      # Subset only negative expected impacts trigger a payout 
      sub2_model_df <- subset(sub_model_df, sub_model_df$effect < 0)
      
      for (strike in 1:length(strike_temperature)){
        
        # Subset only temperature above the strike level trigger a payout
        sub3_model_df <- subset(sub2_model_df, sub2_model_df$Temperature >= strike_temperature[strike]) 
        
        # Calculate the cumulative payout
        temp_df[strike,t] <- sum(sub3_model_df$effect) * (-1)
        rm(sub3_model_df)
      }
      
      rm(sub2_model_df, sub_model_df)  
    }
    temp_list[[m]] <- temp_df
    rm(temp_df,temp_model)
  }
  list_farm_wheat_payouts_kn3[[i]] <- temp_list
  rm(temp_list)
} 

# Calculate the premium
# Structure: [farm i, model m, strike level]
array_premium_wheat_kn3 <- array(dim=c(nrow(farm_yields[[1]]), ncol(list_knot_locations_kn3_wheat[[i]]),length(strike_temperature)))
dimnames(array_premium_wheat_kn3)[[3]] <- strike_temperature

for (i in 1:nrow(farm_yields[[1]])){
  for (m in 1:ncol(list_knot_locations_kn3_wheat[[i]])){
    print(paste0('i:',i,'m:',m))
    for (strike in 1:length(strike_temperature)){
      temp1 <- which(!is.na(detr_wheat_yields[i,]))
      array_premium_wheat_kn3[i,m,strike] <- mean(as.numeric(list_farm_wheat_payouts_kn3[[i]][[m]][strike, temp1]), na.rm=T)
      rm(temp1)
    }
  }
}


# -------------------------------------------
# Insured revenue and expected utility
# -------------------------------------------

# Calculation of revenue [[strike]][m,i,t]
list_farm_wheat_wealth_modelskn3_strike <- list()

for (strike in 1:length(strike_temperature)){
  
  array_farm_wheat_wealth_modelskn3 <- array(dim=c(ncol(list_knot_locations_kn3_wheat[[1]]), nrow(farm_yields[[1]]), length(seq(2000,last_year,1))))
  dimnames(array_farm_wheat_wealth_modelskn3) [[3]] <-seq(2000,last_year,1) 
  
  for (m in 1:ncol(list_knot_locations_kn3_wheat[[1]])){
    for (i in 1:nrow(farm_yields[[1]])){
      print(paste0('strike:',strike,' m:',m,' i:',i))
      for (t in 1: length(seq(2000,last_year,1))){
        
        array_farm_wheat_wealth_modelskn3 [m,i,t] <- as.numeric(detr_wheat_yields[i,t]) + as.numeric(list_farm_wheat_payouts_kn3 [[i]][[m]] [strike,t]) - as.numeric(array_premium_wheat_kn3[i,m,strike]) 
        
      }
    }
  }
  
  list_farm_wheat_wealth_modelskn3_strike [[strike]] <- array_farm_wheat_wealth_modelskn3 
  rm(array_farm_wheat_wealth_modelskn3)
}

# Utility of being insured with specification m.
# List has structure: [[strike]][mi,i,t]
list_farm_wheat_wealth_modelkn3_strike_utility <- list()

for (strike in 1:length(strike_temperature)){
  
  temp_array <- array(dim=c(ncol(list_knot_locations_kn3_wheat[[i]]), nrow(farm_yields[[1]]), length(seq(2000,last_year,1))))
  dimnames(temp_array) [[3]] <-seq(2000,last_year,1) 
  
  for (m in 1:ncol(list_knot_locations_kn3_wheat[[i]])){
    for (i in 1:nrow(farm_yields[[1]])){
      for (t in 1: length(seq(2000,last_year,1))){
        
        temp_array [m,i,t] <- utility_function(alpha = alpha, revenue = list_farm_wheat_wealth_modelskn3_strike [[strike]][m,i,t])
        
      }
    }
  }
  
  list_farm_wheat_wealth_modelkn3_strike_utility  [[strike]] <- temp_array 
  rm(temp_array)
}

# Expected utility statistics of being insured
# Structure of list: 
summary_EU_wheat_insured_kn3 <- list() # [[strike]] [m,i,c(EU,E,CE,RP)]

for (strike in 1:length(strike_temperature)){
  temp_array <- array(dim=c(ncol(list_knot_locations_kn3_wheat[[1]]), nrow(farm_yields[[1]]), 4))
  dimnames(temp_array) [[3]] <- c("EU", "Expected Revenue", "CE", "RP")
  
  for (m in 1:ncol(list_knot_locations_kn3_wheat[[1]])){
    for (i in 1:nrow(farm_yields[[1]])){
      
      # Expected utility
      temp_array  [m,i,1] <- mean(as.numeric(list_farm_wheat_wealth_modelkn3_strike_utility[[strike]][m,i,]), na.rm=T)
      
      # Expected Revenue
      temp_array  [m,i,2] <- mean(as.numeric(list_farm_wheat_wealth_modelskn3_strike[[strike]][m,i,]), na.rm=T)
      
      # Certainty Equivalent
      temp_array  [m,i,3] <- inverse_utility_function(alpha = alpha, eu = as.numeric(temp_array[m,i,1]))
      
      # Risk Premium
      temp_array  [m,i,4] <- temp_array  [m,i,2] -  temp_array  [m,i,3]
    }
  }
  summary_EU_wheat_insured_kn3 [[strike]] <- temp_array
  rm(temp_array)
}

# -------------------------------------------
# Testing
# -------------------------------------------

average_rel_change_wheat_knot3_uninsured <- as.data.frame(matrix(nrow=ncol(list_knot_locations_kn3_wheat[[1]]), ncol=length(strike_temperature)))
colnames(average_rel_change_wheat_knot3_uninsured) <- strike_temperature

for (m in 1:ncol(list_knot_locations_kn3_wheat[[1]])){
  for (strike in 1:length(strike_temperature)){
    
    average_rel_change_wheat_knot3_uninsured [m,strike] <- mean((summary_EU_wheat_insured_kn3[[strike]][m,,4] - summary_EU_wheat_uninsured[,4]) / summary_EU_wheat_uninsured[,4])
    
  }
}

average_rel_change_wheat_knot3_uninsured_test <- as.data.frame(matrix(nrow=ncol(list_knot_locations_kn3_wheat[[1]]), ncol=length(strike_temperature)))
colnames(average_rel_change_wheat_knot3_uninsured_test) <- strike_temperature

for (m in 1:ncol(list_knot_locations_kn3_wheat[[1]])){
  for (strike in 1:length(strike_temperature)){
    
    average_rel_change_wheat_knot3_uninsured_test [m,strike] <- wilcox.test(summary_EU_wheat_insured_kn3[[strike]][m,,4], summary_EU_wheat_uninsured[,4], paired=T, alternative = "l")$p.value
    
  }
}



# ==============================================================================================================
# 8. Rapeseed: out-of-sample calibration and testing for three knots
# ==============================================================================================================

# -------------------------------------------
# Definition of knots (out-of-sample)
# -------------------------------------------

# List with knots for each farm and model: [[i]] == farm; [knot,model]
list_knot_locations_kn3_rapeseed <- list()

for (i in 1:nrow(farm_yields[[2]])){
  
  # Leave-out data from farm i.
  temp_sample <- temp_rapeseed_exposure2[which(temp_rapeseed_exposure2$Farm != i),]
  temp_knot_df <- as.data.frame(matrix(NA, nrow=3, ncol=3))
  row.names(temp_knot_df) <- c("knot1", "knot2", "knot3")
  colnames(temp_knot_df)  <- c("Best fit","Equal", "Quantile") 
  #omitting NA values
  temp_sample <- na.omit(temp_sample)
  # Model 1: Best fit
  
  # lower and upper bound define lowest and highest knot location
  lower_bound <- ceiling(quantile(temp_sample$Temperature,0.05, type=1))
  upper_bound <- floor(quantile(temp_sample$Temperature,0.95, type=1))
  temp_range <- seq(lower_bound, upper_bound,1) 
  kn3_combi <- combn(temp_range,3, simplify = T)
  
  # Minimum space between 2 knots in degree-Celsius
  required_space <- 5
  
  # Get all knot combinations
  differences <- matrix(NA, nrow=3, ncol=ncol(kn3_combi))
  differences[1,] <- abs(kn3_combi[1,] - kn3_combi[2,])
  differences[2,] <- abs(kn3_combi[1,] - kn3_combi[3,])
  differences[3,] <- abs(kn3_combi[2,] - kn3_combi[3,])
  knots_3_combi_good <- kn3_combi[,which(differences[1,] >= required_space & differences[2,] >= required_space & differences[3,] >= required_space)]
  
  # vector containing residual sum of squares (RSS)
  RSS_3knots <- vector(length=ncol(knots_3_combi_good ))
  
  # Get the RSS for each model
  for (c in 1:ncol(knots_3_combi_good)){
    
    # Get new time series and aggregate hourly values  
    temp1 <- as.matrix(rcspline.eval(temp_sample$Temperature,knots= knots_3_combi_good[,c], inclx=T))
    temp2 <- cbind(temp_sample,temp1)
    
    temp3<-temp2 %>%
      group_by(Farm,year)%>%
      summarise_at(vars(x,V2),sum)
    
    # Match yearly values with yearly yields
    sub_rapeseed_yield_melted <- rapeseed_yield_melted[which(rapeseed_yield_melted$Farm != i),]
    temp4 <- join(sub_rapeseed_yield_melted,temp3, by=c("Farm", "year"))
    
    # Run regression
    temp_reg <- lm(yield ~ x + V2 + year + I(year^2) + as.factor(Farm)-1, data = temp4)
    RSS_3knots[c] <- sum(resid(temp_reg)^2)
    
    rm(temp_reg,temp4,sub_rapeseed_yield_melted,temp3,temp2,temp1)
    
    print(paste(c/ncol(knots_3_combi_good),"for farm i", i/nrow(farm_yields[[2]])))
    
  }
  
  # Best number of knots
  temp_knot_df[,1] <- knots_3_combi_good[,which.min(RSS_3knots)]
  rm(RSS_3knots, knots_3_combi_good, differences, kn3_combi,temp_range,upper_bound, lower_bound)
  
  
  # Model 2: equally spaced
  temp_knot_df[,2] <- c(min(temp_sample$Temperature) + ((max(temp_sample$Temperature)-min(temp_sample$Temperature)) / 4),
                        min(temp_sample$Temperature) + 2* ((max(temp_sample$Temperature)-min(temp_sample$Temperature)) / 4),
                        min(temp_sample$Temperature) + 3* ((max(temp_sample$Temperature)-min(temp_sample$Temperature)) / 4))
  
  
  
  # Model 3: 10%, 50%, 90% quantile
  temp_knot_df[,3] <- c(quantile(temp_sample$Temperature,0.1, type=1),
                        quantile(temp_sample$Temperature,0.5, type=1),
                        quantile(temp_sample$Temperature,0.9, type=1))
  
  
  
  list_knot_locations_kn3_rapeseed[[i]] <-  temp_knot_df
  rm(temp_sample, temp_knot_df)
  print(i / nrow(farm_yields[[2]]))
}


# Testing whether model with best fit is better than linear model
# Leave-out data from farm i

# 1 = cubic spline is superior to linear model
comparison_vector <- vector()

for (i in 1:nrow(farm_yields[[2]])){
  temp_sample <- temp_rapeseed_exposure2[which(temp_rapeseed_exposure2$Farm != i),]
  
  # The RCS model
  # Get new time series and aggregate hourly values  
  temp1 <- as.matrix(rcspline.eval(temp_sample$Temperature,knots= list_knot_locations_kn3_rapeseed[[i]][,1], inclx=T))
  temp2 <- cbind(temp_sample,temp1)
  
  temp3 <- temp2 %>%
    group_by(Farm,year)%>%
    summarise_at(vars(x,V2),sum)
  
  # Match yearly values with yearly yields
  sub_rapeseed_yield_melted <- rapeseed_yield_melted[which(rapeseed_yield_melted$Farm != i),]
  temp4 <- join(sub_rapeseed_yield_melted,temp3, by=c("Farm", "year"))
  
  # Run cubic model (restricted cubic spline model)
  temp_cub_reg <- lm(yield ~ x + V2 + year + I(year^2) + as.factor(Farm)-1, data = temp4)
  
  # Run linear model
  temp_lin_reg <- lm(yield ~ x + year + I(year^2) + as.factor(Farm)-1, data = temp4)
  
  AIC_lin <- AIC(temp_lin_reg)
  AIC_cub <- AIC(temp_cub_reg)
  
  if (AIC_cub < AIC_lin){
    comparison_vector[i] <- 1
  } else {comparison_vector[i] <- 0}
  
  rm(temp_sample, temp1, temp2, temp3,sub_rapeseed_yield_melted,temp_cub_reg, temp_lin_reg,AIC_lin, AIC_cub)
  print(i / nrow(farm_yields[[2]]))
}

# Check 
comparison_vector
rm(comparison_vector)
save.image('env_until-1172-d2.RData')
# -------------------------------------------
# Out-of-sample calibration: leave-out-farm i
# -------------------------------------------

# Yields together with new hourly temperature time series: [[i]] == farm, [[m]] == new time series for knot specification m
list_rapeseed_matrix_modelkn3_aggregated <- list() 

# New time series of hourly temperature exposures: [[i]] [[m]] == new time series for knot specification m
list_rapeseed_matrix_modelkn3_hourly <- list()
memory.size(max=T)


for (i in 1:nrow(farm_yields[[2]])){
  temp_hourly <- list() #[[m]]
  temp_aggregated <- list() #[[m]]
  
  for (m in 1:ncol(list_knot_locations_kn3_rapeseed[[i]])){
    
    # Get new time series for RCS   
    temp1 <- as.matrix(rcspline.eval(temp_rapeseed_exposure2$Temperature,knots= list_knot_locations_kn3_rapeseed[[i]][,m], inclx=T))
    temp2 <- cbind(temp_rapeseed_exposure2, temp1)
    
    # Aggregate to yearly values
    temp3<-temp2 %>%
      group_by(Farm,year)%>%
      summarise_at(vars(x,V2),sum)
    
    temp_hourly[[m]] <- temp2
    temp_aggregated[[m]] <- join(rapeseed_yield_melted,temp3, by=c("Farm","year"))
    
    rm(temp1,temp2,temp3)
  }
  
  list_rapeseed_matrix_modelkn3_hourly[[i]]     <- temp_hourly
  list_rapeseed_matrix_modelkn3_aggregated[[i]] <- temp_aggregated
  
  rm(temp_hourly, temp_aggregated)   
  
}

# Now, we have all data together (yields and temperature time series)
# We continue with regression. See the paper for more details.

# List with model outputs. [[i]] == farm i; [[m]] == knot specification m
list_farm_rapeseed_panelmodelkn3_modeloutputs <- list()

for (i in 1:nrow(farm_yields[[2]])){
  
  # Contains the m models of farm i.
  list_farm_rapeseed_panelmodelkn3_models.of.farm <- list() 
  for (m in 1:ncol(list_knot_locations_kn3_rapeseed[[2]])){
    print(paste0('i: ',i,'m: ',m))
    # Aggregated data without farm i (leave-out-farm i for out-of-sample training)
    temp1 <- subset(list_rapeseed_matrix_modelkn3_aggregated[[i]][[m]], list_rapeseed_matrix_modelkn3_aggregated[[i]][[m]] [,"Farm"] != i)
    
    # Run panel regression
    temp_reg <- lm(yield ~ x + V2 + year + I(year^2) + as.factor(Farm)-1, data = temp1)
    list_farm_rapeseed_panelmodelkn3_models.of.farm[[m]] <- temp_reg
    
    rm(temp1,temp_reg)
    
  }
  
  list_farm_rapeseed_panelmodelkn3_modeloutputs [[i]] <- list_farm_rapeseed_panelmodelkn3_models.of.farm   
  rm(list_farm_rapeseed_panelmodelkn3_models.of.farm)
}


# Calculation of hourly effects (marginal temperature effect)
list_farm_rapeseed_hourly_effect_kn3 <- list() # [[farm]] [[model]][]

for (i in 1:nrow(farm_yields[[2]])){
  
  temp_list <- list()
  for (m in 1:ncol(list_knot_locations_kn3_rapeseed[[i]])){
    print(paste0('i: ',i,' m: ',m)) 
    # Subset of farm i
    temp1 <- which(list_rapeseed_matrix_modelkn3_hourly[[i]][[m]][,"Farm"] == i) 
    temp2 <- list_rapeseed_matrix_modelkn3_hourly[[i]][[m]][temp1,]  
    
    # Calculate temperature effect
    temp2$effect <- as.matrix(temp2[,6:7]) %*% coef(list_farm_rapeseed_panelmodelkn3_modeloutputs [[i]][[m]])[1:2]
    
    temp_list[[m]] <- temp2  
    rm(temp1,temp2)  
  }
  list_farm_rapeseed_hourly_effect_kn3[[i]] <- temp_list
  rm(temp_list)
}

# -------------------------------------------
# Historical payouts & premium
# -------------------------------------------

# Different strike level temperatures
strike_temperature <- c(seq(13,36,by=1))

# Historical payouts with following structure: [[i]] == of farm i, [[m]] with model specification m, 
# [[m]] contains a data.frame with [strike level, year]
list_farm_rapeseed_payouts_kn3 <- list() 

for (i in 1:nrow(farm_yields[[2]])){
  
  # temp list for specification[[m]]
  temp_list <- list() 
  for (m in 1:ncol(list_knot_locations_kn3_rapeseed[[i]])){
    
    temp_model <- list_farm_rapeseed_hourly_effect_kn3 [[i]] [[m]]
    
    # DF for payouts
    temp_df <- as.data.frame(matrix(NA, nrow=length(strike_temperature), ncol=length(seq(2000,last_year,1))))
    row.names(temp_df) <- strike_temperature 
    colnames(temp_df) <- seq(2000,last_year,1)
    
    for (t in 1:ncol(temp_df)){
      # Subset of year
      sub_model_df <- subset(temp_model, temp_model$year == as.numeric(colnames(temp_df)[t]))
      # Subset only negative expected impacts trigger a payout 
      sub2_model_df <- subset(sub_model_df, sub_model_df$effect < 0)
      print(paste('i: ',i,' m: ',m,' t:',t))
      for (strike in 1:length(strike_temperature)){
        
        # Subset only temperature above the strike level trigger a payout
        sub3_model_df <- subset(sub2_model_df, sub2_model_df$Temperature >= strike_temperature[strike]) 
        
        # Calculate the cumulative payout.
        # -1 because negative yield response means yield reduction for which the payout formula compensates.
        temp_df[strike,t] <- sum(sub3_model_df$effect) * (-1)
        rm(sub3_model_df)
      }
      
      rm(sub2_model_df, sub_model_df)  
    }
    temp_list[[m]] <- temp_df
    rm(temp_df,temp_model)
  }
  list_farm_rapeseed_payouts_kn3[[i]] <- temp_list
  rm(temp_list)
} 

# Calculate the premium
# Structure: [farm i, model m, strike level]
array_premium_rapeseed_kn3 <- array(dim=c(nrow(farm_yields[[2]]), ncol(list_knot_locations_kn3_rapeseed[[1]]),length(strike_temperature)))
dimnames(array_premium_rapeseed_kn3)[[3]] <- strike_temperature

for (i in 1:nrow(farm_yields[[2]])){
  print(i)
  for (m in 1:ncol(list_knot_locations_kn3_rapeseed[[i]])){
    for (strike in 1:length(strike_temperature)){
      temp1 <- which(!is.na(detr_rapeseed_yields[i,]))
      array_premium_rapeseed_kn3[i,m,strike] <- mean(as.numeric(list_farm_rapeseed_payouts_kn3[[i]][[m]][strike, temp1]), na.rm=T)
      rm(temp1)
    }  
  }
}

# -------------------------------------------
# Insured revenue and expected utility
# -------------------------------------------

# Calculation of revenue [[strike]][m,i,t]
list_farm_rapeseed_wealth_modelskn3_strike <- list()

for (strike in 1:length(strike_temperature)){
  
  array_farm_rapeseed_wealth_modelskn3 <- array(dim=c(ncol(list_knot_locations_kn3_rapeseed[[1]]), nrow(farm_yields[[2]]), length(seq(2000,last_year,1))))
  dimnames(array_farm_rapeseed_wealth_modelskn3) [[3]] <-seq(2000,last_year,1) 
  
  for (m in 1:ncol(list_knot_locations_kn3_rapeseed[[1]])){
    for (i in 1:nrow(farm_yields[[2]])){
      for (t in 1: length(seq(2000,last_year,1))){
        print(paste0('strike: ',strike,' i: ',i))
        array_farm_rapeseed_wealth_modelskn3 [m,i,t] <- as.numeric(detr_rapeseed_yields[i,t]) + as.numeric(list_farm_rapeseed_payouts_kn3 [[i]][[m]] [strike,t]) - as.numeric(array_premium_rapeseed_kn3[i,m,strike]) 
        
      }
    }
  }
  
  list_farm_rapeseed_wealth_modelskn3_strike [[strike]] <- array_farm_rapeseed_wealth_modelskn3 
  rm(array_farm_rapeseed_wealth_modelskn3)
}

# Utility of being insured with specification m.
# List has structure: [[strike]][mi,i,t]
list_farm_rapeseed_wealth_modelkn3_strike_utility <- list()

for (strike in 1:length(strike_temperature)){
  
  temp_array <- array(dim=c(ncol(list_knot_locations_kn3_rapeseed[[1]]), nrow(farm_yields[[2]]), length(seq(2000,last_year,1))))
  dimnames(temp_array) [[3]] <-seq(2000,last_year,1) 
  
  for (m in 1:ncol(list_knot_locations_kn3_rapeseed[[1]])){
    for (i in 1:nrow(farm_yields[[2]])){
      for (t in 1: length(seq(2000,last_year,1))){
        
        temp_array [m,i,t] <- utility_function(alpha = alpha, revenue = list_farm_rapeseed_wealth_modelskn3_strike [[strike]][m,i,t])
        
      }
    }
  }
  
  list_farm_rapeseed_wealth_modelkn3_strike_utility  [[strike]] <- temp_array 
  rm(temp_array)
}

# Expected utility statistics of being insured
# Structure of list: 
summary_EU_rapeseed_insured_kn3 <- list() # [[strike]] [m,i,c(EU,E,CE,RP)]

for (strike in 1:length(strike_temperature)){
  temp_array <- array(dim=c(ncol(list_knot_locations_kn3_rapeseed[[1]]), nrow(farm_yields[[2]]), 4))
  dimnames(temp_array) [[3]] <- c("EU", "Expected Revenue", "CE", "RP")
  
  for (m in 1:ncol(list_knot_locations_kn3_rapeseed[[1]])){
    for (i in 1:nrow(farm_yields[[2]])){
      
      # Expected utility
      temp_array  [m,i,1] <- mean(as.numeric(list_farm_rapeseed_wealth_modelkn3_strike_utility[[strike]][m,i,]), na.rm=T)
      
      # Expected Revenue
      temp_array  [m,i,2] <- mean(as.numeric(list_farm_rapeseed_wealth_modelskn3_strike[[strike]][m,i,]), na.rm=T)
      
      # Certainty Equivalent
      temp_array  [m,i,3] <- inverse_utility_function(alpha = alpha, eu = as.numeric(temp_array[m,i,1]))
      
      # Risk Premium
      temp_array  [m,i,4] <- temp_array  [m,i,2] -  temp_array  [m,i,3]
    }
  }
  summary_EU_rapeseed_insured_kn3 [[strike]] <- temp_array
  rm(temp_array)
}

# -------------------------------------------
# Testing
# -------------------------------------------

average_rel_change_rapeseed_knot3_uninsured <- as.data.frame(matrix(nrow=ncol(list_knot_locations_kn3_rapeseed[[1]]), ncol=length(strike_temperature)))
colnames(average_rel_change_rapeseed_knot3_uninsured) <- strike_temperature

for (m in 1:ncol(list_knot_locations_kn3_rapeseed[[1]])){
  for (strike in 1:length(strike_temperature)){
    
    average_rel_change_rapeseed_knot3_uninsured [m,strike] <- mean((summary_EU_rapeseed_insured_kn3[[strike]][m,,4] - summary_EU_rapeseed_uninsured[,4]) / summary_EU_rapeseed_uninsured[,4])
    
  }
}

average_rel_change_rapeseed_knot3_uninsured_test <- as.data.frame(matrix(nrow=ncol(list_knot_locations_kn3_rapeseed[[1]]), ncol=length(strike_temperature)))
colnames(average_rel_change_rapeseed_knot3_uninsured_test) <- strike_temperature

for (m in 1:ncol(list_knot_locations_kn3_rapeseed[[1]])){
  for (strike in 1:length(strike_temperature)){
    
    average_rel_change_rapeseed_knot3_uninsured_test [m,strike] <-wilcox.test(summary_EU_rapeseed_insured_kn3[[strike]][m,,4], summary_EU_rapeseed_uninsured[,4], paired=T, alternative="l")$p.value
    
  }
}



# ==============================================================================================================
# 9. Winter wheat: out-of-sample calibration and testing for five knots
# ==============================================================================================================

# -------------------------------------------
# Definition of knots
# -------------------------------------------

knot_locations_kn5_wheat            <- as.data.frame(matrix(NA, nrow=5, ncol=1))
row.names(knot_locations_kn5_wheat) <- c("knot1", "knot2", "knot3","knot4","knot5")
colnames(knot_locations_kn5_wheat)  <- c("Schlenker") 

# Equally spaced across temperature support
knot_locations_kn5_wheat [,1] <- c(5,10,15,20,25)


'# quantiles (see also robustness checks)
knot_locations_kn5_wheat [,2] <- c(quantile(temp_wheat_exposure2$Temperature,0.05, type=1),
                                    quantile(temp_wheat_exposure2$Temperature,0.275, type=1),
                                    quantile(temp_wheat_exposure2$Temperature,0.5, type=1),
                                    quantile(temp_wheat_exposure2$Temperature,0.725, type=1),
                                    quantile(temp_wheat_exposure2$Temperature,0.95, type=1))'

# -------------------------------------------
# Out-of-sample calibration: leave-out-farm i
# -------------------------------------------

# Yields together with new hourly temperature time series: [[m]] == new time series for knot specification m
list_wheat_matrix_modelkn5_aggregated <- list() # [[m]]

# New time series of hourly temperature exposures: [[m]] == new time series for knot specification m
list_wheat_matrix_modelkn5_hourly <- list() # [[m]]

for (m in 1:ncol(knot_locations_kn5_wheat)){
  print('m: ',m)
  # Get new time series for RCS   
  temp1 <- as.matrix(rcspline.eval(temp_wheat_exposure2$Temperature,knots= knot_locations_kn5_wheat[,m], inclx=T))
  temp2 <- cbind(temp_wheat_exposure2, temp1)
  
  # Aggregate to yearly values
  temp3<-temp2 %>%
    group_by(Farm,year)%>%
    summarise_at(vars(x,V2,V3,V4),sum)
  
  list_wheat_matrix_modelkn5_hourly[[m]]     <- temp2
  list_wheat_matrix_modelkn5_aggregated[[m]] <- join(wheat_yield_melted,temp3, by=c("Farm", "year"))
  
  rm(temp1,temp2,temp3)
}

# Now, we have all data together (yields and temperature time series)
# We continue with regression. See the paper for more details.

# List with model outputs. [[i]] == farm i; [[m]] == knot specification m
list_farm_wheat_panelmodelkn5_modeloutputs <- list()

for (i in 1:nrow(farm_yields[[1]])){
  
  # Contains the m models of farm i.
  list_farm_wheat_panelmodelkn5_models.of.farm <- list() 
  for (m in 1:ncol(knot_locations_kn5_wheat)){
    print(paste0('m: ',m,'i: ',i))    
    # Aggregated data without farm i (leave-out-farm i for out-of-sample training)
    temp1 <- subset(list_wheat_matrix_modelkn5_aggregated[[m]], list_wheat_matrix_modelkn5_aggregated[[m]] [,"Farm"] != i)
    
    # Run panel regression
    temp_reg <- lm(yield ~ x + V2+V3+V4 + year + I(year^2) + as.factor(Farm)-1, data = temp1)
    list_farm_wheat_panelmodelkn5_models.of.farm[[m]] <- temp_reg
    
    rm(temp1,temp_reg)
    
  }
  
  list_farm_wheat_panelmodelkn5_modeloutputs [[i]] <- list_farm_wheat_panelmodelkn5_models.of.farm   
  rm(list_farm_wheat_panelmodelkn5_models.of.farm)
}


# Calculation of hourly effects (marginal temperature effect)
list_farm_wheat_hourly_effect_kn5 <- list() # [[farm]] [[model]][]

for (i in 1:nrow(farm_yields[[1]])){
  
  temp_list <- list()
  for (m in 1:ncol(knot_locations_kn5_wheat)){
    # Subset of farm i
    temp1 <- which(list_wheat_matrix_modelkn5_hourly[[m]][,"Farm"] == i) 
    temp2 <- list_wheat_matrix_modelkn5_hourly[[m]][temp1,]  
    
    # Calculate temperature effect
    temp2$effect <- as.matrix(temp2[,6:9]) %*% coef(list_farm_wheat_panelmodelkn5_modeloutputs [[i]][[m]])[1:4]
    
    temp_list[[m]] <- temp2  
    rm(temp1,temp2)  
  }
  list_farm_wheat_hourly_effect_kn5[[i]] <- temp_list
  rm(temp_list)
}

# -------------------------------------------
# Historical payouts & premium
# -------------------------------------------

# Different strike level temperatures
strike_temperature <- c(seq(13,36,by=1))

# Historical payouts with following structure: [[i]] == of farm i, [[m]] with model specification m, 
# [[m]] contains a data.frame with [strike level, year]
list_farm_wheat_payouts_kn5 <- list() 
for (i in 1:nrow(farm_yields[[1]])){
  
  # temp list for specification[[m]]
  temp_list <- list() 
  for (m in 1:ncol(knot_locations_kn5_wheat)){

    temp_model <- list_farm_wheat_hourly_effect_kn5 [[i]] [[m]]
    
    # DF for payouts
    temp_df <- as.data.frame(matrix(NA, nrow=length(strike_temperature), ncol=length(seq(2000,last_year,1))))
    row.names(temp_df) <- strike_temperature 
    colnames(temp_df) <- seq(2000,last_year,1)
    
    for (t in 1:ncol(temp_df)){
      # Subset of year
      sub_model_df <- subset(temp_model, temp_model$year == as.numeric(colnames(temp_df)[t]))
      # Subset only negative expected impacts trigger a payout 
      sub2_model_df <- subset(sub_model_df, sub_model_df$effect < 0)
      
      for (strike in 1:length(strike_temperature)){
        
        # Subset only temperature above the strike level trigger a payout
        sub3_model_df <- subset(sub2_model_df, sub2_model_df$Temperature >= strike_temperature[strike]) 
        
        # Calculate the cumulative payout
        temp_df[strike,t] <- sum(sub3_model_df$effect) * (-1)
        rm(sub3_model_df)
      }
      
      rm(sub2_model_df, sub_model_df)  
    }
    temp_list[[m]] <- temp_df
    rm(temp_df, temp_model)
  }
  list_farm_wheat_payouts_kn5[[i]] <- temp_list
  rm(temp_list)
} 

# Calculate the premium
# Structure: [farm i, model m, strike level]
array_premium_wheat_kn5 <- array(dim=c(nrow(farm_yields[[1]]), ncol(knot_locations_kn5_wheat),length(strike_temperature)))
dimnames(array_premium_wheat_kn5)[[3]] <- strike_temperature

for (i in 1:nrow(farm_yields[[1]])){
  for (m in 1:ncol(knot_locations_kn5_wheat)){
    for (strike in 1:length(strike_temperature)){
      temp1 <- which(!is.na(detr_wheat_yields[i,]))
      array_premium_wheat_kn5[i,m,strike] <- mean(as.numeric(list_farm_wheat_payouts_kn5[[i]][[m]][strike, temp1]), na.rm=T)
      rm(temp1)
    }  
  }
}

# -------------------------------------------
# Insured revenue and expected utility
# -------------------------------------------

# Calculation of revenue [[strike]][m,i,t]
list_farm_wheat_wealth_modelskn5_strike <- list()

for (strike in 1:length(strike_temperature)){
  print(strike)  
  array_farm_wheat_wealth_modelskn5 <- array(dim=c(length(knot_locations_kn5_wheat), nrow(farm_yields[[1]]), length(seq(2000,last_year,1))))
  dimnames(array_farm_wheat_wealth_modelskn5) [[3]] <-seq(2000,last_year,1) 
  
  for (m in 1:ncol(knot_locations_kn5_wheat)){
    for (i in 1:nrow(farm_yields[[1]])){
      for (t in 1: length(seq(2000,last_year,1))){
        
        array_farm_wheat_wealth_modelskn5 [m,i,t] <- as.numeric(detr_wheat_yields[i,t]) + as.numeric(list_farm_wheat_payouts_kn5 [[i]][[m]] [strike,t]) - as.numeric(array_premium_wheat_kn5[i,m,strike]) 
        
      }
    }
  }
  
  list_farm_wheat_wealth_modelskn5_strike [[strike]] <- array_farm_wheat_wealth_modelskn5 
  rm(array_farm_wheat_wealth_modelskn5)
}

# Utility of being insured with specification m.
# List has structure: [[strike]][m,i,t]
list_farm_wheat_wealth_modelkn5_strike_utility <- list()

for (strike in 1:length(strike_temperature)){
  print(strike)  
  temp_array <- array(dim=c(length(knot_locations_kn5_wheat), nrow(farm_yields[[1]]), length(seq(2000,last_year,1))))
  dimnames(temp_array) [[3]] <-seq(2000,last_year,1) 
  
  for (m in 1:ncol(knot_locations_kn5_wheat)){
    for (i in 1:nrow(farm_yields[[1]])){
      for (t in 1: length(seq(2000,last_year,1))){
        
        temp_array [m,i,t] <- utility_function(alpha = alpha, revenue = list_farm_wheat_wealth_modelskn5_strike [[strike]][m,i,t])
        
      }
    }
  }
  
  list_farm_wheat_wealth_modelkn5_strike_utility  [[strike]] <- temp_array 
  rm(temp_array)
}

# Expected utility statitsics of being insured
# Structure of list: 
summary_EU_wheat_insured_kn5 <- list() # [[strike]] [m,i,c(EU,E,CE,RP)]

for (strike in 1:length(strike_temperature)){
  temp_array <- array(dim=c(length(knot_locations_kn5_wheat), nrow(farm_yields[[1]]), 4))
  dimnames(temp_array) [[3]] <- c("EU", "Expected Revenue", "CE", "RP")
  
  for (m in 1:ncol(knot_locations_kn5_wheat)){
    for (i in 1:nrow(farm_yields[[1]])){
      
      # Expected utility
      temp_array  [m,i,1] <- mean(as.numeric(list_farm_wheat_wealth_modelkn5_strike_utility[[strike]][m,i,]), na.rm=T)
      
      # Expected Revenue
      temp_array  [m,i,2] <- mean(as.numeric(list_farm_wheat_wealth_modelskn5_strike[[strike]][m,i,]), na.rm=T)
      
      # Certainty Equivalent
      temp_array  [m,i,3] <- inverse_utility_function(alpha = alpha, eu = as.numeric(temp_array[m,i,1]))
      
      # Risk Premium
      temp_array  [m,i,4] <- temp_array  [m,i,2] -  temp_array  [m,i,3]
    }
  }
  summary_EU_wheat_insured_kn5 [[strike]] <- temp_array
  rm(temp_array)
}

# -------------------------------------------
# Testing
# -------------------------------------------

average_rel_change_wheat_knot5_uninsured <- as.data.frame(matrix(nrow=ncol(knot_locations_kn5_wheat), ncol=length(strike_temperature)))
colnames(average_rel_change_wheat_knot5_uninsured) <- strike_temperature

for (m in 1:ncol(knot_locations_kn5_wheat)){
  for (strike in 1:length(strike_temperature)){
    
    average_rel_change_wheat_knot5_uninsured [m,strike] <- mean((summary_EU_wheat_insured_kn5[[strike]][m,,4] - summary_EU_wheat_uninsured[,4]) / summary_EU_wheat_uninsured[,4])
    
  }
}

average_rel_change_wheat_knot5_uninsured_test <- as.data.frame(matrix(nrow=ncol(knot_locations_kn5_wheat), ncol=length(strike_temperature)))
colnames(average_rel_change_wheat_knot5_uninsured_test) <- strike_temperature

for (m in 1:ncol(knot_locations_kn5_wheat)){
  for (strike in 1:length(strike_temperature)){
    
    average_rel_change_wheat_knot5_uninsured_test [m,strike] <-wilcox.test(summary_EU_wheat_insured_kn5[[strike]][m,,4], summary_EU_wheat_uninsured[,4], paired=T, alternative="l")$p.value
    
  }
}


# ==============================================================================================================
# 10. Winter rapeseed: out-of-sample calibration and testing for five knots
# ==============================================================================================================

# -------------------------------------------
# Definition of knots
# -------------------------------------------

# Here only 1 model with 5 knots
knot_locations_kn5_rapeseed            <- as.data.frame(matrix(NA, nrow=5, ncol=1))
row.names(knot_locations_kn5_rapeseed) <- c("knot1", "knot2", "knot3","knot4","knot5")
colnames(knot_locations_kn5_rapeseed)  <- c("Schlenker") 

# Equally spaced across temperature support
knot_locations_kn5_rapeseed [,1] <- c(5,10,15,20,25)


'#quantiles (see also robustness checks)
knot_locations_kn5_rapeseed [,2] <-c(quantile(temp_rapeseed_exposure2$Temperature,0.05, type=1),
                                    quantile(temp_rapeseed_exposure2$Temperature,0.275, type=1),
                                    quantile(temp_rapeseed_exposure2$Temperature,0.5, type=1),
                                    quantile(temp_rapeseed_exposure2$Temperature,0.725, type=1),
                                    quantile(temp_rapeseed_exposure2$Temperature,0.95, type=1))'
      

# -------------------------------------------
# Out-of-sample calibration: leave-out-farm i
# -------------------------------------------

# Yields together with new hourly temperature time series: [[m]] == new time series for knot specification m
list_rapeseed_matrix_modelkn5_aggregated <- list() 

# New time series of hourly temperature exposures: [[m]] == new time series for knot specification m
list_rapeseed_matrix_modelkn5_hourly <- list() 

for (m in 1:ncol(knot_locations_kn5_rapeseed)){
  
  # Get new time series for RCS   
  temp1 <- as.matrix(rcspline.eval(temp_rapeseed_exposure2$Temperature,knots= knot_locations_kn5_rapeseed[,m], inclx=T))
  temp2 <- cbind(temp_rapeseed_exposure2, temp1)
  
  # Aggregate to yearly values
  temp3 <- temp2 %>%
    group_by(Farm,year)%>%
    summarise_at(vars(x,V2,V3,V4),sum)
  
  list_rapeseed_matrix_modelkn5_hourly[[m]]     <- temp2
  list_rapeseed_matrix_modelkn5_aggregated[[m]] <- join(rapeseed_yield_melted,temp3, by=c("Farm", "year"))
  
  rm(temp1,temp2,temp3)
}

# Now, we have all data together (yields and temperature time series)
# We continue with regression. See the paper for more details.

# List with model outputs. [[i]] == farm i; [[m]] == knot specification m
list_farm_rapeseed_panelmodelkn5_modeloutputs <- list()

for (i in 1:nrow(farm_yields[[2]])){
  
  # Contains the m models of farm i.
  list_farm_rapeseed_panelmodelkn5_models.of.farm <- list() 
  for (m in 1:ncol(knot_locations_kn5_rapeseed)){
    
    # Aggregated data without farm i (leave-out-farm i for out-of-sample training)
    temp1 <- subset(list_rapeseed_matrix_modelkn5_aggregated[[m]], list_rapeseed_matrix_modelkn5_aggregated[[m]] [,"Farm"] != i)
    
    # Run panel regression
    temp_reg <- lm(yield ~ x + V2+V3+V4 + year + I(year^2) + as.factor(Farm)-1, data = temp1)
    list_farm_rapeseed_panelmodelkn5_models.of.farm[[m]] <- temp_reg
    
    rm(temp1,temp_reg)
    
  }
  
  list_farm_rapeseed_panelmodelkn5_modeloutputs [[i]] <- list_farm_rapeseed_panelmodelkn5_models.of.farm   
  rm(list_farm_rapeseed_panelmodelkn5_models.of.farm)
}


# Calculation of hourly effects (marginal temperature effect)
list_farm_rapeseed_hourly_effect_kn5 <- list() # [[farm]] [[model]][]

for (i in 1:nrow(farm_yields[[2]])){
  
  temp_list <- list()
  for (m in 1:ncol(knot_locations_kn5_rapeseed)){
    
    # Subset of farm i
    temp1 <- which(list_rapeseed_matrix_modelkn5_hourly[[m]][,"Farm"] == i) 
    temp2 <- list_rapeseed_matrix_modelkn5_hourly[[m]][temp1,]  
    
    # Calculate temperature effect
    temp2$effect <- as.matrix(temp2[,6:9]) %*% coef(list_farm_rapeseed_panelmodelkn5_modeloutputs [[i]][[m]])[1:4]
    
    temp_list[[m]] <- temp2  
    rm(temp1,temp2)  
  }
  list_farm_rapeseed_hourly_effect_kn5[[i]] <- temp_list
  rm(temp_list)
}

# -------------------------------------------
# Historical payouts & premium
# -------------------------------------------

# Different strike level temperatures
strike_temperature <- c(seq(13,36,by=1))

# Historical payouts with following structure: [[i]] == of farm i, [[m]] with model specification m, 
# [[m]] contains a data.frame with [strike level, year]
list_farm_rapeseed_payouts_kn5 <- list() 
for (i in 1:nrow(farm_yields[[2]])){
  
  # temp list for specification[[m]]
  temp_list <- list() 
  for (m in 1:ncol(knot_locations_kn5_rapeseed)){
    
    temp_model <- list_farm_rapeseed_hourly_effect_kn5 [[i]] [[m]]
    
    # DF for payouts
    temp_df <- as.data.frame(matrix(NA, nrow=length(strike_temperature), ncol=length(seq(2000,last_year,1))))
    row.names(temp_df) <- strike_temperature 
    colnames(temp_df) <- seq(2000,last_year,1)
    
    for (t in 1:ncol(temp_df)){
      # Subset of year
      sub_model_df <- subset(temp_model, temp_model$year == as.numeric(colnames(temp_df)[t]))
      # Subset only negative expected impacts trigger a payout 
      sub2_model_df <- subset(sub_model_df, sub_model_df$effect < 0)
      
      for (strike in 1:length(strike_temperature)){
        
        # Subset only temperature above the strike level trigger a payout
        sub3_model_df <- subset(sub2_model_df, sub2_model_df$Temperature >= strike_temperature[strike]) 
        
        # Calculate the cumulative payout
        temp_df[strike,t] <- sum(sub3_model_df$effect) * (-1)
        rm(sub3_model_df)
      }
      
      rm(sub2_model_df, sub_model_df)  
    }
    temp_list[[m]] <- temp_df
    rm(temp_df, temp_model)
  }
  list_farm_rapeseed_payouts_kn5[[i]] <- temp_list
  rm(temp_list)
} 

# Calculate the premium
# Structure: [farm i, model m, strike level]
array_premium_rapeseed_kn5 <- array(dim=c(nrow(farm_yields[[2]]), ncol(knot_locations_kn5_rapeseed),length(strike_temperature)))
dimnames(array_premium_rapeseed_kn5)[[3]] <- strike_temperature

for (i in 1:nrow(farm_yields[[2]])){
  for (m in 1:ncol(knot_locations_kn5_rapeseed)){
    for (strike in 1:length(strike_temperature)){
      temp1 <- which(!is.na(detr_rapeseed_yields[i,]))
      array_premium_rapeseed_kn5[i,m,strike] <- mean(as.numeric(list_farm_rapeseed_payouts_kn5[[i]][[m]][strike,temp1 ]), na.rm=T)
      rm(temp1)
    }  
  }
}

# -------------------------------------------
# Insured revenue and expected utility
# -------------------------------------------

# Calculation of revenue [[strike]][m,i,t]
list_farm_rapeseed_wealth_modelskn5_strike <- list()

for (strike in 1:length(strike_temperature)){
  
  array_farm_rapeseed_wealth_modelskn5 <- array(dim=c(length(knot_locations_kn5_rapeseed), nrow(farm_yields[[2]]), length(seq(2000,last_year,1))))
  dimnames(array_farm_rapeseed_wealth_modelskn5) [[3]] <-seq(2000,last_year,1) 
  
  for (m in 1:ncol(knot_locations_kn5_rapeseed)){
    for (i in 1:nrow(farm_yields[[2]])){
      for (t in 1: length(seq(2000,last_year,1))){
        
        array_farm_rapeseed_wealth_modelskn5 [m,i,t] <- as.numeric(detr_rapeseed_yields[i,t]) + as.numeric(list_farm_rapeseed_payouts_kn5 [[i]][[m]] [strike,t]) - as.numeric(array_premium_rapeseed_kn5[i,m,strike]) 
        
      }
    }
  }
  
  list_farm_rapeseed_wealth_modelskn5_strike [[strike]] <- array_farm_rapeseed_wealth_modelskn5 
  rm(array_farm_rapeseed_wealth_modelskn5)
}

# Utility of being insured with specification m.
# List has structure: [[strike]][mi,i,t]
list_farm_rapeseed_wealth_modelkn5_strike_utility <- list()

for (strike in 1:length(strike_temperature)){
  
  temp_array <- array(dim=c(length(knot_locations_kn5_rapeseed), nrow(farm_yields[[2]]), length(seq(2000,last_year,1))))
  dimnames(temp_array) [[3]] <-seq(2000,last_year,1) 
  
  for (m in 1:ncol(knot_locations_kn5_rapeseed)){
    for (i in 1:nrow(farm_yields[[2]])){
      for (t in 1: length(seq(2000,last_year,1))){
        
        temp_array [m,i,t] <- utility_function(alpha = alpha, revenue = list_farm_rapeseed_wealth_modelskn5_strike [[strike]][m,i,t])
        
      }
    }
  }
  
  list_farm_rapeseed_wealth_modelkn5_strike_utility  [[strike]] <- temp_array 
  rm(temp_array)
}

# Expected utility statitsics of being insured
# Structure of list: 
summary_EU_rapeseed_insured_kn5 <- list() # [[strike]] [m,i,c(EU,E,CE,RP)]

for (strike in 1:length(strike_temperature)){
  temp_array <- array(dim=c(length(knot_locations_kn5_rapeseed), nrow(farm_yields[[2]]), 4))
  dimnames(temp_array) [[3]] <- c("EU", "Expected Revenue", "CE", "RP")
  
  for (m in 1:ncol(knot_locations_kn5_rapeseed)){
    for (i in 1:nrow(farm_yields[[2]])){
      
      # Expected utility
      temp_array  [m,i,1] <- mean(as.numeric(list_farm_rapeseed_wealth_modelkn5_strike_utility[[strike]][m,i,]), na.rm=T)
      
      # Expected Revenue
      temp_array  [m,i,2] <- mean(as.numeric(list_farm_rapeseed_wealth_modelskn5_strike[[strike]][m,i,]), na.rm=T)
      
      # Certainty Equivalent
      temp_array  [m,i,3] <- inverse_utility_function(alpha = alpha, eu = as.numeric(temp_array[m,i,1]))
      
      # Risk Premium
      temp_array  [m,i,4] <- temp_array  [m,i,2] -  temp_array  [m,i,3]
    }
  }
  summary_EU_rapeseed_insured_kn5 [[strike]] <- temp_array
  rm(temp_array)
}

# -------------------------------------------
# Testing
# -------------------------------------------

average_rel_change_rapeseed_kn5_uninsured <- as.data.frame(matrix(nrow=ncol(knot_locations_kn5_rapeseed), ncol=length(strike_temperature)))
colnames(average_rel_change_rapeseed_kn5_uninsured) <- strike_temperature

for (m in 1:ncol(knot_locations_kn5_rapeseed)){
  for (strike in 1:length(strike_temperature)){
    
    average_rel_change_rapeseed_kn5_uninsured [m,strike] <- mean((summary_EU_rapeseed_insured_kn5[[strike]][m,,4] - summary_EU_rapeseed_uninsured[,4]) / summary_EU_rapeseed_uninsured[,4])
    
  }
}

average_rel_change_rapeseed_kn5_uninsured_test <- as.data.frame(matrix(nrow=ncol(knot_locations_kn5_rapeseed), ncol=length(strike_temperature)))
colnames(average_rel_change_rapeseed_kn5_uninsured_test) <- strike_temperature

for (m in 1:ncol(knot_locations_kn5_rapeseed)){
  for (strike in 1:length(strike_temperature)){
    
    average_rel_change_rapeseed_kn5_uninsured_test [m,strike] <- wilcox.test(summary_EU_rapeseed_insured_kn5[[strike]][m,,4], summary_EU_rapeseed_uninsured[,4], paired=T, alternative="l")$p.value
    
  }
}


# =========================================================================================================
# ---------------------------------------------------------------------------------------------------------
#
# Temperature Effects on Crop Yields in Heat Index Insurance
#
# Supplementary R code for plots in the main paper
# 
# Authors: Janic Bucheli, Tobias Dalhaus, Robert Finger
#
# Corresponding author: Janic Bucheli (jbucheli@ethz.ch)
#
# Please check our github repository for more codes and information
# https://github.com/AECP-ETHZ/Temperature-effects-on-crop-yields-in-heat-index-insurance
#
# Citation: Bucheli, J., Dalhaus T., & Finger R. 2021. Temperature Effects on Crop Yields in Heat Index Insurance.
#           Food Policy.
# 
# ---------------------------------------------------------------------------------------------------------
# =========================================================================================================

# This file shows the codes to create figures 2-4 of the main paper-
# Run the scrip main_codes_masterfile.R before you run this script.

# Content:
# 1) Figure 2: hourly temperature effects and payouts for winter wheat
# 2) Figure 3: hourly temperature effects and payouts for winter rapeseed
# 3) Figure 4: Out-of-sample risk reductions

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

# ====================================================================
#
# 1) Figure 2: hourly temperature effects and payouts for winter wheat
#
# ====================================================================

# The plots are representative for a farm drawn at random.
# Note that plots slightly vary between farms because of the out-of-sample calibration
set.seed(123)
farm_ID <- round(runif(1,1,nrow(farm_yields[[1]])))

# -------------------------------------------
# Histogram of temperature exposure
# -------------------------------------------

sub_temperature_exposure_wheat <- temp_wheat_exposure2[which(temp_wheat_exposure2$Farm != farm_ID),]

temperature_wheat <- ggplot(sub_temperature_exposure_wheat, aes(x=Temperature)) + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=10),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  ylab("Total exposure \n (1'000 hours)")+ xlab("Temperature (°C)")+
  geom_histogram(binwidth=1, color="black", fill="brown1")+
  scale_x_continuous(breaks = seq(-5,39, by=5), limits = c(-5,40))+
  scale_y_continuous(breaks = c(0,100000,200000), labels = c(0,100,200))

# -------------------------------------------
# Model 1: 
# -------------------------------------------

# Temperature time series
temperatures_ts_M1 <- rcspline.eval(sub_temperature_exposure_wheat$Temperature,knots= list_knot_locations_kn3_wheat[[farm_ID]][,1], inclx=T)  

# Add new ts to dates
temp1 <- cbind(sub_temperature_exposure_wheat, temperatures_ts_M1)

# Aggregation to yearly values
final_wheat_temperature_aggregated_M1 <-temp1 %>%
  group_by(Farm,year)%>%
  summarise_at(vars(x,V2),sum)
rm(temp1)

# Combine yearly values with yields
temp1 <- wheat_yield_melted[which(wheat_yield_melted$Farm != farm_ID),]
data_wheat_final_M1 <- join(temp1, final_wheat_temperature_aggregated_M1, by=c("Farm","year"))
rm(temperatures_ts_M1,temp1,final_wheat_temperature_aggregated_M1)

# New TS for temperature support of interest
marginal_temperatures_M1 <- rcspline.eval(seq(-10,39,0.225),knots=list_knot_locations_kn3_wheat[[farm_ID]][,1], inclx=T)

# Regression output with reliable R-squared
temp_reg1 <- feols(yield ~ x + V2 + year + I(year^2) | Farm, data=data_wheat_final_M1)
summary(temp_reg1)
rm(temp_reg1)

# 
# Statistical Uncertainty
# 

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample some years
  data_temp <- data_wheat_final_M1[which(data_wheat_final_M1$year %in% sample(unique(data_wheat_final_M1$year),replace=T)),]
  
  # Run regression with random subsample (data_temp)
  fit_boot  <- lm( yield ~ x + V2 +year + I(year^2) + as.factor(Farm)-1, data=data_temp)
  
  # Save marginal effect of temperatures 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1 %*% coef(fit_boot)[1:2]))
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Change structure
spline_sub_melt_M1 <- reshape2::melt(spline_fitted_M1)

# Get median response for each column (= a temperature from seq(3,39,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)

# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}

# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- reshape2::melt(dist_colmedian_M1_normalized)

# Last preparation before plotting

spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), mean, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), mean, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(-10,39,0.225),seq(1:length(seq(-10,39,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# 
# Plot: hourly temperature effect
# 

model_1 <- ggplot()+ ggtitle("Hourly temperature effects: Model 1") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=10),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Hourly yield response \n (dt/ha)")+
  scale_x_continuous(breaks = seq(-5,39, by=5), limits = c(-5,40))+
  scale_y_continuous(breaks = c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1), limits = c(-0.4,0.1))+
  geom_hline(yintercept=c(seq(-0.5,0.1,by=0.1)), colour="gray")+
  geom_vline(xintercept=list_knot_locations_kn3_wheat[[farm_ID]][,1], linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

rm(temp1,temp2,temp3,temp4,temp5)
rm(dist_colmedian_M1_normalized_melted, colmedians_spline_M1, spline_fitted_M1,dist_colmedian_M1_normalized,dist_colmedian_M1,spline_sub_melt_M1)

# 
# Preparation of hourly payouts
# 

# We change the sign of the effect (payouts are a positive number)

spline_merged_M1$average_effect <- spline_merged_M1$average_effect * (-1) 
spline_merged_M1$lower_bound <- spline_merged_M1$lower_bound * (-1) 
spline_merged_M1$upper_bound <- spline_merged_M1$upper_bound * (-1) 
spline_merged_M1$value.x <- spline_merged_M1$value.x * (-1) 

# Prepare lower bound (no values below 0)
temp <- subset(spline_merged_M1[,c(2,3,6,7,8,9,10)])
colnames(temp)[5] <- "upper_bound"
colnames(temp)[6] <- "lower_bound"

# Remove negative values(required for geom_ribbon())

temp[which(temp$lower_bound<0),6] <- 0

payout_wheat_model_1 <- ggplot(temp) +
  ggtitle("Hourly payouts: Model 1")+ theme(panel.grid.major = element_blank(),
                                            plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
                                            axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
                                            axis.text = element_text(family="Times New Roman", size=10),
                                            panel.grid.minor = element_blank(),
                                            panel.background = element_blank(),
                                            axis.line = element_line(colour = "black"),
                                            legend.position="none")+
  xlab("Temperature (°C)") + ylab("Payout (dt/ha)")+
  scale_x_continuous(breaks = seq(10,39, by=5), limits = c(10,40))+
  scale_y_continuous(breaks = c(0,0.1, 0.2,0.3,0.4), limits = c(0,0.46))+
  geom_hline(yintercept=c(seq(0,0.4,by=0.1)), colour="gray")+
  geom_hline(yintercept=0, colour="darkgray")+
  geom_vline(xintercept= seq(5,38, by=5), colour="gray", linetype="dashed")+
  
  # Add lines
  
  geom_line(data=temp, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="royalblue1")+
  geom_line(data=temp, aes(y=average_effect, x=Temp_bin), size=0.75, color="royalblue4")+
  geom_line(data=temp, aes(y=lower_bound, x=Temp_bin), size=0.75, color="royalblue4")+
  geom_line(data=temp, aes(y=upper_bound, x=Temp_bin), size=0.75, color="royalblue4")

rm(temp,spline_merged_M1)

# -------------------------------------------
# Model 2: 
# -------------------------------------------

# Temperature time series
temperatures_ts_M2 <- rcspline.eval(sub_temperature_exposure_wheat$Temperature,knots= list_knot_locations_kn3_wheat[[farm_ID]][,2], inclx=T)  

# Add new ts to dates
temp1 <- cbind(sub_temperature_exposure_wheat, temperatures_ts_M2)

# Aggregation to yearly values
final_wheat_temperature_aggregated_M2 <-temp1 %>%
  group_by(Farm,year)%>%
  summarise_at(vars(x,V2),sum)
rm(temp1)

# Combine yearly values with yields
temp1 <- wheat_yield_melted[which(wheat_yield_melted$Farm != farm_ID),]
data_wheat_final_M2 <- join(temp1, final_wheat_temperature_aggregated_M2, by=c("Farm","year"))
rm(temperatures_ts_M2,temp1,final_wheat_temperature_aggregated_M2)

# New TS for temperature support of interest
marginal_temperatures_M2 <- rcspline.eval(seq(-10,39,0.225),knots=list_knot_locations_kn3_wheat[[farm_ID]][,2], inclx=T)

# Regression output with reliable R-squared
temp_reg1 <- feols(yield ~ x + V2 + year + I(year^2) | Farm, data=data_wheat_final_M2)
summary(temp_reg1)
rm(temp_reg1)

# 
# Statistical uncertainty 
# 

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M2 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M2))

for (l in 1: 1000){
  # Sample some years
  data_temp <- data_wheat_final_M2[which(data_wheat_final_M2$year %in% sample(unique(data_wheat_final_M2$year),replace=T)),]
  
  # Run regression with random subsample (data_temp)
  fit_boot  <- lm( yield ~ x + V2 + year + I(year^2)  + as.factor(Farm)-1, data=data_temp)
  
  # Save marginal effect of temperatures 
  spline_fitted_M2[l,]<-as.vector((marginal_temperatures_M2 %*% coef(fit_boot)[1:2]))
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Change structure
spline_sub_melt_M2 <- reshape2::melt(spline_fitted_M2)

# Get median response for each column (= a temperature from seq(3,39,0.225))
colmedians_spline_M2 <- colMedians(spline_fitted_M2)

# Get distance from median response for each value
dist_colmedian_M2 <- matrix(NA, nrow=nrow(spline_fitted_M2),ncol=ncol(spline_fitted_M2))

for(l in 1:ncol(spline_fitted_M2)){
  dist_colmedian_M2[,l] <- spline_fitted_M2[,l] - colmedians_spline_M2[l]
}

# Normalize distance to median
dist_colmedian_M2_normalized <- matrix(NA, nrow=nrow(spline_fitted_M2),ncol=ncol(spline_fitted_M2))

for(l in 1:ncol(spline_fitted_M2)){
  dist_colmedian_M2_normalized[,l]<-abs(dist_colmedian_M2[,l]/max(abs(dist_colmedian_M2[,l])))
}

# Change structure
dist_colmedian_M2_normalized_melted <- reshape2::melt(dist_colmedian_M2_normalized)

# Last preparation before plotting

spline_merged_M2 <- merge(spline_sub_melt_M2,dist_colmedian_M2_normalized_melted,by=c("Var1","Var2"))
spline_merged_M2$value.y_minus1 <- 1-spline_merged_M2$value.y

temp1 <- aggregate(spline_merged_M2$value.y_minus1, list(spline_merged_M2$Var1), mean, na.rm=T) 
temp2 <- aggregate(spline_merged_M2$value.x, list(spline_merged_M2$Var2), mean, na.rm=T)

temp3 <- aggregate(spline_merged_M2$value.x, list(spline_merged_M2$Var2), quantile, probs=0.025) 
temp4 <- aggregate(spline_merged_M2$value.x, list(spline_merged_M2$Var2), quantile, probs=0.975)

spline_merged_M2 <- merge(spline_merged_M2, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M2 <- merge(spline_merged_M2, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M2)[7]<-"average_effect"

spline_merged_M2<-merge(spline_merged_M2, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M2)[8]<-"lower_bound"

spline_merged_M2<-merge(spline_merged_M2, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M2)[9]<-"upper_bound"

temp5 <- cbind(seq(-10,39,0.225),seq(1:length(seq(-10,39,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M2 <-merge(spline_merged_M2, temp5, by=c("Var2"))

# 
# Plot: hourly temperature effect
# 

model_2 <- ggplot()+ ggtitle("Hourly temperature effects: Model 2") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=10),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Hourly yield response \n (dt/ha)")+
  scale_x_continuous(breaks = seq(-5,39, by=5), limits = c(-5,40))+
  scale_y_continuous(breaks = c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1), limits = c(-0.4,0.1))+
  geom_hline(yintercept=c(seq(-0.5,0.1,by=0.1)), colour="gray")+
  geom_vline(xintercept=list_knot_locations_kn3_wheat[[farm_ID]][,2], linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M2, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M2, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M2, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M2, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")  

rm(temp1,temp2,temp3,temp4,temp5)
rm(dist_colmedian_M2_normalized_melted, colmedians_spline_M2, spline_fitted_M2,dist_colmedian_M2_normalized,dist_colmedian_M2,spline_sub_melt_M2)

# 
# Preparation of hourly payouts
# 

# We change the sign of the effect (payouts are a positive number)

spline_merged_M2$average_effect <- spline_merged_M2$average_effect * (-1) 
spline_merged_M2$lower_bound <- spline_merged_M2$lower_bound * (-1) 
spline_merged_M2$upper_bound <- spline_merged_M2$upper_bound * (-1) 
spline_merged_M2$value.x <- spline_merged_M2$value.x * (-1) 

# Prepare lower bound (no values below 0)
temp <- subset(spline_merged_M2[,c(2,3,6,7,8,9,10)])
colnames(temp)[5] <- "upper_bound"
colnames(temp)[6] <- "lower_bound"

# Remove negative values(required for geom_ribbon())

temp[which(temp$lower_bound<0),6] <- 0

# 
# Plot: hourly payout function
# 

payout_wheat_model_2 <- ggplot(temp) +
  ggtitle("Hourly payouts: Model 2")+ theme(panel.grid.major = element_blank(),
                                            plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
                                            axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
                                            axis.text = element_text(family="Times New Roman", size=10),
                                            panel.grid.minor = element_blank(),
                                            panel.background = element_blank(),
                                            axis.line = element_line(colour = "black"),
                                            legend.position="none")+
  xlab("Temperature (°C)") + ylab("Payout (dt/ha)")+
  scale_x_continuous(breaks = seq(10,39, by=5), limits = c(10,40))+
  scale_y_continuous(breaks = c(0,0.1, 0.2,0.3,0.4), limits = c(0,0.46))+
  geom_hline(yintercept=c(seq(0,0.4,by=0.1)), colour="gray")+
  geom_hline(yintercept=0, colour="darkgray")+
  geom_vline(xintercept= seq(5,38, by=5), colour="gray", linetype="dashed")+
  
  # Add lines
  
  geom_line(data=temp, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="royalblue1")+
  geom_line(data=temp, aes(y=average_effect, x=Temp_bin), size=0.75, color="royalblue4")+
  geom_line(data=temp, aes(y=lower_bound, x=Temp_bin), size=0.75, color="royalblue4")+
  geom_line(data=temp, aes(y=upper_bound, x=Temp_bin), size=0.75, color="royalblue4")

rm(temp,spline_merged_M2)

# -------------------------------------------
# Model 3: 
# -------------------------------------------

# Temperature time series
temperatures_ts_M3 <- rcspline.eval(sub_temperature_exposure_wheat$Temperature,knots= list_knot_locations_kn3_wheat[[farm_ID]][,3], inclx=T)  

# Add new ts to dates
temp1 <- cbind(sub_temperature_exposure_wheat, temperatures_ts_M3)

# Aggregation to yearly values
final_wheat_temperature_aggregated_M3 <-temp1 %>%
  group_by(Farm,year)%>%
  summarise_at(vars(x,V2),sum)
rm(temp1)

# Combine yearly values with yields
temp1 <- wheat_yield_melted[which(wheat_yield_melted$Farm != farm_ID),]
data_wheat_final_M3 <- join(temp1, final_wheat_temperature_aggregated_M3, by=c("Farm","year"))
rm(temperatures_ts_M3,temp1,final_wheat_temperature_aggregated_M3)

# New TS for temperature support of interest
marginal_temperatures_M3 <- rcspline.eval(seq(-10,39,0.225),knots=list_knot_locations_kn3_wheat[[farm_ID]][,3], inclx=T)

temp_reg1 <- feols(yield ~ x + V2 + year + I(year^2) | Farm, data=data_wheat_final_M3)
summary(temp_reg1)
rm(temp_reg1)

# 
# Statistical uncertainty
# 

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M3 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M3))

for (l in 1: 1000){
  # Sample some years
  data_temp <- data_wheat_final_M3[which(data_wheat_final_M3$year %in% sample(unique(data_wheat_final_M3$year),replace=T)),]
  data_temp$year2 <- (data_temp$year)^2 
  
  # Run regression with random subsample (data_temp)
  fit_boot  <- lm( yield ~ x + V2  + year +I(year^2)+ as.factor(Farm)-1, data=data_temp)
  
  # Save marginal effect of temperatures 
  spline_fitted_M3[l,]<-as.vector((marginal_temperatures_M3 %*% coef(fit_boot)[1:2]))
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Change structure
spline_sub_melt_M3 <- reshape2::melt(spline_fitted_M3)

# Get median response for each column (= a temperature from seq(3,39,0.225))
colmedians_spline_M3 <- colMedians(spline_fitted_M3)

# Get distance from median response for each value
dist_colmedian_M3 <- matrix(NA, nrow=nrow(spline_fitted_M3),ncol=ncol(spline_fitted_M3))

for(l in 1:ncol(spline_fitted_M3)){
  dist_colmedian_M3[,l] <- spline_fitted_M3[,l] - colmedians_spline_M3[l]
}

# Normalize distance to median
dist_colmedian_M3_normalized <- matrix(NA, nrow=nrow(spline_fitted_M3),ncol=ncol(spline_fitted_M3))

for(l in 1:ncol(spline_fitted_M3)){
  dist_colmedian_M3_normalized[,l]<-abs(dist_colmedian_M3[,l]/max(abs(dist_colmedian_M3[,l])))
}

# Change structure
dist_colmedian_M3_normalized_melted <- reshape2::melt(dist_colmedian_M3_normalized)

# Last preparation before plotting

spline_merged_M3 <- merge(spline_sub_melt_M3,dist_colmedian_M3_normalized_melted,by=c("Var1","Var2"))
spline_merged_M3$value.y_minus1 <- 1-spline_merged_M3$value.y

temp1 <- aggregate(spline_merged_M3$value.y_minus1, list(spline_merged_M3$Var1), mean, na.rm=T) 
temp2 <- aggregate(spline_merged_M3$value.x, list(spline_merged_M3$Var2), mean, na.rm=T)
temp3 <- aggregate(spline_merged_M3$value.x, list(spline_merged_M3$Var2), quantile, probs=0.025) 
temp4 <- aggregate(spline_merged_M3$value.x, list(spline_merged_M3$Var2), quantile, probs=0.975)

spline_merged_M3 <- merge(spline_merged_M3, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M3 <- merge(spline_merged_M3, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M3)[7]<-"average_effect"

spline_merged_M3<-merge(spline_merged_M3, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M3)[8]<-"lower_bound"

spline_merged_M3<-merge(spline_merged_M3, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M3)[9]<-"upper_bound"

temp5 <- cbind(seq(-10,39,0.225),seq(1:length(seq(-10,39,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M3 <-merge(spline_merged_M3, temp5, by=c("Var2"))

# 
# Plot: hourly temperature effect
# 

model_3 <- ggplot()+ ggtitle("Hourly temperature effects: Model 3") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=10),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Hourly yield response \n (dt/ha)")+
  scale_x_continuous(breaks = seq(-5,39, by=5), limits = c(-5,40))+
  scale_y_continuous(breaks = c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1), limits = c(-0.4,0.1))+
  geom_hline(yintercept=c(seq(-0.5,0.1,by=0.1)), colour="gray")+
  geom_vline(xintercept=list_knot_locations_kn3_wheat[[farm_ID]][,3], linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M3, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M3, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M3, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M3, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")  

rm(temp1,temp2,temp3,temp4,temp5)
rm(dist_colmedian_M3_normalized_melted, colmedians_spline_M3, spline_fitted_M3,dist_colmedian_M3_normalized,dist_colmedian_M3,spline_sub_melt_M3)

# 
# Preparation of hourly payouts
# 

# We change the sign of the effect (payouts are a positive number)

spline_merged_M3$average_effect <- spline_merged_M3$average_effect * (-1) 
spline_merged_M3$lower_bound <- spline_merged_M3$lower_bound * (-1) 
spline_merged_M3$upper_bound <- spline_merged_M3$upper_bound * (-1) 
spline_merged_M3$value.x <- spline_merged_M3$value.x * (-1) 

# Prepare lower bound (no values below 0)
temp <- subset(spline_merged_M3[,c(2,3,6,7,8,9,10)])
colnames(temp)[5] <- "upper_bound"
colnames(temp)[6] <- "lower_bound"

# Remove negative values(required for geom_ribbon())

temp[which(temp$lower_bound<0),6] <- 0

# 
# Plot: hourly payout function
# 

payout_wheat_model_3 <- ggplot(temp) +
  ggtitle("Hourly payouts: Model 3")+ theme(panel.grid.major = element_blank(),
                                            plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
                                            axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
                                            axis.text = element_text(family="Times New Roman", size=10),
                                            panel.grid.minor = element_blank(),
                                            panel.background = element_blank(),
                                            axis.line = element_line(colour = "black"),
                                            legend.position="none")+
  xlab("Temperature (°C)") + ylab("Payout (dt/ha)")+
  scale_x_continuous(breaks = seq(10,39, by=5), limits = c(10,40))+
  scale_y_continuous(breaks = c(0,0.1, 0.2,0.3,0.4), limits = c(0,0.46))+
  geom_hline(yintercept=c(seq(0,0.4,by=0.1)), colour="gray")+
  geom_hline(yintercept=0, colour="darkgray")+
  geom_vline(xintercept= seq(5,38, by=5), colour="gray", linetype="dashed")+
  
  # Add lines
  
  geom_line(data=temp, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="royalblue1")+
  geom_line(data=temp, aes(y=average_effect, x=Temp_bin), size=0.75, color="royalblue4")+
  geom_line(data=temp, aes(y=lower_bound, x=Temp_bin), size=0.75, color="royalblue4")+
  geom_line(data=temp, aes(y=upper_bound, x=Temp_bin), size=0.75, color="royalblue4")

rm(temp,spline_merged_M3)


# -------------------------------------------
# Model 4: 
# -------------------------------------------

# Temperature time series
temperatures_ts_M4 <- rcspline.eval(sub_temperature_exposure_wheat$Temperature,knots= c(5,10,15,20,25), inclx=T)  

# Add new ts to dates
temp1 <- cbind(sub_temperature_exposure_wheat, temperatures_ts_M4)

# Aggregation to yearly values
final_wheat_temperature_aggregated_M4 <-temp1 %>%
  group_by(Farm,year)%>%
  summarise_at(vars(x,V2,V3,V4),sum)
rm(temp1)

# Combine yearly values with yields
temp1 <- wheat_yield_melted[which(wheat_yield_melted$Farm != farm_ID),]
data_wheat_final_M4 <- join(temp1, final_wheat_temperature_aggregated_M4, by=c("Farm","year"))
rm(temperatures_ts_M4,temp1,final_wheat_temperature_aggregated_M4)

# New TS for temperature support of interest
marginal_temperatures_M4 <- rcspline.eval(seq(-10,39,0.225),knots=knot_locations_kn5_wheat[,1], inclx=T)

temp_reg1 <- feols(yield ~ x + V2 +V3 +V4 + year + I(year^2) | Farm, data=data_wheat_final_M4)
summary(temp_reg1)
rm(temp_reg1)

# -------------------------------------------
# Statistical uncertainty
# -------------------------------------------

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M4 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M4))

for (l in 1: 1000){
  # Sample some years
  data_temp <- data_wheat_final_M4[which(data_wheat_final_M4$year %in% sample(unique(data_wheat_final_M4$year),replace=T)),]
  
  # Run regression with random subsample (data_temp)
  fit_boot  <- lm( yield ~ x + V2 +V3 +V4 + year + I(year^2)  + as.factor(Farm) -1, data=data_temp)
  
  # Save marginal effect of temperatures 
  spline_fitted_M4[l,]<-as.vector((marginal_temperatures_M4 %*% coef(fit_boot)[1:4]))
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Change structure
spline_sub_melt_M4 <- reshape2::melt(spline_fitted_M4)

# Get median response for each column (= a temperature from seq(-10,39,0.225))
colmedians_spline_M4 <- colMedians(spline_fitted_M4)

# Get distance from median response for each value
dist_colmedian_M4 <- matrix(NA, nrow=nrow(spline_fitted_M4),ncol=ncol(spline_fitted_M4))

for(l in 1:ncol(spline_fitted_M4)){
  dist_colmedian_M4[,l] <- spline_fitted_M4[,l] - colmedians_spline_M4[l]
}

# Normalize distance to median
# 1= largest value; 0= no distance
dist_colmedian_M4_normalized <- matrix(NA, nrow=nrow(spline_fitted_M4),ncol=ncol(spline_fitted_M4))

for(l in 1:ncol(spline_fitted_M4)){
  dist_colmedian_M4_normalized[,l]<-abs(dist_colmedian_M4[,l]/max(abs(dist_colmedian_M4[,l])))
}

# Change structure
dist_colmedian_M4_normalized_melted <- reshape2::melt(dist_colmedian_M4_normalized)

# Last preparation before plotting
# Var1 = 1:1000; Var2= temperature
# value.y ist normalized distance to median; value.x is estimated effect
spline_merged_M4 <- merge(spline_sub_melt_M4,dist_colmedian_M4_normalized_melted,by=c("Var1","Var2"))
spline_merged_M4$value.y_minus1 <- 1-spline_merged_M4$value.y

temp1 <- aggregate(spline_merged_M4$value.y_minus1, list(spline_merged_M4$Var1), mean, na.rm=T) 
temp2 <- aggregate(spline_merged_M4$value.x, list(spline_merged_M4$Var2), mean, na.rm=T)
temp3 <- aggregate(spline_merged_M4$value.x, list(spline_merged_M4$Var2), quantile, probs=0.025) 
temp4 <- aggregate(spline_merged_M4$value.x, list(spline_merged_M4$Var2), quantile, probs=0.975)

spline_merged_M4 <- merge(spline_merged_M4, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M4 <- merge(spline_merged_M4, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M4)[7]<-"average_effect"

spline_merged_M4<-merge(spline_merged_M4, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M4)[8]<-"lower_bound"

spline_merged_M4<-merge(spline_merged_M4, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M4)[9]<-"upper_bound"

temp5 <- cbind(seq(-10,39,0.225),seq(1:length(seq(-10,39,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M4 <-merge(spline_merged_M4, temp5, by=c("Var2"))

# 
# Plot: hourly temperature effect
# 

model_4 <- ggplot()+ ggtitle("Hourly temperature effects: Model 4") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=10),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Hourly yield response \n (dt/ha)")+
  scale_x_continuous(breaks = seq(-5,39, by=5), limits = c(-5,40))+
  scale_y_continuous(breaks = c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1), limits = c(-0.4,0.1))+
  geom_hline(yintercept=c(seq(-0.5,0.1,by=0.1)), colour="gray")+
  geom_vline(xintercept=knot_locations_kn5_wheat[,1], linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M4, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M4, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M4, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M4, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")  

rm(temp1,temp2,temp3,temp4,temp5)
rm(dist_colmedian_M4_normalized_melted, colmedians_spline_M4, spline_fitted_M4,dist_colmedian_M4_normalized,dist_colmedian_M4,spline_sub_melt_M4)

# 
# Preparation of hourly payouts
# 

# We change the sign of the effect (payouts are a positive number)

spline_merged_M4$average_effect <- spline_merged_M4$average_effect * (-1) 
spline_merged_M4$lower_bound <- spline_merged_M4$lower_bound * (-1) 
spline_merged_M4$upper_bound <- spline_merged_M4$upper_bound * (-1) 
spline_merged_M4$value.x <- spline_merged_M4$value.x * (-1) 

# Prepare lower bound (no values below 0)
temp <- subset(spline_merged_M4[,c(2,3,6,7,8,9,10)])
colnames(temp)[5] <- "upper_bound"
colnames(temp)[6] <- "lower_bound"

# Remove negative values(required for geom_ribbon())

temp[which(temp$lower_bound<0),6] <- 0

# 
# Plot: hourly payout function
# 

payout_wheat_model_4 <- ggplot(temp) +
  ggtitle("Hourly payouts: Model 4")+ theme(panel.grid.major = element_blank(),
                                            plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
                                            axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
                                            axis.text = element_text(family="Times New Roman", size=10),
                                            panel.grid.minor = element_blank(),
                                            panel.background = element_blank(),
                                            axis.line = element_line(colour = "black"),
                                            legend.position="none")+
  xlab("Temperature (°C)") + ylab("Payout (dt/ha)")+
  scale_x_continuous(breaks = seq(10,39, by=5), limits = c(10,40))+
  scale_y_continuous(breaks = c(0,0.1, 0.2,0.3,0.4), limits = c(0,0.46))+
  geom_hline(yintercept=c(seq(0,0.4,by=0.1)), colour="gray")+
  geom_hline(yintercept=0, colour="darkgray")+
  geom_vline(xintercept= seq(5,38, by=5), colour="gray", linetype="dashed")+
  
  # Add lines
  
  geom_line(data=temp, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="royalblue1")+
  geom_line(data=temp, aes(y=average_effect, x=Temp_bin), size=0.75, color="royalblue4")+
  geom_line(data=temp, aes(y=lower_bound, x=Temp_bin), size=0.75, color="royalblue4")+
  geom_line(data=temp, aes(y=upper_bound, x=Temp_bin), size=0.75, color="royalblue4")

rm(temp,spline_merged_M4)


models_payouts_wheat <- ggarrange(model_1,payout_wheat_model_1,model_2,payout_wheat_model_2,model_3,payout_wheat_model_3,model_4,payout_wheat_model_4,temperature_wheat, ncol=2, byrow=T, heights = c(1,1,1,1,0.5))
#ggsave(plot=models_payouts_wheat, file="models_payouts_wheat_d2.png", width = 16, height = 20, unit="cm")
rm(model_1,payout_wheat_model_1,model_2,payout_wheat_model_2,model_3,payout_wheat_model_3,model_4,payout_wheat_model_4,temperature_wheat)

# =======================================================================
#
# 2) Figure 3: hourly temperature effects and payouts for winter rapeseed
#
# =======================================================================

# The plots are representative for a farm drawn at random.
# Note that plots slightly vary between farms because of the out-of-sample calibration
set.seed(123)
farm_ID <- round(runif(1,1,nrow(farm_yields[[2]])))

# -------------------------------------------
# Histogram of temperature exposure
# -------------------------------------------

sub_temperature_exposure_rapeseed <- temp_rapeseed_exposure2[which(temp_rapeseed_exposure2$Farm != farm_ID),]

temperature_rapeseed <- ggplot(sub_temperature_exposure_rapeseed, aes(x=Temperature)) + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=10),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  ylab("Total exposure \n (1'000 hours)")+ xlab("Temperature (°C)")+
  geom_histogram(binwidth=1, color="black", fill="brown1")+
  scale_x_continuous(breaks = seq(-5,39, by=5), limits = c(-5,40))+
  scale_y_continuous(breaks = c(0,100000,200000), labels = c(0,100,200))

# -------------------------------------------
# Model 1: 
# -------------------------------------------

# Temperature time series
temperatures_ts_M1 <- rcspline.eval(sub_temperature_exposure_rapeseed$Temperature,knots= list_knot_locations_kn3_rapeseed[[farm_ID]][,1], inclx=T)  

# Add new ts to dates
temp1 <- cbind(sub_temperature_exposure_rapeseed, temperatures_ts_M1)

# Aggregation to yearly values
final_rapeseed_temperature_aggregated_M1 <-temp1 %>%
  group_by(Farm,year)%>%
  summarise_at(vars(x,V2),sum)
rm(temp1)

# Combine yearly values with yields
temp1 <- rapeseed_yield_melted[which(rapeseed_yield_melted$Farm != farm_ID),]
data_rapeseed_final_M1 <- join(temp1, final_rapeseed_temperature_aggregated_M1, by=c("Farm","year"))
rm(temperatures_ts_M1,temp1,final_rapeseed_temperature_aggregated_M1)

# New TS for temperature support of interest
marginal_temperatures_M1 <- rcspline.eval(seq(-10,39,0.225),knots=list_knot_locations_kn3_rapeseed[[farm_ID]][,1], inclx=T)

# Report regression output
temp_reg1 <- feols(yield ~ x + V2  + year + I(year^2) | Farm, data=data_rapeseed_final_M1)
summary(temp_reg1)
rm(temp_reg1)

# -------------------------------------------
# Statistical uncertainty
# -------------------------------------------

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample some years
  data_temp <- data_rapeseed_final_M1[which(data_rapeseed_final_M1$year %in% sample(unique(data_rapeseed_final_M1$year),replace=T)),]
  
  # Run regression with random subsample (data_temp)
  fit_boot  <- lm( yield ~ x + V2 +year + I(year^2) + as.factor(Farm)-1, data=data_temp)
  
  # Save marginal effect of temperatures 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1 %*% coef(fit_boot)[1:2]))
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Change structure
spline_sub_melt_M1 <- reshape2::melt(spline_fitted_M1)

# Get median response for each column (= a temperature from seq(3,39,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)

# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}

# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- reshape2::melt(dist_colmedian_M1_normalized)

# Last preparation before plotting

spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), mean, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), mean, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(-10,39,0.225),seq(1:length(seq(-10,39,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# 
# Plot: hourly temperature effect
# 

model_1 <- ggplot()+ ggtitle("Hourly temperature effects: Model 1") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=10),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Hourly yield response \n (dt/ha)")+
  scale_x_continuous(breaks = seq(-5,39, by=5), limits = c(-5,40))+
  scale_y_continuous(breaks = c(-0.2,-0.1,0,0.1), limits = c(-0.2,0.1))+
  geom_hline(yintercept=c(seq(-0.5,0.1,by=0.1)), colour="gray")+
  geom_vline(xintercept=list_knot_locations_kn3_rapeseed[[farm_ID]][,1], linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

rm(temp1,temp2,temp3,temp4,temp5)
rm(dist_colmedian_M1_normalized_melted, colmedians_spline_M1, spline_fitted_M1,dist_colmedian_M1_normalized,dist_colmedian_M1,spline_sub_melt_M1)

# 
# Preparation of hourly payouts
# 

# We change the sign of the effect (payouts are a positive number)

spline_merged_M1$average_effect <- spline_merged_M1$average_effect * (-1) 
spline_merged_M1$lower_bound <- spline_merged_M1$lower_bound * (-1) 
spline_merged_M1$upper_bound <- spline_merged_M1$upper_bound * (-1) 
spline_merged_M1$value.x <- spline_merged_M1$value.x * (-1) 

# Prepare lower bound (no values below 0)
temp <- subset(spline_merged_M1[,c(2,3,6,7,8,9,10)])
colnames(temp)[5] <- "upper_bound"
colnames(temp)[6] <- "lower_bound"

# Remove negative values(required for geom_ribbon())

temp[which(temp$lower_bound<0),6] <- 0

# 
# Plot: hourly payout function
# 

payout_rapeseed_model_1 <- ggplot(temp) +
  ggtitle("Hourly payouts: Model 1")+ theme(panel.grid.major = element_blank(),
                                            plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
                                            axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
                                            axis.text = element_text(family="Times New Roman", size=10),
                                            panel.grid.minor = element_blank(),
                                            panel.background = element_blank(),
                                            axis.line = element_line(colour = "black"),
                                            legend.position="none")+
  xlab("Temperature (°C)") + ylab("Payout (dt/ha)")+
  scale_x_continuous(breaks = seq(10,39, by=5), limits = c(10,40))+
  scale_y_continuous(breaks = c(0,0.1, 0.2,0.3,0.4), limits = c(0,0.2))+
  geom_hline(yintercept=c(seq(0,0.4,by=0.1)), colour="gray")+
  geom_hline(yintercept=0, colour="darkgray")+
  geom_vline(xintercept= seq(5,38, by=5), colour="gray", linetype="dashed")+
  
  # Add lines
  
  geom_line(data=temp, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="royalblue1")+
  geom_line(data=temp, aes(y=average_effect, x=Temp_bin), size=0.75, color="royalblue4")+
  geom_line(data=temp, aes(y=lower_bound, x=Temp_bin), size=0.75, color="royalblue4")+
  geom_line(data=temp, aes(y=upper_bound, x=Temp_bin), size=0.75, color="royalblue4")

rm(temp,spline_merged_M1)

# -------------------------------------------
# Model 2: 
# -------------------------------------------

# Temperature time series
temperatures_ts_M2 <- rcspline.eval(sub_temperature_exposure_rapeseed$Temperature,knots= list_knot_locations_kn3_rapeseed[[farm_ID]][,2], inclx=T)  

# Add new ts to dates
temp1 <- cbind(sub_temperature_exposure_rapeseed, temperatures_ts_M2)

# Aggregation to yearly values
final_rapeseed_temperature_aggregated_M2 <-temp1 %>%
  group_by(Farm,year)%>%
  summarise_at(vars(x,V2),sum)
rm(temp1)

# Combine yearly values with yields
temp1 <- rapeseed_yield_melted[which(rapeseed_yield_melted$Farm != farm_ID),]
data_rapeseed_final_M2 <- join(temp1, final_rapeseed_temperature_aggregated_M2, by=c("Farm","year"))
rm(temperatures_ts_M2,temp1,final_rapeseed_temperature_aggregated_M2)

# New TS for temperature support of interest
marginal_temperatures_M2 <- rcspline.eval(seq(-10,39,0.225),knots=list_knot_locations_kn3_rapeseed[[farm_ID]][,2], inclx=T)

# Report regression output
temp_reg1 <- feols(yield ~ x + V2  + year + I(year^2) | Farm, data=data_rapeseed_final_M2)
summary(temp_reg1)
rm(temp_reg1)

# -------------------------------------------
# Statistical uncertainty
# -------------------------------------------

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M2 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M2))

for (l in 1: 1000){
  # Sample some years
  data_temp <- data_rapeseed_final_M2[which(data_rapeseed_final_M2$year %in% sample(unique(data_rapeseed_final_M2$year),replace=T)),]
  
  # Run regression with random subsample (data_temp)
  fit_boot  <- lm( yield ~ x + V2 +year + I(year^2) + as.factor(Farm)-1, data=data_temp)
  
  # Save marginal effect of temperatures 
  spline_fitted_M2[l,]<-as.vector((marginal_temperatures_M2 %*% coef(fit_boot)[1:2]))
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Change structure
spline_sub_melt_M2 <-reshape2:: melt(spline_fitted_M2)

# Get median response for each column (= a temperature from seq(3,39,0.225))
colmedians_spline_M2 <- colMedians(spline_fitted_M2)

# Get distance from median response for each value
dist_colmedian_M2 <- matrix(NA, nrow=nrow(spline_fitted_M2),ncol=ncol(spline_fitted_M2))

for(l in 1:ncol(spline_fitted_M2)){
  dist_colmedian_M2[,l] <- spline_fitted_M2[,l] - colmedians_spline_M2[l]
}

# Normalize distance to median
dist_colmedian_M2_normalized <- matrix(NA, nrow=nrow(spline_fitted_M2),ncol=ncol(spline_fitted_M2))

for(l in 1:ncol(spline_fitted_M2)){
  dist_colmedian_M2_normalized[,l]<-abs(dist_colmedian_M2[,l]/max(abs(dist_colmedian_M2[,l])))
}

# Change structure
dist_colmedian_M2_normalized_melted <-reshape2::melt(dist_colmedian_M2_normalized)

# Last preparation before plotting

spline_merged_M2 <- merge(spline_sub_melt_M2,dist_colmedian_M2_normalized_melted,by=c("Var1","Var2"))
spline_merged_M2$value.y_minus1 <- 1-spline_merged_M2$value.y

temp1 <- aggregate(spline_merged_M2$value.y_minus1, list(spline_merged_M2$Var1), mean, na.rm=T) 
temp2 <- aggregate(spline_merged_M2$value.x, list(spline_merged_M2$Var2), mean, na.rm=T)

temp3 <- aggregate(spline_merged_M2$value.x, list(spline_merged_M2$Var2), quantile, probs=0.025) 
temp4 <- aggregate(spline_merged_M2$value.x, list(spline_merged_M2$Var2), quantile, probs=0.975)

spline_merged_M2 <- merge(spline_merged_M2, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M2 <- merge(spline_merged_M2, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M2)[7]<-"average_effect"

spline_merged_M2<-merge(spline_merged_M2, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M2)[8]<-"lower_bound"

spline_merged_M2<-merge(spline_merged_M2, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M2)[9]<-"upper_bound"

temp5 <- cbind(seq(-10,39,0.225),seq(1:length(seq(-10,39,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M2 <-merge(spline_merged_M2, temp5, by=c("Var2"))

# 
# Plot: hourly temperature effect
# 

model_2 <- ggplot()+ ggtitle("Hourly temperature effects: Model 2") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=10),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Hourly yield response \n (dt/ha)")+
  scale_x_continuous(breaks = seq(-5,39, by=5), limits = c(-5,40))+
  scale_y_continuous(breaks = c(-0.2,-0.1,0,0.1), limits = c(-0.2,0.1))+
  geom_hline(yintercept=c(seq(-0.5,0.1,by=0.1)), colour="gray")+
  geom_vline(xintercept=list_knot_locations_kn3_rapeseed[[farm_ID]][,2], linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M2, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M2, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M2, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M2, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

rm(temp1,temp2,temp3,temp4,temp5)
rm(dist_colmedian_M2_normalized_melted, colmedians_spline_M2, spline_fitted_M2,dist_colmedian_M2_normalized,dist_colmedian_M2,spline_sub_melt_M2)

# 
# Preparation of hourly payouts
# 

# We change the sign of the effect (payouts are a positive number)

spline_merged_M2$average_effect <- spline_merged_M2$average_effect * (-1) 
spline_merged_M2$lower_bound <- spline_merged_M2$lower_bound * (-1) 
spline_merged_M2$upper_bound <- spline_merged_M2$upper_bound * (-1) 
spline_merged_M2$value.x <- spline_merged_M2$value.x * (-1) 

# Prepare lower bound (no values below 0)
temp <- subset(spline_merged_M2[,c(2,3,6,7,8,9,10)])
colnames(temp)[5] <- "upper_bound"
colnames(temp)[6] <- "lower_bound"

# Remove negative values(required for geom_ribbon())

temp[which(temp$lower_bound<0),6] <- 0

# 
# Plot: hourly payout function
# 

payout_rapeseed_model_2 <- ggplot(temp) +
  ggtitle("Hourly payouts: Model 2")+ theme(panel.grid.major = element_blank(),
                                            plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
                                            axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
                                            axis.text = element_text(family="Times New Roman", size=10),
                                            panel.grid.minor = element_blank(),
                                            panel.background = element_blank(),
                                            axis.line = element_line(colour = "black"),
                                            legend.position="none")+
  xlab("Temperature (°C)") + ylab("Payout (dt/ha)")+
  scale_x_continuous(breaks = seq(10,39, by=5), limits = c(10,40))+
  scale_y_continuous(breaks = c(0,0.1, 0.2), limits = c(0,0.2))+
  geom_hline(yintercept=c(seq(0,0.4,by=0.1)), colour="gray")+
  geom_hline(yintercept=0, colour="darkgray")+
  geom_vline(xintercept= seq(5,38, by=5), colour="gray", linetype="dashed")+
  
  # Add lines
  
  geom_line(data=temp, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="royalblue1")+
  geom_line(data=temp, aes(y=average_effect, x=Temp_bin), size=0.75, color="royalblue4")+
  geom_line(data=temp, aes(y=lower_bound, x=Temp_bin), size=0.75, color="royalblue4")+
  geom_line(data=temp, aes(y=upper_bound, x=Temp_bin), size=0.75, color="royalblue4")

rm(temp,spline_merged_M2)

# -------------------------------------------
# Model 3: 
# -------------------------------------------

# Temperature time series
temperatures_ts_M3 <- rcspline.eval(sub_temperature_exposure_rapeseed$Temperature,knots= list_knot_locations_kn3_rapeseed[[farm_ID]][,3], inclx=T)  

# Add new ts to dates
temp1 <- cbind(sub_temperature_exposure_rapeseed, temperatures_ts_M3)

# Aggregation to yearly values
final_rapeseed_temperature_aggregated_M3 <-temp1 %>%
  group_by(Farm,year)%>%
  summarise_at(vars(x,V2),sum)
rm(temp1)

# Combine yearly values with yields
temp1 <- rapeseed_yield_melted[which(rapeseed_yield_melted$Farm != farm_ID),]
data_rapeseed_final_M3 <- join(temp1, final_rapeseed_temperature_aggregated_M3, by=c("Farm","year"))
rm(temperatures_ts_M3,temp1,final_rapeseed_temperature_aggregated_M3)

# New TS for temperature support of interest
marginal_temperatures_M3 <- rcspline.eval(seq(-10,39,0.225),knots=list_knot_locations_kn3_rapeseed[[farm_ID]][,3], inclx=T)

temp_reg1 <- feols(yield ~ x + V2  + year + I(year^2) | Farm, data=data_rapeseed_final_M3)
summary(temp_reg1)
rm(temp_reg1)

# -------------------------------------------
# Statistical uncertainty 
# -------------------------------------------

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M3 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M3))

for (l in 1: 1000){
  # Sample some years
  data_temp <- data_rapeseed_final_M3[which(data_rapeseed_final_M3$year %in% sample(unique(data_rapeseed_final_M3$year),replace=T)),]
  
  # Run regression with random subsample (data_temp)
  fit_boot  <- lm( yield ~ x + V2 +year + I(year^2) + as.factor(Farm)-1, data=data_temp)
  
  # Save marginal effect of temperatures 
  spline_fitted_M3[l,]<-as.vector((marginal_temperatures_M3 %*% coef(fit_boot)[1:2]))
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Change structure
spline_sub_melt_M3 <-reshape2::melt(spline_fitted_M3)


# Get median response for each column (= a temperature from seq(3,39,0.225))
colmedians_spline_M3 <- colMedians(spline_fitted_M3)

# Get distance from median response for each value
dist_colmedian_M3 <- matrix(NA, nrow=nrow(spline_fitted_M3),ncol=ncol(spline_fitted_M3))

for(l in 1:ncol(spline_fitted_M3)){
  dist_colmedian_M3[,l] <- spline_fitted_M3[,l] - colmedians_spline_M3[l]
}

# Normalize distance to median
dist_colmedian_M3_normalized <- matrix(NA, nrow=nrow(spline_fitted_M3),ncol=ncol(spline_fitted_M3))

for(l in 1:ncol(spline_fitted_M3)){
  dist_colmedian_M3_normalized[,l]<-abs(dist_colmedian_M3[,l]/max(abs(dist_colmedian_M3[,l])))
}

# Change structure
dist_colmedian_M3_normalized_melted <-reshape2::melt(dist_colmedian_M3_normalized)

# Last preparation before plotting

spline_merged_M3 <- merge(spline_sub_melt_M3,dist_colmedian_M3_normalized_melted,by=c("Var1","Var2"))
spline_merged_M3$value.y_minus1 <- 1-spline_merged_M3$value.y

temp1 <- aggregate(spline_merged_M3$value.y_minus1, list(spline_merged_M3$Var1), mean, na.rm=T) 
temp2 <- aggregate(spline_merged_M3$value.x, list(spline_merged_M3$Var2), mean, na.rm=T)

temp3 <- aggregate(spline_merged_M3$value.x, list(spline_merged_M3$Var2), quantile, probs=0.025) 
temp4 <- aggregate(spline_merged_M3$value.x, list(spline_merged_M3$Var2), quantile, probs=0.975)

spline_merged_M3 <- merge(spline_merged_M3, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M3 <- merge(spline_merged_M3, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M3)[7]<-"average_effect"

spline_merged_M3<-merge(spline_merged_M3, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M3)[8]<-"lower_bound"

spline_merged_M3<-merge(spline_merged_M3, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M3)[9]<-"upper_bound"

temp5 <- cbind(seq(-10,39,0.225),seq(1:length(seq(-10,39,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M3 <-merge(spline_merged_M3, temp5, by=c("Var2"))

# 
# Plot: hourly temperature effect
#

model_3 <- ggplot()+ ggtitle("Hourly temperature effects: Model 3") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=10),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Hourly yield response \n (dt/ha)")+
  scale_x_continuous(breaks = seq(-5,39, by=5), limits = c(-5,40))+
  scale_y_continuous(breaks = c(-0.2,-0.1,0,0.1), limits = c(-0.2,0.1))+
  geom_hline(yintercept=c(seq(-0.5,0.1,by=0.1)), colour="gray")+
  geom_vline(xintercept=list_knot_locations_kn3_rapeseed[[farm_ID]][,3], linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M3, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M3, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M3, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M3, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

rm(temp1,temp2,temp3,temp4,temp5)
rm(dist_colmedian_M3_normalized_melted, colmedians_spline_M3, spline_fitted_M3,dist_colmedian_M3_normalized,dist_colmedian_M3,spline_sub_melt_M3)

# 
# Preparation of hourly payouts
#

# We change the sign of the effect (payouts are a positive number)

spline_merged_M3$average_effect <- spline_merged_M3$average_effect * (-1) 
spline_merged_M3$lower_bound <- spline_merged_M3$lower_bound * (-1) 
spline_merged_M3$upper_bound <- spline_merged_M3$upper_bound * (-1) 
spline_merged_M3$value.x <- spline_merged_M3$value.x * (-1) 

# Prepare lower bound (no values below 0)
temp <- subset(spline_merged_M3[,c(2,3,6,7,8,9,10)])
colnames(temp)[5] <- "upper_bound"
colnames(temp)[6] <- "lower_bound"

# Remove negative values(required for geom_ribbon())

temp[which(temp$lower_bound<0),6] <- 0

#
# Plot: hourly payout function
#

payout_rapeseed_model_3 <- ggplot(temp) +
  ggtitle("Hourly payouts: Model 3")+ theme(panel.grid.major = element_blank(),
                                            plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
                                            axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
                                            axis.text = element_text(family="Times New Roman", size=10),
                                            panel.grid.minor = element_blank(),
                                            panel.background = element_blank(),
                                            axis.line = element_line(colour = "black"),
                                            legend.position="none")+
  xlab("Temperature (°C)") + ylab("Payout (dt/ha)")+
  scale_x_continuous(breaks = seq(10,39, by=5), limits = c(10,40))+
  scale_y_continuous(breaks = c(0,0.1, 0.2), limits = c(0,0.2))+
  geom_hline(yintercept=c(seq(0,0.4,by=0.1)), colour="gray")+
  geom_hline(yintercept=0, colour="darkgray")+
  geom_vline(xintercept= seq(5,38, by=5), colour="gray", linetype="dashed")+
  
  # Add lines
  
  geom_line(data=temp, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="royalblue1")+
  geom_line(data=temp, aes(y=average_effect, x=Temp_bin), size=0.75, color="royalblue4")+
  geom_line(data=temp, aes(y=lower_bound, x=Temp_bin), size=0.75, color="royalblue4")+
  geom_line(data=temp, aes(y=upper_bound, x=Temp_bin), size=0.75, color="royalblue4")

rm(temp,spline_merged_M3)

# -------------------------------------------
# Model 4: 
# -------------------------------------------

# Temperature time series
temperatures_ts_M4 <- rcspline.eval(sub_temperature_exposure_rapeseed$Temperature,knots= c(5,10,15,20,25), inclx=T)  

# Add new ts to dates
temp1 <- cbind(sub_temperature_exposure_rapeseed, temperatures_ts_M4)

# Aggregation to yearly values
final_rapeseed_temperature_aggregated_M4 <-temp1 %>%
  group_by(Farm,year)%>%
  summarise_at(vars(x,V2,V3,V4),sum)
rm(temp1)

# Combine yearly values with yields
temp1 <- rapeseed_yield_melted[which(rapeseed_yield_melted$Farm != farm_ID),]
data_rapeseed_final_M4 <- join(temp1, final_rapeseed_temperature_aggregated_M4, by=c("Farm","year"))
rm(temperatures_ts_M4,temp1,final_rapeseed_temperature_aggregated_M4)

# New TS for temperature support of interest
marginal_temperatures_M4 <- rcspline.eval(seq(-10,39,0.225),knots=c(5,10,15,20,25), inclx=T)

temp_reg1 <- feols(yield ~ x + V2 + V3 + V4 + year + I(year^2) | Farm, data=data_rapeseed_final_M4)
summary(temp_reg1)
rm(temp_reg1)

# -------------------------------------------
# Statistical uncertainty
# -------------------------------------------

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M4 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M4))

for (l in 1: 1000){
  # Sample some years
  data_temp <- data_rapeseed_final_M4[which(data_rapeseed_final_M4$year %in% sample(unique(data_rapeseed_final_M4$year),replace=T)),]
  
  # Run regression with random subsample (data_temp)
  fit_boot  <- lm( yield ~ x + V2+V3+V4 +year + I(year^2) + as.factor(Farm)-1, data=data_temp)
  
  # Save marginal effect of temperatures 
  spline_fitted_M4[l,]<-as.vector((marginal_temperatures_M4 %*% coef(fit_boot)[1:4]))
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Change structure
spline_sub_melt_M4 <-reshape2::melt(spline_fitted_M4)

# Get median response for each column (= a temperature from seq(3,39,0.225))
colmedians_spline_M4 <- colMedians(spline_fitted_M4)

# Get distance from median response for each value
dist_colmedian_M4 <- matrix(NA, nrow=nrow(spline_fitted_M4),ncol=ncol(spline_fitted_M4))

for(l in 1:ncol(spline_fitted_M4)){
  dist_colmedian_M4[,l] <- spline_fitted_M4[,l] - colmedians_spline_M4[l]
}

# Normalize distance to median
dist_colmedian_M4_normalized <- matrix(NA, nrow=nrow(spline_fitted_M4),ncol=ncol(spline_fitted_M4))

for(l in 1:ncol(spline_fitted_M4)){
  dist_colmedian_M4_normalized[,l]<-abs(dist_colmedian_M4[,l]/max(abs(dist_colmedian_M4[,l])))
}

# Change structure
dist_colmedian_M4_normalized_melted <-reshape2::melt(dist_colmedian_M4_normalized)

# Last preparation before plotting

spline_merged_M4 <- merge(spline_sub_melt_M4,dist_colmedian_M4_normalized_melted,by=c("Var1","Var2"))
spline_merged_M4$value.y_minus1 <- 1-spline_merged_M4$value.y

temp1 <- aggregate(spline_merged_M4$value.y_minus1, list(spline_merged_M4$Var1), mean, na.rm=T) 
temp2 <- aggregate(spline_merged_M4$value.x, list(spline_merged_M4$Var2), mean, na.rm=T)

temp3 <- aggregate(spline_merged_M4$value.x, list(spline_merged_M4$Var2), quantile, probs=0.025) 
temp4 <- aggregate(spline_merged_M4$value.x, list(spline_merged_M4$Var2), quantile, probs=0.975)

spline_merged_M4 <- merge(spline_merged_M4, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M4 <- merge(spline_merged_M4, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M4)[7]<-"average_effect"

spline_merged_M4<-merge(spline_merged_M4, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M4)[8]<-"lower_bound"

spline_merged_M4<-merge(spline_merged_M4, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M4)[9]<-"upper_bound"

temp5 <- cbind(seq(-10,39,0.225),seq(1:length(seq(-10,39,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M4 <-merge(spline_merged_M4, temp5, by=c("Var2"))

# 
# Plot: hourly temperature effect
# 

model_4 <- ggplot()+ ggtitle("Hourly temperature effects: Model 4") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=10),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Hourly yield response \n (dt/ha)")+
  scale_x_continuous(breaks = seq(-5,39, by=5), limits = c(-5,40))+
  scale_y_continuous(breaks = c(-0.2,-0.1,0,0.1), limits = c(-0.2,0.1))+
  geom_hline(yintercept=c(seq(-0.5,0.1,by=0.1)), colour="gray")+
  geom_vline(xintercept=knot_locations_kn5_rapeseed[,1], linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M4, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M4, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M4, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M4, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

rm(temp1,temp2,temp3,temp4,temp5)
rm(dist_colmedian_M4_normalized_melted, colmedians_spline_M4, spline_fitted_M4,dist_colmedian_M4_normalized,dist_colmedian_M4,spline_sub_melt_M4)

# 
# Preparation of hourly payouts
# 

# We change the sign of the effect (payouts are a positive number)

spline_merged_M4$average_effect <- spline_merged_M4$average_effect * (-1) 
spline_merged_M4$lower_bound <- spline_merged_M4$lower_bound * (-1) 
spline_merged_M4$upper_bound <- spline_merged_M4$upper_bound * (-1) 
spline_merged_M4$value.x <- spline_merged_M4$value.x * (-1) 

# Prepare lower bound (no values below 0)
temp <- subset(spline_merged_M4[,c(2,3,6,7,8,9,10)])
colnames(temp)[5] <- "upper_bound"
colnames(temp)[6] <- "lower_bound"

# Remove negative values(required for geom_ribbon())

temp[which(temp$lower_bound<0),6] <- 0

# 
# Plot: hourly payout function
# 

payout_rapeseed_model_4 <- ggplot(temp) +
  ggtitle("Hourly payouts: Model 4")+ theme(panel.grid.major = element_blank(),
                                            plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
                                            axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
                                            axis.text = element_text(family="Times New Roman", size=10),
                                            panel.grid.minor = element_blank(),
                                            panel.background = element_blank(),
                                            axis.line = element_line(colour = "black"),
                                            legend.position="none")+
  xlab("Temperature (°C)") + ylab("Payout (dt/ha)")+
  scale_x_continuous(breaks = seq(10,39, by=5), limits = c(10,40))+
  scale_y_continuous(breaks = c(0,0.1, 0.2), limits = c(0,0.2))+
  geom_hline(yintercept=c(seq(0,0.4,by=0.1)), colour="gray")+
  geom_hline(yintercept=0, colour="darkgray")+
  geom_vline(xintercept= seq(5,38, by=5), colour="gray", linetype="dashed")+
  
  # Add lines
  
  geom_line(data=temp, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="royalblue1")+
  geom_line(data=temp, aes(y=average_effect, x=Temp_bin), size=0.75, color="royalblue4")+
  geom_line(data=temp, aes(y=lower_bound, x=Temp_bin), size=0.75, color="royalblue4")+
  geom_line(data=temp, aes(y=upper_bound, x=Temp_bin), size=0.75, color="royalblue4")

rm(temp,spline_merged_M4)

models_payouts_rapeseed <- ggarrange(model_1,payout_rapeseed_model_1,model_2,payout_rapeseed_model_2,model_3,payout_rapeseed_model_3,model_4,payout_rapeseed_model_4,temperature_rapeseed, ncol=2, byrow=T, heights = c(1,1,1,1,0.5))
ggsave(plot=models_payouts_rapeseed, file="Plots/T Effect/models_payouts_rapeseed_d2.png", width = 16, height = 20, unit="cm")
rm(model_1,payout_rapeseed_model_1,model_2,payout_rapeseed_model_2,model_3,payout_rapeseed_model_3,model_4,payout_rapeseed_model_4,temperature_rapeseed)

# ===========================================================================================================

# 3) Figure 4: Out-of-sample risk reductions

# ===========================================================================================================


# 15, 20 and 25 degrees have the following position in "strike_temperature"
reported_strikes <- c(15,20,25)
strike_temperature_levels <- which( strike_temperature %in% reported_strikes)

# -------------------------------------------
# For winter wheat
# -------------------------------------------

# Relative change in risk premium (for plots)
# Positive means risk-decreasing

# Calculation of changes in risk premium per farm [strike, farm, model]
rel_RP_wheat_models <- array(dim=c(3,nrow(farm_yields[[1]]), ncol=4))
dimnames(rel_RP_wheat_models) [[1]] <- c("15", "20", "25")
dimnames(rel_RP_wheat_models) [[3]] <- c("Model 1", "Model 2", "Model 3", "Model 4")

for (l in 1:length(strike_temperature_levels)){
  for (i in 1:nrow(farm_yields[[1]])){
    
    # Model 1: Best fit
    rel_RP_wheat_models[l,i,1] <- (-100)*(summary_EU_wheat_insured_kn3[[strike_temperature_levels[l]]] [1,i,4] - summary_EU_wheat_uninsured[i,4]) / summary_EU_wheat_uninsured[i,4]
    
    # Model 2: Equal spaces
    rel_RP_wheat_models[l,i,2] <- (-100)*(summary_EU_wheat_insured_kn3[[strike_temperature_levels[l]]] [2,i,4] - summary_EU_wheat_uninsured[i,4]) / summary_EU_wheat_uninsured[i,4]
    
    # Model 3: Quantiles
    rel_RP_wheat_models[l,i,3] <- (-100)*(summary_EU_wheat_insured_kn3[[strike_temperature_levels[l]]] [3,i,4] - summary_EU_wheat_uninsured[i,4]) / summary_EU_wheat_uninsured[i,4]
    
    # Model 4: Schlenker (5 space)
    rel_RP_wheat_models[l,i,4] <- (-100)*(summary_EU_wheat_insured_kn5[[strike_temperature_levels[l]]] [1,i,4] - summary_EU_wheat_uninsured[i,4]) / summary_EU_wheat_uninsured[i,4]
  }
}

# Plot for strike level 20
# rel_RP_wheat_models[2,,] contains RP changes for strike level temperature 20

# data.frame for Asteriks
label.df <- data.frame(Var2=c("Model 1","Model 2","Model 3","Model 4"), Value = c(45,45,45,45))

rel_RP_wheat_models_melted <-reshape2::melt(as.matrix(rel_RP_wheat_models[2,,]))

wheat_20 <- ggplot(rel_RP_wheat_models_melted, aes(x=Var2, y=value)) + ggtitle("a) winter wheat \n strike level: 20°C") +
  geom_boxplot(notch = F, fill="gray", outlier.size = 0.1, coef=0) + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "black", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=10, colour = "black"),
        axis.text = element_text(family="Times New Roman", size=10, color="black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  ylab("Reduction in risk premium (%)") + xlab("")+
  scale_y_continuous(breaks = c(-20,-10,0,10,20,30,40), limits = c(-23, 50))+
  geom_hline(yintercept=c(-20,-10,0,10,20,30,40), linetype="dashed", colour="gray", alpha = 0.5)+
  geom_hline(yintercept=0, linetype="dashed", colour="grey20", alpha=0.6)+
  geom_text(data=label.df, aes(x=Var2, y=c(45,45,45,45)), label="***", size=4)


# Plot for strike level 25
rel_RP_wheat_models_melted_25 <-reshape2::melt(as.matrix(rel_RP_wheat_models[3,,]))

wheat_25 <- ggplot(rel_RP_wheat_models_melted_25, aes(x=Var2, y=value)) + ggtitle("b) winter wheat \n strike level: 25°C") +
  geom_boxplot(notch = F, fill="gray", outlier.size = 0.1, coef=0) + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "black", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=10, colour = "black"),
        axis.text = element_text(family="Times New Roman", size=10, color="black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  ylab("Reduction in risk premium (%)") + xlab("")+
  scale_y_continuous(breaks = c(-20,-10,0,10,20,30,40), limits = c(-23, 50))+
  geom_hline(yintercept=c(-20,-10,0,10,20,30,40), linetype="dashed", colour="gray", alpha=0.5)+
  geom_hline(yintercept=0, linetype="dashed", colour="grey20", alpha=0.6)+
  geom_text(data=label.df, aes(x=Var2, y=c(45,45,45,45)), label="***", size=4)

# -------------------------------------------
# For rapeseed
# -------------------------------------------

# Relative change in risk premium (for plots)
# Positive means risk-decreasing

rel_RP_rapeseed_models <- array(dim=c(3,nrow(farm_yields[[2]]), ncol=4))
dimnames(rel_RP_rapeseed_models) [[1]] <- c("15", "20", "25")
dimnames(rel_RP_rapeseed_models) [[3]] <- c("Model 1", "Model 2", "Model 3", "Model 4")

for (l in 1:length(strike_temperature_levels)){
  for (i in 1:nrow(farm_yields[[2]])){
    
    # Model 1: Best fit
    rel_RP_rapeseed_models[l,i,1] <- (-100)*(summary_EU_rapeseed_insured_kn3[[strike_temperature_levels[l]]] [1,i,4] - summary_EU_rapeseed_uninsured[i,4]) / summary_EU_rapeseed_uninsured[i,4]
    # Model 2: Equal spaces
    rel_RP_rapeseed_models[l,i,2] <- (-100)*(summary_EU_rapeseed_insured_kn3[[strike_temperature_levels[l]]] [2,i,4] - summary_EU_rapeseed_uninsured[i,4]) / summary_EU_rapeseed_uninsured[i,4]
    # Model 3: Quantiles
    rel_RP_rapeseed_models[l,i,3] <- (-100)*(summary_EU_rapeseed_insured_kn3[[strike_temperature_levels[l]]] [3,i,4] - summary_EU_rapeseed_uninsured[i,4]) / summary_EU_rapeseed_uninsured[i,4]
    # Model 4: (5 spaces)
    rel_RP_rapeseed_models[l,i,4] <- (-100)*(summary_EU_rapeseed_insured_kn5[[strike_temperature_levels[l]]] [1,i,4] - summary_EU_rapeseed_uninsured[i,4]) / summary_EU_rapeseed_uninsured[i,4]
  }
}

# Plot for strike level 15
rel_RP_rapeseed_models_melted <-reshape2::melt(as.matrix(rel_RP_rapeseed_models[1,,]))

rapeseed_15 <- ggplot(rel_RP_rapeseed_models_melted, aes(x=Var2, y=value)) + ggtitle("c) winter rapeseed \n strike level: 15°C") +
  geom_boxplot(notch = F, fill="gray", outlier.size = 0.1, coef=0) + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "black", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=10, colour = "black"),
        axis.text = element_text(family="Times New Roman", size=10, color="black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  ylab("Reduction in risk premium (%)") + xlab("")+
  scale_y_continuous(breaks = c(-40,-20,0,20,40,60),limits = c(-45, 70))+ 
  geom_hline(yintercept=c(-40,-20,0,20,40,60), linetype="dashed", colour="gray", alpha=0.5)+
  geom_hline(yintercept=0, linetype="dashed", colour="grey20", alpha=0.6)+
  geom_text(data=label.df, aes(x=Var2, y=c(65,65,65,65)), label="***", size=4)

# Plot for strike level 20
rel_RP_rapeseed_models_melted_20 <-reshape2::melt(as.matrix(rel_RP_rapeseed_models[2,,]))

rapeseed_20 <- ggplot(rel_RP_rapeseed_models_melted_20, aes(x=Var2, y=value)) + ggtitle("d) winter rapeseed \n strike level: 20°C") +
  geom_boxplot(notch = F, fill="gray", outlier.size = 0.1, coef=0) + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "black", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=10, colour = "black"),
        axis.text = element_text(family="Times New Roman", size=10, color="black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  ylab("Reduction in risk premium (%)") + xlab("")+ 
  scale_y_continuous(breaks = c(-40,-20,0,20,40,60), limits = c(-45, 70))+  
  geom_hline(yintercept=c(-40,-20,0,20,40,60), linetype="dashed", colour="gray", alpha=0.6)+
  geom_hline(yintercept=0, linetype="dashed", colour="grey20", alpha=0.6)+
  geom_text(data=label.df, aes(x=Var2, y=c(65,65,65,65)), label="***", size=4)

plot_risk <- ggarrange(wheat_20, wheat_25,rapeseed_15,rapeseed_20, nrow=2, ncol=2)
#ggsave(plot=plot_risk, file="Plots/risk-reduction/risk-reductions_round2_extremeaversion_d2.png", width = 16, height = 13, unit="cm")




