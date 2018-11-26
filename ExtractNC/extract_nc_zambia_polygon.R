#########################################################################################
# Script to extract weather data from nc format .
# Yujun Zhou 10/09/2018

# Goal: extract daily weather data for each district, ready for calculating weather related measures. 

# Purpose: fast extraction of data in nc files, without transforming to rasters. 


# Input: 
# 1. Point shapefile containing geocoordinates and district information. 
# Transformed from a polygon , done in ArcGIS or QGIS. 
# Processing stesp in GIS:
# a) load a district polygon shapefile
# b) select a geoprocessing tool to generate random points inside polygon (the more the better)

# * Basically lots of points in each district and average them out to approximate a polygon.

# c) join by spatial location, to add the district (and any other columns if needed) to the point
# d) save the point shapfile 


# 2. precip and temperature in nc format( not contained in the project, too big)
# one read every three hours, 1990-2010,
# geocodinates


# Output:
# extracted daily weather data 1990-2016, average at the district level 
## saved weather data in .rda files

# Steps of extraction 

# 1. get the district point data 
# 2. get the lat, lon, time from the nc file (one for each year), as well as the data 
# 3. for every  point in the district point data, find the nearest point in the nc file.
# 4. join the weather and date information to the district point data
# 5. append all the points with the added in weather values
# 6. each points aggregtaed by day, (average precip, min/max temp)
# 7. repeats for all the other days in the same year.
# 8. repeats for all the other years.

# Note: for the date variable, it is every 3 hours, I will just take an average for the 8 daily reads
# The date format in this data is basically days from 1900-01-01 , use the following code to see
# as.Date( time_prec[1:10],origin = "1900-01-01")
#########################################################################################

library(ncdf4)
library(reshape2)
library(dplyr)
library(tidyr)

library(sp)
require(rgdal)

######################################################
# read in the point data and format it 
##################################################

# read in the point from QGIS that generate random points in the polygon
district.point.shape <- readOGR(dsn = "data/clean/random_wgs_points.shp", layer = "random_wgs_points")
# make the point shapefile to table
district.point.df= as.data.frame(district.point.shape) %>% select (coords.x1, coords.x2,District_n)
# rename the columns
colnames(district.point.df)=c("lon","lat","district")

# add point id
district.point.df= tibble::rownames_to_column(district.point.df, var = "id")

# check if enough points
# dim(district.point.df)

#head(district.point.df)

# round the lat and lons to 3 digits to help with join
district.point.df$lon = round(district.point.df$lon,3)
district.point.df$lat = round(district.point.df$lat,3)

# Plot to see if the point is right.
# plot(district.point.shape)

#################################################################################
### Extract Precipitaion data 
#################################################################################

prec_list = list()
# Looping over differnet years. 
# remember to test for a single year first

start.year = 1990
end.year = 2016

for(year in start.year:end.year){
  
  ##############################################################################
  # read in the nc weather data for a given year 
  ##############################################################################
  print(paste0("starting_",year,"_prec"))
  start_time <- Sys.time()
  
  
  # Read in the nc data
  prec_nc <-nc_open(paste0("C://Users//Administrator//Desktop//mswep_precip//prec_",year ,"_5km_zambia.nc"))

  # save information into separate data frames 
  lat_prec <-ncdf4::ncvar_get(prec_nc, varid="lat")
  lon_prec <- ncdf4::ncvar_get(prec_nc, varid="lon")
  time_prec <-ncdf4::ncvar_get(prec_nc, varid="time")
  prec_data <- ncdf4::ncvar_get(prec_nc, varid="precipitation")
  
  # dim(prec_data)
  # head(prec_data)
  
  ##############################################################################
  # loop over the points in each district 
  # for each point in the point data frame, find every day's weather in a given year
  ##############################################################################
  for (i in 1:NROW(district.point.df)){

    # save the point coordinates temporarily 
    pt_lon <- district.point.df$lon[i]
    pt_lat <- district.point.df$lat[i]
    pt_id = district.point.df$id[i]
    
    # find the nearest point location for any given point in your district point data 
    lon_location = which.min(abs(lon_prec-pt_lon))
    lat_location = which.min(abs(lat_prec-pt_lat))
    
    # save all the weather data in the given point in the dataframe "windowed_prec"
    windowed_prec <-data.frame(prec_data[lon_location, lat_location,])
    names(windowed_prec) <-c("prec")
    
    # save the series in a matrix
    windowed_prec.3hr <-windowed_prec$prec
    
    # reshape the long series into a matrix ( 8 columns for 8 reads in a day, and 365 rows for 365 days) 
    dim(windowed_prec.3hr)<-c(8,NROW(windowed_prec.3hr)/8) 
    
    windowed_prec.daily<- data.frame(colSums(windowed_prec.3hr)) # sum daily precip
    
    names(windowed_prec.daily) <- c("prec")

    if (year==start.year){
      prec_list[[pt_id]] = windowed_prec.daily
    } else {
      prec_list[[pt_id]]<- rbind(prec_list[[pt_id]],windowed_prec.daily)
    }
    
  } # for point
  
  ##############################################################################
  # Formatting the extracted weather data in a given year 
  # aggregate the points in each district 
  ##############################################################################
  prec.list.names <- unique(names(prec_list))
  prec.df.point =  data.frame(
    setNames(
      lapply(prec.list.names, function(x) unlist(prec_list[names(prec_list) %in% x], 
                                                 use.names = FALSE)), prec.list.names))
  
  
  prec.df.point.transpose= as.data.frame(t(prec.df.point)) %>%
                           tibble::rownames_to_column(var = "id")
  
   # remove the "X" in the data 
   prec.df.point.transpose$id = as.character(as.numeric(as.factor(prec.df.point.transpose$id)))
  

   # join the original point data frame to have the district information 
   prec.district =  dplyr::inner_join(district.point.df,prec.df.point.transpose,by="id") %>% 
                    
                    group_by(district) %>%  # aggregate by district
     
                    select(-id,-lat,-lon) %>% 
                    
                    summarise_all(funs(mean(.))) # average of all the points in the same district
   
   # reshape and ready for join 
   district.names <- as.character(prec.district$district)
   prec.district.date = as.data.frame(t(prec.district[,-1]))
   rownames(prec.district.date) <- NULL
   colnames(prec.district.date) <- district.names
  
   
   
   ##############################################################################
   # Formatting the date variable and add as a column into the extracted weather data
   ##############################################################################
   # reshape the long series into a matrix ( 8 columns for 8 reads in a day, and 365 rows for 365 days) 
   date.3hr = time_prec
   dim(date.3hr)<-c(8,NROW(date.3hr)/8) 
   date.day = date.3hr[1,]
   # format the date into days 
   # [1] "days since 1900-1-1 00:00:00"

   
   date.day =  as.Date( date.day,origin = "1900-01-01")
   
   # save the date into the 
   prec.district.date$date = date.day
  
   # clean the temporary 
   prec_list = list()
   
   # save the data and avoid problems in combing data.
  if (year==start.year){
    extracted.prec = prec.district.date
  } else {
    extracted.prec = rbind(extracted.prec,prec.district.date)
  }
  
   # counting the processing time 
   end_time <- Sys.time()
   print("time for one year is")
   print(end_time - start_time)
  
} # for year


# reorder to make date as the first column
extracted.prec = extracted.prec %>% select(date,everything())

# head(extracted.prec)

# save data and clean the memory 
save(extracted.prec,file="data/clean/rain_9016.rda")
prec_data<-NULL


#################################################################################
### Extract tmax, tmin data 
#################################################################################

tmax_list = list()
tmin_list = list()

# Looping over differnet years. 
# remember to test for a single year first

start.year = 1990
end.year = 2016

for(year in start.year:end.year){
  
  ##############################################################################
  # read in the nc weather data for a given year 
  ##############################################################################
  print(paste0("starting_",year,"_temp"))
  start_time <- Sys.time()
  
  
  # Read in the nc data
  temp_nc <-nc_open(paste0("C://Users//Administrator//Desktop//mswep_temp//tas_",year ,"_5km_zambia.nc"))
  
  # save information into separate data frames 
  lat_temp <-ncdf4::ncvar_get(temp_nc, varid="lat")
  lon_temp <- ncdf4::ncvar_get(temp_nc, varid="lon")
  time_temp <-ncdf4::ncvar_get(temp_nc, varid="time")
  temp_data <- ncdf4::ncvar_get(temp_nc, varid="data")
  
  # dim(temp_data)
  # head(temp_data)
  
  ##############################################################################
  # loop over the points in each district 
  # for each point in the point data frame, find every day's weather in a given year
  ##############################################################################
  for (i in 1:NROW(district.point.df)){
    #
    
    # save the point coordinates temporarily 
    pt_lon <- district.point.df$lon[i]
    pt_lat <- district.point.df$lat[i]
    pt_id = district.point.df$id[i]
    
    # find the nearest point location for any given point in your district point data 
    lon_location = which.min(abs(lon_temp-pt_lon))
    lat_location = which.min(abs(lat_temp-pt_lat))
    
    # save all the weather data in the given point in the dataframe "windowed_temp"
    windowed_temp <-data.frame(temp_data[lon_location, lat_location,])
    names(windowed_temp) <-c("temp")
    
    # save the series in a matrix
    windowed_temp.3hr <-windowed_temp$temp
    
    # reshape the long series into a matrix ( 8 columns for 8 reads in a day, and 365 rows for 365 days) 
    dim(windowed_temp.3hr)<-c(8,NROW(windowed_temp.3hr)/8) 
    
    #windowed_temp.daily<- data.frame(colSums(windowed_temp.3hr)) # sum daily precip
    
    
    tmax.daily.K <- data.frame(apply(windowed_temp.3hr, 2, function(x) max(x, na.rm = TRUE))) ## find daily max
    names(tmax.daily.K) <- c("tmax")
    tmax.daily.C <- tmax.daily.K - 273.15 # convert to C
    
    tmin.daily.K <- data.frame(apply(windowed_temp.3hr, 2, function(x) min(x, na.rm = TRUE))) ## find daily min
    names(tmin.daily.K) <- c("tmin")
    tmin.daily.C <- tmin.daily.K -273.15 # convert to C
    
    
    
    
    if (year==start.year){
      tmax_list[[pt_id]] = tmax.daily.C
      tmin_list[[pt_id]] = tmin.daily.C
      
    } else {
      tmax_list[[pt_id]]<- rbind(tmax_list[[pt_id]],tmax.daily.C)
      tmin_list[[pt_id]]<- rbind(tmin_list[[pt_id]],tmin.daily.C)
      
    }
    
  } # for point
  
  ##############################################################################
  # Formatting the extracted weather data in a given year 
  # aggregate the points in each district 
  ##############################################################################
  tmax.list.names <- unique(names(tmax_list))
  tmax.df.point =  data.frame(
    setNames(
      lapply(tmax.list.names, function(x) unlist(tmax_list[names(tmax_list) %in% x], 
                                                 use.names = FALSE)), tmax.list.names))
  
  
  tmax.df.point.transpose= as.data.frame(t(tmax.df.point)) %>%
    tibble::rownames_to_column(var = "id")
  
  # remove the "X" in the data 
  tmax.df.point.transpose$id = as.character(as.numeric(as.factor(tmax.df.point.transpose$id)))
  
  
  # join the original point data frame to have the district information 
  tmax.district =  dplyr::inner_join(district.point.df,tmax.df.point.transpose,by="id") %>% 
    
    group_by(district) %>%  # aggregate by district
    
    select(-id,-lat,-lon) %>% 
    
    summarise_all(funs(mean(.))) # average of all the points in the same district
  
  # reshape and ready for join 
  district.names <- as.character(tmax.district$district)
  tmax.district.date = as.data.frame(t(tmax.district[,-1]))
  rownames(tmax.district.date) <- NULL
  colnames(tmax.district.date) <- district.names
  
  
  ################################################################################
  #### Do the same for tmins 
  ########################################################################
  tmin.list.names <- unique(names(tmin_list))
  tmin.df.point =  data.frame(
    setNames(
      lapply(tmin.list.names, function(x) unlist(tmin_list[names(tmin_list) %in% x], 
                                                 use.names = FALSE)), tmin.list.names))
  
  
  tmin.df.point.transpose= as.data.frame(t(tmin.df.point)) %>%
    tibble::rownames_to_column(var = "id")
  
  # remove the "X" in the data 
  tmin.df.point.transpose$id = as.character(as.numeric(as.factor(tmin.df.point.transpose$id)))
  
  
  # join the original point data frame to have the district information 
  tmin.district =  dplyr::inner_join(district.point.df,tmin.df.point.transpose,by="id") %>% 
    
    group_by(district) %>%  # aggregate by district
    
    select(-id,-lat,-lon) %>% 
    
    summarise_all(funs(mean(.))) # average of all the points in the same district
  
  # reshape and ready for join 
  district.names <- as.character(tmin.district$district)
  tmin.district.date = as.data.frame(t(tmin.district[,-1]))
  rownames(tmin.district.date) <- NULL
  colnames(tmin.district.date) <- district.names
  
  
  ##############################################################################
  # Formatting the date variable and add as a column into the extracted weather data
  ##############################################################################
  # reshape the long series into a matrix ( 8 columns for 8 reads in a day, and 365 rows for 365 days) 
  # divide by 24 to convert date from hours to days 
  ## Time unit of the temperature nc file: "hours since 2016-1-1 00:00:00"
  
  temp.date.3hr = time_temp/24
  dim(temp.date.3hr)<-c(8,NROW(temp.date.3hr)/8) 
  temp.date.day = temp.date.3hr[1,]
  # format the date into days 
  
  
  ## "hours since 2016-1-1 00:00:00"
  
  temp.date.day =  as.Date( temp.date.day,origin = paste(as.character(year),"-01-01",sep=""))
  
  # save the date into the 
  tmin.district.date$date = temp.date.day
  tmax.district.date$date = temp.date.day
  
  # clean the temporary list 
  tmax_list = list()
  tmin_list = list()
  
  
  # save the data and avoid problems in combing data.
  if (year==start.year){
    extracted.tmin = tmin.district.date
    extracted.tmax = tmax.district.date
  } else {
    extracted.tmin = rbind(extracted.tmin,tmin.district.date)
    extracted.tmax = rbind(extracted.tmax,tmax.district.date)
    
  }
  
  # counting the processing time 
  end_time <- Sys.time()
  print("time for one year is")
  print(end_time - start_time)
  
} # for year


# reorder to make date as the first column
extracted.tmin = extracted.tmin %>% select(date,everything())
extracted.tmax = extracted.tmax %>% select(date,everything())

# head(extracted.prec)

# save data and clean the memory 
save(extracted.tmin,file="data/clean/tmin_9016.rda")
save(extracted.tmax,file="data/clean/tmax_9016.rda")
temp_data<-NULL


