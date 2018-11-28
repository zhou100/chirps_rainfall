#########################################################################################
# Script to extract weather data from nc format .
# Yujun Zhou 10/09/2018
# updated to global nc, Nov 27 2018

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

# 0. download and unzip the data 
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
library(ggplot2) #Calls: fortify, ggplot
library(rgdal)   
library(sp)      
library(raster)
library(gdalUtils)
library(RCurl)
library(R.utils)
library(rgeos)

############################################################################################################
# 1. read in the point shapefile and format it to data frame with lat,lon and district/province names.
########################################################################################################

# read in the point from QGIS that generate random points in the polygon
district.point.shape <- readOGR(dsn = "shapefiles/Philippines/phl_points_adm3/phl_points_adm3.shp", layer = "phl_points_adm3")


district.point.df= as.data.frame(district.point.shape)  # make the point shapefile to table

head(district.point.df)  # view the columns

# select lat and lons, as well as the district/province information; depending on the level of aggregation you want. 
district.point.df= district.point.df  %>% dplyr::select (xcoord, ycoord,ADM2_EN) 

# rename the columns
colnames(district.point.df)=c("lon","lat","province_name")

# add point id
district.point.df= tibble::rownames_to_column(district.point.df, var = "id")

# check if enough points
# dim(district.point.df)

#head(district.point.df)

# round the lat and lons to 3 digits to help with join, if needed 
# district.point.df$lon = round(district.point.df$lon,3)
# district.point.df$lat = round(district.point.df$lat,3)

# Plot to see if the point is right.
# plot(district.point.shape)




############################################################################################################
# 2. download, process and extract daily chirps data 1981-2018
########################################################################################################

## Define URL and file names for bulk download
url <-"ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_daily/netcdf/p25/"

filename <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
filename <- strsplit(filename, "\r\n")
#filename <- strsplit(filename,"\n") # mac version 

filenames <- unlist(filename)
filenames
filenames = filenames[-1] # remove the by month folder 

# ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_daily/old.netcdf/p25/
  
# create a folder for saving the rainfall data, 
dir.create("data") # comment out if the folder already exists
dir.create("data/chirps") # comment out if the folder already exists 
dir.create("data/chirps/nc_global") # comment out if the folder already exists


for (filename in filenames) {
  download.file(paste(url, filename, sep = ""),
                paste(getwd(), "/data/chirps/nc_global/", filename, sep = ""),method='curl')
}

# 
# ## Unzip all the gz. files in working directory
# zip.list <- list.files(getwd(),
#                        pattern = "tar.gz$",
#                        full.names = TRUE)
# #zip.list
# for(zipfile in zip.list) {
#   gunzip(zipfile)
# }
# 





#################################################################################
### Extract Precipitaion data 
#################################################################################

prec_list = list()
# Looping over differnet years. 
# remember to test for a single year first

 

start.year = 1981
end.year = 2018


for(year in start.year:end.year){
  
  ##############################################################################
  # read in the nc weather data for a given year 
  ##############################################################################
  print(paste0("starting_",year,"_prec"))
  start_time <- Sys.time()
  
  # Read in the nc data
  prec_nc <-nc_open(paste0("data/chirps/nc_global/chirps-v2.0.",year,".days_p25.nc"))

  # save information into separate data frames 
  lat_prec <-ncdf4::ncvar_get(prec_nc, varid="latitude")
  lon_prec <- ncdf4::ncvar_get(prec_nc, varid="longitude")
  time_prec <-ncdf4::ncvar_get(prec_nc, varid="time")
  prec_data <- ncdf4::ncvar_get(prec_nc,varid = "precip")
  
  dim(prec_data)
  head(prec_data)
  
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
    windowed_prec.daily <-as.data.frame(prec_data[lon_location, lat_location,  ])
    # names(windowed_prec) <-c("prec")
    
    # save the series in a matrix
    #windowed_prec.daily <-windowed_prec$prec
    
    # reshape the long series into a matrix ( 8 columns for 8 reads in a day, and 365 rows for 365 days) 
    #dim(windowed_prec.3hr)<-c(8,NROW(windowed_prec.3hr)/8) 
    
    #windowed_prec.daily<- data.frame(colSums(windowed_prec.3hr)) # sum daily precip
    
   # names(windowed_prec.daily) <- c("prec")

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
                    
     dplyr::group_by(province_name) %>%  # aggregate by district
     
     dplyr::select(-id,-lat,-lon) %>% 
                    
                    summarise_all(funs(mean(.,na.rm=TRUE))) # average of all the points in the same district
   
   # reshape and ready for join 
   district.names <- as.character(prec.district$province_name)
   prec.district.date = as.data.frame(t(prec.district[,-1]))
   rownames(prec.district.date) <- NULL
   colnames(prec.district.date) <- district.names
  
   
   
   ##############################################################################
   # Formatting the date variable and add as a column into the extracted weather data
   ##############################################################################
   # reshape the long series into a matrix ( 8 columns for 8 reads in a day, and 365 rows for 365 days) 
   date.day= time_prec
   # format the date into days 
   # [1] "days since 1900-1-1 00:00:00"

   
   date.day =  as.Date( date.day,origin = "1980-01-01")
   
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
extracted.prec = extracted.prec %>% dplyr::select(date,everything())

# head(extracted.prec)

# save data and clean the memory 
dir.create("data/clean")
save(extracted.prec,file="data/clean/phil_rain_8018.rda")
prec_data<-NULL





############################################################################################################
# 3. download, process and extract monthly temperature data 1981-2018
########################################################################################################

dir.create("data/GHCN")
url <-"ftp://ftp.cdc.noaa.gov/Datasets/ghcncams/air.mon.mean.nc"

# if you prefer udel data, use this link
# url = "ftp://ftp.cdc.noaa.gov/Datasets/udel.airt.precip/air.mon.mean.v401.nc"

download.file(url,paste(getwd(), "/data/GHCN/","air.mon.mean.nc",sep = ""))

# Read in the nc data
temp_nc <-nc_open("data/GHCN/air.mon.mean.nc")

#print basic information:
print(temp_nc) # lon_temp: degrees_east
#print(prec_nc)  # lon_prec: degrees_east


# save information into separate data frames 
lat_temp <-ncdf4::ncvar_get(temp_nc, varid="lat")
lon_temp_nc <- ncdf4::ncvar_get(temp_nc, varid="lon")
time_temp <-ncdf4::ncvar_get(temp_nc, varid="time")
temp_data <- ncdf4::ncvar_get(temp_nc, varid="air")

# need to transform lons to positive/negativv
lon_temp = ifelse(lon_temp_nc>180,lon_temp_nc-360,lon_temp_nc )

tmean_list = list()

#################################################################################
### Extract tmean
#################################################################################

  
  # dim(temp_data)
  # head(temp_data)
  
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
    lon_location = which.min(abs(lon_temp-pt_lon))
    lat_location = which.min(abs(lat_temp-pt_lat))
    
    # save all the weather data in the given point in the dataframe "windowed_temp"
    windowed_temp <-as.data.frame(temp_data[lon_location, lat_location,])
    names(windowed_temp) <-c("temp")
  
     tmean.C = windowed_temp-273.15
    
    
     tmean_list[[pt_id]] = tmean.C
    
    
  } # for point
  
  ##############################################################################
  # Formatting the extracted weather data 
  # aggregate the points in each district/province 
  ##############################################################################
  tmean.list.names <- unique(names(tmean_list))
  tmean.df.point =  data.frame(
    setNames(
      lapply(tmean.list.names, function(x) unlist(tmean_list[names(tmean_list) %in% x], 
                                                 use.names = FALSE)), tmean.list.names))
  

  tmean.df.point.transpose= as.data.frame(t(tmean.df.point)) %>%
    tibble::rownames_to_column(var = "id")

  # remove the "X" in the id to help with join
  tmean.df.point.transpose["id"]= gsub(tmean.df.point.transpose$id,pattern="X",replacement = "")
  
  # join the original point data frame to have the district information 
  tmean.district =  dplyr::inner_join(district.point.df,tmean.df.point.transpose,by="id") %>% 
    
    group_by(province_name) %>%  # aggregate by district
    
    dplyr::select(-id,-lat,-lon) %>% 
    
    summarise_all(funs(mean(.,na.rm=TRUE))) # average of all the points in the same district
  
  # reshape and ready for join 
  district.names <- as.character(tmean.district$province_name)
  tmean.district.date = as.data.frame(t(tmean.district[,-1]))
  rownames(tmean.district.date) <- NULL
  colnames(tmean.district.date) <- district.names
  
  
 
  
  # save the date into the data frame
  time_d <- as.POSIXct(time_temp*3600,origin='1800-01-01 00:00:0.0')
  date_formatted = as.Date(time_d)
  
  tmean.district.date$date = date_formatted
  
  # clean the temporary list 
  tmean_list = list()
  temp_data<-NULL
  

# reorder to make date as the first column
extracted.tmean = tmean.district.date %>% dplyr::select(date,everything())

# head(extracted.prec)

# save data and clean the memory 
save(extracted.tmean,file="data/clean/tmean_4918.rda")


