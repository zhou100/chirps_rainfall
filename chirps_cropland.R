

#setwd("C:\\Users\\Protensia\\Desktop\\")


library(rgdal)   
library(sp)      
library(raster)
library(gdalUtils)
library(RCurl)
library(R.utils)
library(rgeos)


#Read Zambia map (shapefile)
# "../../map" is a relative path from your working directory - it means up two directories, then inside the map directory
cropland=readOGR(dsn = "cropland_sage/cropland_country.shp")


## Define URL and file names for bulk download
url <-"ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_monthly/bils/"

filename <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
filename <- strsplit(filename, "\r\n")

filenames <- unlist(filename)

for (filename in filenames) {
  download.file(paste(url, filename, sep = ""), 
                paste(getwd(), "/", filename, sep = ""))
}


## Unzip all the gz. files in working directory
zip.list <- list.files(getwd(),
                       pattern = "tar.gz$",
                       full.names = TRUE)
#zip.list
for(zipfile in zip.list) {
  gunzip(zipfile)
}

tar.list <- list.files(getwd(),
                       pattern = "tar$",
                       full.names = TRUE)

for(tar in tar.list) {
  untar(tar)
}


## Stack raster layers in a list
rlist <- list.files(path=getwd(),
                    pattern = "bil$",
                    full.names=TRUE)
r <- stack(rlist)

# change the projection to help with match 
newproj<-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

cropland_proj <- spTransform(cropland,CRS=CRS(newproj))



detach("package:R.utils", unload=TRUE) # to prevent errors in the extract below
mat.data <- c()

for(i in 1:nlayers(r)) {
  crs(r[[i]])<-newproj
  clip1_cropland <- crop(r[[i]], extent(cropland_proj)) #crop to extent of polygon
  clip2_cropland <- rasterize(cropland_proj, clip1_cropland, mask=TRUE)
  ex <- extract(clip2_cropland, cropland_proj)
  #mat <- t(mclapply(ex, FUN = mean,mc.cores = 4)) #multiple core version
  mat <- t(lapply(ex, function(x){median(x,na.rm = TRUE)}   ))
  mat.data <-rbind(mat.data, mat)
}

colnames(mat.data)<-cropland$ISO3V10
rain<-as.data.frame(mat.data)

date_list<- gsub(getwd(), "",rlist)
date_list<- gsub("/v2p0chirps", "",date_list)
date_list<- gsub(".bil", "",date_list)

rain$year<-substr(date_list,1,4)
rain$month<-substr(date_list,5,6)
rain$yearmon<- paste(rain$year,rain$month,sep = "-")


write.csv(rain,"CHIRPS_cropland_global.csv")



