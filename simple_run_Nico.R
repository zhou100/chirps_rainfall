#username <- "nicolasgatti1987"
mydir <- paste("/Users/nicol/Desktop/UIUC/Baylis/Fire grant/Climate/Rainfall")
#mydir <- paste("C:\\Users\\Administrator\\Desktop\\", username, sep ="")

setwd(mydir)

library(ggplot2) #Calls: fortify, ggplot
library(rgdal)   
library(sp)      
library(raster)
library(gdalUtils)
library(parallel)
library(RCurl)
library(R.utils)
library(rgeos)

starttime <- proc.time() #begin processing timer

#Read Zambia map (shapefile)
# "../../map" is a relative path from your working directory - it means up two directories, then inside the map directory
ind=readOGR(dsn = "map.shp")
#Zam=readOGR("Ward",layer="ward_2010_exported")
 

## Define URL and file names for bulk download
url <-"ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/indonesia_monthly/bils/"

filename <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
filename <- strsplit(filename, "\r\n")
#filename <- strsplit(filename)
filenames <- unlist(filename)
#filenames
head(filename)

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

## Stack raster layers in a list
rlist <- list.files(path="C:/Users/nicol/Desktop/UIUC/Baylis/Fire grant/Climate/Rainfall/",
                    pattern = "bil$",
                    full.names=TRUE)
name_list_bil<- list.files(path="C:/Users/nicol/Desktop/UIUC/Baylis/Fire grant/Climate/Rainfall/",
                           pattern = "bil$",
                           full.names=FALSE)
name_list_bil<-unlist(strsplit(name_list_bil, ".bil"))

for (i in 1:length(rlist)){
  input_name=raster(rlist[i])
  output_name=paste(name_list_bil[i],"tif", sep =".")
  writeRaster(input_name, output_name,format="GTiff",datatype='INT1U',overwrite=TRUE)
}

tif_list <- list.files(path="C:/Users/nicol/Desktop/UIUC/Baylis/Fire grant/Climate/Rainfall/",
                       pattern = "tif$",
                       full.names=TRUE)
head(tif_list)

r <- stack(tif_list)


time2 <- proc.time() #start timer
cat("Download time:","\n")
summary(time2 - starttime)


#detach("package:R.utils", unload=TRUE) # to prevent errors in the extract below
mat.data <- c()
time1 <- proc.time() #start timer

for(i in 1:nlayers(r)) {
  ex <- extract(r[[i]], ind)
  clip1_ind <- crop(r[[i]], extent(ind)) #crop to extent of polygon
  clip2_ind <- rasterize(ind, clip1_ind, mask=TRUE)
  ex <- extract(clip2_ind, ind)
  #mat <- t(mclapply(ex, FUN = mean,mc.cores = 4)) #multiple core version
  mat <- t(lapply(ex, FUN = mean ))
  mat.data <-rbind(mat.data, mat)
}


colnames(mat.data)<-ind$NAME2       # Assign district names 

write.csv(mat.data,"CHIRPS_Indonesia_buffer.csv")

time3 <- proc.time() #end timer
cat("Processing time:","\n")
summary(time3 - time2)


endtime <- proc.time() #end timer
cat("Entire time:","\n")
summary(time2 - starttime)
 


