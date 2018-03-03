# set your working directory 
username <- "zhou100"
mydir <- paste("/Users/yujunzhou/Google Drive/dataImprove/", username, sep ="")
setwd (mydir) 


library(ggplot2) #Calls: fortify, ggplot
library(rgdal)   
library(sp)      
library(raster)
library(gdalUtils)
library(parallel)
library(RCurl)
library(R.utils)
library(rgeos)


#Read Zambia map (shapefile)
# "../../map" is a relative path from your working directory - it means up two directories, then inside the map directory
Zam=readOGR(paste(getwd(),"/buffer/buffer.shp",sep = ""),layer="buffer")
#Zam=readOGR("Ward",layer="ward_2010_exported")
 

start_year = 2017 # pick your start year
end_year = 2017 # pick your end year

## Define URL and file names for bulk download
url<-character(length = length(seq(start_year,end_year)))
for (i in 1:length(url)){
  year<-seq(start_year,end_year)[i]
  url[i]<-paste(paste("ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_daily/tifs/p05",year,sep="/"),"/",sep="")
}


for (i in 1:length(seq(start_year,end_year))){
  
filename <- getURL(url[i], ftp.use.epsv = FALSE, dirlistonly = TRUE)
filename <- strsplit(filename, "\n")
# if you are winodws user, use this line instead
# filename <- strsplit(filename, "\r\n") # windows user
filenames <- unlist(filename)


for (filename in filenames) {
  download.file(paste(url[i], filename, sep = ""), 
                paste(getwd(), "/", filename, sep = ""))
}
}


## Unzip all the gz. files in working directory

zip.list <- list.files(getwd(),
                       pattern = "tif.gz$",
                       full.names = TRUE)
#zip.list
for(zipfile in zip.list) {
  gunzip(zipfile)
}

# 
rlist <- list.files(path=getwd(), 
                    pattern = "tif$",
                    full.names=TRUE)
## Stack raster layers in a list

r <- stack(rlist[1:5])

#rlist
#r

time2 <- proc.time() #start timer
cat("Download time:","\n")
summary(time2 - starttime)


detach("package:R.utils", unload=TRUE) # to prevent errors in the extract below
mat.data <- c()
time1 <- proc.time() #start timer

for(i in 1:nlayers(r)) {
  #ex <- extract(r[[i]], Zam)
  clip1_zam <- crop(r[[i]], extent(Zam)) #crop to extent of polygon
  clip2_zam <- rasterize(Zam, clip1_zam, mask=TRUE)
  ex <- extract(clip2_zam, Zam)
  #mat <- t(mclapply(ex, FUN = mean,mc.cores = 4)) #multiple core version
  mat <- t(lapply(ex, FUN = mean ))
  mat.data <-rbind(mat.data, mat)
}

dim()

colnames(mat.data)<-Zam$Dist_name       # Assign distrcit names 
write.csv(mat.data,"CHIRPS_Zambia_buffer.csv")

time3 <- proc.time() #end timer
cat("Processing time:","\n")
summary(time3 - time2)


endtime <- proc.time() #end timer
cat("Entire time:","\n")
summary(time2 - starttime)
 


