# set your working directory 
# setwd ("/Users/yujunzhou/desktop") 


library(ggplot2) #Calls: fortify, ggplot
library(rgdal)   
library(sp)      
library(raster)
library(gdalUtils)
library(parallel)
library(RCurl)
library(R.utils)
library(rgeos)


#Read TZlhz map (shapefile)
# "../../map" is a relative path from your working directory - it means up two directories, then inside the map directory
TZlhz=readOGR("TZ_LHZ_2009_proj",layer="buffer")
#TZlhz=readOGR("Ward",layer="ward_2010_exported")
 

start_year = 2008 # pick your start year
end_year = 2017 # pick your end year

## Define URL and file names for bulk download
url<-character(length = length(seq(start_year,end_year))) 
for (i in 1:length(url)){
  year<-seq(start_year,end_year)[i]
  url[i]<-paste(paste("ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_daily/tifs/p05",year,sep="/"),"/",sep="")
}


starttime <- proc.time() #start timer

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

# put the raster file names into a list 
rlist <- list.files(path=getwd(), 
                    pattern = "tif$",
                    full.names=TRUE)
## Stack raster layers in a list
r <- stack(rlist)

#rlist
#r

time2 <- proc.time() #start timer
cat("Download time:","\n")
summary(time2 - starttime)


detach("package:R.utils", unload=TRUE) # to prevent errors in the extract below
time1 <- proc.time() #start timer


mat.data.master <- c()
for(i in 1:nlayers(r)) {
  #ex <- extract(r[[i]], TZlhz)
  clip1_zam <- crop(r[[i]], extent(TZlhz)) #crop to extent of polygon
  clip2_zam <- rasterize(TZlhz, clip1_zam, mask=TRUE)
  ex <- extract(clip2_zam, TZlhz)
  #mat <- t(mclapply(ex, FUN = mean,mc.cores = 4)) #multiple core version
  mat.data <- t(lapply(ex, FUN = mean ))
  mat.data.master <-rbind(mat.data.master, mat.data)
}


colnames(mat.data.master)<-TZlhz$HHID   # Assign household ID 
overlap <- mat.data.master[ , colSums(is.na(mat.data.master)) != 0] # check for the overlapped ones 
mat.data.master<-mat.data.master[ , colSums(is.na(mat.data.master)) == 0] # save the complete data 

Zam_overlap<-subset(TZlhz, HHID %in% colnames(overlap))
mat.data.overlap <- c()
for(i in 1:nlayers(r)) {
  clip1_zam_overlap <- crop(r[[i]], extent(Zam_overlap)) #crop to extent of polygon
  clip2_zam_overlap <- rasterize(Zam_overlap, clip1_zam_overlap, update=TRUE,updateValue=NA)
  ex <- extract(clip2_zam_overlap, Zam_overlap)
  #mat <- t(mclapply(ex, FUN = mean,mc.cores = 4)) #multiple core version
  mat.data <- t(lapply(ex, FUN = mean ))
  mat.data.overlap <-rbind(mat.data.overlap, mat.data)
}

colnames(mat.data.overlap)<-Zam_overlap$HHID 
mat.data.master<-dplyr::bind_cols(mat.data.master,as.data.frame(mat.data.overlap))



write.csv(mat.data.master,"CHIRPS_TZlhz_buffer.csv")

time3 <- proc.time() #end timer
cat("Processing time:","\n")
summary(time3 - time2)


endtime <- proc.time() #end timer
cat("Entire time:","\n")
summary(time2 - starttime)
 


