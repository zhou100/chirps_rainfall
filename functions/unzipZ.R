unZipdotZ<-function(path,destfile,remove=TRUE){
  
  # this function is called for the side effect of uncompressing a .Z file
  
  # Zfile is a path to the Zfile
  
  # destfile is the uncompressed file to be written
  
  # no protection against overwriting
  
  # remove the Z file
  
  if(!file.exists(Zfile))stop( cat(Zfile,” does not exist”))
  
  handle <- file(Zfile, “rb”)
  
  data <- readBin(handle, “raw”, 99999999)
  
  close(handle)
  
  uncomp_data <- uncompress(data)
  
  handle <- file(destfile, “wb”)
  
  writeBin(uncomp_data, handle)
  
  close(handle)
  
  if(remove==TRUE)unlink(Zfile)
  
}