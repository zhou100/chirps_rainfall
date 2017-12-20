username <- "zhou100"
mydir <- paste("/Users/yujunzhou/Google Drive/dataImprove/", username, sep ="")

library(readr)
CHIRPS_Zambia_buffer <- read_csv("~/Google Drive/dataImprove/zhou100/CHIRPS_Zambia_buffer.csv")
buffer<-as.data.frame(CHIRPS_Zambia_buffer)
sub.buffer<-buffer[ , colSums(is.na(buffer)) == 0]
dlist<-colnames(sub.buffer)
dlist<-dlist[2:length(dlist)]
dlist<-as.data.frame(dlist)

colnames(dlist)<-"HHID"
#list2<-matrix(0,498,1)
#list2<-as.data.frame(list2)

dlist<-cbind(dlist,a=0)
write.csv(dlist,"delete_list.csv")

