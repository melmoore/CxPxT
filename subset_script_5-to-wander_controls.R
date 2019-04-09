#Making a subset datafram of CxPxT for Geoff


library(readr)
library(Rmisc)



cpt <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt gr wide.csv")

long<-read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt.gr.long.load.csv")


#Removing those that died

cpt$died<-ifelse(cpt$date.died=="", "N", "Y")
cpt<-subset(cpt,!(died %in% "Y"))

long<-subset(long,died=="0")


#Subset to only controls:

cpt.con<-subset(cpt,treatment=="control")

long.con<-subset(long,treatment=="control")


#Figuring out how to subset to only 5th--wandering

cpt.con$instar.T8   #first molt to 5th at this time point
cpt.con$instar.T36  #Last time point with wanderers



#Removing extraneous rows that Geoff doesn't need


cpt.c5<-cpt.con[,-c(34:154)]
cpt.c5<-cpt.c5[,-c(505:729)]
cpt.c5<-cpt.c5[,-c(5:23)]
cpt.c5<-cpt.c5[,-grep("frass.",colnames(cpt.c5))]
cpt.c5<-cpt.c5[,-grep("fed.",colnames(cpt.c5))]
cpt.c5<-cpt.c5[,-grep("diet.",colnames(cpt.c5))]
cpt.c5<-cpt.c5[,-grep("sacrifice.",colnames(cpt.c5))]
cpt.c5<-cpt.c5[,-grep("chng.",colnames(cpt.c5))]
cpt.c5<-cpt.c5[,-grep(".expct",colnames(cpt.c5))]
cpt.c5<-cpt.c5[,-grep("dry.",colnames(cpt.c5))]
cpt.c5<-cpt.c5[,-grep("d.time",colnames(cpt.c5))]
cpt.c5<-cpt.c5[,-c(170:180)]
cpt.c5<-cpt.c5[,-c(211:213)]
cpt.c5<-cpt.c5[,-grep("date.",colnames(cpt.c5))]
colnames(cpt.c5)<-gsub("timepoint", "tp", colnames(cpt.c5))
cpt.c5<-cpt.c5[,-grep("time",colnames(cpt.c5))]
cpt.c5<-cpt.c5[,-c(101:113)]
cpt.c5<-cpt.c5[,-c(99:103)]
cpt.c5<-cpt.c5[,-130]
cpt.c5<-cpt.c5[,-97]
cpt.c5<-cpt.c5[,-grep("hour",colnames(cpt.c5))]


colnames(cpt.c5)



#subsetting long data frame for only 5ths and wanderers

long.con5<-subset(long.con,instar=="5" | instar=="w")


write.csv(cpt.c5, "CxPxT_control_5-to-wander.csv", row.names = FALSE)
write.csv(long.con5,"CxPxT_control_5-to-wand_long.csv", row.names = FALSE)


