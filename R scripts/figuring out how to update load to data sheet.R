
#replaced original cpt gr wide with data set with load on 7/25/17, using this script


library(plyr)
library(readr)
load <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/7.25.17.csv", 
                 col_types = cols(temp = col_factor(levels = c("20", 
                                                               "25", "30")), treatment = col_factor(levels = c("control", 
                                                                                                               "para"))))
View(load)

wide <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt gr wide no load.csv", 
                 col_types = cols(temp = col_factor(levels = c("20", 
                                                               "25", "30")), treatment = col_factor(levels = c("control", 
                                                                                                               "para"))))

wide$num.unem<-NULL
wide$load<-NULL

load<-rename(load, c("ID"="num"))

keepvars<-c("num","treatment","temp","num.unem","load")
sub.l<-load[keepvars]

test<-merge(wide,sub.l,by=c("num","treatment","temp"))



wide$num %in% load$num

nrow(wide)
nrow(load)
nrow(test)
colnames(test)

test$num
test$load


write.csv(test,"cpt gr wide.csv",row.names=FALSE)

