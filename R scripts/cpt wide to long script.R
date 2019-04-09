#CxPxT shaping script--changing from wide format to long format--to get initial graph of mass by time

#1. Put data for larval measurements in long format.  There are fields Date.zh, Time.zh, Mass.zh, Fed.zh, where z is a number (integer) from 0 to 144. I’d like to create a new variable Hours whose value = z, and put the data in long form, i.e.
# BugID    treatment           Temp           Hours    Date      Time      Fed        Mass  (and either repeat or drop the rest of the fields for now—let’s figure these out later)
# 2.Plot ln(Mass) vs Hours for each individual (maybe include mean line as well) for each Temp.test as a separate panel.


library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(Rmisc)
library(readr)


data <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt gr wide.csv", 
                 col_types = cols(died = col_factor(levels = c("0", 
                "1")), sacrifice = col_factor(levels = c("0", 
                "1")), temp = col_factor(levels = c("30", 
                "25", "20")), treatment = col_factor(levels = c("control", 
                                                                "para"))))
View(data)


#Converting data from wide format to long

data_Date<- melt(data, id.vars=c("bug.id","treatment","temp","load","mass.T0","died"), measure.vars=c("date.3","date.T1","date.T2","date.T3",	"date.T4",	"date.T5",	"date.T6",	"date.T7",	"date.T8",	"date.T9",	"date.T10",	"date.T11",	"date.T12",	"date.T13",	"date.T14",	"date.T15",	"date.T16",	"date.T17",	"date.T18",	"date.T19",	"date.T20",	"date.T21",	"date.T22",	"date.T23",	"date.T24",	"date.T25",	"date.T26",	"date.T27",	"date.T28",	"date.T29",	"date.T30",	"date.T31",	"date.T32",	"date.T33",	"date.T34",	"date.T35",	"date.T36",	"date.T37",	"date.T38"),variable.name="Timepoint",value.name="Date")
data_Date$Timepoint<-as.character(data_Date$Timepoint)
data_Date$Timepoint[data_Date$Timepoint == "date.3"] <- "T0"
data_Date$Timepoint<- gsub("date.", "",data_Date$Timepoint)


data$T0.expct<-0
data_Expct<-melt(data,id.vars=c("bug.id","treatment","temp","load","mass.T0","died"),measure.vars=c("T0.expct","T1.expct","T2.expct","T3.expct","T4.expct","T5.expct","T6.expct","T7.expct","T8.expct","T9.expct","T10.expct","T11.expct","T12.expct","T13.expct","T14.expct","T15.expct","T16.expct","T17.expct","T18.expct","T19.expct","T20.expct","T21.expct","T22.expct","T23.expct","T24.expct","T25.expct","T26.expct","T27.expct","T28.expct","T29.expct","T30.expct","T31.expct","T32.expct","T33.expct","T34.expct","T35.expct","T36.expct","T37.expct","T38.expct"),variable.name="Timepoint",value.name="expct.hour") 
data_Expct$Timepoint<- gsub(".expct", "",data_Expct$Timepoint)



data_Age<-melt(data,id.vars=c("bug.id","treatment","temp","load","mass.T0","died"),measure.vars=c("age.T0","age.T1","age.T2","age.T3","age.T4","age.T5","age.T6","age.T7","age.T8","age.T9","age.T10","age.T11","age.T12","age.T13","age.T14","age.T15","age.T16","age.T17","age.T18","age.T19","age.T20","age.T21","age.T22","age.T23","age.T24","age.T25","age.T26","age.T27","age.T28","age.T29","age.T30","age.T31","age.T32","age.T33","age.T34","age.T35","age.T36","age.T37","age.T38"),variable.name="Timepoint",value.name="age") 
data_Age$Timepoint<- gsub("age.", "",data_Age$Timepoint)


data$fed.T0<-NA
data_Fed<-melt(data,id.vars=c("bug.id","treatment","temp","load","mass.T0","died"),measure.vars=c("fed.T0","fed.T1","fed.T2","fed.T3","fed.T4","fed.T5","fed.T6","fed.T7","fed.T8","fed.T9","fed.T10","fed.T11","fed.T12","fed.T13","fed.T14","fed.T15","fed.T16","fed.T17","fed.T18","fed.T19","fed.T20","fed.T21","fed.T22","fed.T23","fed.T24","fed.T25","fed.T26","fed.T27","fed.T28","fed.T29","fed.T30","fed.T31","fed.T32","fed.T33","fed.T34","fed.T35","fed.T36","fed.T37"),variable.name="Timepoint",value.name="fed")   
data_Fed$Timepoint<- gsub("fed.", "",data_Fed$Timepoint)


data_Mass<-melt(data,id.vars=c("bug.id","treatment","temp","load","mass.T0","died"),measure.vars=c("mass.T0","mass.T1","mass.T2","mass.T3","mass.T4","mass.T5","mass.T6","mass.T7","mass.T8","mass.T9","mass.T10","mass.T11","mass.T12","mass.T13","mass.T14","mass.T15","mass.T16","mass.T17","mass.T18","mass.T19","mass.T20","mass.T21","mass.T22","mass.T23","mass.T24","mass.T25","mass.T26","mass.T27","mass.T28","mass.T29","mass.T30","mass.T31","mass.T32","mass.T33","mass.T34","mass.T35","mass.T36","mass.T37"),variable.name="Timepoint",value.name="mass")  
data_Mass$Timepoint<- gsub("mass.", "",data_Mass$Timepoint)


data_mass_gain<-melt(data,id.vars=c("bug.id","treatment","temp","load","mass.T0","died"),measure.vars=c("mass.gain.T0","mass.gain.T1","mass.gain.T2","mass.gain.T3","mass.gain.T4","mass.gain.T5","mass.gain.T6","mass.gain.T7","mass.gain.T8","mass.gain.T9","mass.gain.T10","mass.gain.T11","mass.gain.T12","mass.gain.T13","mass.gain.T14","mass.gain.T15","mass.gain.T16","mass.gain.T17","mass.gain.T18","mass.gain.T19","mass.gain.T20","mass.gain.T21","mass.gain.T22","mass.gain.T23","mass.gain.T24","mass.gain.T25","mass.gain.T26","mass.gain.T27","mass.gain.T28","mass.gain.T29","mass.gain.T30","mass.gain.T31","mass.gain.T32","mass.gain.T33","mass.gain.T34","mass.gain.T35","mass.gain.T36","mass.gain.T37"),variable.name="Timepoint",value.name="mass.gain")  
data_mass_gain$Timepoint<- gsub("mass.gain.", "",data_mass_gain$Timepoint)



data_Time<-melt(data,id.vars=c("bug.id","treatment","temp","load","mass.T0","died"),measure.vars=c("time.T0","time.T1","time.T2","time.T3","time.T4","time.T5","time.T6","time.T7","time.T8","time.T9","time.T10","time.T11","time.T12","time.T13","time.T14","time.T15","time.T16","time.T17","time.T18","time.T19","time.T20","time.T21","time.T22","time.T23","time.T24","time.T25","time.T26","time.T27","time.T28","time.T29","time.T30","time.T31","time.T32","time.T33","time.T34","time.T35","time.T36","time.T37","time.T38"),variable.name="Timepoint",value.name="time") 
data_Time$Timepoint<- gsub("time.", "",data_Time$Timepoint)


data_Instar<-melt(data,id.vars=c("bug.id","treatment","temp","load","mass.T0","died"),measure.vars=c("instar.T0","instar.T1","instar.T2","instar.T3","instar.T4","instar.T5","instar.T6","instar.T7","instar.T8","instar.T9","instar.T10","instar.T11","instar.T12","instar.T13","instar.T14","instar.T15","instar.T16","instar.T17","instar.T18","instar.T19","instar.T20","instar.T21","instar.T22","instar.T23","instar.T24","instar.T25","instar.T26","instar.T27","instar.T28","instar.T29","instar.T30","instar.T31","instar.T32","instar.T33","instar.T34","instar.T35","instar.T36","instar.T37","instar.T38"),variable.name="Timepoint",value.name="instar")
data_Instar$Timepoint<- gsub("instar.", "",data_Instar$Timepoint)


data_diet<-melt(data,id.vars=c("bug.id","treatment","temp","load","mass.T0","died"),measure.vars=c("diet.cont.in.T0","diet.cont.in.T1","diet.cont.in.T2","diet.cont.in.T3","diet.cont.in.T4","diet.cont.in.T5","diet.cont.in.T6","diet.cont.in.T7","diet.cont.in.T8","diet.cont.in.T9","diet.cont.in.T10","diet.cont.in.T11","diet.cont.in.T12","diet.cont.in.T13","diet.cont.in.T14","diet.cont.in.T15","diet.cont.in.T16","diet.cont.in.T17","diet.cont.in.T18","diet.cont.in.T19","diet.cont.in.T20","diet.cont.in.T21","diet.cont.in.T22","diet.cont.in.T23","diet.cont.in.T24","diet.cont.in.T25","diet.cont.in.T26","diet.cont.in.T27","diet.cont.in.T28","diet.cont.in.T29","diet.cont.in.T30","diet.cont.in.T31","diet.cont.in.T32","diet.cont.in.T33","diet.cont.in.T34","diet.cont.in.T35","diet.cont.in.T36","diet.cont.in.T37"),variable.name="Timepoint",value.name="diet.cont.in")
data_diet$Timepoint<- gsub("diet.cont.in.", "",data_diet$Timepoint)

data$dry.totcnsmp.T0<-0
data_cnsmp<-melt(data,id.vars=c("bug.id","treatment","temp","load","mass.T0","died"),measure.vars=c("dry.totcnsmp.T0","dry.totcnsmp.T1","dry.totcnsmp.T2","dry.totcnsmp.T3","dry.totcnsmp.T4","dry.totcnsmp.T5","dry.totcnsmp.T6","dry.totcnsmp.T7","dry.totcnsmp.T8","dry.totcnsmp.T9","dry.totcnsmp.T10","dry.totcnsmp.T11","dry.totcnsmp.T12","dry.totcnsmp.T13","dry.totcnsmp.T14","dry.totcnsmp.T15","dry.totcnsmp.T16","dry.totcnsmp.T17","dry.totcnsmp.T18","dry.totcnsmp.T19","dry.totcnsmp.T20","dry.totcnsmp.T21","dry.totcnsmp.T22","dry.totcnsmp.T23","dry.totcnsmp.T24","dry.totcnsmp.T25","dry.totcnsmp.T26","dry.totcnsmp.T27","dry.totcnsmp.T28","dry.totcnsmp.T29","dry.totcnsmp.T30","dry.totcnsmp.T31","dry.totcnsmp.T32","dry.totcnsmp.T33","dry.totcnsmp.T34","dry.totcnsmp.T35","dry.totcnsmp.T36","dry.totcnsmp.T37"),variable.name="Timepoint",value.name="tot.cnsmp")
data_cnsmp$Timepoint<-gsub("dry.totcnsmp.", "",data_cnsmp$Timepoint)

View(data_cnsmp)






#Merging long data into one data frame

data_1<- merge(data_Date, data_Time)
data_2<- merge(data_Fed, data_Mass)
data_3<- merge(data_Expct,data_Age)   
data_4<-merge(data_Instar,data_cnsmp)

data_5<- merge(data_1, data_2)  
data_6<-merge(data_3,data_4)
data_7<-merge(data_5,data_mass_gain)

cpt<-merge(data_6,data_7)
View(cpt)


#Makes mass gain values less than 1 equal to 1 to avoid creating negatives when taking the log

cpt$mass.gain[cpt$mass.gain < 1] <- 1
cpt$log.mg<-log(cpt$mass.gain)


cpt$tot.cnsmp[cpt$tot.cnsmp==0]<-1
cpt$log.cnsmp<-log(cpt$tot.cnsmp)


write.csv(cpt, "cpt.gr.long.load.csv", row.names = FALSE, quote = FALSE)




