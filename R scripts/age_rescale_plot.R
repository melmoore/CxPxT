library(scales)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(Rmisc)
library(readr)
library(nlme)
library(lme4)
library(cowplot)
library(viridis)


cpt <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt.gr.long.load.csv", 
                col_types = cols(temp = col_factor(levels = c("20", 
                                                              "25", "30")), treatment = col_factor(levels = c("control", 
                                                                                                              "para"))))

cpt$log.mT0<-log(cpt$mass.T0)
cpt$log.mass<-log(cpt$mass)
cpt$day.age<-(cpt$age/24)
View(cpt)

cpt.cl<-subset(cpt,died=="0")



wide <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt gr wide.csv", 
                 col_types = cols(temp = col_factor(levels = c("20", "25", "30")), 
                                  treatment = col_factor(levels = c("control", 
                                                                    "para"))))
View(wide)

wide.cl<-subset(wide,died=="0")


#--------------------------------------------------

#creating data frames for each temp to scale age separately


cpt.20<-subset(cpt.cl,temp=="20")
cpt.25<-subset(cpt.cl,temp=="25")
cpt.30<-subset(cpt.cl,temp=="30")


#Finding maximum hour age for each temperature

max(cpt.20$age,na.rm=TRUE)
max(cpt.25$age,na.rm=TRUE)
max(cpt.30$age,na.rm=TRUE)


#Scaling age for each temperature by dividing by the maximum age

cpt.20$age.sc<-cpt.20$age/745.2833
cpt.25$age.sc<-cpt.25$age/405.3167
cpt.30$age.sc<-cpt.30$age/362.0833


#Creating a binary column for mongos/wanderers--filtering out

wide.cl$load[is.na(wide.cl$load)]<-0
wide.cl$date.em[is.na(wide.cl$date.em)]<-0

wide.cl$mongo<-ifelse(wide.cl$treatment=="para" & wide.cl$load=="0" & wide.cl$date.em=="0", 1,0)

wide.cl<-subset(wide.cl,mongo=="0")

#Removing individuals from cpt that were mongos or wanderers (found from wide dataframe)

cpt.cl<-subset(cpt.cl, bug.id!="25_p_11" & bug.id!="20_p_11" & bug.id!="25_p_24" & bug.id!="30_p_27" & bug.id!="20_p_44" & bug.id!="20_p_56" 
               & bug.id!="25_p_48" & bug.id!="25_p_51" & bug.id!="25_p_53" & bug.id!="30_p_23")



#converting age at each instar to the scaled age by dividing by max age at each temperature

wide.20<-subset(wide.cl,temp=="20")
wide.20$agesc.4<-wide.20$age.4/745.2833
wide.20$agesc.5<-wide.20$age.5/745.2833
wide.20$agesc.wand<-wide.20$age.wander/745.2833
wide.20$agesc.em<-wide.20$age.em/745.2833

wide.25<-subset(wide.cl,temp=="25")
wide.25$agesc.4<-wide.25$age.4/405.3167
wide.25$agesc.5<-wide.25$age.5/405.3167
wide.25$agesc.wand<-wide.25$age.wander/405.3167
wide.25$agesc.em<-wide.25$age.em/405.3167

wide.30<-subset(wide.cl,temp=="30")
wide.30$agesc.4<-wide.30$age.4/362.0833
wide.30$agesc.5<-wide.30$age.5/362.0833
wide.30$agesc.wand<-wide.30$age.wander/362.0833
wide.30$agesc.em<-wide.30$age.em/362.0833




#Finding the means for scaled age for each instar at each temperature

agesc4.20.sum<-summarySE(wide.20,measurevar = "agesc.4",
                         groupvars = c("treatment","temp"),
                         na.rm=TRUE)
agesc4.20.sum



agesc5.20.sum<-summarySE(wide.20,measurevar = "agesc.5",
                         groupvars = c("treatment","temp"),
                         na.rm=TRUE)
agesc5.20.sum


agescwand.20.sum<-summarySE(wide.20,measurevar = "agesc.wand",
                            groupvars = c("treatment","temp"),
                            na.rm=TRUE)
agescwand.20.sum


agescem.20.sum<-summarySE(wide.20,measurevar = "agesc.em",
                          groupvars = c("treatment","temp"),
                          na.rm=TRUE)
agescem.20.sum





agesc4.25.sum<-summarySE(wide.25,measurevar = "agesc.4",
                         groupvars = c("treatment","temp"),
                         na.rm=TRUE)
agesc4.25.sum



agesc5.25.sum<-summarySE(wide.25,measurevar = "agesc.5",
                         groupvars = c("treatment","temp"),
                         na.rm=TRUE)
agesc5.25.sum


agescwand.25.sum<-summarySE(wide.25,measurevar = "agesc.wand",
                            groupvars = c("treatment","temp"),
                            na.rm=TRUE)
agescwand.25.sum


agescem.25.sum<-summarySE(wide.25,measurevar = "agesc.em",
                          groupvars = c("treatment","temp"),
                          na.rm=TRUE)
agescem.25.sum





agesc4.30.sum<-summarySE(wide.30,measurevar = "agesc.4",
                         groupvars = c("treatment","temp"),
                         na.rm=TRUE)
agesc4.30.sum




agesc5.30.sum<-summarySE(wide.30,measurevar = "agesc.5",
                         groupvars = c("treatment","temp"),
                         na.rm=TRUE)
agesc5.30.sum


agescwand.30.sum<-summarySE(wide.30,measurevar = "agesc.wand",
                            groupvars = c("treatment","temp"),
                            na.rm=TRUE)
agescwand.30.sum


agescem.30.sum<-summarySE(wide.30,measurevar = "agesc.em",
                          groupvars = c("treatment","temp"),
                          na.rm=TRUE)
agescem.30.sum




agesc4.sum<-rbind(agesc4.20.sum,agesc4.25.sum,agesc4.30.sum)
agesc5.sum<-rbind(agesc5.20.sum,agesc5.25.sum,agesc5.30.sum)
agescwand.sum<-rbind(agescwand.20.sum,agescwand.25.sum,agescwand.30.sum)
agescem.sum<-rbind(agescem.20.sum,agescem.25.sum,agescem.30.sum)



#Creating columns for log mass gain and consumption at each instar in wide data set

wide.cl$lmg.4<-log(wide.cl$mass.gain.4)
wide.cl$lmg.5<-log(wide.cl$mass.gain.5)
wide.cl$lmg.wand<-log(wide.cl$mass.gain.wan)
wide.cl$lmg.em<-log(wide.cl$mass.gain.em)
wide.cl$lc.4<-log(wide.cl$cnsmp.4)
wide.cl$lc.5<-log(wide.cl$cnsmp.5)
wide.cl$lc.wand<-log(wide.cl$cnsmp.wan)
wide.cl$lc.em<-log(wide.cl$cnsmp.em)



#Finding mean log mass gain for each instar

lmg4.sum<-summarySE(wide.cl,measurevar = "lmg.4",
                       groupvars = c("temp","treatment"),
                       na.rm=TRUE)
lmg4.sum


lmg5.sum<-summarySE(wide.cl,measurevar = "lmg.5",
                    groupvars = c("temp","treatment"),
                    na.rm=TRUE)
lmg5.sum


lmgwand.sum<-summarySE(wide.cl,measurevar = "lmg.wand",
                       groupvars = c("temp","treatment"),
                       na.rm=TRUE)
lmgwand.sum


lmgem.sum<-summarySE(wide.cl,measurevar = "lmg.em",
                     groupvars = c("temp","treatment"),
                     na.rm=TRUE)
lmgem.sum



#Finding averages for log consumption at each instar

lc4.sum<-summarySE(wide.cl,measurevar = "lc.4",
                   groupvars = c("temp","treatment"),
                   na.rm=TRUE)
lc4.sum



lc5.sum<-summarySE(wide.cl,measurevar = "lc.5",
          groupvars = c("temp","treatment"),
          na.rm=TRUE)
lc5.sum


lcwand.sum<-summarySE(wide.cl,measurevar = "lc.wand",
                      groupvars = c("temp","treatment"),
                      na.rm=TRUE)
lcwand.sum


lcem.sum<-summarySE(wide.cl,measurevar = "lc.em",
                    groupvars = c("temp","treatment"),
                    na.rm=TRUE)
lcem.sum




#Creating a data frame for the average scaled age, log mass gain, and log consumption for each instar at 
  ##each temperature

temp<-c(20,20,25,25,30,30,
        20,20,25,25,30,30,
        20,20,25,25,30,30,
        20,20,25,25,30,30)


treatment<-c("control","para","control","para","control","para",
             "control","para","control","para","control","para",
             "control","para","control","para","control","para",
             "control","para","control","para","control","para")



instar<-c(3,3,3,3,3,3,
          4,4,4,4,4,4,
          5,5,5,5,5,5,
          "wand","em","wand","em","wand","em")


age.sc<-c(0,0,0,0,0,0,
          (agesc4.sum[,4]),(agesc5.sum[,4]),
          (agescwand.sum[1,4]),(agescem.sum[2,4]),
          (agescwand.sum[3,4]),(agescem.sum[4,4]),
          (agescwand.sum[5,4]),(agescem.sum[6,4]))

agesc.se<-c(0,0,0,0,0,0,
           (agesc4.sum[,6]),(agesc5.sum[,6]),
           (agescwand.sum[1,6]),(agescem.sum[2,6]),
           (agescwand.sum[3,6]),(agescem.sum[4,6]),
           (agescwand.sum[5,6]),(agescem.sum[6,6]))
  
  
  
log.mg<-c(0,0,0,0,0,0,
          (lmg4.sum[,4]),(lmg5.sum[,4]),
          (lmgwand.sum[1,4]),(lmgem.sum[2,4]),
          (lmgwand.sum[3,4]),(lmgem.sum[4,4]),
          (lmgwand.sum[5,4]),(lmgem.sum[6,4]))


lmg.se<-c(0,0,0,0,0,0,
          (lmg4.sum[,6]),(lmg5.sum[,6]),
          (lmgwand.sum[1,6]),(lmgem.sum[2,6]),
          (lmgwand.sum[3,6]),(lmgem.sum[4,6]),
          (lmgwand.sum[5,6]),(lmgem.sum[6,6]))



lc<-c(0,0,0,0,0,0,
      (lc4.sum[,4]),(lc5.sum[,4]),
      (lcwand.sum[1,4]),(lcem.sum[2,4]),
      (lcwand.sum[3,4]),(lcem.sum[4,4]),
      (lcwand.sum[5,4]),(lcem.sum[6,4]))


lc.se<-c(0,0,0,0,0,0,
         (lc4.sum[,6]),(lc5.sum[,6]),
         (lcwand.sum[1,6]),(lcem.sum[2,6]),
         (lcwand.sum[3,6]),(lcem.sum[4,6]),
         (lcwand.sum[5,6]),(lcem.sum[6,6]))



avg.agesc<-data.frame(temp,treatment,instar,age.sc,agesc.se,log.mg,lmg.se,lc,lc.se)





#Creating a dataset with the average scaled age, log mass gain and log consumption for each time point at 
  ##each temperature

tp.agesc20.sum<-summarySE(cpt.20,measurevar = "age.sc",
                          groupvars = c("temp","treatment","Timepoint"),
                          na.rm=TRUE)
tp.agesc20.sum



tp.agesc25.sum<-summarySE(cpt.25,measurevar = "age.sc",
                          groupvars = c("temp","treatment","Timepoint"),
                          na.rm=TRUE)
tp.agesc25.sum



tp.agesc30.sum<-summarySE(cpt.30,measurevar = "age.sc",
                          groupvars = c("temp","treatment","Timepoint"),
                          na.rm=TRUE)
tp.agesc30.sum



tp.agesc.sum<-rbind(tp.agesc20.sum,tp.agesc25.sum,tp.agesc30.sum)



tp.lmg20.sum<-summarySE(cpt.20,measurevar = "log.mg",
                        groupvars = c("temp","treatment","Timepoint"),
                        na.rm = TRUE)
tp.lmg20.sum



tp.lmg25.sum<-summarySE(cpt.25,measurevar = "log.mg",
                      groupvars = c("temp","treatment","Timepoint"),
                      na.rm = TRUE)
tp.lmg25.sum


tp.lmg30.sum<-summarySE(cpt.30,measurevar = "log.mg",
                        groupvars = c("temp","treatment","Timepoint"),
                        na.rm = TRUE)
tp.lmg30.sum


tp.lmg.sum<-rbind(tp.lmg20.sum,tp.lmg25.sum,tp.lmg30.sum)




tp.lc20.sum<-summarySE(cpt.20,measurevar = "log.cnsmp",
                        groupvars = c("temp","treatment","Timepoint"),
                        na.rm = TRUE)
tp.lc20.sum



tp.lc25.sum<-summarySE(cpt.25,measurevar = "log.cnsmp",
                       groupvars = c("temp","treatment","Timepoint"),
                       na.rm = TRUE)
tp.lc25.sum



tp.lc30.sum<-summarySE(cpt.30,measurevar = "log.cnsmp",
                       groupvars = c("temp","treatment","Timepoint"),
                       na.rm = TRUE)
tp.lc30.sum


tp.lc.sum<-rbind(tp.lc20.sum,tp.lc25.sum,tp.lc30.sum)


tp.sum<-tp.lmg.sum
tp.sum$age.sc<-tp.agesc.sum[,5]
tp.sum$agesc.se<-tp.agesc.sum[,7]
tp.sum$lc<-tp.lc.sum[,5]
tp.sum$lc.se<-tp.lc.sum[,7]



#Making a plot of average timepoint log mass gain (small points) and average instar log mass gain (larg points)
  ##The average instar points don't all fall on the timepoint lines--not sure why or how to fix

agesc.lmg.plot<-ggplot(tp.sum,aes(x=age.sc,y=log.mg,group=treatment,color=treatment))
agesc.lmg.plot+geom_point(size=2
             )+geom_errorbar(aes(ymin=log.mg-se, ymax=log.mg+se),
                              width=.03, size=1
             )+geom_errorbarh(aes(xmin=age.sc-agesc.se,xmax=age.sc+agesc.se),
                              height=.03, size=1
             )+geom_line(size=1.2
             )+scale_color_manual(values=c("black","red"),
                                  name="Treatment",
                                  labels=c("Control","Parasitized")
             )+geom_point(data=avg.agesc,aes(x=age.sc,y=log.mg,group=treatment,color=treatment),
                          size=5, shape=15
             )+geom_errorbar(data=avg.agesc, aes(ymin=log.mg-lmg.se, ymax=log.mg+lmg.se),
                             width=.1, size=1
             )+facet_wrap(~temp)



agesc.lc.plot<-ggplot(tp.sum,aes(x=age.sc,y=lc,group=treatment,color=treatment))
agesc.lc.plot+geom_point(size=2
)+geom_errorbar(aes(ymin=lc-lc.se, ymax=lc+lc.se),
                width=.03, size=1
)+geom_errorbarh(aes(xmin=age.sc-agesc.se,xmax=age.sc+agesc.se),
                 height=.03, size=1
)+geom_line(size=1.2
)+scale_color_manual(values=c("black","red"),
                     name="Treatment",
                     labels=c("Control","Parasitized")
)+geom_point(data=avg.agesc,aes(x=age.sc,y=lc,group=treatment,color=treatment),
             size=5, shape=15
)+geom_errorbar(data=avg.agesc, aes(ymin=lc-lc.se, ymax=lc+lc.se),
                width=.1, size=1
)+geom_errorbarh(data=avg.agesc,aes(xmin=age.sc-agesc.se,xmax=age.sc+agesc.se),
                 height=.5, size=1
)+facet_wrap(~temp)












