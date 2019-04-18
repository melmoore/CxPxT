#CxPxT plots for manuscript

#load libraries

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

#----------------

#load data

cpt <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt.gr.long.load.csv", 
                col_types = cols(temp = col_factor(levels = c("20", 
                                                              "25", "30")), treatment = col_factor(levels = c("control", 
                                                                                                              "para"))))
View(cpt)

wide <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt gr wide.csv", 
                 col_types = cols(temp = col_factor(levels = c("20", "25", "30")), 
                                  treatment = col_factor(levels = c("control", 
                                                                    "para"))))
View(wide)



#clean data-remove para wanderers and WOWEs

cpt$log.mT0<-log(cpt$mass.T0)
cpt$log.mass<-log(cpt$mass)
cpt$day.age<-(cpt$age/24)


cpt.cl<-subset(cpt,died=="0")
cpt.cl$load[is.na(cpt.cl$load)]<-0
cpt.cl$load<-ifelse(cpt.cl$treatment=="para" & cpt.cl$load==0, 1.5, cpt.cl$load)
cpt.cl<-subset(cpt.cl, load!=1.5)
cpt.cl$load[cpt.cl$load==0]<-NA


wide.cl<-subset(wide,died=="0")
wide.cl$load[is.na(wide.cl$load)]<-0
wide.cl$load<-ifelse(wide.cl$treatment=="para" & wide.cl$load==0, 1.5, wide.cl$load)
wide.cl<-subset(wide.cl, load!=1.5)
wide.cl$load[wide.cl$load==0]<-NA


#------------------------

#Figure 1, average consumption and mass gain at each time point


#SummarySE for log mass gain
log.mg.sum<-summarySE(cpt.cl, measurevar="log.mg", 
                      groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
log.mg.sum
log.mg.sum$treatment<-as.factor(log.mg.sum$treatment)


#SummarySE for day age
dage.sum<-summarySE(cpt.cl, measurevar="day.age", 
                    groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
dage.sum


#Adding age and age se to the log mass gain summarySE
log.mg.sum$day.age<-dage.sum[,5]
log.mg.sum$day.age.se<-dage.sum[,7]



#SummarySE for consumption
log.cnsmp.sum<-summarySE(cpt.cl, measurevar="log.cnsmp", 
                         groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
log.cnsmp.sum
log.cnsmp.sum$treatment<-as.factor(log.cnsmp.sum$treatment)


#SummarySE for day age
dage.sum<-summarySE(cpt.cl, measurevar="day.age", 
                    groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
dage.sum


#Adding age and age se to the log mass gain summarySE
log.cnsmp.sum$day.age<-dage.sum[,5]
log.cnsmp.sum$day.age.se<-dage.sum[,7]



#log mass gain for cowplot

#annotation for temperature treatment
ann_text <- data.frame(day.age = c(0,0,0),log.mg = c(9,9,9),name=c("20","25","30"),
                       temp = factor(c(20,25,30),levels = c("20","25","30")), 
                       treatment = factor(c("control","control","control"),levels=c("control","para")))


#annotation for instar
instr_text <- read_csv("data files/instr_text.csv", 
                       col_types = cols(temp = col_factor(levels = c("20", "25", "30")), 
                                        treatment = col_factor(levels = c("control", "para"))))


lmg.plot<-ggplot(log.mg.sum,aes(x=day.age,y=log.mg,group=treatment))
lmg.plot<-lmg.plot+geom_point(aes(shape=treatment, color=treatment),size=2
)+geom_line(aes(linetype=treatment, color=treatment),size=1
)+geom_errorbar(aes(ymin=log.mg-se,ymax=log.mg+se, color=treatment),
                width=.5,size=.7
)+geom_errorbarh(aes(xmin=day.age-day.age.se,xmax=day.age+day.age.se, color=treatment),
                 height=1,size=.5
)+scale_color_manual(values=c("black", "#DA8E03"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized")
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+scale_shape_manual(name="Treatment",breaks=c("control","para"),values=c(16,17),
                     labels=c("Control","Parasitized")
)+geom_point(data=avg.inst,aes(x=day.age,y=log.mg, color=treatment),
             shape=15,
             size=4, show.legend = FALSE
)+scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25, 30),
                     limits = c(0,10)
)+labs(x="Age [day]",y="Log mass gain [mg]"
)+facet_wrap(~temp,dir="v")+theme(strip.background = element_blank(),strip.text.x = element_blank()
)+geom_label(data=ann_text,aes(label=name),size=5,show.legend = FALSE
)+geom_text(data=instr_text,aes(label=name),size=6,show.legend = FALSE
)+ theme(legend.position="none",
         axis.line.x=element_line(colour = 'black', size = 1),
         axis.line.y=element_line(colour = 'black', size = 1),
         axis.ticks = element_line(colour = 'black', size = 1),
         axis.ticks.length = unit(2, "mm"),
         axis.text.x = element_text(size = 18),
         axis.text.y = element_text(size = 18),
         axis.title.x = element_text(size = 18),
         axis.title.y = element_text(size = 18))


lmg.plot


#log consumption for cowplot

#annotation for temperature treatments
ann_text2 <- data.frame(day.age = c(0,0,0),log.cnsmp = c(9,9,9),name=c("20","25","30"),
                        temp = factor(c(20,25,30),levels = c("20","25","30")), 
                        treatment = factor(c("control","control","control"),levels=c("control","para")))

#annotation for instar labels
instr_text_cnsmp <- read_csv("data files/instr_text_cnsmp.csv", 
                             col_types = cols(temp = col_factor(levels = c("20", "25", "30")),
                                              treatment = col_factor(levels = c("control", "para"))))


cnsmp.plot<-ggplot(log.cnsmp.sum,aes(x=day.age,y=log.cnsmp,group=treatment))
cnsmp.plot<-cnsmp.plot+geom_point(aes(shape=treatment,color=treatment),size=2
)+geom_line(aes(linetype=treatment,color=treatment),size=1
)+geom_errorbar(aes(ymin=log.cnsmp-se,ymax=log.cnsmp+se,color=treatment),
                width=.5,size=.7
)+geom_errorbarh(aes(xmin=day.age-day.age.se,xmax=day.age+day.age.se,color=treatment),
                 height=1,size=.5
)+scale_color_manual(values=c("black", "#DA8E03"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+scale_shape_manual(name="Treatment",breaks=c("control","para"),values=c(16,17),
                     labels=c("Control","Parasitized")
)+geom_point(data=avg.inst,aes(x=day.age,y=log.cnsmp,color=treatment),shape=15,size=4,show.legend = FALSE
)+labs(x="Age [day]", y="Log consumption [dry mg]"
)+scale_y_continuous(limits=c(0,10)
)+scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25, 30)
)+facet_wrap(~temp,dir="v")+theme(strip.background = element_blank(),strip.text.x = element_blank()
)+geom_label(data=ann_text2,aes(label=name),size=5,show.legend = FALSE
)+geom_text(data=instr_text_cnsmp,aes(label=name),size=6,show.legend = FALSE             
)+theme(legend.position = c(0.7, 0.2),
        axis.line.x=element_line(colour = 'black', size = 1),
        axis.line.y=element_line(colour = 'black', size = 1),
        axis.ticks = element_line(colour = 'black', size = 1),
        axis.ticks.length = unit(2, "mm"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.background = element_rect(color="black",linetype="solid"),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        legend.key.width=unit(4,"line"),
        legend.key.height = unit(2,"line"))
cnsmp.plot


fig1<-plot_grid(lmg.plot, cnsmp.plot, labels = c("A", "B"),align="h")
fig1


#-----------------------

#Figure 2: Reaction norm of mass and age at the end of development

wide.cl$age.end<-coalesce(wide.cl$age.em, wide.cl$age.wander)

wide.cl$day.age.end<-wide.cl$age.end/24

age.end.sum<-summarySE(wide.cl, measurevar = "day.age.end",
                       groupvars = c("temp","treatment"),
                       na.rm=TRUE)
age.end.sum


age.end.plot<-ggplot(age.end.sum, aes(x=temp,y=day.age.end,group=treatment,color=treatment))
age.end.plot+geom_point(aes(shape=treatment),
                        size=5
)+geom_line(aes(linetype=treatment),
            size=2
)+geom_errorbar(aes(ymin=day.age.end-se,ymax=day.age.end+se),
                width=.3, size=1.2
)+scale_color_manual(values=c("black", "#DA8E03"),
                     breaks=c("control","para"),
                     labels=c("Control","Parasitized"),
                     name="Treatment"
)+scale_linetype_manual(values=c("solid", "dashed"),
                        breaks=c("control","para"),
                        labels=c("Control","Parasitized"),
                        name="Treatment"
)+scale_shape_manual(name="Treatment",breaks=c("control","para"),values=c(16,17),
                     labels=c("Control","Parasitized")          
)+labs(x="Rearing temperature [C]", y="Age to Wander or Emergence [days]"
)+theme(legend.position = c(0.7, 0.7),
        axis.line.x=element_line(colour = 'black', size = 1),
        axis.line.y=element_line(colour = 'black', size = 1),
        axis.ticks = element_line(colour = 'black', size = 1),
        axis.ticks.length = unit(2, "mm"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.background = element_rect(color="black",linetype="solid"),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        legend.key.width=unit(5,"line"),
        legend.key.height = unit(2,"line"))







#--------------------------

#Figure 3: number emerged by load, development time to emergence by load

para<-subset(wide.cl,treatment=="para")
para<-subset(para,num.ovp<=2)


#num em by total load

load.plot<-ggplot(para,aes(y=num.em,x=load,color=temp,linetype=temp))
load.plot<-load.plot+geom_point(aes(shape=temp, alpha=temp),
                                size=5
)+geom_smooth(se=FALSE,method="lm",
              size=2
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     name="Temperature [C]",
                     breaks=c("20","25","30"),
                     labels=c("20","25","30"),
                     guide=guide_legend(keywidth=1.8)
)+scale_linetype_manual(values=c("solid","dotted","longdash"),
                        name="Temperature [C]",
                        breaks=c("20","25","30"),
                        labels=c("20","25","30"),
                        guide=guide_legend(keywidth=1.8)
)+labs(x="Total load",y="Number of emerged wasp larvae"
)+scale_shape_manual(name="Temperature [C]",
                     breaks=c("20","25","30"),
                     labels=c("20","25","30"),
                     values=c(16,17,15)
)+scale_alpha_manual(breaks=c("20", "25", "30"),
                     values = c(1, .7, 1)
)+theme(axis.line.x=element_line(colour = 'black', size = 1),
              axis.line.y=element_line(colour = 'black', size = 1),
              axis.ticks = element_line(colour = 'black', size = 1),
              axis.ticks.length = unit(2, "mm"),
              axis.text.x = element_text(size = 18),
              axis.text.y = element_text(size = 18),
              axis.title.x = element_text(size = 18),
              axis.title.y = element_text(size = 18),
              legend.position="none")

load.plot



#internal dev time by load

loaddev.plot<-ggplot(para,aes(x=load,y=int.wasp.dev,group=temp,color=temp,linetype=temp))
loaddev.plot<-loaddev.plot+geom_point(aes(shape=temp),
                                      size=5
)+geom_smooth(se=FALSE,method="lm",
              size=2
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     name="Temperature [C]",
                     breaks=c("20","25","30"),
                     labels=c("20","25","30")
)+scale_linetype_manual(values=c("solid","dotted","longdash"),
                        name="Temperature [C]",breaks=c("20","25","30"),
                        labels=c("20","25","30")
)+scale_shape_manual(name="Temperature [C]",
                     breaks=c("20","25","30"),
                     labels=c("20","25","30"),
                     values=c(16,17,15)
)+labs(x="Total Load",y="Days to Wasp Emergence"
)+theme(axis.line.x=element_line(colour = 'black', size = 1),
        axis.line.y=element_line(colour = 'black', size = 1),
        axis.ticks = element_line(colour = 'black', size = 1),
        axis.ticks.length = unit(2, "mm"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.background = element_rect(color="black",linetype="solid"),
        legend.position = c(0.6, 0.9),
        legend.key.width=unit(5,"line"))

loaddev.plot


fig3<-plot_grid(load.plot,loaddev.plot,labels = c("A", "B"),align="h")
fig3


#-----------------------------

#Figure 4--mass gain and consumption of para treatment by load

#making load numeric, subset long dataframe to only para treatment
cpt.cl$load<-as.numeric(cpt.cl$load)
para.lng<-subset(cpt.cl,treatment=="para")


para.lng<-subset(para.lng,load>0)
para.lng<-subset(para.lng,load<350)


#creating a load bin column, separating by load size
para.lng$bin<-ifelse(para.lng$load<50, 50,
                     ifelse(para.lng$load>50 & para.lng$load<=100, 100,
                            ifelse(para.lng$load>100 & para.lng$load<=150, 150,
                                   ifelse(para.lng$load>150 & para.lng$load<=200, 200,
                                          ifelse(para.lng$load>200 & para.lng$load<=350, 300, 0)))))


para.lng20<-subset(para.lng,temp=="20")
para.lng25<-subset(para.lng,temp=="25")
para.lng30<-subset(para.lng,temp=="30")



#Finding mean and variance of log.mg for each temperature, at each timpoint, separated by load bin

#20
lb20.lmg.sum<-summarySE(para.lng20,measurevar = "log.mg",
                        groupvars = c("expct.hour","bin"),
                        na.rm=TRUE)

lb20.lmg.sum


lb20.age.sum<-summarySE(para.lng20,measurevar = "day.age",
                        groupvars = c("expct.hour","bin"),
                        na.rm=TRUE)
lb20.age.sum


lb20.lmg.sum$day.age<-lb20.age.sum[,4]
lb20.lmg.sum$dage.se<-lb20.age.sum[,6]


#25
lb25.lmg.sum<-summarySE(para.lng25,measurevar = "log.mg",
                        groupvars = c("expct.hour","bin"),
                        na.rm=TRUE)


lb25.lmg.sum


lb25.age.sum<-summarySE(para.lng25,measurevar = "day.age",
                        groupvars = c("expct.hour","bin"),
                        na.rm=TRUE)
lb25.age.sum


lb25.lmg.sum$day.age<-lb25.age.sum[,4]
lb25.lmg.sum$dage.se<-lb25.age.sum[,6]


#30
lb30.lmg.sum<-summarySE(para.lng30,measurevar = "log.mg",
                        groupvars = c("expct.hour","bin"),
                        na.rm=TRUE)


lb30.lmg.sum


lb30.age.sum<-summarySE(para.lng30,measurevar = "day.age",
                        groupvars = c("expct.hour","bin"),
                        na.rm=TRUE)
lb30.age.sum


lb30.lmg.sum$day.age<-lb30.age.sum[,4]
lb30.lmg.sum$dage.se<-lb30.age.sum[,6]


#Finding mean and variance of log cnsmp for each temperature, at each timpoint, separated by load bin

#20
lb20.cnsmp.sum<-summarySE(para.lng20,measurevar = "log.cnsmp",
                          groupvars = c("expct.hour","bin"),
                          na.rm=TRUE)
lb20.cnsmp.sum
lb20.age.sum

lb20.cnsmp.sum$day.age<-lb20.age.sum[,4]
lb20.cnsmp.sum$dage.se<-lb20.age.sum[,6]


#25
lb25.cnsmp.sum<-summarySE(para.lng25,measurevar = "log.cnsmp",
                          groupvars = c("expct.hour","bin"),
                          na.rm=TRUE)
lb25.cnsmp.sum
lb25.age.sum

lb25.cnsmp.sum$day.age<-lb25.age.sum[,4]
lb25.cnsmp.sum$dage.se<-lb25.age.sum[,6]


#30
lb30.cnsmp.sum<-summarySE(para.lng30,measurevar = "log.cnsmp",
                          groupvars = c("expct.hour","bin"),
                          na.rm=TRUE)
lb30.cnsmp.sum
lb30.age.sum

lb30.cnsmp.sum$day.age<-lb30.age.sum[,4]
lb30.cnsmp.sum$dage.se<-lb30.age.sum[,6]



#Plotting mean log.mg for each temperature, binned by load

loadbin50.20.plot<-ggplot(lb20.lmg.sum,aes(x=day.age,y=log.mg,group=as.factor(bin),color=as.factor(bin)))
loadbin50.20.plot<-loadbin50.20.plot+geom_line(size=1.2,show.legend=FALSE
)+geom_point(size=3,show.legend=FALSE
)+geom_errorbar(aes(ymin=log.mg-se,ymax=log.mg+se),
                width=.5,size=.7
)+scale_color_manual(values=c("#440D54","#647ABD","#1F9F88","#95D840","#D5A913"),
                     guide=FALSE
)+scale_y_continuous(limits=c(0,9),
                     breaks=c(0,2,4,6,8)
)+labs(y="Log mass gain [mg]", x=NULL, title="20"
)+theme(axis.line.x=element_line(colour = 'black', size = 1),
        axis.line.y=element_line(colour = 'black', size = 1),
        axis.ticks = element_line(colour = 'black', size = 1),
        axis.ticks.length = unit(2, "mm"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size=24))


loadbin50.20.plot


loadbin50.25.plot<-ggplot(lb25.lmg.sum,aes(x=day.age,y=log.mg,group=as.factor(bin),color=as.factor(bin)))
loadbin50.25.plot<-loadbin50.25.plot+geom_line(size=1,show.legend=FALSE
)+geom_point(size=3,show.legend=FALSE
)+geom_errorbar(aes(ymin=log.mg-se,ymax=log.mg+se),
                width=.5,size=.7
)+scale_color_manual(values=c("#440D54","#647ABD","#1F9F88","#95D840","#D5A913"),
                     guide=FALSE
)+scale_y_continuous(limits=c(0,9),
                     breaks=c(0,2,4,6,8)
)+labs(y=NULL, x=NULL, title="25"
)+theme(axis.line.x=element_line(colour = 'black', size = 1),
        axis.line.y=element_line(colour = 'black', size = 1),
        axis.ticks = element_line(colour = 'black', size = 1),
        axis.ticks.length = unit(2, "mm"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size=24))

loadbin50.25.plot



loadbin50.30.plot<-ggplot(lb30.lmg.sum,aes(x=day.age,y=log.mg,group=as.factor(bin),color=as.factor(bin)))
loadbin50.30.plot<-loadbin50.30.plot+geom_line(size=1
)+geom_point(size=3
)+geom_errorbar(aes(ymin=log.mg-se,ymax=log.mg+se),
                width=.5,size=.7
)+scale_color_manual(values=c("#440D54","#647ABD","#1F9F88","#95D840","#D5A913"),
                     labels=c("<50","50-100","100-150","150-200","200-300"),
                     name="Load size"
)+scale_y_continuous(limits=c(0,9),
                     breaks=c(0,2,4,6,8)
)+labs(y=NULL, x=NULL, title="30"
)+theme(legend.position = c(0.7, 0.37),
        axis.line.x=element_line(colour = 'black', size = 1),
        axis.line.y=element_line(colour = 'black', size = 1),
        axis.ticks = element_line(colour = 'black', size = 1),
        axis.ticks.length = unit(2, "mm"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size=24),
        legend.key.width=unit(2.5,"line"))

loadbin50.30.plot


#Making average consumption by age plots for load bins for each temp

lb50cnsmp.20.plot<-ggplot(lb20.cnsmp.sum,aes(x=day.age,y=log.cnsmp,group=as.factor(bin),color=as.factor(bin)))
lb50cnsmp.20.plot<-lb50cnsmp.20.plot+geom_line(size=1.2, show.legend=FALSE
)+geom_point(size=3, show.legend=FALSE
)+geom_errorbar(aes(ymin=log.cnsmp-se,ymax=log.cnsmp+se),
                width=.5,size=.7
)+scale_color_manual(values=c("#440D54","#647ABD","#1F9F88","#95D840","#D5A913"),
                     guide=FALSE
)+scale_y_continuous(limits=c(0,8),
                     breaks=c(0,2,4,6,8)
)+labs(y="Log consumption [mg]", x="Age [days]"
)+theme(axis.line.x=element_line(colour = 'black', size = 1),
        axis.line.y=element_line(colour = 'black', size = 1),
        axis.ticks = element_line(colour = 'black', size = 1),
        axis.ticks.length = unit(2, "mm"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size=24))


lb50cnsmp.20.plot


lb50cnsmp.25.plot<-ggplot(lb25.cnsmp.sum,aes(x=day.age,y=log.cnsmp,group=as.factor(bin),color=as.factor(bin)))
lb50cnsmp.25.plot<-lb50cnsmp.25.plot+geom_line(size=1.2,show.legend=FALSE
)+geom_point(size=3,show.legend=FALSE
)+geom_errorbar(aes(ymin=log.cnsmp-se,ymax=log.cnsmp+se),
                width=.5,size=.7
)+scale_color_manual(values=c("#440D54","#647ABD","#1F9F88","#95D840","#D5A913"),
                     guide=FALSE
)+scale_y_continuous(limits=c(0,8),
                     breaks=c(0,2,4,6,8)
)+labs(y=NULL, x="Age [days]"
)+theme(axis.line.x=element_line(colour = 'black', size = 1),
        axis.line.y=element_line(colour = 'black', size = 1),
        axis.ticks = element_line(colour = 'black', size = 1),
        axis.ticks.length = unit(2, "mm"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size=24))

lb50cnsmp.25.plot



lb50cnsmp.30.plot<-ggplot(lb30.cnsmp.sum,aes(x=day.age,y=log.cnsmp,group=as.factor(bin),color=as.factor(bin)))
lb50cnsmp.30.plot<-lb50cnsmp.30.plot+geom_line(size=1.2
)+geom_point(size=3
)+geom_errorbar(aes(ymin=log.cnsmp-se,ymax=log.cnsmp+se),
                width=.5,size=.7
)+scale_color_manual(values=c("#440D54","#647ABD","#1F9F88","#95D840","#D5A913"),
                     labels=c("<50","50-100","100-150","150-200","200-300"),
                     name="Load size"
)+scale_y_continuous(limits=c(0,8),
                     breaks=c(0,2,4,6,8)
)+labs(y=NULL, x="Age [days]"
)+theme(legend.position = c(0.7, 0.37),
        axis.line.x=element_line(colour = 'black', size = 1),
        axis.line.y=element_line(colour = 'black', size = 1),
        axis.ticks = element_line(colour = 'black', size = 1),
        axis.ticks.length = unit(2, "mm"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        plot.title = element_text(size=24),
        legend.key.width=unit(2.5,"line"))


lb50cnsmp.30.plot


fig4<-plot_grid(loadbin50.20.plot, loadbin50.25.plot, loadbin50.30.plot,
                      lb50cnsmp.20.plot, lb50cnsmp.25.plot, lb50cnsmp.30.plot, 
                      labels = c("A", "B","C", "D", "E","F"),
                      align="v", ncol=3, nrow=2)
fig4

#may need to still increase axis lines, maybe points and lines of plot as well?





#------------------------

#Supplemental figure--percent em by load

#calculating the percent of emerged wasp larvae (num em / load)

para$percem<-para$num.em/para$load

#Plotting percent emergned for supplemental figure

percem.plot<-ggplot(para, aes(x=load, y=percem,color=temp, linetype=temp))
percem.plot+geom_point(aes(shape=temp, alpha=temp),
                       size=5
)+geom_smooth(method=lm, se=FALSE,
              size=2
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     name="Temperature [C]",
                     breaks=c("20","25","30"),
                     labels=c("20","25","30"),
                     guide=guide_legend(keywidth=1.8)
)+scale_linetype_manual(values=c("solid","dotted","longdash"),
                        name="Temperature [C]",
                        breaks=c("20","25","30"),
                        labels=c("20","25","30"),
                        guide=guide_legend(keywidth=1.8)
)+scale_alpha_manual(breaks=c(20, 25, 30),
                     values=c(1, .7, 1),
                     name="Temperature [C]",
                     labels=c("20", "25", "30")
)+labs(x="Total load",y="Percent of emerged larvae"
)+scale_shape_manual(name="Temperature [C]",
                     breaks=c("20","25","30"),
                     labels=c("20","25","30"),
                     values=c(16,17,15)
)+theme(axis.line.x=element_line(colour = 'black', size = 1),
        axis.line.y=element_line(colour = 'black', size = 1),
        axis.ticks = element_line(colour = 'black', size = 1),
        axis.ticks.length = unit(2, "mm"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.key.width=unit(4,"line"))



#--------------------------

#subset data to only necessary columns, remove NAs

#subset to only columns in model, and remove rows with NAs--for mass
cpt.sub<-select(cpt.cl, bug.id, temp, treatment, log.mass, age)
cpt.sub<-na.omit(cpt.sub)

#subset to only columns in model, and remove rows with NAs--for cnsmp
cpt.sub2<-select(cpt.cl, bug.id, temp, treatment, log.cnsmp, age)
cpt.sub2<-na.omit(cpt.sub2)


#make bug.id a factor so the model will run
cpt.sub2$bug.id<-as.factor(cpt.sub2$bug.id)

#consumption GAMM model
gam_cnsmp_mod<-gam(log.cnsmp ~ s(age, by= interaction(treatment,temp, k=10,bs="ts")) + s(bug.id,bs="fs") + treatment * temp,
                   method="ML", data=cpt.sub2, na.action = na.omit)


#make bug.id a factor so the model will run
cpt.sub$bug.id<-as.factor(cpt.sub$bug.id)

#Mass GAMM model
gam_mass_mod<-gam(log.mass ~ s(age, by= interaction(treatment,temp, k=10,bs="ts")) + s(bug.id,bs="fs") + treatment * temp,
                  method="ML", data=cpt.sub, na.action = na.omit)


cpt.sub2$pred<-predict(gam_cnsmp_mod, level=0)
cpt.sub2$resid<-residuals(gam_cnsmp_mod, level=0)

cpt.sub$pred<-predict(gam_mass_mod, level=0)
cpt.sub$resid<-residuals(gam_mass_mod, level=0)


#plotting residuals against age for consumption
cnsmp_gam_ra<-ggplot(cpt.sub2, aes(x=age, y=resid, color=treatment))
cnsmp_gam_ra+geom_point(size=3, alpha=.7
)+scale_color_manual(values = c("black", "#DA8E03"),
                     breaks=c("control", "para"),
                     name="Treatment",
                     labels=c("Control", "Parasitized")
)+geom_hline(aes(yintercept=0),
             size=1.5,
             color="black",
             linetype="dashed"
)+labs(x="Age [hours]", y="Residuals"
)+facet_wrap(~temp
)+theme(axis.line.x=element_line(colour = 'black', size = 1),
        axis.line.y=element_line(colour = 'black', size = 1),
        axis.ticks = element_line(colour = 'black', size = 1),
        axis.ticks.length = unit(2, "mm"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.key.width=unit(4,"line"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        strip.text.x = element_text(size=18))


#plotting residuals against age for consumption
mass_gam_ra<-ggplot(cpt.sub, aes(x=age, y=resid, color=treatment))
mass_gam_ra+geom_point(size=3, alpha=.7
)+scale_color_manual(values = c("black", "#DA8E03"),
                     breaks=c("control", "para"),
                     name="Treatment",
                     labels=c("Control", "Parasitized")
)+geom_hline(aes(yintercept=0),
             size=1.5,
             color="black",
             linetype="dashed"
)+labs(x="Age [hours]", y="Residuals"
)+facet_wrap(~temp
)+theme(axis.line.x=element_line(colour = 'black', size = 1),
        axis.line.y=element_line(colour = 'black', size = 1),
        axis.ticks = element_line(colour = 'black', size = 1),
        axis.ticks.length = unit(2, "mm"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.key.width=unit(4,"line"),
        strip.background = element_rect(color="black", fill="white", linetype="solid"),
        strip.text.x = element_text(size=18))




