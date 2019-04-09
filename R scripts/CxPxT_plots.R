##CxPxT plots!!

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
cpt.cl$load[is.na(cpt.cl$load)]<-0
cpt.cl$load<-ifelse(cpt.cl$treatment=="para" & cpt.cl$load==0, 1.5, cpt.cl$load)
cpt.cl<-subset(cpt.cl, load!=1.5)
cpt.cl$load[cpt.cl$load==0]<-NA


wide <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt gr wide.csv", 
                 col_types = cols(temp = col_factor(levels = c("20", "25", "30")), 
                                  treatment = col_factor(levels = c("control", 
                                  "para"))))
View(wide)

wide.cl<-subset(wide,died=="0")
wide.cl$load[is.na(wide.cl$load)]<-0
wide.cl$load<-ifelse(wide.cl$treatment=="para" & wide.cl$load==0, 1.5, wide.cl$load)
wide.cl<-subset(wide.cl, load!=1.5)
wide.cl$load[wide.cl$load==0]<-NA



avg <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt avg data.csv")
View(avg)
avg$temp<-as.factor(avg$temp)
avg$treat<-as.factor(avg$treat)


avg.long <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt avg long.csv")
View(avg.long)
avg.long$temp<-as.factor(avg.long$temp)
avg.long$treat<-as.factor(avg.long$treat)



theme_set(theme_classic(base_size=14))


#raw data--mass by age, color by temp, linetype by treatment

k<- ggplot(cpt.cl, aes(age, log.mass, group=interaction(bug.id, temp), color=factor(temp)))
k+geom_line(aes(linetype=treatment)
)+theme_bw(
)+theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border= element_blank(), 
        axis.line.x = element_line(color="black", size = 1.5),
        axis.line.y = element_line(color="black", size = 1.5),text = element_text(size = 15)
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     guide=guide_legend(title="Temperature [C]")
)+scale_linetype_manual(values=c("solid","dashed"),name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8))



#raw data--mass gain by age, color by temp, linetype by treatment

k<- ggplot(cpt.cl, aes(age, log.mg, group=interaction(bug.id, temp), color=factor(temp)))
k+geom_line(aes(linetype=treatment)
)+theme_bw(
)+theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border= element_blank(), 
        axis.line.x = element_line(color="black", size = 1.5),
        axis.line.y = element_line(color="black", size = 1.5),text = element_text(size = 15)
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     guide=guide_legend(title="Temperature [C]")
)+scale_linetype_manual(values=c("solid","dashed"),name="Treatment",breaks=c("control","para"),
                        labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8))




#raw data--mass by age, color and linetype by treatment, facet_wrap by temp

j<- ggplot(cpt.cl, aes(age, log.mass, group=interaction(bug.id, temp), color=treatment))
j+geom_line(aes(linetype=treatment)
)+theme_bw(
)+theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border= element_blank(), 
        axis.line.x = element_line(color="black", size = 1.5),
        axis.line.y = element_line(color="black", size = 1.5),text = element_text(size = 15)
)+scale_color_manual(values=c("black","red"),name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_manual(values=c("solid","dashed"),name="Treatment",breaks=c("control","para"),
                        labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+facet_wrap(~temp,dir="v")


#raw data--mass gain by age, color and linetype by treatment, facet_wrap by temp

j<- ggplot(cpt.cl, aes(age, log.mg, group=interaction(bug.id, temp), color=treatment))
j+geom_line(aes(linetype=treatment)
)+theme_bw(
)+theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border= element_blank(), 
        axis.line.x = element_line(color="black", size = 1.5),
        axis.line.y = element_line(color="black", size = 1.5),text = element_text(size = 15)
)+scale_color_manual(values=c("black","red"),name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_manual(values=c("solid","dashed"),name="Treatment",breaks=c("control","para"),
                        labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+facet_wrap(~temp,dir="v")



#raw data--consumption by age, color by temp, linetype by treatment

c<-ggplot(cpt.cl, aes(age, log.cnsmp, group=interaction(bug.id, temp), color=factor(temp)))
c+geom_line(aes(linetype=treatment)
)+theme_bw(
)+theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border= element_blank(), 
        axis.line.x = element_line(color="black", size = 1.5),
        axis.line.y = element_line(color="black", size = 1.5),text = element_text(size = 15)
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     guide=guide_legend(title="Temperature [C]")
)+scale_linetype_manual(values=c("solid","dashed"),name="Treatment",breaks=c("control","para"),
                        labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8))



#Plotting cnsmp from T1, when consumption was first recorded (not starting at 0--T0)

lng.cnsmp<-subset(cpt.cl,Timepoint!="T0")

c<-ggplot(lng.cnsmp, aes(age, log.cnsmp, group=interaction(bug.id, temp), color=factor(temp)))
c+geom_line(aes(linetype=treatment)
)+theme_bw(
)+theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border= element_blank(), 
        axis.line.x = element_line(color="black", size = 1.5),
        axis.line.y = element_line(color="black", size = 1.5),text = element_text(size = 15)
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     guide=guide_legend(title="Temperature [C]")
)+scale_linetype_manual(values=c("solid","dashed"),name="Treatment",breaks=c("control","para"),
                        labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8))



View(lng.cnsmp)

#raw data--consumption by age, color and linetype by treatment, facet_wrap by temp

lc<- ggplot(cpt.cl, aes(age, log.cnsmp, group=interaction(bug.id, temp), color=treatment))
lc+geom_line(aes(linetype=treatment)
)+theme_bw(
)+theme(plot.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border= element_blank(), 
        axis.line.x = element_line(color="black", size = 1.5),
        axis.line.y = element_line(color="black", size = 1.5),text = element_text(size = 15)
)+scale_color_manual(values=c("black","red"),name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_manual(values=c("solid","dashed"),name="Treatment",breaks=c("control","para"),
                        labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+facet_wrap(~temp,dir="v")




require(cowplot)
theme_set(theme_cowplot())


#for plot grid--raw data log mass gain

rlmg.plot<- ggplot(cpt.cl, aes(day.age, log.mg, group=interaction(bug.id, temp), color=factor(temp)))
rlmg.plot<-rlmg.plot+geom_line(aes(linetype=treatment)
)+theme(legend.position="none"
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     guide=guide_legend(title="Temperature [C]")
)+scale_linetype_manual(values=c("solid","dashed"),name="Treatment",breaks=c("control","para"),
                        labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+labs(x="Age [day]",y="Log of Mass Gain [mg]")


rlmg.plot



#for plot grid--raw data log consumption

rlc.plot<-ggplot(cpt.cl, aes(day.age, log.cnsmp, group=interaction(bug.id, temp), color=factor(temp)))
rlc.plot<-rlc.plot+geom_line(aes(linetype=treatment)
)+theme(legend.position = c(0.7, 0.3)
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     guide=guide_legend(title="Temperature [C]")
)+scale_linetype_manual(values=c("solid","dashed"),name="Treatment",breaks=c("control","para"),
                        labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_y_continuous(limits=c(0,10)
)+labs(x="Age [day]",y="Log of Consumption [mg]")


rlc.plot


require(cowplot)
theme_set(theme_cowplot())

fig1<-plot_grid(rlmg.plot, rlc.plot, labels = c("A", "B"),align="h")
fig1

save_plot("fig1_raw lmg and lc.png", fig1,ncol=2,base_aspect_ratio = 1.5)


#Avg instar data--log mass by age, color by treatment, facet_wrap by temp

avg.plot <- ggplot(avg.long, aes(x=age,y=log.mass,colour=treat,group=treat))
avg.plot+geom_point()+geom_errorbar(aes(ymin=log.mass-log.mass.se,ymax=log.mass+log.mass.se),size=.5
)+geom_errorbarh(aes(xmin=age-age.se,xmax=age+age.se),size=.5
)+geom_line(aes(linetype=treat,color=treat),size=1
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+labs(title="Avg log(mass) X avg age",x="avg age [hour]",
       y="Avg log(mass)"
)+scale_y_continuous(limits=c(3,11),breaks = c(4,6,8,10)
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1)
)+facet_wrap(~temp)






#Avg instar data--log mass by age, color by temp, facet_wrap by treatment

avg.plot <- ggplot(avg.long, aes(x=age,y=log.mass,colour=temp,group=temp))
avg.plot+geom_point()+geom_errorbar(aes(ymin=log.mass-log.mass.se,ymax=log.mass+log.mass.se),width=.2
)+geom_errorbarh(aes(xmin=age-age.se,xmax=age+age.se)
)+geom_line(aes(linetype=temp,color=temp),size=1
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"), 
                     name="Temperature [C]",breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Temperature [C]",breaks=c("20","25","30"),
                          labels=c("20","25","30")
)+labs(title="Avg log(mass) X avg age",x="avg age [hour]",
       y="Avg log(mass) [mg]"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1)
)+facet_wrap(~treat)











#Average instar data--log mass gain by age, color by treatment, facet by temp

avg.plot <- ggplot(avg.long, aes(x=age,y=log.mg,colour=treat,group=treat))
avg.plot+geom_point(size=1.8)+geom_errorbar(aes(ymin=log.mg-log.mg.se,ymax=log.mg+log.mg.se),width=3
)+geom_errorbarh(aes(xmin=age-age.se,xmax=age+age.se),size=1
)+geom_line(aes(linetype=treat,color=treat),size=1
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+labs(title="Avg mass gain by avg age since 3rd instar",x="avg age [hour]",
       y="Avg log(mass gain)"
)+scale_y_continuous(limits=c(0,11),breaks = c(0,2,4,6,8,10)
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1)
)+facet_wrap(~temp)





#Average instar data--log mass gain by age, color by temp, facet by treatment
avg.plot <- ggplot(avg.long, aes(x=age,y=log.mg,colour=temp,group=temp))
avg.plot+geom_point()+geom_errorbar(aes(ymin=log.mg-log.mg.se,ymax=log.mg+log.mg.se),width=.2
)+geom_errorbarh(aes(xmin=age-age.se,xmax=age+age.se)
)+geom_line(aes(linetype=temp,color=temp),size=1
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"), 
                     name="Temperature [C]",breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Temperature [C]",breaks=c("20","25","30"),
                          labels=c("20","25","30")
)+labs(title="Avg mass gain by avg age (since 3rd instar)",x="avg age [hour]",
       y="Avg log(mass gain)"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1)
)+facet_wrap(~treat)





#Average instar data--consumption by age, color by treatment, facet_wrap by temp

avg.plot <- ggplot(avg.long, aes(x=age,y=log.cnsmp,colour=treat,group=treat))
avg.plot+geom_point(size=1.8)+geom_errorbar(aes(ymin=log.cnsmp-log.cnsmp.se,ymax=log.cnsmp+log.cnsmp.se),width=1
)+geom_errorbarh(aes(xmin=age-age.se,xmax=age+age.se),size=1
)+geom_line(aes(linetype=treat,color=treat),size=1.2
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+labs(title="Avg log(consumption) X avg age",x="avg age [hour]",
       y="Avg log(consumption)"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1)
)+facet_wrap(~temp)




#Average instar data--consumption by age, color by temp, facet_wrap by treatment

avg.plot <- ggplot(avg.long, aes(x=age,y=log.cnsmp,colour=temp,group=temp))
avg.plot<-avg.plot+geom_point()+geom_errorbar(aes(ymin=log.cnsmp-log.cnsmp.se,ymax=log.cnsmp+log.cnsmp.se),width=.2
)+geom_errorbarh(aes(xmin=age-age.se,xmax=age+age.se)
)+geom_line(aes(linetype=temp,color=temp),size=1
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"), 
                     name="Temperature [C]",breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Temperature [C]",breaks=c("20","25","30"),
                          labels=c("20","25","30")
)+labs(title="Avg log(cnsmp) X avg age",x="avg age [hour]",
       y="Avg log(cnsmp) [mg]"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1)
)+facet_wrap(~treat)






#Attempting to look at mass gain and consumption together--however, mass gain is in wet mass, and consumption
#is in dry mass, so in the graph, consumption is below mass gain, which doesn't make much sense


avg.long.20<-subset(avg.long,temp=="20")
avg.long.25<-subset(avg.long,temp=="25")
avg.long.30<-subset(avg.long,temp=="30")


shapes<-c("s1"=16,"s2"=17)


#Average age data--log mass gain and log consumption plotted on same graph, 20C data only, color by treat,
#point shape by metric

test<-ggplot(avg.long.20,aes(x=age,y=log.mg,group=treat,color=treat))
test+geom_point(size=3)+geom_line(aes(linetype=treat,shape="s1"),size=1
                )+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
                )+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                                          labels=c("Control","Parasitized")
                )+labs(title="20C",x="avg age [hour]",
                       y="Avg log(cnsmp) or log(mg)"
                )+geom_point(data=avg.long.20,aes(x=age,y=log.cnsmp,shape="s2"),size=3
                )+geom_line(data=avg.long.20,aes(x=age,y=log.cnsmp,linetype=treat),size=1
                )+scale_shape_manual(name="Metric",breaks=c("s1","s2"),values=shapes,
                                     labels=c("Mass Gain","Consumption"))


test.25<-ggplot(avg.long.25,aes(x=age,y=log.mg,group=treat,color=treat))
test.25+geom_point(size=3)+geom_line(aes(linetype=treat,shape="s1"),size=1
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+labs(title="25C",x="avg age [hour]",
       y="Avg log(cnsmp) or log(mg)"
)+geom_point(data=avg.long.25,aes(x=age,y=log.cnsmp,shape="s2"),size=3
)+geom_line(data=avg.long.25,aes(x=age,y=log.cnsmp,linetype=treat),size=1
)+scale_shape_manual(name="Metric",breaks=c("s1","s2"),values=shapes,
                     labels=c("Mass Gain","Consumption"))




test.30<-ggplot(avg.long.30,aes(x=age,y=log.mg,group=treat,color=treat))
test.30+geom_point(size=3)+geom_line(aes(linetype=treat,shape="s1"),size=1
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+labs(title="25C",x="avg age [hour]",
       y="Avg log(cnsmp) or log(mg)"
)+geom_point(data=avg.long.30,aes(x=age,y=log.cnsmp,shape="s2"),size=3
)+geom_line(data=avg.long.30,aes(x=age,y=log.cnsmp,linetype=treat),size=1
)+scale_shape_manual(name="Metric",breaks=c("s1","s2"),values=shapes,
                     labels=c("Mass Gain","Consumption"))






test<-ggplot(avg.long,aes(x=age,y=log.mg,group=treat,color=treat))
test+geom_point(size=2)+geom_line(aes(linetype=treat,shape="s1")
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+labs(title="Average mass gain and consumption",x="avg age [hour]",
       y="Avg log(consumption) or log(mass.gain)"
)+geom_point(data=avg.long,aes(x=age,y=log.cnsmp,shape="s2"),size=2
)+geom_line(data=avg.long,aes(x=age,y=log.cnsmp,linetype=treat)
)+scale_shape_manual(name="Metric",breaks=c("s1","s2"),values=shapes,
                     labels=c("Mass Gain","Consumption")
)+facet_wrap(~temp,dir="v")






#change hours to days (divide age by 24)
#Add large/diff points where molts occur

wide.cl$day.age.4<-(wide.cl$age.4/24)
wide.cl$day.age.5<-(wide.cl$age.5/24)
wide.cl$day.age.wand<-(wide.cl$age.wander/24)
wide.cl$day.age.em<-(wide.cl$age.em/24)
wide.cl$lmg4<-log(wide.cl$mass.gain.4)
wide.cl$lmg5<-log(wide.cl$mass.gain.5)
wide.cl$lmg.em<-log(wide.cl$mass.gain.em)
wide.cl$lmg.wand<-log(wide.cl$mass.gain.wan)
wide.cl$log.cnsmp4<-log(wide.cl$cnsmp.4)
wide.cl$log.cnsmp5<-log(wide.cl$cnsmp.5)
wide.cl$log.cnsmp.em<-log(wide.cl$cnsmp.em)
wide.cl$log.cnsmp.wand<-log(wide.cl$cnsmp.wan)

dage4.sum<-summarySE(wide.cl,measurevar = "day.age.4",groupvars = c("temp","treatment"),na.rm=TRUE)
dage4.sum

dage5.sum<-summarySE(wide.cl,measurevar = "day.age.5",groupvars = c("temp","treatment"),na.rm=TRUE)
dage5.sum

dagewand.sum<-summarySE(wide.cl,measurevar = "day.age.wand",groupvars = c("temp","treatment"),na.rm=TRUE)
dagewand.sum

dagewand.sum<-subset(dagewand.sum,N>3)

dage.em.sum<-summarySE(wide.cl,measurevar = "day.age.em",groupvars = c("temp","treatment"),na.rm=TRUE)
dage.em.sum

dage.em.sum<-subset(dage.em.sum,N>0)

lmg4.sum<-summarySE(wide.cl,measurevar = "lmg4",groupvars = c("temp","treatment"),na.rm=TRUE)
lmg4.sum  


lmg5.sum<-summarySE(wide.cl,measurevar = "lmg5",groupvars = c("temp","treatment"),na.rm=TRUE)  
lmg5.sum  


lmgem.sum<-summarySE(wide.cl,measurevar = "lmg.em",groupvars = c("temp","treatment"),na.rm=TRUE)  
lmgem.sum

lmgem.sum<-subset(lmgem.sum,N>0)

lmgwand.sum<-summarySE(wide.cl,measurevar = "lmg.wand",groupvars = c("temp","treatment"),na.rm=TRUE)  
lmgwand.sum  

lmgwand.sum<-subset(lmgwand.sum,N>3)

lc4.sum<-summarySE(wide.cl,measurevar = "log.cnsmp4",groupvars = c("temp","treatment"),na.rm=TRUE)
lc4.sum

lc5.sum<-summarySE(wide.cl,measurevar = "log.cnsmp5",groupvars = c("temp","treatment"),na.rm=TRUE)
lc5.sum


lc.em.sum<-summarySE(wide.cl,measurevar = "log.cnsmp.em",groupvars = c("temp","treatment"),na.rm=TRUE)
lc.em.sum

lc.em.sum<-subset(lc.em.sum,N>0)


lc.wand.sum<-summarySE(wide.cl,measurevar = "log.cnsmp.wand",groupvars = c("temp","treatment"),na.rm=TRUE)
lc.wand.sum

lc.wand.sum<-subset(lc.wand.sum,N>3)


day.age<-c(0,0,0,0,0,0,dage4.sum[,4],dage5.sum[,4],dagewand.sum[,4],dage.em.sum[,4])
dage.se<-c(0,0,0,0,0,0,dage4.sum[,6],dage5.sum[,6],dagewand.sum[,6],dage.em.sum[,6])
log.mg<-c(0,0,0,0,0,0,lmg4.sum[,4],lmg5.sum[,4],lmgwand.sum[,4],lmgem.sum[,4])
lmg.se<-c(0,0,0,0,0,0,lmg4.sum[,6],lmg5.sum[,6],lmgwand.sum[,6],lmgem.sum[,6])
log.cnsmp<-c(0,0,0,0,0,0,lc4.sum[,4],lc5.sum[,4],lc.wand.sum[,4],lc.em.sum[,4])
lc.se<-c(0,0,0,0,0,0,lc4.sum[,6],lc5.sum[,6],lc.wand.sum[,6],lc.em.sum[,6])
temp<-c("20","20","25","25","30","30","20","20","25","25","30","30","20","20","25","25","30","30",
        "20","25","30","20","25","30")
treatment<-c("control","para","control","para","control","para","control","para","control","para",
             "control","para","control","para","control","para","control","para","control","control",
             "control","para","para","para")
instar<-c("3","3","3","3","3","3","4","4","4","4","4","4","5","5","5","5","5","5","wander","wander",
          "wander","emergence","emergence","emergence")



avg.inst <- data.frame(day.age,dage.se,log.mg,lmg.se,log.cnsmp,lc.se,temp,treatment,instar)


View(avg.inst)


#trying to get averages for all age time points.....


mg.sum <- summarySE(cpt.cl, measurevar="mass.gain", 
                      groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
mg.sum


#SummarySE for log mass gain

log.mg.sum<-summarySE(cpt.cl, measurevar="log.mg", 
                      groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
log.mg.sum
log.mg.sum$treatment<-as.factor(log.mg.sum$treatment)




#SummarySE for age

age.sum<-summarySE(cpt.cl, measurevar="age", 
                    groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)

age.sum


#SummarySE for day age

dage.sum<-summarySE(cpt.cl, measurevar="day.age", 
                   groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)

dage.sum

#Adding age and age se to the log mass gain summarySE

log.mg.sum$day.age<-dage.sum[,5]
log.mg.sum$day.age.se<-dage.sum[,7]


#Messy and inelegant way of getting rid of the mongo data, but oh well
#fixed this at start of script with .cl manipulations
subset(log.mg.sum, expct.hour > 340 & temp=="30" & treatment=="para")
subset(log.mg.sum, expct.hour > 460 & temp=="25" & treatment=="para")

log.mg.sum.ex<-log.mg.sum[-c(178:187),]
log.mg.sum.ex<-log.mg.sum.ex[-c(129:133),]


#Average timepoint data--log mass gain by time point, color, symbol and linetype by treatment, 
#facet_wrap by temp. Error bars==SE (vertical: log mass gain, horizontal: hour age) 

ann_text <- data.frame(day.age = c(0,0,0),log.mg = c(9,9,9),name=c("20","25","30"),
                       temp = factor(c(20,25,30),levels = c("20","25","30")), 
                       treatment = factor(c("control","control","control"),levels=c("control","para")))


#Stand alone plot

e<-ggplot(log.mg.sum,aes(x=day.age,y=log.mg,group=treatment,color=treatment))
e+geom_point(aes(shape=treatment),size=2)+geom_line(aes(linetype=treatment),size=1
)+geom_errorbar(data=log.mg.sum,aes(ymin=log.mg-se,ymax=log.mg+se),width=.5,size=.7
)+geom_errorbarh(data=log.mg.sum,aes(xmin=day.age-day.age.se,xmax=day.age+day.age.se),height=1,size=.5
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+scale_shape_manual(name="Treatment",breaks=c("control","para"),values=c(16,17),
                     labels=c("Control","Parasitized")
)+geom_point(data=avg.inst,aes(x=day.age,y=log.mg),shape=15,size=4,show.legend = FALSE
)+labs(title="Average log mass gain by hour age",x="Age [day]",
       y="Log of Mass Gain [mg]"
)+facet_wrap(~temp,dir="v")+theme(strip.background = element_blank(),strip.text.x = element_blank()
)+geom_label(data=ann_text,aes(label=name),size=5,show.legend = FALSE)
                                  
                                  



#Stand alone plot--with error bars for avg instar points (exaggerated height for visualization)

e<-ggplot(log.mg.sum,aes(x=day.age,y=log.mg,group=treatment,color=treatment))
e+geom_point(aes(shape=treatment),size=2)+geom_line(aes(linetype=treatment),size=1
)+geom_errorbar(data=log.mg.sum,aes(ymin=log.mg-se,ymax=log.mg+se),width=.5,size=.7
)+geom_errorbarh(data=log.mg.sum,aes(xmin=day.age-day.age.se,xmax=day.age+day.age.se),height=1,size=.5
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+scale_shape_manual(name="Treatment",breaks=c("control","para"),values=c(16,17),
                     labels=c("Control","Parasitized")
)+geom_point(data=avg.inst,aes(x=day.age,y=log.mg),shape=15,size=4,show.legend = FALSE
)+geom_errorbarh(data=avg.inst,aes(xmin=day.age-dage.se,xmax=day.age+dage.se),height=3,size=1
)+labs(title="Average log mass gain by hour age",x="Age [day]",
       y="Log of Mass Gain [mg]"
)+facet_wrap(~temp,dir="v")+theme(strip.background = element_blank(),strip.text.x = element_blank()
)+geom_label(data=ann_text,aes(label=name),size=5,show.legend = FALSE)






#Average timepoint data--color by temp, symbol and linetype by treatment. 
#Error bars==SE (vertical: log mass gain, horizontal: hour age)

e2<-ggplot(log.mg.sum.ex,aes(x=expct.hour,y=log.mg,group=interaction(treatment, temp), color=temp))
e2+geom_point(aes(pch=treatment),size=2)+geom_line(aes(linetype=treatment,color=temp),size=1
)+geom_errorbar(aes(ymin=log.mg-se,ymax=log.mg+se)
)+geom_errorbarh(aes(xmin=age-age.se,xmax=age+age.se)
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"), 
                     name="Temperature [C]",breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+scale_shape_manual(name="Treatment",breaks=c("control","para"),values=c(16,17),
                      labels=c("Control","Parasitized")
)+labs(title="Average log mass gain by hour age",x="Age [hour]",
         y="Avg log(mass gain)")

      

               
                     
#SummarySE for consumption

log.cnsmp.sum<-summarySE(cpt.cl, measurevar="log.cnsmp", 
                      groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
log.cnsmp.sum
log.cnsmp.sum$treatment<-as.factor(log.cnsmp.sum$treatment)


#SummarySE for age

age.sum<-summarySE(cpt.cl, measurevar="age", 
                   groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)

age.sum



#SummarySE for day age

dage.sum<-summarySE(cpt.cl, measurevar="day.age", 
                   groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)

dage.sum

#Adding age and age se to the log mass gain summarySE

log.cnsmp.sum$day.age<-dage.sum[,5]
log.cnsmp.sum$day.age.se<-dage.sum[,7]


#Messy and inelegant way of getting rid of the mongo data, but oh well
#fixed this at start of script with .cl manipulations

subset(log.cnsmp.sum, expct.hour > 340 & temp=="30" & treatment=="para")
subset(log.cnsmp.sum, expct.hour > 460 & temp=="25" & treatment=="para")

log.cnsmp.sum.ex<-log.cnsmp.sum[-c(178:187),]
log.cnsmp.sum.ex<-log.cnsmp.sum.ex[-c(129:133),]


log.cnsmp.sum2<-subset(log.cnsmp.sum,expct.hour!="T0")
log.cnsmp.sum3<-subset(log.cnsmp.sum4,expct.hour=="6")
log.cnsmp.sum3

log.cnsmp.sum4<-summarySE(cpt.cl, measurevar="log.cnsmp", 
                          groupvars=c("temp","expct.hour"),na.rm=TRUE)


test.cn<-ggplot(log.cnsmp.sum2,aes(x=day.age,y=log.cnsmp,group=treatment,color=treatment))
test.cn+geom_point(aes(shape=treatment),size=2)+geom_line(aes(linetype=treatment),size=1
)+geom_errorbar(aes(ymin=log.cnsmp-se,ymax=log.cnsmp+se),width=.5,size=.7
)+geom_errorbarh(aes(xmin=day.age-day.age.se,xmax=day.age+day.age.se),height=1,size=.5
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+scale_shape_manual(name="Treatment",breaks=c("control","para"),values=c(16,17),
                     labels=c("Control","Parasitized")
#)+geom_point(data=avg.inst,aes(x=day.age,y=log.cnsmp),shape=15,size=4,show.legend = FALSE
)+labs(title="Average log consumption by hour age",x="Age [day]",
       y="Log of Consumption [mg]"
)+scale_y_continuous(limits=c(0,10)
)+facet_wrap(~temp,dir="v")



ann_text2 <- data.frame(day.age = c(0,0,0),log.cnsmp = c(9,9,9),name=c("20","25","30"),
                       temp = factor(c(20,25,30),levels = c("20","25","30")), 
                       treatment = factor(c("control","control","control"),levels=c("control","para")))

#Stand alone plot:

cn<-ggplot(log.cnsmp.sum,aes(x=day.age,y=log.cnsmp,group=treatment,color=treatment))
cn+geom_point(aes(shape=treatment),size=2)+geom_line(aes(linetype=treatment),size=1
)+geom_errorbar(aes(ymin=log.cnsmp-se,ymax=log.cnsmp+se),width=.5,size=.7
)+geom_errorbarh(aes(xmin=day.age-day.age.se,xmax=day.age+day.age.se),height=1,size=.5
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+scale_shape_manual(name="Treatment",breaks=c("control","para"),values=c(16,17),
                     labels=c("Control","Parasitized")
)+geom_point(data=avg.inst,aes(x=day.age,y=log.cnsmp),shape=15,size=4,show.legend = FALSE
)+labs(title="Average log consumption by hour age",x="Age [day]",
       y="Log of Consumption [mg]"
)+scale_y_continuous(limits=c(0,10)
)+facet_wrap(~temp,dir="v")+theme(strip.background = element_blank(),strip.text.x = element_blank()
)+geom_label(data=ann_text2,aes(label=name),size=5,show.legend = FALSE)






#Using cowplot to combine avg timepoint graphs for log mass gain and log consumption

require(cowplot)
theme_set(theme_cowplot())

#log mass gain for cowplot


ann_text <- data.frame(day.age = c(0,0,0),log.mg = c(9,9,9),name=c("20","25","30"),
                       temp = factor(c(20,25,30),levels = c("20","25","30")), 
                       treatment = factor(c("control","control","control"),levels=c("control","para")))


lmg.plot<-ggplot(log.mg.sum,aes(x=day.age,y=log.mg,group=treatment,color=treatment))
lmg.plot<-lmg.plot+geom_point(aes(shape=treatment),size=2)+geom_line(aes(linetype=treatment),size=1
)+geom_errorbar(aes(ymin=log.mg-se,ymax=log.mg+se),width=.5,size=.7
)+geom_errorbarh(aes(xmin=day.age-day.age.se,xmax=day.age+day.age.se),height=1,size=.5
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized")
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+scale_shape_manual(name="Treatment",breaks=c("control","para"),values=c(16,17),
                     labels=c("Control","Parasitized")
)+geom_point(data=avg.inst,aes(x=day.age,y=log.mg),shape=15,size=4,show.legend = FALSE
)+labs(x="Age [day]",y="Log mass gain [mg]"
)+facet_wrap(~temp,dir="v")+theme(strip.background = element_blank(),strip.text.x = element_blank()
)+geom_label(data=ann_text,aes(label=name),size=5,show.legend = FALSE)+ theme(legend.position="none")

lmg.plot


#log consumption for cowplot

ann_text2 <- data.frame(day.age = c(0,0,0),log.cnsmp = c(9,9,9),name=c("20","25","30"),
                        temp = factor(c(20,25,30),levels = c("20","25","30")), 
                        treatment = factor(c("control","control","control"),levels=c("control","para")))


cnsmp.plot<-ggplot(log.cnsmp.sum,aes(x=day.age,y=log.cnsmp,group=treatment,color=treatment))
cnsmp.plot<-cnsmp.plot+geom_point(aes(shape=treatment),size=2)+geom_line(aes(linetype=treatment),size=1
)+geom_errorbar(aes(ymin=log.cnsmp-se,ymax=log.cnsmp+se),width=.5,size=.7
)+geom_errorbarh(aes(xmin=day.age-day.age.se,xmax=day.age+day.age.se),height=1,size=.5
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+scale_shape_manual(name="Treatment",breaks=c("control","para"),values=c(16,17),
                     labels=c("Control","Parasitized")
)+geom_point(data=avg.inst,aes(x=day.age,y=log.cnsmp),shape=15,size=4,show.legend = FALSE
)+labs(x="Age [day]", y="Log consumption [dry mg]"
)+scale_y_continuous(limits=c(0,10)
)+facet_wrap(~temp,dir="v")+theme(strip.background = element_blank(),strip.text.x = element_blank()
)+geom_label(data=ann_text2,aes(label=name),size=5,show.legend = FALSE
)+theme(legend.position = c(0.6, 0.2))
cnsmp.plot

fig2<-plot_grid(lmg.plot, cnsmp.plot, labels = c("A", "B"),align="h")
fig2

save_plot("fig2_avg_lmg+lc_nom2.png", fig2,ncol=2,base_aspect_ratio = 1.3)



#Load plots


para<-subset(wide.cl,treatment=="para")
para<-subset(para,died=="0")
para<-subset(para,suc.ovp=="1")
para<-subset(para,load>0)
para<-subset(para,num.ovp<=2)


#num em by total load

load.plot<-ggplot(para,aes(y=num.em,x=load,color=temp,linetype=temp))
load.plot<-load.plot+geom_point(aes(shape=temp))+geom_smooth(se=FALSE,method="lm"
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),name="Temperature [C]",breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_manual(values=c("solid","dotted","longdash"),name="Temperature [C]",breaks=c("20","25","30"),
                        labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+labs(x="Total load",y="Number of emerged wasp larvae"
)+scale_shape_manual(name="Temperature [C]",breaks=c("20","25","30"),labels=c("20","25","30"),
                     values=c(16,17,15)
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1),
        legend.position="none")

load.plot

para$num.em


#internal dev time by load

loaddev.plot<-ggplot(para,aes(x=load,y=int.wasp.dev,group=temp,color=temp,linetype=temp))
loaddev.plot<-loaddev.plot+geom_point(aes(shape=temp))+geom_smooth(se=FALSE,method="lm"
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),name="Temperature [C]",breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_manual(values=c("solid","dotted","longdash"),name="Temperature [C]",breaks=c("20","25","30"),
                        labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+scale_shape_manual(name="Temperature [C]",breaks=c("20","25","30"),labels=c("20","25","30"),
                     values=c(16,17,15),guide=guide_legend(keywidth=1.8)
)+labs(x="Total Load",y="Days to Wasp Emergence"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1),
        legend.position = c(0.6, 0.9))

loaddev.plot


fig3<-plot_grid(load.plot,loaddev.plot,labels = c("A", "B"),align="h")
fig3

save_plot("fig3_load.png", fig3,ncol=2,base_aspect_ratio = 1.5)
getwd()



#Trying to look at prop surv vs load

para$tot.elsurv<-(para$num.em/para$load)
para$tot.llsurv<-(para$num.coc/para$load)


elsurv.plot<-ggplot(para,aes(x=load,y=tot.elsurv,group=temp,color=temp))
elsurv.plot+geom_point()+geom_smooth(se=FALSE,method=lm
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),name="Temperature [C]",breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8))


llsurv.plot<-ggplot(para,aes(x=load,y=tot.llsurv,group=temp,color=temp))
llsurv.plot+geom_point()+geom_smooth(se=FALSE,method=lm
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),name="Temperature [C]",breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8))



elsurv.sum<-summarySE(para, measurevar = "tot.elsurv",
                      groupvars = "temp", na.rm=TRUE)
elsurv.sum


numem.sum<-summarySE(para, measurevar = "num.em",
                     groupvars = "temp", na.rm=TRUE)
numem.sum



#Rescaling age for each temperature, so I can compare the curves on the same scale


cpt.cn<-subset(cpt,Timepoint!="T0")

cpt.20<-subset(cpt.cn,temp=="20")

cpt.20$rdage<-rescale(cpt.20$day.age,to=c(0,1))
cpt.20$rage<-rescale(cpt.20$age,to=c(0,1))



lmrage.sum.20<-summarySE(cpt.20, measurevar="log.mass", 
                      groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
lmrage.sum.20


lmg.rage.sum.20<-summarySE(cpt.20, measurevar="log.mg", 
                         groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
lmg.rage.sum.20


lc.rage.sum.20<-summarySE(cpt.20, measurevar="log.cnsmp", 
                          groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
lc.rage.sum.20


rage.sum.20<-summarySE(cpt.20, measurevar="rage", 
                       groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
rage.sum.20


rdage.sum.20<-summarySE(cpt.20, measurevar="rdage", 
                        groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
rdage.sum.20


rage.sum.20$log.mass<-lmrage.sum.20[,5]
rage.sum.20$log.mass.se<-lmrage.sum.20[,7]
rage.sum.20$log.mg<-lmg.rage.sum.20[,5]
rage.sum.20$log.mg.se<-lmg.rage.sum.20[,7]
rage.sum.20$rdage<-rdage.sum.20[,5]
rage.sum.20$rdage.se<-rdage.sum.20[,7]
rage.sum.20$log.cnsmp<-lc.rage.sum.20[,5]
rage.sum.20$lc.se<-lc.rage.sum.20[,7]


#Rescaled age, only 20C

rage20.plot<-ggplot(rage.sum.20,aes(x=rage,y=log.mass,group=treatment, color=treatment))
rage20.plot<-rage20.plot+geom_point(aes(pch=treatment),size=2)+geom_line(aes(linetype=treatment,color=treatment),size=1
)+geom_errorbar(aes(ymin=log.mass-log.mass.se,ymax=log.mass+log.mass.se)
)+geom_errorbarh(aes(xmin=rage-se,xmax=rage+se)
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+scale_shape_manual(name="Treatment",breaks=c("control","para"),values=c(16,17),
                     labels=c("Control","Parasitized")
)+labs(title="20C",x="Scaled age [0-1]",
       y="Avg log(mass)"
)+theme(legend.position = "none")

rage20.plot


#25

cpt.25<-subset(cpt.cn,temp=="25" & expct.hour<384)

cpt.25$rdage<-rescale(cpt.25$day.age,to=c(0,1))
cpt.25$rage<-rescale(cpt.25$age,to=c(0,1))



lmrage.sum.25<-summarySE(cpt.25, measurevar="log.mass", 
                         groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
lmrage.sum.25


lmg.rage.sum.25<-summarySE(cpt.25, measurevar="log.mg", 
                           groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
lmg.rage.sum.25


rage.sum.25<-summarySE(cpt.25, measurevar="rage", 
                       groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
rage.sum.25


rdage.sum.25<-summarySE(cpt.25, measurevar="rdage", 
                        groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
rdage.sum.25


lc.rage.sum.25<-summarySE(cpt.25, measurevar="log.cnsmp", 
                          groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
lc.rage.sum.25


rage.sum.25$log.mass<-lmrage.sum.25[,5]
rage.sum.25$log.mass.se<-lmrage.sum.25[,7]
rage.sum.25$log.mg<-lmg.rage.sum.25[,5]
rage.sum.25$log.mg.se<-lmg.rage.sum.25[,7]
rage.sum.25$rdage<-rdage.sum.25[,5]
rage.sum.25$rdage.se<-rdage.sum.25[,7]
rage.sum.25$log.cnsmp<-lc.rage.sum.25[,5]
rage.sum.25$lc.se<-lc.rage.sum.25[,7]



#Rescaled age, only 25C

rage25.plot<-ggplot(rage.sum.25,aes(x=rage,y=log.mass,group=treatment, color=treatment))
rage25.plot<-rage25.plot+geom_point(aes(pch=treatment),size=2)+geom_line(aes(linetype=treatment,color=treatment),size=1
)+geom_errorbar(aes(ymin=log.mass-log.mass.se,ymax=log.mass+log.mass.se)
)+geom_errorbarh(aes(xmin=rage-se,xmax=rage+se)
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+scale_shape_manual(name="Treatment",breaks=c("control","para"),values=c(16,17),
                     labels=c("Control","Parasitized")
)+labs(title="25C",x="Scaled age [0-1]",
       y="Avg log(mass)"
)+theme(legend.position = "none")


rage25.plot



#30


cpt.30<-subset(cpt.cn,temp=="30" & expct.hour<384)

cpt.30$rdage<-rescale(cpt.30$day.age,to=c(0,1))
cpt.30$rage<-rescale(cpt.30$age,to=c(0,1))



lmrage.sum.30<-summarySE(cpt.30, measurevar="log.mass", 
                         groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
lmrage.sum.30


lmg.rage.sum.30<-summarySE(cpt.30, measurevar="log.mg", 
                           groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
lmg.rage.sum.30


rage.sum.30<-summarySE(cpt.30, measurevar="rage", 
                       groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
rage.sum.30


rdage.sum.30<-summarySE(cpt.30, measurevar="rdage", 
                        groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
rdage.sum.30


lc.rage.sum.30<-summarySE(cpt.30, measurevar="log.cnsmp", 
                          groupvars=c("temp", "treatment","expct.hour"),na.rm=TRUE)
lc.rage.sum.30


rage.sum.30$log.mass<-lmrage.sum.30[,5]
rage.sum.30$log.mass.se<-lmrage.sum.30[,7]
rage.sum.30$log.mg<-lmg.rage.sum.30[,5]
rage.sum.30$log.mg.se<-lmg.rage.sum.30[,7]
rage.sum.30$rdage<-rdage.sum.30[,5]
rage.sum.30$rdage.se<-rdage.sum.30[,7]
rage.sum.30$log.cnsmp<-lc.rage.sum.30[,5]
rage.sum.30$lc.se<-lc.rage.sum.30[,7]



#Rescaled age, only 30C

rage30.plot<-ggplot(rage.sum.30,aes(x=rage,y=log.mass,group=treatment, color=treatment))
rage30.plot<-rage30.plot+geom_point(aes(pch=treatment),size=2)+geom_line(aes(linetype=treatment,color=treatment),size=1
)+geom_errorbar(aes(ymin=log.mass-log.mass.se,ymax=log.mass+log.mass.se)
)+geom_errorbarh(aes(xmin=rage-se,xmax=rage+se)
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+scale_shape_manual(name="Treatment",breaks=c("control","para"),values=c(16,17),
                     labels=c("Control","Parasitized")
)+labs(title="30C",x="Scaled age [0-1]",
       y="Avg log(mass)"
)+theme(legend.position = "none")

rage30.plot



require(cowplot)

scage.plot<-plot_grid(rage20.plot, rage25.plot, rage30.plot, 
                      labels = c("A", "B","c"),
                      align="v",
                      ncol=1,
                      nrow=3)
scage.plot



#Cnsmp

rage20.lc.plot<-ggplot(rage.sum.20,aes(x=rage,y=log.cnsmp,group=treatment, color=treatment))
rage20.lc.plot<-rage20.lc.plot+geom_point(aes(pch=treatment),size=2)+geom_line(aes(linetype=treatment,color=treatment),size=1
)+geom_errorbar(aes(ymin=log.cnsmp-lc.se,ymax=log.cnsmp+lc.se)
)+geom_errorbarh(aes(xmin=rage-se,xmax=rage+se)
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+scale_shape_manual(name="Treatment",breaks=c("control","para"),values=c(16,17),
                     labels=c("Control","Parasitized")
)+labs(title="20C",x="Scaled age [0-1]",
       y="Avg log(cnsmp)"
)+theme(legend.position = "none")

rage20.lc.plot




rage25.lc.plot<-ggplot(rage.sum.25,aes(x=rage,y=log.cnsmp,group=treatment, color=treatment))
rage25.lc.plot<-rage25.lc.plot+geom_point(aes(pch=treatment),size=2)+geom_line(aes(linetype=treatment,color=treatment),size=1
)+geom_errorbar(aes(ymin=log.cnsmp-lc.se,ymax=log.cnsmp+lc.se)
)+geom_errorbarh(aes(xmin=rage-se,xmax=rage+se)
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+scale_shape_manual(name="Treatment",breaks=c("control","para"),values=c(16,17),
                     labels=c("Control","Parasitized")
)+labs(title="25C",x="Scaled age [0-1]",
       y="Avg log(cnsmp)"
)+theme(legend.position = "none")

rage25.lc.plot



rage30.lc.plot<-ggplot(rage.sum.30,aes(x=rage,y=log.cnsmp,group=treatment, color=treatment))
rage30.lc.plot<-rage30.lc.plot+geom_point(aes(pch=treatment),size=2)+geom_line(aes(linetype=treatment,color=treatment),size=1
)+geom_errorbar(aes(ymin=log.cnsmp-lc.se,ymax=log.cnsmp+lc.se)
)+geom_errorbarh(aes(xmin=rage-se,xmax=rage+se)
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+scale_shape_manual(name="Treatment",breaks=c("control","para"),values=c(16,17),
                     labels=c("Control","Parasitized")
)+labs(title="30C",x="Scaled age [0-1]",
       y="Avg log(cnsmp)"
)+theme(legend.position = c(.85,.3))

rage30.lc.plot



scage.lc.plot<-plot_grid(rage20.lc.plot, rage25.lc.plot, rage30.lc.plot, 
                      labels = c("A", "B","c"),
                      align="v",
                      ncol=1,
                      nrow=3)
scage.lc.plot




#Plotting raw mass gain and consumption of para caterpillars, binning by load

cpt.cl$load<-as.numeric(cpt.cl$load)
para.lng<-subset(cpt.cl,treatment=="para")


##Creating bins
para.50<-subset(para.lng,load>0 & load<=50)
para.100<-subset(para.lng,load>50 & load<=100)
para.150<-subset(para.lng,load>100 & load<=150)
para.200<-subset(para.lng,load>150 & load<=200)
para.250<-subset(para.lng,load>200 & load<=250)
para.300<-subset(para.lng,load>250 & load<=300)
para.350<-subset(para.lng,load>300 & load<=350)



#Plotting bins

bin50.plot<- ggplot(para.50, aes(day.age, log.mg, group=interaction(bug.id, temp), color=factor(temp)))
bin50.plot+geom_line(aes(linetype=treatment)
)+theme(legend.position="none"
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     guide=guide_legend(title="Temperature [C]")
)+scale_linetype_manual(values=c("solid","dashed"),name="Treatment",breaks=c("control","para"),
                        labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+labs(x="Age [day]",y="Log of Mass Gain [mg]")



bin100.plot<- ggplot(para.100, aes(day.age, log.mg, group=interaction(bug.id, temp), color=factor(temp)))
bin100.plot+geom_line(aes(linetype=treatment)
)+theme(legend.position="none"
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     guide=guide_legend(title="Temperature [C]")
)+scale_linetype_manual(values=c("solid","dashed"),name="Treatment",breaks=c("control","para"),
                        labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+labs(x="Age [day]",y="Log of Mass Gain [mg]")




bin150.plot<- ggplot(para.150, aes(day.age, log.mg, group=interaction(bug.id, temp), color=factor(temp)))
bin150.plot+geom_line(aes(linetype=treatment)
)+theme(legend.position="none"
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     guide=guide_legend(title="Temperature [C]")
)+scale_linetype_manual(values=c("solid","dashed"),name="Treatment",breaks=c("control","para"),
                        labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+labs(x="Age [day]",y="Log of Mass Gain [mg]")



bin200.plot<- ggplot(para.200, aes(day.age, log.mg, group=interaction(bug.id, temp), color=factor(temp)))
bin200.plot+geom_line(aes(linetype=treatment)
)+theme(legend.position="none"
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     guide=guide_legend(title="Temperature [C]")
)+scale_linetype_manual(values=c("solid","dashed"),name="Treatment",breaks=c("control","para"),
                        labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+labs(x="Age [day]",y="Log of Mass Gain [mg]")



bin250.plot<- ggplot(para.250, aes(day.age, log.mg, group=interaction(bug.id, temp), color=factor(temp)))
bin250.plot+geom_line(aes(linetype=treatment)
)+theme(legend.position="none"
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     guide=guide_legend(title="Temperature [C]")
)+scale_linetype_manual(values=c("solid","dashed"),name="Treatment",breaks=c("control","para"),
                        labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+labs(x="Age [day]",y="Log of Mass Gain [mg]")



bin300.plot<- ggplot(para.300, aes(day.age, log.mg, group=interaction(bug.id, temp), color=factor(temp)))
bin300.plot+geom_line(aes(linetype=treatment)
)+theme(legend.position="none"
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     guide=guide_legend(title="Temperature [C]")
)+scale_linetype_manual(values=c("solid","dashed"),name="Treatment",breaks=c("control","para"),
                        labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+labs(x="Age [day]",y="Log of Mass Gain [mg]")



bin350.plot<- ggplot(para.350, aes(day.age, log.mg, group=interaction(bug.id, temp), color=factor(temp)))
bin350.plot+geom_line(aes(linetype=treatment)
)+theme(legend.position="none"
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     guide=guide_legend(title="Temperature [C]")
)+scale_linetype_manual(values=c("solid","dashed"),name="Treatment",breaks=c("control","para"),
                        labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+labs(x="Age [day]",y="Log of Mass Gain [mg]")



#Creating column "bin" for each subset, then remerging to long data set so that I can facet wrap by bin

para.50$bin<-50
para.100$bin<-100
para.150$bin<-150
para.200$bin<-200
para.250$bin<-250
para.300$bin<-300
para.350$bin<-350


para.bin<-rbind(para.50,para.100,para.150,para.200,para.250,para.300,para.350)




#Creating a plot of raw para mass gain separated out by load bin (facet_wrap(~temp))

bin.plot3a<- ggplot(para.bin, aes(day.age, log.mg, group=interaction(bug.id, bin), color=factor(bin)))
bin.plot3a+geom_line(aes(color=factor(bin)),
                    size=1.2
)+scale_color_viridis(discrete=TRUE
)+facet_wrap(~temp,dir="v")





#Creating raw para log.mg plots with avg para lines/pts overlaid

lmgpara.sum<-summarySE(para.lng,measurevar = "log.mg",
                       groupvars = c("temp","expct.hour"),
                       na.rm=TRUE)

lmgpara.sum


dagepara.sum<-summarySE(para.lng,measurevar = "day.age",
                        groupvars = c("temp","expct.hour"),
                        na.rm = TRUE)
dagepara.sum

lmgpara.sum$day.age<-dagepara.sum[,4]
lmgpara.sum$dage.se<-dagepara.sum[,6]

lmgpara.sum<-subset(lmgpara.sum,N>=3)


lmgpara.sum1<-lmgpara.sum
lmgpara.sum2<-lmgpara.sum
lmgpara.sum3<-lmgpara.sum
lmgpara.sum4<-lmgpara.sum
lmgpara.sum5<-lmgpara.sum
lmgpara.sum6<-lmgpara.sum

lmgpara.sum$bin<-50
lmgpara.sum1$bin<-100
lmgpara.sum2$bin<-150
lmgpara.sum3$bin<-200
lmgpara.sum4$bin<-250
lmgpara.sum5$bin<-300
lmgpara.sum6$bin<-350

lmgpara.sum.bin<-rbind(lmgpara.sum,lmgpara.sum1,lmgpara.sum2,lmgpara.sum3,lmgpara.sum4,lmgpara.sum5,lmgpara.sum6)


#Raw para log.mg, separated by bin, with avg para log.mg over laid

bin.plot4<- ggplot(para.bin, aes(day.age, log.mg, group=interaction(bug.id, temp), color=factor(temp)))
bin.plot4+geom_line(aes(linetype=treatment)
)+theme(legend.position="none"
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     guide=guide_legend(title="Temperature [C]")
)+scale_linetype_manual(values=c("solid","dashed"),name="Treatment",breaks=c("control","para"),
                        labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+geom_point(data=lmgpara.sum.bin,aes(x=day.age,y=log.mg, group=temp, color=temp),
             size=2, alpha=.5
)+geom_line(data=lmgpara.sum.bin,aes(x=day.age,y=log.mg,group=temp, color=temp),
            size=1,alpha=.5
)+labs(x="Age [day]",y="Log of Mass Gain [mg]"
)+facet_wrap(~bin,dir="v")



#Creating a plot of mass gain separated out by load bin (facet_wrap(~temp))
##separating out avg para temps for color in graph
###lines with pts are avg para log.mg

lmgpara.sum.bin20<-subset(lmgpara.sum.bin,temp=="20")
lmgpara.sum.bin25<-subset(lmgpara.sum.bin,temp=="25")
lmgpara.sum.bin30<-subset(lmgpara.sum.bin,temp=="30")

bin.plot3<- ggplot(para.bin, aes(day.age, log.mg, group=interaction(bug.id, bin), color=factor(bin)))
bin.plot3+geom_line(aes(color=factor(bin)),
                    size=1.2
)+scale_color_viridis(discrete=TRUE
)+geom_line(data=lmgpara.sum.bin20,aes(x=day.age,y=log.mg,group=temp),
            size=1.5,alpha=.5,color="#56B4E9"
)+geom_point(data=lmgpara.sum.bin20,aes(x=day.age,y=log.mg,group=temp),
             size=3,alpha=.5,color="#56B4E9"
)+geom_line(data=lmgpara.sum.bin25,aes(x=day.age,y=log.mg,group=temp),
            size=1.5,alpha=.5,color="#000000"
)+geom_point(data=lmgpara.sum.bin25,aes(x=day.age,y=log.mg,group=temp),
             size=3,alpha=.5,color="#000000"
)+geom_line(data=lmgpara.sum.bin30,aes(x=day.age,y=log.mg,group=temp),
            size=1.5,alpha=.5,color="#E69F00"
)+geom_point(data=lmgpara.sum.bin30,aes(x=day.age,y=log.mg,group=temp),
             size=3,alpha=.5,color="#E69F00"
)+facet_wrap(~temp)








#Creating a plot of consumption seperated out by load bin


bin.plot2<-ggplot(para.bin, aes(day.age, log.cnsmp, group=interaction(bug.id, temp), color=factor(temp)))
bin.plot2+geom_line(aes(linetype=treatment)
)+theme(legend.position = c(0.7, 0.3)
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     guide=guide_legend(title="Temperature [C]")
)+scale_linetype_manual(values=c("solid","dashed"),name="Treatment",breaks=c("control","para"),
                        labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_y_continuous(limits=c(0,10)
)+labs(x="Age [day]",y="Log of Consumption [mg]"
)+facet_wrap(~bin)




#Attempting to make an average mass gain plot, separated by load bin

lmg.sum<-summarySE(para.bin,measurevar = "log.mg",
                   groupvars = c("temp","expct.hour","bin"),
                   na.rm = TRUE)
lmg.sum


age.sum<-summarySE(para.bin,measurevar = "age",
                   groupvars = c("temp","expct.hour","bin"),
                   na.rm = TRUE)
age.sum


lmg.sum$age<-age.sum[,5]
lmg.sum$age.se<-age.sum[,7]



avgbin.plot<-ggplot(lmg.sum, aes(age, log.mg, group=temp, color=factor(temp)))
avgbin.plot+geom_line(size=1
)+geom_point(size=2
)+geom_errorbar(aes(ymin=log.mg-se,ymax=log.mg+se)
)+geom_errorbarh(aes(xmin=age-age.se,xmax=age+age.se)
)+theme(legend.position = c(0.7, 0.3)
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     guide=guide_legend(title="Temperature [C]")
)+scale_y_continuous(limits=c(0,10)
)+labs(x="Age [day]",y="Log of Consumption [mg]"
)+facet_wrap(~bin)




#Making bin columns for wide para

para.w<-subset(wide.cl,treatment=="para")

##Creating bins
paraw.50<-subset(para.w,load>0 & load<=50)
paraw.100<-subset(para.w,load>50 & load<=100)
paraw.150<-subset(para.w,load>100 & load<=150)
paraw.200<-subset(para.w,load>150 & load<=200)
paraw.250<-subset(para.w,load>200 & load<=250)
paraw.300<-subset(para.w,load>250 & load<=300)
paraw.350<-subset(para.w,load>300 & load<=350)


paraw.50$bin<-50
paraw.100$bin<-100
paraw.150$bin<-150
paraw.200$bin<-200
paraw.250$bin<-250
paraw.300$bin<-300
paraw.350$bin<-350

paraw.bin<-rbind(paraw.50,paraw.100,paraw.150,paraw.200,paraw.250,paraw.300,paraw.350)


binwdev.sum<-summarySE(paraw.bin,measurevar = "int.wasp.dev",
                       groupvars = c("temp","bin"),
                       na.rm = TRUE)
binwdev.sum



#Plotting average wasp development, divided by load bin


#shape and line type by bin
wbin.plot<-ggplot(binwdev.sum,aes(x=temp,y=int.wasp.dev,group=as.factor(bin),color=as.factor(bin)))
wbin.plot+geom_point(aes(shape=as.factor(bin)),
                     size=4
        )+geom_line(aes(linetype=as.factor(bin)),
                    size=1.2)


#shape and linetype by temp
wbin.plot2<-ggplot(binwdev.sum,aes(x=bin,y=int.wasp.dev,group=temp,color=temp))
wbin.plot2+geom_point(aes(shape=temp),
                      size=4
         )+geom_line(aes(linetype=temp),
                     size=1.2)







#Creating a dataframe of average instar mass gain, separated by load bin

lmg4.sum<-summarySE(paraw.bin,measurevar = "mass.gain.4",
                    groupvars = c("temp","bin"),
                    na.rm = TRUE)
lmg4.sum


lmg5.sum<-summarySE(paraw.bin,measurevar = "mass.gain.5",
                    groupvars = c("temp","bin"),
                    na.rm = TRUE)
lmg5.sum

lmgem.sum<-summarySE(paraw.bin,measurevar = "mass.gain.em",
                     groupvars = c("temp","bin"),
                     na.rm = TRUE)
lmgem.sum



age4.sum<-summarySE(paraw.bin,measurevar = "age.4",
                    groupvars = c("temp","bin"),
                    na.rm = TRUE)
age4.sum


age5.sum<-summarySE(paraw.bin,measurevar = "age.5",
                    groupvars = c("temp","bin"),
                    na.rm = TRUE)
age5.sum


ageem.sum<-summarySE(paraw.bin,measurevar = "age.em",
                     groupvars = c("temp","bin"),
                     na.rm = TRUE)
ageem.sum




temp<-lmg4.sum[,1]
bin<-lmg4.sum[,2]
lmg<-c(0,0,0,0,0,0,
       0,0,0,0,0,0,
       0,0,0,0,0,0,0,
       lmg4.sum[,4],
       lmg5.sum[,4],
       lmgem.sum[,4])

lmg.se<-c(0,0,0,0,0,0,
          0,0,0,0,0,0,
          0,0,0,0,0,0,0,
          lmg4.sum[,6],
          lmg5.sum[,6],
          lmgem.sum[,6])

age<-c(0,0,0,0,0,0,
       0,0,0,0,0,0,
       0,0,0,0,0,0,0,
       age4.sum[,4],
       age5.sum[,4],
       ageem.sum[,4])

age.se<-c(0,0,0,0,0,0,
          0,0,0,0,0,0,
          0,0,0,0,0,0,0,
          age4.sum[,6],
          age5.sum[,6],
          ageem.sum[,6])


#Ns shouldn't be 0s for 3rds, but don't want to take the time to look that up right now
N.lmg<-c(0,0,0,0,0,0,
         0,0,0,0,0,0,
         0,0,0,0,0,0,0,
         lmg4.sum[,3],
         lmg5.sum[,3],
         lmgem.sum[,3])

N.age<-c(0,0,0,0,0,0,
         0,0,0,0,0,0,
         0,0,0,0,0,0,0,
         age4.sum[,3],
         age5.sum[,3],
         ageem.sum[,3])



avgpw.bin<-data.frame(temp,bin,lmg,lmg.se,age,age.se,N.lmg,N.age)
View(avgpw.bin)



#Plotting average mass gain at each instar by temp and load bin

aibin.plot<-ggplot(avgpw.bin,aes(x=age,y=lmg,group=as.factor(bin),color=as.factor(bin)))
aibin.plot+geom_point(aes(shape=as.factor(bin)),
                      size=6
         )+geom_line(aes(linetype=as.factor(bin)),
                     size=1.2
         )+facet_wrap(~temp)


aibin.plot2<-ggplot(avgpw.bin,aes(x=age,y=lmg,group=temp,color=temp))
aibin.plot2+geom_point(aes(shape=temp),
                       size=4
)+geom_line(aes(linetype=temp),
            size=1.2
)+geom_errorbar(aes(ymin=lmg-lmg.se, ymax=lmg+lmg.se),
                width=.5,size=1
)+geom_errorbarh(aes(xmin=age-age.se,xmax=age+age.se)
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),
                     guide=guide_legend(title="Temperature [C]")
)+scale_linetype_manual(values=c("solid","dashed","dotted"),name="Temperature [C]",breaks=c("20","25","30"),
                        labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+scale_shape_manual(values=c(16,17,15),name="Temperature [C]",breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+facet_wrap(~bin)





#Creating individual plots for the different temperatures--raw log.mg for para, load by color gradient

#removing mongos, sacrifices and wanderers. Removing para at 30 with huge load (more than 350)
para.lng<-subset(para.lng,load>0)
para.lng<-subset(para.lng,load<350)

para.lng20<-subset(para.lng,temp=="20")
para.lng25<-subset(para.lng,temp=="25")
para.lng30<-subset(para.lng,temp=="30")



#Viridis color gradient
load20.plot<-ggplot(para.lng20,aes(x=day.age,y=log.mg,group=bug.id,color=load))
load20.plot<-load20.plot+geom_line(size=1
          )+scale_color_viridis(option="viridis",
                                breaks=c(50,100,150,200,250,300)
          )+labs(y="Log of Mass Gain [mg]", x=NULL, title="20")



load25.plot<-ggplot(para.lng25,aes(x=day.age,y=log.mg,group=bug.id,color=load))
load25.plot<-load25.plot+geom_line(size=1
)+scale_color_viridis(option="viridis",
                      breaks=c(50,100,150,200,250,300)
)+labs(y=NULL, x=NULL, title="25")


load30.plot<-ggplot(para.lng30,aes(x=day.age,y=log.mg,group=bug.id,color=load))
load30.plot<-load30.plot+geom_line(size=1
)+scale_color_viridis(option="viridis",
                      breaks=c(50,100,150,200,250,300)
)+labs(y=NULL, x=NULL, title="30")

    



#custom color gradient

load20.plot<-ggplot(para.lng20,aes(x=age,y=log.mg,group=bug.id,color=load))
load20.plot+geom_line(size=2
)+scale_color_gradient(low="black",high="green")


load25.plot<-ggplot(para.lng25,aes(x=age,y=log.mg,group=bug.id,color=load))
load25.plot+geom_line(size=2
)+scale_color_gradient(low="blue",high="red")


load30.plot<-ggplot(para.lng30,aes(x=age,y=log.mg,group=bug.id,color=load))
load30.plot+geom_line(size=2
)+scale_color_gradient(low="blue",high="red")




#Consumption plots--raw data, separated by temp, colored by load

#Viridis color gradient
cnsmp.ld20.plot<-ggplot(para.lng20,aes(x=day.age,y=log.cnsmp,group=bug.id,color=load))
cnsmp.ld20.plot<-cnsmp.ld20.plot+geom_line(size=1
)+scale_color_viridis(option="viridis",
                      breaks=c(50,100,150,200,250,300)
)+labs(y="Log of Consumption [mg]", x="Age [days]", title=NULL)



cnsmp.ld25.plot<-ggplot(para.lng25,aes(x=day.age,y=log.cnsmp,group=bug.id,color=load))
cnsmp.ld25.plot<-cnsmp.ld25.plot+geom_line(size=1
)+scale_color_viridis(option="viridis",
                      breaks=c(50,100,150,200,250,300)
)+labs(y=NULL, x="Age [days]",title=NULL)



cnsmp.ld30.plot<-ggplot(para.lng30,aes(x=day.age,y=log.cnsmp,group=bug.id,color=load))
cnsmp.ld30.plot<-cnsmp.ld30.plot+geom_line(size=1
)+scale_color_viridis(option="viridis",
                      breaks=c(50,100,150,200,250,300)
)+labs(y=NULL, x="Age [days]",title=NULL)



#Attempting to put separate temp plots together

test.plot<-plot_grid(load20.plot, load25.plot, load30.plot, labels = c("A", "B","C"),align="h", ncol=1, nrow=3)
test.plot   #This still looks gross


test.plot2<-plot_grid(load20.plot, load25.plot, load30.plot, labels = c("A", "B","C"),align="v", ncol=3, nrow=1)
test.plot2   #Maybe better?


test.plot3<-plot_grid(cnsmp.ld20.plot, cnsmp.ld25.plot, cnsmp.ld30.plot, labels = c("D", "E","F"),align="v", ncol=3, nrow=1)
test.plot3

test.plot4<-plot_grid(load20.plot, load25.plot, load30.plot,
                      cnsmp.ld20.plot, cnsmp.ld25.plot, cnsmp.ld30.plot, 
                      labels = c("A", "B","C", "D", "E","F"),
                      align="v", ncol=3, nrow=2)
test.plot4





#Creating load bins for each temperature (bins=100)

para.lng20.100<-subset(para.lng20,load>0 & load<=100)
para.lng20.200<-subset(para.lng20,load>100 & load<=200)
para.lng20.300<-subset(para.lng20,load>200 & load<=300)


para.lng25.100<-subset(para.lng25,load>0 & load<=100)
para.lng25.200<-subset(para.lng25,load>100 & load<=200)
para.lng25.300<-subset(para.lng25,load>200 & load<=300)

para.lng30.100<-subset(para.lng30,load>0 & load<=100)
para.lng30.200<-subset(para.lng30,load>100 & load<=200)
para.lng30.300<-subset(para.lng30,load>200 & load<=300)



#Giving each subset a column named "bin", then binding them back together to form one data fram

para.lng20.100$bin<-100
para.lng20.200$bin<-200
para.lng20.300$bin<-300
para.lng20.bin<-rbind(para.lng20.100,para.lng20.200,para.lng20.300)


para.lng25.100$bin<-100
para.lng25.200$bin<-200
para.lng25.300$bin<-300
para.lng25.bin<-rbind(para.lng25.100,para.lng25.200,para.lng25.300)


para.lng30.100$bin<-100
para.lng30.200$bin<-200
para.lng30.300$bin<-300
para.lng30.bin<-rbind(para.lng30.100,para.lng30.200,para.lng30.300)




#Creating average data frame for each load bin at each temperature

lmg20.sum<-summarySE(para.lng20.bin,measurevar = "log.mg",
                   groupvars = c("temp","expct.hour","bin"),
                   na.rm = TRUE)
lmg20.sum


dage20.sum<-summarySE(para.lng20.bin,measurevar = "day.age",
                     groupvars = c("temp","expct.hour","bin"),
                     na.rm = TRUE)
dage20.sum


lmg20.sum$day.age<-dage20.sum[,5]
lmg20.sum$dage.se<-dage20.sum[,7]





lmg25.sum<-summarySE(para.lng25.bin,measurevar = "log.mg",
                     groupvars = c("temp","expct.hour","bin"),
                     na.rm = TRUE)
lmg25.sum


dage25.sum<-summarySE(para.lng25.bin,measurevar = "day.age",
                      groupvars = c("temp","expct.hour","bin"),
                      na.rm = TRUE)
dage25.sum

lmg25.sum$day.age<-dage25.sum[,5]
lmg25.sum$dage.se<-dage25.sum[,7]





lmg30.sum<-summarySE(para.lng30.bin,measurevar = "log.mg",
                     groupvars = c("temp","expct.hour","bin"),
                     na.rm = TRUE)
lmg30.sum


dage30.sum<-summarySE(para.lng30.bin,measurevar = "day.age",
                      groupvars = c("temp","expct.hour","bin"),
                      na.rm = TRUE)
dage30.sum

lmg30.sum$day.age<-dage30.sum[,5]
lmg30.sum$dage.se<-dage30.sum[,7]



#Making separate dataframes with the averages of each bin, so that I can color them separately from the raw data in the plot

#Bins at 20

lmg20.100.sum<-summarySE(para.lng20.100,measurevar = "log.mg",
                                    groupvars = c("expct.hour"),
                                    na.rm = TRUE)
lmg20.100.sum


dage20.100.sum<-summarySE(para.lng20.100,measurevar = "day.age",
                          groupvars = c("expct.hour"),
                          na.rm = TRUE)

dage20.100.sum


lmg20.100.sum$day.age<-dage20.100.sum[,3]
lmg20.100.sum$dage.se<-dage20.100.sum[,5]



lmg20.200.sum<-summarySE(para.lng20.200,measurevar = "log.mg",
                         groupvars = c("temp","expct.hour"),
                         na.rm = TRUE)
lmg20.200.sum


dage20.200.sum<-summarySE(para.lng20.200,measurevar = "day.age",
                          groupvars = c("temp","expct.hour"),
                          na.rm = TRUE)

dage20.200.sum


lmg20.200.sum$day.age<-dage20.200.sum[,4]
lmg20.200.sum$dage.se<-dage20.200.sum[,6]


lmg20.300.sum<-summarySE(para.lng20.300,measurevar = "log.mg",
                         groupvars = c("temp","expct.hour"),
                         na.rm = TRUE)
lmg20.300.sum


dage20.300.sum<-summarySE(para.lng20.300,measurevar = "day.age",
                          groupvars = c("temp","expct.hour"),
                          na.rm = TRUE)

dage20.300.sum


lmg20.300.sum$day.age<-dage20.300.sum[,4]
lmg20.300.sum$dage.se<-dage20.300.sum[,6]







#Creating plots for each temperature with average, binned data on top of raw data


load20bin.plot<-ggplot(para.lng20,aes(x=day.age,y=log.mg))
load20bin.plot<-load20bin.plot+geom_line(aes(group=bug.id,color=load),
                                         size=1
)+scale_color_viridis(option="viridis",
                      breaks=c(50,100,150,200,250,300)
)+geom_point(data=lmg20.100.sum,aes(x=day.age,y=log.mg),
             size=4,color="#FCB216"
)+geom_line(data=lmg20.100.sum,aes(x=day.age,y=log.mg),
            size=2,linetype="solid",color="#FCB216"
)+geom_point(data=lmg20.200.sum,aes(x=day.age,y=log.mg),
             size=4,color="#EE6A23"
)+geom_line(data=lmg20.200.sum,aes(x=day.age,y=log.mg),
            size=2,linetype="solid",color="#EE6A23"
)+geom_point(data=lmg20.300.sum,aes(x=day.age,y=log.mg),
             size=4,color="#CA4049"
)+geom_line(data=lmg20.300.sum,aes(x=day.age,y=log.mg),
            size=2,linetype="solid",color="#CA4049"
)+labs(y="Log of Mass Gain [mg]", x=NULL, title="20")

load20bin.plot







load25bin.plot<-ggplot(para.lng25,aes(x=day.age,y=log.mg,group=bug.id,color=load))
load25bin.plot<-load25bin.plot+geom_line(size=1
)+scale_color_viridis(option="viridis",
                      breaks=c(50,100,150,200,250,300)
)+labs(y=NULL, x=NULL, title="25")


load30bin.plot<-ggplot(para.lng30,aes(x=day.age,y=log.mg,group=bug.id,color=load))
load30bin.plot<-load30bin.plot+geom_line(size=1
)+scale_color_viridis(option="viridis",
                      breaks=c(50,100,150,200,250,300)
)+labs(y=NULL, x=NULL, title="30")




#Creating average only plots for load bins at each temperature--creating different load bin sizes


#20

para.lng20$load<-as.numeric(para.lng20$load)

loadbin20.50<-subset(para.lng20,load<50)
loadbin20.100<-subset(para.lng20,load>50 & load<=100)
loadbin20.150<-subset(para.lng20,load>100 & load<=150)
loadbin20.200<-subset(para.lng20, load>150 & load<=200)
loadbin20.300<-subset(para.lng20, load>200 & load<=350)

loadbin20.50$bin<-50
loadbin20.100$bin<-100
loadbin20.150$bin<-150
loadbin20.200$bin<-200
loadbin20.300$bin<-300

loadbin20<-rbind(loadbin20.50,loadbin20.100,loadbin20.150,loadbin20.200,loadbin20.300)


lb20.lmg.sum<-summarySE(loadbin20,measurevar = "log.mg",
                        groupvars = c("expct.hour","bin"),
                        na.rm=TRUE)


lb20.lmg.sum


lb20.age.sum<-summarySE(loadbin20,measurevar = "day.age",
                        groupvars = c("expct.hour","bin"),
                        na.rm=TRUE)
lb20.age.sum


lb20.lmg.sum$day.age<-lb20.age.sum[,4]
lb20.lmg.sum$dage.se<-lb20.age.sum[,6]



#25


loadbin25.50<-subset(para.lng25,load<50)
loadbin25.100<-subset(para.lng25,load>50 & load<=100)
loadbin25.150<-subset(para.lng25,load>100 & load<=150)
loadbin25.200<-subset(para.lng25, load>150 & load<=200)
loadbin25.300<-subset(para.lng25, load>250 & load<=350)

loadbin25.50$bin<-50
loadbin25.100$bin<-100
loadbin25.150$bin<-150
loadbin25.200$bin<-200
loadbin25.300$bin<-300

loadbin25<-rbind(loadbin25.50,loadbin25.100,loadbin25.150,loadbin25.200,loadbin25.300)


lb25.lmg.sum<-summarySE(loadbin25,measurevar = "log.mg",
                        groupvars = c("expct.hour","bin"),
                        na.rm=TRUE)


lb25.lmg.sum


lb25.age.sum<-summarySE(loadbin25,measurevar = "day.age",
                        groupvars = c("expct.hour","bin"),
                        na.rm=TRUE)
lb25.age.sum


lb25.lmg.sum$day.age<-lb25.age.sum[,4]
lb25.lmg.sum$dage.se<-lb25.age.sum[,6]





#30

loadbin30.50<-subset(para.lng30,load<50)
loadbin30.100<-subset(para.lng30,load>50 & load<=100)
loadbin30.150<-subset(para.lng30,load>100 & load<=150)
loadbin30.200<-subset(para.lng30, load>150 & load<=200)
loadbin30.300<-subset(para.lng30, load>250 & load<=350)

loadbin30.50$bin<-50
loadbin30.100$bin<-100
loadbin30.150$bin<-150
loadbin30.200$bin<-200
loadbin30.300$bin<-300

loadbin30<-rbind(loadbin30.50,loadbin30.100,loadbin30.150,loadbin30.200,loadbin30.300)


lb30.lmg.sum<-summarySE(loadbin30,measurevar = "log.mg",
                        groupvars = c("expct.hour","bin"),
                        na.rm=TRUE)


lb30.lmg.sum


lb30.age.sum<-summarySE(loadbin30,measurevar = "day.age",
                        groupvars = c("expct.hour","bin"),
                        na.rm=TRUE)
lb30.age.sum


lb30.lmg.sum$day.age<-lb30.age.sum[,4]
lb30.lmg.sum$dage.se<-lb30.age.sum[,6]









loadbin50.20.plot<-ggplot(lb20.lmg.sum,aes(x=day.age,y=log.mg,group=as.factor(bin),color=as.factor(bin)))
loadbin50.20.plot<-loadbin50.20.plot+geom_line(size=1.2,show.legend=FALSE
)+geom_point(size=3,show.legend=FALSE
)+geom_errorbar(aes(ymin=log.mg-se,ymax=log.mg+se),
                width=.5,size=.7
)+scale_color_manual(values=c("#440D54","#647ABD","#1F9F88","#95D840","#D5A913"),
                     guide=FALSE
)+scale_y_continuous(limits=c(0,9),
                     breaks=c(0,2,4,6,8)
)+labs(y="Log mass gain [mg]", x=NULL, title="20")

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
)+labs(y=NULL, x=NULL, title="25")

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
)+theme(legend.position = c(0.7, 0.4))

loadbin50.30.plot




#Making avg dataframes for consumption for each temperature

#20

lb20.cnsmp.sum<-summarySE(loadbin20,measurevar = "log.cnsmp",
                          groupvars = c("expct.hour","bin"),
                          na.rm=TRUE)
lb20.cnsmp.sum


lb20.age.sum


lb20.cnsmp.sum$day.age<-lb20.age.sum[,4]
lb20.cnsmp.sum$dage.se<-lb20.age.sum[,6]



#25

lb25.cnsmp.sum<-summarySE(loadbin25,measurevar = "log.cnsmp",
                          groupvars = c("expct.hour","bin"),
                          na.rm=TRUE)
lb25.cnsmp.sum


lb25.age.sum


lb25.cnsmp.sum$day.age<-lb25.age.sum[,4]
lb25.cnsmp.sum$dage.se<-lb25.age.sum[,6]




#30

lb30.cnsmp.sum<-summarySE(loadbin30,measurevar = "log.cnsmp",
                          groupvars = c("expct.hour","bin"),
                          na.rm=TRUE)
lb30.cnsmp.sum


lb30.age.sum


lb30.cnsmp.sum$day.age<-lb30.age.sum[,4]
lb30.cnsmp.sum$dage.se<-lb30.age.sum[,6]



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
)+labs(y="Log consumption [mg]", x="Age [days]")


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
)+labs(y=NULL, x="Age [days]")

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
)+theme(legend.position = c(0.7, 0.3))

lb50cnsmp.30.plot




#Combining log mass and log cnsmp plots for each temp into one figure (2 rows by 3 columns)

test.plot5<-plot_grid(loadbin50.20.plot, loadbin50.25.plot, loadbin50.30.plot,
                      lb50cnsmp.20.plot, lb50cnsmp.25.plot, lb50cnsmp.30.plot, 
                      labels = c("A", "B","C", "D", "E","F"),
                      align="v", ncol=3, nrow=2)
test.plot5







#Want to make a reaction norm for time from 3rd to either wandering or wasp emergence

wide.cl$age.end<-coalesce(wide.cl$age.em, wide.cl$age.wander)

wide.cl$day.age.end<-wide.cl$age.end/24

age.end.sum<-summarySE(wide.cl, measurevar = "day.age.end",
                       groupvars = c("temp","treatment"),
                       na.rm=TRUE)
age.end.sum


age.end.plot<-ggplot(age.end.sum, aes(x=temp,y=day.age.end,group=treatment,color=treatment))
age.end.plot+geom_point(aes(shape=treatment),
                        size=3
           )+geom_line(aes(linetype=treatment),
                       size=1.2
           )+geom_errorbar(aes(ymin=day.age.end-se,ymax=day.age.end+se),
                           width=.3, size=1
           )+scale_color_manual(values=c("black","red"),
                                breaks=c("control","para"),
                                labels=c("Control","Parasitized"),
                                name="Treatment"
           )+scale_linetype_manual(values=c("solid", "dashed"),
                                   breaks=c("control","para"),
                                   labels=c("Control","Parasitized"),
                                   name="Treatment"
           )+scale_shape_manual(name="Treatment",breaks=c("control","para"),values=c(16,17),
                                labels=c("Control","Parasitized")          
           )+labs(x="Rearing temperature [C]", y="Age to Wandering/Emergence [days]")





#------

#calc avg %surv for wasps at each temp

wide.para<-subset(wide.cl, treatment=="para" & load!=0)


wide.para<-wide.para %>% mutate(perc.surv = num.em/load)

wide.para$perc.surv




perc.surv.sum<-summarySE(wide.para, measurevar = "perc.surv",
                         groupvars = "temp", na.rm=FALSE)
perc.surv.sum



#Calc mean # surv for wasps at each temp

numem.sum<-summarySE(wide.para, measurevar = "num.em",
                     groupvars = "temp", na.rm=TRUE)
numem.sum


#-------------

#running model for lmg without mongos

lm.mod2<-lme(log.mass~(age+I(age^2)):(temp*treatment)+temp,random=~age|bug.id,
             data=cpt.cl,na.action=na.exclude,method="ML")
anova(lm.mod2)
summary(lm.mod2)


#running model for lc without mongos

#removed rows with timepoint T0, so that the starting consumption is what was measured at T1 (6 hours)--saved in df lng.cnsmp
cpt.cl.cnsmp<-subset(cpt.cl,Timepoint!="T0")

lc.mod2<-lme(log.cnsmp~(age+I(age^2)):(temp*treatment)+temp,random=~age|bug.id,
             data=cpt.cl.cnsmp,
             na.action=na.omit,method="ML")
anova(lc.mod2)
summary(lc.mod2)




#running lmg load model without mongos--think I did this originally, just checking
#is the same as in my manuscript

#subsetting to only para
cpt.cl.para<-subset(cpt.cl, treatment=="para")

#making load numeric
cpt.cl.para$load<-as.numeric(cpt.cl.para$load)

lmload.mod2<-lme(log.mass~(age+I(age^2)):(temp*load)+temp,random=~age|bug.id,
                 data=cpt.cl.para,na.action=na.omit,method="ML")
anova(lmload.mod2)



#-----------------

#Means and variations for CxPxT measurements

mass3.sum<-summarySE(wide.cl, measurevar = "mass.T0",
                     groupvars = c("temp", "treatment"),
                     na.rm = TRUE)
mass3.sum


mass4.sum<-summarySE(wide.cl, measurevar = "mass.4",
                     groupvars = c("temp", "treatment"),
                     na.rm = TRUE)
mass4.sum  
  

mass5.sum<-summarySE(wide.cl, measurevar = "mass.5",
                     groupvars = c("temp", "treatment"),
                     na.rm = TRUE)
mass5.sum
  

masswand.sum<-summarySE(wide.cl, measurevar = "mass.wander",
                        groupvars = c("temp", "treatment"),
                        na.rm = TRUE)
masswand.sum


massem.sum<-summarySE(wide.cl, measurevar = "mass.befem",
                      groupvars = c("temp", "treatment"),
                      na.rm = TRUE)
massem.sum  


cnsmp4.sum<-summarySE(wide.cl, measurevar = "cnsmp.4",
                      groupvars = c("temp", "treatment"),
                      na.rm = TRUE)
cnsmp4.sum


cnsmp5.sum<-summarySE(wide.cl, measurevar = "cnsmp.5",
                     groupvars = c("temp", "treatment"),
                     na.rm = TRUE)
cnsmp5.sum



cnsmpwand.sum<-summarySE(wide.cl, measurevar = "cnsmp.wan",
                         groupvars = c("temp", "treatment"),
                         na.rm = TRUE)
cnsmpwand.sum


cnsmpem.sum<-summarySE(wide.cl, measurevar = "cnsmp.em",
                       groupvars = c("temp", "treatment"),
                       na.rm = TRUE)
cnsmpem.sum


wide.cl$dage.3<-wide.cl$date.3 - wide.cl$date.hatch

dage3.sum<-summarySE(wide.cl, measurevar = "dage.3",
                     groupvars = c("temp", "treatment"),
                     na.rm = TRUE)
dage3.sum



wide.cl$dage.4<-wide.cl$date.4 - wide.cl$date.hatch

dage4.sum<-summarySE(wide.cl, measurevar = "dage.4",
                     groupvars = c("temp", "treatment"),
                     na.rm = TRUE)
dage4.sum


wide.cl$dage.5<-wide.cl$date.5 - wide.cl$date.hatch

dage5.sum<-summarySE(wide.cl, measurevar = "dage.5",
                     groupvars = c("temp", "treatment"),
                     na.rm = TRUE)
dage5.sum


wide.cl$dage.wand<-wide.cl$date.wander - wide.cl$date.hatch

dagewand.sum<-summarySE(wide.cl, measurevar = "dage.wand",
                        groupvars = c("temp", "treatment"),
                        na.rm = TRUE)
dagewand.sum



wide.cl$dage.em<-wide.cl$date.em - wide.cl$date.hatch

dageem.sum<-summarySE(wide.cl, measurevar = "dage.em",
                      groupvars = c("temp", "treatment"),
                      na.rm = TRUE)
dageem.sum



day.age.4sum<-summarySE(wide.cl, measurevar = "day.age.4",
                        groupvars = c("temp", "treatment"),
                        na.rm = TRUE)
day.age.4sum



day.age.5sum<-summarySE(wide.cl, measurevar = "day.age.5",
                        groupvars = c("temp", "treatment"),
                        na.rm = TRUE)
day.age.5sum



day.age.wandsum<-summarySE(wide.cl, measurevar = "day.age.wand",
                        groupvars = c("temp", "treatment"),
                        na.rm = TRUE)
day.age.wandsum



day.age.emsum<-summarySE(wide.cl, measurevar = "day.age.em",
                           groupvars = c("temp", "treatment"),
                           na.rm = TRUE)
day.age.emsum



mg4.sum<-summarySE(wide.cl, measurevar = "mass.gain.4",
                     groupvars = c("temp", "treatment"),
                     na.rm = TRUE)
mg4.sum


mg5.sum<-summarySE(wide.cl, measurevar = "mass.gain.5",
                   groupvars = c("temp", "treatment"),
                   na.rm = TRUE)
mg5.sum


mgwand.sum<-summarySE(wide.cl, measurevar = "mass.gain.wan",
                     groupvars = c("temp", "treatment"),
                     na.rm = TRUE)
mgwand.sum



mgem.sum<-summarySE(wide.cl, measurevar = "mass.gain.em",
                      groupvars = c("temp", "treatment"),
                      na.rm = TRUE)
mgem.sum





#----------------------------

#Calculating range, mean, SD and SE for parasitoid load and emergence

wide.para<-subset(wide.cl, treatment=="para" & num.ovp<3)


#parasitoid load range of each temp treatment

wide.para$percem<-wide.para$num.em/wide.para$load

load.20<-subset(wide.para, temp=="20")
range(load.20$load)

load.25<-subset(wide.para, temp=="25")
range(load.25$load)

load.30<-subset(wide.para, temp=="30")
range(load.30$load)


#number emerged range of each temp

range(load.20$num.em)
range(load.25$num.em)
range(load.30$num.em)



#% emerged range of each temp

range(load.20$percem)
range(load.25$percem)
range(load.30$percem)


#mean, SD and SE of parastioid load

load.sum<-summarySE(wide.para, measurevar = "load",
                    groupvars = "temp",
                    na.rm = TRUE)
load.sum


#mean, SD and SE of number emerged

numem.sum<-summarySE(wide.para, measurevar = "num.em",
                     groupvars = "temp",
                     na.rm = TRUE)
numem.sum


#mean, SD and sE of percent emerged

percem.sum<-summarySE(wide.para, measurevar = "percem",
                      groupvars = "temp",
                      na.rm = TRUE)
percem.sum




#Plotting percent emergned for supplemental figure

percem.plot<-ggplot(wide.para, aes(x=load, y=percem,color=temp, linetype=temp))
percem.plot+geom_point(aes(shape=temp),
                       size=3
)+geom_smooth(method=lm, se=FALSE,
              size=1.2
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"),name="Temperature [C]",breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_manual(values=c("solid","dotted","longdash"),name="Temperature [C]",breaks=c("20","25","30"),
                        labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+labs(x="Total load",y="Percent of emerged larvae"
)+scale_shape_manual(name="Temperature [C]",breaks=c("20","25","30"),labels=c("20","25","30"),
                     values=c(16,17,15)
)+theme(plot.title=element_text(face="bold",vjust=1),
        axis.line.x=element_line(colour = 'black', size = 1.2),
        axis.line.y=element_line(colour = 'black', size = 1.2),
        axis.ticks = element_line(colour = 'black', size = 1.2),
        axis.ticks.length = unit(2.5, "mm"),
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 14,face = "bold"),
        axis.title.x = element_text(size = 16, vjust=-.35,face = "bold",
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 16, vjust=1.5,face = "bold",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)))




#--------------------

#Calculate the survival, death, wander and WOWE for cpt treatments

#create a class column

wide$suc.ovp[is.na(wide$suc.ovp)]<-0
wide$load[is.na(wide$load)]<-0
wide$date.wander[is.na(wide$date.wander)]<-0
wide$date.em[is.na(wide$date.em)]<-0
wide$date.died[is.na(wide$date.died)]<-0
wide$date.5[is.na(wide$date.5)]<-0

#cleaning some erroneous data

#removing some sacrificed individuals that got skipped during cleaning
wide[2, 11]<-1
wide[217, 11]<-1
wide<-subset(wide, sacrifice==0)

#adding mongo column (0s for all except the 2 recorded mongos in the data set)
wide$mongo<-0
wide[77, 895]<-1
wide[87, 895]<-1

wide$class<-ifelse(wide$date.wander>0, "wander",
                   ifelse(wide$date.em>0, "emerge",
                          ifelse(wide$date.died>0, "dead",
                          ifelse(wide$mongo==1, "mongo", "unk"))))



#count the number of each class in each treatment
wide_outcome<-wide %>% dplyr::count(temp, treatment, class)
View(wide_outcome)


#put into wide format
wide_outcome<-wide_outcome %>% tidyr::spread(class, n, fill=0)

#count the total sample size in each treatment
wide_tot.n<-wide %>% dplyr::count(temp, treatment)
View(wide_tot.n)

#add total sample size to outcome data frame
wide_outcome$tot.n<-wide_tot.n$n


#calculate the proportion of emergence, wanderers and mongos
wide_outcome$prop.wand<-wide_outcome$wander/wide_outcome$tot.n
wide_outcome$prop.emerge<-wide_outcome$emerge/wide_outcome$tot.n
wide_outcome$prop.mongo<-wide_outcome$mongo/wide_outcome$tot.n

#calculate effective sample size
con_effn<-(wide_outcome$tot.n - (wide_outcome$mongo + wide_outcome$dead))  
para_effn<-(wide_outcome$tot.n - (wide_outcome$mongo + wide_outcome$dead + wide_outcome$wander))

wide_outcome$eff_n <- ifelse(wide_outcome$treatment=="control", con_effn,
                             ifelse(wide_outcome$treatment=="para", para_effn, 0))


