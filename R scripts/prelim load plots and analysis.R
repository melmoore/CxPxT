
#Testing glm models for cpt data

library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(Rmisc)
library(readr)
library(nlme)
library(lme4)


wide <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt gr wide.csv", 
                 col_types = cols(temp = col_factor(levels = c("20", 
                                                               "25", "30")), treatment = col_factor(levels = c("control", 
                                                                                                               "para"))))
para<-subset(wide,treatment=="para")
para<-subset(para,died=="0")
para<-subset(para,suc.ovp=="1")
para<-para[!is.na(para$instar.em),]
para<-para[!is.na(para$num.unem),]
para<-subset(para,num.ovp<=2)

#Kate's code: mod1<- glmer(cbind(AP,CD)~(1|Mother)+Year+Total,family=binomial,data=WVlate)


mod1<- glmer(cbind(num.unem,num.em)~(1|bug.id)+temp*load,family=binomial,data=para)
anova(mod1)
summary(mod1)


mod1a<- glm(cbind(num.unem,num.em)~temp*poly(load,2),family=binomial,data=para)
summary(mod1a)

mod1b<- glm(cbind(num.unem,num.em)~temp*load,family=binomial,data=para)

anova(mod1a,mod1b,test="Chi")

#Warning about convergence seems to go away when using poly(load)

mod2<- glmer(cbind(num.unem,num.em)~(1|bug.id)+temp*poly(load,2),family=binomial,data=para)
anova(mod2)
summary(mod2)

mod2a<- glmer(cbind(num.unem,num.em)~(1|bug.id)+temp*poly(load,1),family=binomial,data=para)
summary(mod2a)

mod3<- glmer(cbind(num.unem,num.em)~(1|bug.id)+temp+poly(load,2),family=binomial,data=para)
anova(mod3)
summary(mod3)

mod3a<- glmer(cbind(num.unem,num.em)~(1|bug.id)+temp+poly(load,1),family=binomial,data=para)
summary(mod3a)


anova(mod2,mod3,mod2a,mod3a)
anova(mod1,mod2)


para$fit<-predict(mod2a)

load.plot<-ggplot(para,aes(y=num.em,x=load,color=temp))
load.plot+geom_point()+geom_line(aes(y=fit)) #getting closer, but the lines are all the way at the bottom of the 
#graph--does this mean my model doesn't fit very well?


theme_set(theme_classic())
load.plot<-ggplot(para,aes(y=num.em,x=load,color=temp,linetype=temp))
load.plot+geom_point()+geom_smooth(method="lm",se=FALSE
        )+scale_color_manual(values=c("#56B4E9","#009E73","#E69F00"),name="Temperature [C]",breaks=c("20","25","30"),
                             labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
        )+scale_linetype_manual(values=c("solid","dotted","longdash"),name="Temperature [C]",breaks=c("20","25","30"),
                                labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
        )+labs(title="Effects of temperature and load on wasp larvae emergence",x="Total load",
                 y="Number emerged"
        )+theme(axis.title.y=element_text(vjust=1.5),
                axis.title.x=element_text(vjust=-.35),
                plot.title=element_text(face="bold",vjust=1))


load.plot<-ggplot(para,aes(y=num.em,x=load,color=temp,linetype=temp))
load.plot+geom_point()+geom_smooth(se=FALSE
)+scale_color_manual(values=c("#56B4E9","#009E73","#E69F00"),name="Temperature [C]",breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_manual(values=c("solid","dotted","longdash"),name="Temperature [C]",breaks=c("20","25","30"),
                        labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+labs(title="Effects of temperature and load on wasp larvae emergence",x="Total load",
       y="Number emerged"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1))



#Plotting "survivor" curve of wasp life stages.

#Making standardized wasp columns (standardized by dividing by load)

para$stnd.load<-(para$load/para$load)
para$stnd.em<-(para$num.em/para$load)
para$stnd.coc<-(para$num.coc/para$load)


#Making long format for plotting

keepvars<-c("bug.id","temp","stnd.load","stnd.em","stnd.coc")
para.sub<-para[keepvars]

keycol<-"stage"
valuecol<-"surv"
gathercols<-c("stnd.load","stnd.em","stnd.coc")

para.lng<-gather_(para.sub,keycol,valuecol,gathercols)
para.lng$stage<-factor(para.lng$stage,levels=c("stnd.load","stnd.em","stnd.coc"))

stage.plot<-ggplot(para.lng,aes(x=stage,y=surv,group=temp,color=temp,linetype=temp))
stage.plot+geom_point(aes(shape=temp))+geom_jitter(aes(shape=temp))+geom_smooth(se=FALSE,
         )+scale_color_manual(values=c("#56B4E9","#009E73","#E69F00"),name="Temperature [C]",breaks=c("20","25","30"),
                                 labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
         )+scale_linetype_manual(values=c("solid","dotted","longdash"),name="Temperature [C]",breaks=c("20","25","30"),
                                 labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
         )+scale_shape_manual(name="Temperature [C]",breaks=c("20","25","30"),labels=c("20","25","30"),
                              values=c(16,17,15),guide=guide_legend(keywidth=1.8)
         )+labs(title="Wasp survival at larval and pupal stages",x="Developmental stage",
                 y="% survival"
         )+theme(axis.title.y=element_text(vjust=1.5),
                  axis.title.x=element_text(vjust=-.35),
                  plot.title=element_text(face="bold",vjust=1))


mod4<-glmer(cbind(num.coc,num.fail.spin)~(1|bug.id)+temp*poly(load),family=binomial,data=para)
anova(mod4)
summary(mod4)

mod4a<-glmer(cbind(num.coc,num.fail.spin)~(1|bug.id)+temp*poly(load,1),family=binomial,data=para)
anova(mod4a)
summary(mod4a)

mod4b<-glmer(cbind(num.coc,num.fail.spin)~(1|bug.id)+temp+poly(load,1),family=binomial,data=para)
anova(mod4b)
summary(mod4b)


mod4c<-glmer(cbind(num.coc,num.fail.spin)~(1|bug.id)+temp,family=binomial,data=para)
anova(mod4c)
summary(mod4c)

anova(mod4a,mod4b,mod4c)


mod4d<-glm(cbind(num.coc,num.fail.spin)~temp*load,family=binomial,data=para)
summary(mod4d)

coc.plot<-ggplot(para,aes(y=num.coc,x=load,color=temp))
coc.plot+geom_point()+geom_smooth(method="lm",se=FALSE)


coc2.plot<-ggplot(para,aes(y=num.coc,x=num.em,color=temp))
coc2.plot+geom_point()+geom_smooth(method="lm",se=FALSE)



mod5<-glmer(cbind(num.coc,num.fail.spin)~(1|bug.id)+temp*num.em,family=binomial,data=para)


mod5a<-glm(cbind(num.coc,num.fail.spin)~temp*num.em,family=binomial,data=para)
summary(mod5a)




mod6<-lme(int.wasp.dev~temp*load,random=~1|bug.id,data=para,method="ML",na.action=na.omit)
summary(mod6)
anova(mod6)

mod6a<-lme(int.wasp.dev~temp+load,random=~1|bug.id,data=para,method="ML",na.action=na.omit)
summary(mod6a)

anova(mod6,mod6a)

loaddev.plot<-ggplot(para,aes(x=load,y=int.wasp.dev,group=temp,color=temp,linetype=temp))
loaddev.plot+geom_point(aes(shape=temp))+geom_smooth(se=FALSE,method="lm"
           )+scale_color_manual(values=c("#56B4E9","#009E73","#E69F00"),name="Temperature [C]",breaks=c("20","25","30"),
                                 labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
           )+scale_linetype_manual(values=c("solid","dotted","longdash"),name="Temperature [C]",breaks=c("20","25","30"),
                                   labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
           )+scale_shape_manual(name="Temperature [C]",breaks=c("20","25","30"),labels=c("20","25","30"),
                                values=c(16,17,15),guide=guide_legend(keywidth=1.8)
           )+labs(title="Effect of load and temperature on internal wasp development",x="Load",
                  y="Development time [days]"
           )+theme(axis.title.y=element_text(vjust=1.5),
                   axis.title.x=element_text(vjust=-.35),
                   plot.title=element_text(face="bold",vjust=1))



mass.plot<-ggplot(para,aes(x=load,y=mass.befem,group=temp,color=temp))
mass.plot+geom_point()+geom_smooth(se=FALSE,method="lm")













