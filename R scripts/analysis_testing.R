library(Rmisc)
library(dplyr)
library(scales)
library(tidyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(readr)
library(nlme)
library(lme4)
library(car)


long <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt.gr.long.load.csv", 
                 col_types = cols(temp = col_factor(levels = c("20", 
                 "25", "30"))))

long$log.mT0<-log(long$mass.T0)
long$log.mass<-log(long$mass)
long$log.cnsmp<-log(long$tot.cnsmp)


wide <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt gr wide.csv", 
                 col_types = cols(temp = col_factor(levels = c("20", 
                 "25", "30")), treatment = col_factor(levels = c("control", 
                 "para"))))

wide.cl<-subset(wide,died==0)

ml.plot<-ggplot(wide,aes(x=load,y=mass.befem,group=temp,color=temp))
ml.plot+geom_point()+geom_smooth(se=FALSE,method="lm")



#Average age, consumption and mass gain at each instar:

mg4.sum<-summarySE(wide, measurevar = "mass.gain.4",
                   groupvars = c("treatment","temp"),
                   na.rm=TRUE)
mg4.sum


mg5.sum<-summarySE(wide, measurevar = "mass.gain.5",
                   groupvars = c("treatment","temp"),
                   na.rm=TRUE)
mg5.sum


mgwand.sum<-summarySE(wide, measurevar = "mass.gain.wan",
                      groupvars = c("treatment","temp"),
                      na.rm=TRUE)
mgwand.sum


mgem.sum<-summarySE(wide, measurevar = "mass.gain.em",
                    groupvars = c("treatment","temp"),
                    na.rm=TRUE)
mgem.sum



cnsmp4.sum<-summarySE(wide, measurevar = "cnsmp.4",
                      groupvars = c("treatment","temp"),
                      na.rm=TRUE)
cnsmp4.sum


cnsmp5.sum<-summarySE(wide, measurevar = "cnsmp.5",
                      groupvars = c("treatment","temp"),
                      na.rm=TRUE)
cnsmp5.sum



cnsmpwand.sum<-summarySE(wide, measurevar = "cnsmp.wan",
                                     groupvars = c("treatment","temp"),
                                     na.rm=TRUE)
cnsmpwand.sum



cnsmpem.sum<-summarySE(wide, measurevar = "cnsmp.em",
                                   groupvars = c("treatment","temp"),
                                   na.rm=TRUE)
cnsmpem.sum



age4.sum<-summarySE(wide, measurevar = "age.4",
                      groupvars = c("treatment","temp"),
                      na.rm=TRUE)
age4.sum



age5.sum<-summarySE(wide, measurevar = "age.5",
                    groupvars = c("treatment","temp"),
                    na.rm=TRUE)
age5.sum



agewand.sum<-summarySE(wide, measurevar = "age.wander",
                       groupvars = c("treatment","temp"),
                       na.rm=TRUE)
agewand.sum


ageem.sum<-summarySE(wide, measurevar = "age.em",
                     groupvars = c("treatment","temp"),
                     na.rm=TRUE)
ageem.sum




#Created these BEFORE talking to James
#With intercept removed in random effects
mg.mod<-lme(log.mg~age:temp+age:treatment+age:temp:treatment,random=~-1+age|bug.id,data=long,na.action=na.omit,method='ML')
anova(mg.mod)
summary(mg.mod)

#with intercept removed in fixed in random effects
mg.mod1<-lme(log.mg~age:temp+age:treatment+age:temp:treatment-1,random=~-1+age|bug.id,data=long,na.action=na.omit,method='ML')
anova(mg.mod1)
summary(mg.mod1)


#With intercept removed in fixed effects
mg.mod2<-lme(log.mg~age:temp+age:treatment+age:temp:treatment-1,random=~age|bug.id,data=long,na.action=na.omit,method='ML')
anova(mg.mod2)
summary(mg.mod2)


#Created these AFTER talking to James

#Modelling log mass instead of log mass gain

lm.mod1<-lme(log.mass~age:temp+age:treatment+age:temp:treatment+temp,random=~age|bug.id,data=long,na.action=na.omit,method="ML")
summary(lm.mod1)
anova(lm.mod1)

lm.mod1a<-lme(log.mass~age:temp+age:treatment+age:temp:treatment,random=~age|bug.id,data=long,na.action=na.omit,method="ML")
summary(lm.mod1a)

anova(lm.mod1,lm.mod1a)

#Trying to add predicted fitted values to data frame to plot the out come (??)

long.cut<-drop_na(long,log.mass,age)

long.cut$fit<-predict(lm.mod1)

theme_set(theme_classic())
mod.plot<-ggplot(long.cut,aes(x=age,y=log.mass,group=interaction(bug.id,temp),color=temp))
mod.plot+geom_line(aes(y=fit, lty=treatment), size=0.8
       )+scale_color_manual(values=c("#56B4E9","#009E73","#E69F00"),
                     name="Temperature[C]",
                     breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
       )+scale_linetype_manual(values=c("dashed","solid"),
                        breaks=c("control","para"),
                        labels=c("Control","Parasitized"),
                        name="Treatment")           


#Joel's model idea:

lm.mod1b<-lme(log.mass~poly(age,2)*temp*treatment,random=~age|bug.id,data=long,na.action=na.omit,method="ML")
anova(lm.mod1b)


#plotting predicted outcomes

long.cut$fit2<-predict(lm.mod1b)

mod.plot<-ggplot(long.cut,aes(x=age,y=log.mass,group=interaction(bug.id,temp),color=temp))
mod.plot+geom_line(aes(y=fit2, lty=treatment), size=0.8
       )+scale_color_manual(values=c("#56B4E9","#009E73","#E69F00"),
                            name="Temperature[C]",
                            breaks=c("20","25","30"),
                            labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
       )+scale_linetype_manual(values=c("dashed","solid"),
                               breaks=c("control","para"),
                               labels=c("Control","Parasitized"),
                               name="Treatment")





#looks like my model is plotting a random slope and random intercept, yay!


#To model consumption, need to start at 2nd time point (first measure of cnsmp)
#removed rows with timepoint T0, so that the starting consumption is what was measured at T1 (6 hours)--saved in df lng.cnsmp

View(long)

lng.cnsmp<-subset(long,Timepoint!="T0")


lc.mod1<-lme(log.cnsmp~age:temp+age:treatment+age:temp:treatment+temp,random=~age|bug.id,data=lng.cnsmp,na.action=na.omit,method="ML")
summary(lc.mod1)

lc.mod1a<-lme(log.cnsmp~age:temp+age:treatment+age:temp:treatment,random=~age|bug.id,data=lng.cnsmp,na.action=na.omit,method="ML")
summary(lc.mod1a)

anova(lc.mod1,lc.mod1a) #With +temp is better, models sig diff, use mod1, not mod1a


#plotting model fit:

long.cut1<-drop_na(lng.cnsmp,log.cnsmp,age)

long.cut1$fit<-predict(lc.mod1)


mod.plot2<-ggplot(long.cut1,aes(x=age,y=log.cnsmp,group=interaction(bug.id,temp),color=temp))
mod.plot2+geom_line(aes(y=fit, lty=treatment), size=0.8)



#After consulting with both James and Joel, have decided on one of these models:

lm.mod2<-lme(log.mass~(age+I(age^2)):(temp*treatment)+temp,random=~age|bug.id,
             data=long,na.action=na.exclude,method="ML")
anova(lm.mod2)
summary(lm.mod2)

lm.mod3<-lme(log.mass~(age+I(age^2)):(temp*treatment)+temp,random=~age+I(age^2)|bug.id,data=long,na.action=na.omit,method="ML")

AIC(lm.mod1,lm.mod2,lm.mod3)

VarCorr(lm.mod2)
VarCorr(lm.mod3)


#Plotting model fits

long.cut$fit3<-predict(lm.mod2)

mod.plot3<-ggplot(long.cut,aes(x=age,y=log.mass,group=interaction(bug.id,temp),color=temp))
mod.plot3+geom_line(aes(y=fit3,lty=treatment))



#Testing lm.mod2 without the age^2:treat term

lm.mod2a<-lme(log.mass~temp+age:temp+age:treatment+I(age^2):temp+age:temp:treatment+I(age^2):temp:treatment,
              random=~age|bug.id,data=long,na.action=na.omit,method="ML")
anova(lm.mod2a)

#Ask Joel how to test these, or if I need to worry about dropping the non sig term




#The model with age as a polynomial in the random effects has the best AIC, but when you look at the correlation between 
#coefficients, this more complicated model has high correlation between age and age^2 (??)--makes interpretation
#super complicated

#Instead, will use the model with random effect of age as a single order term--the fixed effect of age is still a 
#polynomial



#Creating a model for consumption that matches the final mass model (lm.mod2)

#To model consumption, need to start at 2nd time point (first measure of cnsmp)
#removed rows with timepoint T0, so that the starting consumption is what was measured at T1 (6 hours)--saved in df lng.cnsmp

View(long)

lng.cnsmp<-subset(long,Timepoint!="T0")

lc.mod2<-lme(log.cnsmp~(age+I(age^2)):(temp*treatment)+temp,random=~age|bug.id,data=lng.cnsmp,
             na.action=na.omit,method="ML")
anova(lc.mod2)
summary(lc.mod2)

#Testing the more complicated model as well

lc.mod3<-lme(log.cnsmp~(age+I(age^2)):(temp*treatment)+temp,random=~age+I(age^2)|bug.id,data=lng.cnsmp,
             na.action=na.omit,method="ML")


AIC(lc.mod1,lc.mod2,lc.mod3)

VarCorr(lc.mod2) #Age and intercept are more highly correlated here than in mass--discuss with Joel
VarCorr(lc.mod3) #Age and intercept are more highly correlated than in lc.mod2; age and age^2 also very highly correlated




#Wasps

para<-subset(wide.cl,treatment=="para" & suc.ovp=="1" & load!="0" & num.ovp<3)



#survival to emergence
#With load as polynomial
w.mod1<- glmer(cbind(num.em,num.unem)~(1|bug.id)+temp*poly(load,2),family=binomial,data=para)
anova(w.mod1)
summary(w.mod1)


w.mod2<- glmer(cbind(num.em,num.unem)~(1|bug.id)+temp*poly(load,2),family=binomial,data=para,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
anova(w.mod2)
summary(w.mod2)

w.moda<- glmer(cbind(num.em,num.unem)~(1|bug.id)+temp,family=binomial,data=para,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


#Models look similar with and without the increase of iterations

anova(w.mod1,w.mod2) #Have same AIC, but have significant p value....



#with load as poly 1

w.mod1.5<-glmer(cbind(num.em,num.unem)~(1|bug.id)+temp*poly(load,1),family=binomial,data=para)
summary(w.mod1.5)

anova(w.mod1.5,w.mod1)



#without load as polynomial--get warning about convergence and rescaling values (large eigenvalue ratio??)
w.mod3<- glmer(cbind(num.unem,num.em)~(1|bug.id)+temp*load,family=binomial,data=para)
anova(w.mod3)
summary(w.mod3)


#Survival to pupation

w.mod4<-glmer(cbind(num.coc,num.fail.spin)~(1|bug.id)+temp*poly(load,2),family=binomial,data=para)
summary(w.mod4)

w.mod4.5<-glmer(cbind(num.coc,num.fail.spin)~(1|bug.id)+temp*poly(load,1),family=binomial,data=para,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(w.mod4.5)

anova(w.mod4,w.mod4.5)


w.mod5<-glmer(cbind(num.coc,num.fail.spin)~(1|bug.id)+temp*poly(load,1),family=binomial,data=para)

anova(w.mod4.5,w.mod5)


#In this case, the models with and without the iteration control have the same AIC and a non significant p value


#Attempting logit transform of survival proportions


wide$tot.elsurv<-wide$num.em/wide$load
wide$tot.llsurv<-wide$num.coc/wide$load




para$logit.el<-logit(para$tot.elsurv,percents=FALSE)


lgt.plot<-ggplot(para,aes(x=load,y=logit.el,group=temp,color=temp))
lgt.plot+geom_point()+geom_smooth(se=FALSE)



#rescaling load to see if it helps the convergence problem in the glmer model

#Question for James--Do I need to worry that my response variable (num.em vs num.unem) isn't on the same scale as my
#rescaled load?

para$resc.ld<-rescale(para$load,to=c(0,1))

load.hist<-ggplot(para,aes(x=resc.ld))
load.hist+geom_histogram(binwidth=.03)

summarySE(para,measurevar = "resc.ld", groupvars = "temp")


w.mod6<- glmer(cbind(num.em,num.unem)~(1|bug.id)+temp*resc.ld,family=binomial,data=para)
anova(w.mod6)
summary(w.mod6)


w.mod6a<-glmer(cbind(num.em,num.unem)~(1|bug.id)+temp+temp:resc.ld,family=binomial,data=para)
anova(w.mod6a)
summary(w.mod6a)

anova(w.mod6,w.mod6a)

#models aren't sig diff, but have the exact same AIC values...

drop1(w.mod6)


#Comapring w.mod6 models with different variables to get p values
  ##not sure if I did this correctly--am I supposed to run the model without the 
  ##variable against the full model?

w.mod6t<-glmer(cbind(num.em,num.unem)~(1|bug.id)+temp,family=binomial,data=para)
w.mod6l<-glmer(cbind(num.em,num.unem)~(1|bug.id)+resc.ld,family=binomial,data=para)
w.mod6i<-glmer(cbind(num.em,num.unem)~(1|bug.id)+temp:resc.ld,family=binomial,data=para)


#for temp
anova(w.mod6,w.mod6t, test="chi")


#for load
anova(w.mod6, w.mod6l, test="chi")


#for interaction
anova(w.mod6,w.mod6i, test="chi")




#models lacking the variable of interest
w.mod6wt<-glmer(cbind(num.em,num.unem)~(1|bug.id)+temp:resc.ld+resc.ld,family=binomial,data=para)
w.mod6wl<-glmer(cbind(num.em,num.unem)~(1|bug.id)+temp+temp:resc.ld,family=binomial,data=para)
w.mod6wi<-glmer(cbind(num.em,num.unem)~(1|bug.id)+temp+resc.ld,family=binomial,data=para)


anova(w.mod6,w.mod6wt, test="chi")
anova(w.mod6, w.mod6wl, test="chi")
anova(w.mod6, w.mod6wi, test="chi")




#Using a quasibinomial glm to see if I have overdispersion (James' suggestion)
#If I do have overdispersion, then would be a good idea to have a random effect. If not, shouldn't need one

wqb.mod1<-glm(cbind(num.unem,num.em)~temp*resc.ld,family=quasibinomial,data=para)
anova(wqb.mod1,test="F")
summary(wqb.mod1)

#Summary says that the dispersion parameter is 9.567--much larger than 5, which is the cut off James told me for overdisp
#So, should probably use a random effect (w.mod6)


#Modeling late larval survival (num.coc vs num.fail.spin) as a quasibinomal to see if there is overdispersion

wqb.mod2<-glm(cbind(num.fail.spin,num.coc)~temp*resc.ld,family=quasibinomial,data=para)
anova(wqb.mod2,test="F")
summary(wqb.mod2) #overdispersion term is large (>5), so should add random effects


#Modelling late larval survival as a glmm

wll.mod1<- glmer(cbind(num.coc,num.fail.spin)~(1|bug.id)+temp*resc.ld,family=binomial,data=para)
anova(wll.mod1)
summary(wll.mod1)



#Attempting to model total survival (should be load-num.coc)

para$tot.died<-para$load-para$num.coc

wqb.mod3<-glm(cbind(tot.died,num.coc)~temp*resc.ld,family=quasibinomial,data=para)
anova(wqb.mod3,test="F")
summary(wqb.mod3) #overdispersion term is large (>5), so should add random effect


wtot.mod1<-glmer(cbind(tot.died,num.coc)~(1|bug.id)+temp*resc.ld,family=binomial,data=para)
summary(wtot.mod1)


wtot.mod1a<-glmer(cbind(tot.died,num.coc)~(1|bug.id)+temp,family=binomial,data=para)
wtot.mod1b<-glmer(cbind(tot.died,num.coc)~(1|bug.id)+resc.ld,family=binomial,data=para)

anova(wtot.mod1,wtot.mod1a,wtot.mod1b)



#Analyzing wasp development time (from oviposition to emergence, since I didn't track to eclosion)
#Should I do in days (int.wasp.dev) or hours (age.em)? Hours is more similar to cat dev, but is it really relevant,
#since I was only checking once a day? It also happens over several hours....maybe try both

#Should this be a linear model, or a linear mixed effects model?? Ask Joel/James


wdev.hist<-ggplot(para,aes(x=age.em))
wdev.hist+geom_histogram()

wdev.hist2<-ggplot(para,aes(x=int.wasp.dev))
wdev.hist2+geom_histogram()





wdev.mod1<-lm(age.em~temp*resc.ld,data=para,na.action=na.omit)
anova(wdev.mod1)
summary(wdev.mod1)

plot(wdev.mod1)


wdev.mod2<-lm(int.wasp.dev~temp*resc.ld,data=para,na.action=na.omit)
anova(wdev.mod2)
summary(wdev.mod2)



wdev.mod3<-lme(age.em~temp*resc.ld,random=~1|bug.id,data=para,na.action=na.omit,method="ML")
anova(wdev.mod3)
summary(wdev.mod3)



wdev.mod3a<-lme(age.em~temp,random=~1|bug.id,data=para,na.action=na.omit,method="ML")
wdev.mod3b<-lme(age.em~resc.ld,random=~1|bug.id,data=para,na.action=na.omit,method="ML")
wdev.mod3c<-lme(age.em~1,random=~1|bug.id,data=para,na.action=na.omit,method="ML")


anova(wdev.mod3,wdev.mod3a,wdev.mod3b,wdev.mod3c) #Model with both terms is best




colnames(wide)


wide.cl$tot.surv<-wide.cl$num.coc/wide.cl$load

  
prop.plot<-ggplot(wide.cl,aes(x=load,y=tot.surv, group=temp, color=temp))
prop.plot+geom_point(
        )+geom_smooth(se=FALSE,method="lm")



#Trying to add load to model of caterpillar mass gain
  ##can only run with subsetted data with only para caterpillars

long$load<-as.numeric(long$load)
para.lng<-subset(long,treatment=="para")


lmload.mod<-lme(log.mass~(age+I(age^2)):(temp*load)+temp,random=~age|bug.id,data=para.lng,na.action=na.omit,method="ML")
anova(lmload.mod)
summary(lmload.mod)


#removing para caterpillars with loads of 0 (unsuc ovp, mongos, sacrifices)
para.lng2<-subset(para.lng,load>0)

lmload.mod2<-lme(log.mass~(age+I(age^2)):(temp*load)+temp,random=~age|bug.id,data=para.lng2,na.action=na.omit,method="ML")
anova(lmload.mod2)



#Modelling consumption for parastized caterpillars by temp and load

cnsmpload.mod<-lme(tot.cnsmp~(age+I(age^2)):(temp*load)+temp,random=~age|bug.id,data=para.lng2,na.action=na.omit,method="ML")
anova(cnsmpload.mod)


para_m<-select(para.lng2, bug.id, temp, age, log.mass, load)
para_m<-na.omit(para_m)

para_m$m_pred<-predict(lmload.mod2, level=0)
para_m$m_resid<-residuals(lmload.mod2, level=0)


mass_load_resid<-ggplot(para_m, aes(x=m_pred, y=m_resid))
mass_load_resid+geom_point(
)+facet_wrap(~temp)























