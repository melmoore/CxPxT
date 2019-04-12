#Testing GAMM and non-linear models for CxPxT--MASS ONLY

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
library(car)


#------------------------

#load data
cpt <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt.gr.long.load.csv", 
                col_types = cols(temp = col_factor(levels = c("20", 
                                                              "25", "30")), treatment = col_factor(levels = c("control", 
                                                                                                              "para"))))

#make some data adjustments
cpt$log.mT0<-log(cpt$mass.T0)
cpt$log.mass<-log(cpt$mass)
cpt$day.age<-(cpt$age/24)
View(cpt)

#data cleaning--remove dead individuals and WOWEs
cpt.cl<-subset(cpt,died=="0")
cpt.cl$load[is.na(cpt.cl$load)]<-0
cpt.cl$load<-ifelse(cpt.cl$treatment=="para" & cpt.cl$load==0, 1.5, cpt.cl$load)
cpt.cl<-subset(cpt.cl, load!=1.5)
cpt.cl$load[cpt.cl$load==0]<-NA


#functions to try:
#nls() and SSlogis()


#---------------------

#STUFF THAT WORKS

#subset to only columns in model, and remove rows with NAs
cpt.sub<-select(cpt.cl, bug.id, temp, treatment, log.mass, age)
cpt.sub<-na.omit(cpt.sub)

#making a grouped data object
cpt.grp<-groupedData(log.mass ~ temp|treatment, data=cpt.sub)


#NLME MODEL USING SSLOGIS (Logistic funtion)

#initVals<-getInitial(log.mass ~ SSlogis(age, Asym, xmid, scal), data=cpt.grp)

initVals<-c(8.2,0,0,0,0,0,4.2,0,0,0,0,0,115.6,0,0,0,0,0)

testmod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
              data=cpt.grp,
              fixed=Asym + xmid + scal ~ temp*treatment,
              random=Asym ~ 1,
              start=initVals)

summary(testmod)
anova(testmod)
coef(testmod)

#check on how the predict funtion is working with my model (line is squiggly with treatment)
cpt.grp$pred3<-predict(testmod, level=0)
cpt.grp$resid3<-residuals(testmod, level=0)


fit.plot3<-ggplot(cpt.grp, aes(x=age, y=log.mass, group=interaction(temp, treatment), color=treatment))
fit.plot3+geom_point(
)+geom_line(aes(y=pred3),
            size=3
)+facet_wrap(~temp)

fitres.plot3<-ggplot(cpt.grp, aes(x=pred3, y=resid3, color=treatment))
fitres.plot3+geom_point(
)+geom_hline(aes(yintercept=0),
             size=1)

resage.plot3<-ggplot(cpt.grp, aes(x=age, y=resid3, color=treatment))
resage.plot3+geom_point(
)+geom_hline(aes(yintercept=0),
             size=1.2)


#drop all interactions and compare the 2 models with an anova
#maybe try using other function than SSlogis
##maybe try michaelis menten fnct


#tried running model with random effect of individual for all parameters, got lots of warnings about
  ##non convergence and false convergence. Talk to Joel about what the most relevant parameter would
  ##be to have a random effect

#model runs without error for Asym alone, xmid alone, scal alone, xmid+Asym, and scal+Asym
  ##check to see if order matters?
  ##according to nlme() help page, random defaults to all fixed effects also having random effects?

#run the full model with all fixed effects and interactions, random effect of individual
  ##lots of warnings
logis_full_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                     data=cpt.grp,
                     fixed=Asym + xmid + scal ~ temp*treatment,
                     random=Asym + xmid + scal ~ 1|bug.id,
                     start=initVals)


#---------------------------


#testing the model with random effect of individual for different parameters

#run the full model with all fixed effects and interactions, random effect of individual on scal
lf_scal_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                     data=cpt.grp,
                     fixed=Asym + xmid + scal ~ temp*treatment,
                     random=scal ~ 1|bug.id,
                     start=initVals)

summary(lf_scal_mod)
anova(lf_scal_mod)
coef(lf_scal_mod)

#make a new cpt data frame for plotting model testing results
cpt_mt<-cpt.grp

#add predicted values of the fixed effects (level 0)
cpt_mt$pred_scal_fix<-predict(lf_scal_mod, level = 0)
cpt_mt$fit_scal<-fitted(lf_scal_mod, level=0)


#plot predicted fixed effects only (should be applicable to all models, since only varying RE atm)
fit_scal_plot<-ggplot(cpt_mt, aes(x=age, y=log.mass, color=treatment))
fit_scal_plot+geom_point(
)+geom_line(aes(y=pred_scal_fix, group=treatment),
            size=3
)+facet_wrap(~temp)


#plot random effect of individual on scal (plot temps individually)

#Make a new data frame to add predicted values of random effects to
cpt_newdat<-select(cpt_mt, bug.id, temp, treatment, age)
cpt_newdat$pred_r<-predict(lf_scal_mod, cpt_newdat) #predict values from random effects

#20
predr20_plot<-ggplot(subset(cpt_mt, temp==20), aes(x=age, y=log.mass))
predr20_plot+geom_point(
)+geom_line(data=subset(cpt_newdat, temp==20), aes(x=age, y=pred_r, color=treatment, group=interaction(bug.id, treatment)),
            size=1.5, alpha=.7
)+facet_wrap(~treatment)

#25
predr25_plot<-ggplot(subset(cpt_mt, temp==25), aes(x=age, y=log.mass))
predr25_plot+geom_point(
)+geom_line(data=subset(cpt_newdat, temp==25), aes(x=age, y=pred_r, color=treatment, group=interaction(bug.id, treatment)),
            size=1.5, alpha=.7
)+facet_wrap(~treatment)

#30
predr30_plot<-ggplot(subset(cpt_mt, temp==30), aes(x=age, y=log.mass))
predr30_plot+geom_point(
)+geom_line(data=subset(cpt_newdat, temp==30), aes(x=age, y=pred_r, color=treatment, group=interaction(bug.id, treatment)),
            size=1.5, alpha=.7
)+facet_wrap(~treatment)



#run the full model with all fixed effects and interactions, random effect of individual on xmid
lf_xmid_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                  data=cpt.grp,
                  fixed=Asym + xmid + scal ~ temp*treatment,
                  random=xmid ~ 1|bug.id,
                  start=initVals)

summary(lf_xmid_mod)
anova(lf_xmid_mod)
coef(lf_xmid_mod)


#Make a new data frame to add predicted values of random effects to
cpt_newdat2<-select(cpt_mt, bug.id, temp, treatment, age)
cpt_newdat2$pred_r<-predict(lf_xmid_mod, cpt_newdat2) #predict values from random effects


#plot the random effects of individual on xmid

#20
predr20_xmid_plot<-ggplot(subset(cpt_mt, temp==20), aes(x=age, y=log.mass))
predr20_xmid_plot+geom_point(
)+geom_line(data=subset(cpt_newdat2, temp==20), 
            aes(x=age, y=pred_r, color=treatment, group=interaction(bug.id, treatment)),
            size=1.5, alpha=.7
)+facet_wrap(~treatment)


#25
predr25_xmid_plot<-ggplot(subset(cpt_mt, temp==25), aes(x=age, y=log.mass))
predr25_xmid_plot+geom_point(
)+geom_line(data=subset(cpt_newdat2, temp==25), 
            aes(x=age, y=pred_r, color=treatment, group=interaction(bug.id, treatment)),
            size=1.5, alpha=.7
)+facet_wrap(~treatment)


#30
predr30_xmid_plot<-ggplot(subset(cpt_mt, temp==30), aes(x=age, y=log.mass))
predr30_xmid_plot+geom_point(
)+geom_line(data=subset(cpt_newdat2, temp==30), 
            aes(x=age, y=pred_r, color=treatment, group=interaction(bug.id, treatment)),
            size=1.5, alpha=.7
)+facet_wrap(~treatment)




#run the full model with all fixed effects and interactions, random effect of individual on Asym
lf_Asym_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                  data=cpt.grp,
                  fixed=Asym + xmid + scal ~ temp*treatment,
                  random=Asym ~ 1|bug.id,
                  start=initVals)

summary(lf_Asym_mod)
anova(lf_Asym_mod)
coef(lf_Asym_mod)


#Make a new data frame to add predicted values of random effects to
cpt_newdat3<-select(cpt_mt, bug.id, temp, treatment, age)
cpt_newdat3$pred_r<-predict(lf_Asym_mod, cpt_newdat3) #predict values from random effects


#plot the random effects of individual on Asym

#20
predr20_Asym_plot<-ggplot(subset(cpt_mt, temp==20), aes(x=age, y=log.mass))
predr20_Asym_plot+geom_point(
)+geom_line(data=subset(cpt_newdat3, temp==20), 
            aes(x=age, y=pred_r, color=treatment, group=interaction(bug.id, treatment)),
            size=1.5, alpha=.7
)+facet_wrap(~treatment)



#run the full model with all fixed effects and interactions, random effect of individual on scal and Asym
lf_as_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                  data=cpt.grp,
                  fixed=Asym + xmid + scal ~ temp*treatment,
                  random=scal + Asym ~ 1|bug.id,
                  start=initVals)

summary(lf_as_mod)
anova(lf_as_mod)
coef(lf_as_mod)



#Make a new data frame to add predicted values of random effects to
cpt_newdat4<-select(cpt_mt, bug.id, temp, treatment, age)
cpt_newdat4$pred_r<-predict(lf_as_mod, cpt_newdat4) #predict values from random effects


#plot the random effects of individual on Asym and scal--maybe best so far?

#20
predr20_as_plot<-ggplot(subset(cpt_mt, temp==20), aes(x=age, y=log.mass))
predr20_as_plot+geom_point(
)+geom_line(data=subset(cpt_newdat4, temp==20), 
            aes(x=age, y=pred_r, color=treatment, group=interaction(bug.id, treatment)),
            size=1.5, alpha=.7
)+facet_wrap(~treatment)


#25
predr25_as_plot<-ggplot(subset(cpt_mt, temp==25), aes(x=age, y=log.mass))
predr25_as_plot+geom_point(
)+geom_line(data=subset(cpt_newdat4, temp==25), 
            aes(x=age, y=pred_r, color=treatment, group=interaction(bug.id, treatment)),
            size=1.5, alpha=.7
)+facet_wrap(~treatment)




#run the full model with all fixed effects and interactions, random effect of individual on scal and xmid
  ##does not run without opt="nlm"--but not sure what this does
lf_xs_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                data=cpt.grp,
                fixed=Asym + xmid + scal ~ temp*treatment,
                random=xmid + scal ~ 1|bug.id,
                start=initVals,
                control=nlmeControl(opt="nlm"))


#Make a new data frame to add predicted values of random effects to
cpt_newdat5<-select(cpt_mt, bug.id, temp, treatment, age)
cpt_newdat5$pred_r<-predict(lf_xs_mod, cpt_newdat5) #predict values from random effects

#plot the random effects of individual on xmid and scal

#20
predr20_xs_plot<-ggplot(subset(cpt_mt, temp==20), aes(x=age, y=log.mass))
predr20_xs_plot+geom_point(
)+geom_line(data=subset(cpt_newdat5, temp==20), 
            aes(x=age, y=pred_r, color=treatment, group=interaction(bug.id, treatment)),
            size=1.5, alpha=.7
)+facet_wrap(~treatment)


#plot the random effects of individual on xmid and scal

#25
predr25_xs_plot<-ggplot(subset(cpt_mt, temp==25), aes(x=age, y=log.mass))
predr25_xs_plot+geom_point(
)+geom_line(data=subset(cpt_newdat5, temp==25), 
            aes(x=age, y=pred_r, color=treatment, group=interaction(bug.id, treatment)),
            size=1.5, alpha=.7
)+facet_wrap(~treatment)




#run the full model with all fixed effects and interactions, random effect of individual on xmid and Asym
lf_xa_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                data=cpt.grp,
                fixed=Asym + xmid + scal ~ temp*treatment,
                random=xmid + Asym ~ 1|bug.id,
                start=initVals)

#Make a new data frame to add predicted values of random effects to
cpt_newdat6<-select(cpt_mt, bug.id, temp, treatment, age)
cpt_newdat6$pred_r<-predict(lf_xa_mod, cpt_newdat6) #predict values from random effects

#plot the random effects of individual on xmid and scal

#20
predr20_xa_plot<-ggplot(subset(cpt_mt, temp==20), aes(x=age, y=log.mass))
predr20_xa_plot+geom_point(
)+geom_line(data=subset(cpt_newdat6, temp==20), 
            aes(x=age, y=pred_r, color=treatment, group=interaction(bug.id, treatment)),
            size=1.5, alpha=.7
)+facet_wrap(~treatment)



#run the full model with all fixed effects and interactions, random effect of individual on xmid and Asym
#does not like, probably too complex with all the random effects
lf_axs_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                data=cpt.grp,
                fixed=Asym + xmid + scal ~ temp*treatment,
                random=scal + Asym + xmid ~ 1|bug.id,
                start=initVals,
                control=nlmeControl(MaxIter = 100000))


#-----------------------
#model testing against null model with no interactions, full FE model with no random effects, 
  ## and various full models with random effects

initVals_null<-c(8.2,0,0,0,4.2,0,0,0,115.6,0,0,0)

lf_nullRE_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                  data=cpt.grp,
                  fixed=Asym + xmid + scal ~ temp+treatment,
                  random=Asym ~ 1,
                  start=initVals_null)

lf_nullRE_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                    data=cpt.grp,
                    fixed=Asym + xmid + scal ~ temp+treatment,
                    random=Asym ~ 1,
                    start=initVals_null)



lf_nullRE_int_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                    data=cpt.grp,
                    fixed=Asym + xmid + scal ~ temp*treatment,
                    random= Asym ~ 1,
                    start=initVals)



anova(lf_nullRE_mod, testmod) #with no RE, testmod with interactions is better than nullmod without

anova(lf_nullRE_mod, lf_Asym_mod)
anova(lf_nullRE_mod, lf_scal_mod)
anova(lf_nullRE_mod, lf_xmid_mod)
anova(lf_nullRE_mod, lf_as_mod)
anova(lf_nullRE_mod, lf_xa_mod)
anova(lf_nullRE_mod, lf_xs_mod)

anova(lf_nullRE_int_mod, lf_xs_mod)
anova(testmod, lf_xs_mod)

#looks like xmid and scal RE model is best
anova(lf_nullRE_int_mod, lf_Asym_mod, lf_scal_mod, lf_xmid_mod, lf_as_mod, lf_xa_mod, lf_xs_mod)


pred_1<-predict(testmod, level=0:1)

#this null model for RE of xmid + scal without interactions can't converge....
lf_null_xs_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                    data=cpt.grp,
                    fixed=Asym + xmid + scal ~ temp+treatment,
                    random=xmid + scal ~ 1|bug.id,
                    start=initVals_null,
                    control=nlmeControl(opt="nlm", msMaxIter = 10000))




#plot residuals for xmid scal RE model
cpt_newdat5$resid_r<-residuals(lf_xs_mod)
cpt_newdat5$pred_f<-predict(lf_xs_mod, level=0)
cpt_newdat5$resid_f<-residuals(lf_xs_mod, level=0)

fitrest_xs_plot<-ggplot(cpt_newdat5, aes(x=pred_r, y=resid_r, color=treatment))
fitrest_xs_plot+geom_point(
)+geom_hline(aes(yintercept=0), color="black", size=1)

fitres_f_xs_plot<-ggplot(cpt_newdat5, aes(x=pred_f, y=resid_f, color=treatment))
fitres_f_xs_plot+geom_point(
)+geom_hline(aes(yintercept=0), color="black", size=1)



#looking at residuals for asym scal model

cpt_newdat4$resid_r<-residuals(lf_as_mod)
cpt_newdat4$pred_f<-predict(lf_as_mod, level=0)
cpt_newdat4$resid_f<-residuals(lf_as_mod, level=0)


fitrest_as_plot<-ggplot(cpt_newdat4, aes(x=pred_r, y=resid_r, color=treatment))
fitrest_as_plot+geom_point(
)+geom_hline(aes(yintercept=0), color="black", size=1)

fitres_f_as_plot<-ggplot(cpt_newdat4, aes(x=pred_f, y=resid_f, color=treatment))
fitres_f_as_plot+geom_point(
)+geom_hline(aes(yintercept=0), color="black", size=1)



#looking at residuals for asym model
cpt_newdat3$resid_r<-residuals(lf_Asym_mod)
cpt_newdat3$pred_f<-predict(lf_Asym_mod, level=0)
cpt_newdat3$resid_f<-residuals(lf_Asym_mod, level=0)

fitrest_asym_plot<-ggplot(cpt_newdat3, aes(x=pred_r, y=resid_r, color=treatment))
fitrest_asym_plot+geom_point(
)+geom_hline(aes(yintercept=0), color="black", size=1)

fitres_f_asym_plot<-ggplot(cpt_newdat3, aes(x=pred_f, y=resid_f, color=treatment))
fitres_f_asym_plot+geom_point(
)+geom_hline(aes(yintercept=0), color="black", size=1)

ageres_plot<-ggplot(cpt_newdat3, aes(x=age, y=resid_f, color=treatment))
ageres_plot+geom_point(
)+facet_wrap(~temp)


ageres_plot2<-ggplot(cpt_newdat5, aes(x=age, y=resid_f, color=treatment))
ageres_plot2+geom_point(
)+facet_wrap(~temp)

#-------------------

#Other non linear model testing


head(Loblolly) # groupedData  w/  'Seed' is grouping variable :
## Grouped Data: height ~ age | Seed
##    height age Seed
## 1    4.51   3  301
## 15  10.89   5  301
## ..  .....   .  ...

fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),  data = Loblolly,
            fixed = Asym + R0 + lrc ~ 1,
            random = Asym ~ 1, ## <---grouping--->  Asym ~ 1 | Seed
            start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
fm1

age. <- seq(from = 2, to = 30, by = 2)
newLL.301 <- data.frame(age = age., Seed = 301)
newLL.329 <- data.frame(age = age., Seed = 329)
(p301 <- predict(fm1, newLL.301, level = 0:1))
(p329 <- predict(fm1, newLL.329, level = 0:1))
## Prediction are the same at level 0 :
all.equal(p301[,"predict.fixed"],
          p329[,"predict.fixed"])
## and differ by the 'Seed' effect at level 1 :
p301[,"predict.Seed"] -
  p329[,"predict.Seed"]




#MODEL USING SSMICMEN (Michaelis-Menten function)

cpt.grp$age2<-ifelse(cpt.grp$age==0, 0.01, cpt.grp$age)


#Had to fudge so that age has no 0 values (changed to 0.01)
initVals_mcmn<-getInitial(log.mass ~ SSmicmen(age2, Vm, K), data=cpt.grp)

#using the initial values I got with getInitial and SSlogis, just removing the xmid parameter
initVals_mcmn2<-c(7.97,0,0,0,0,0,27.31,0,0,0,0,0)

#no random effect atm
testmod_mcmn<-nlme(log.mass ~ SSmicmen(age, Vm, K),
              data=cpt.grp,
              fixed=Vm + K ~ temp*treatment,
              random=Vm ~ 1,
              start=initVals_mcmn2)

anova(testmod_mcmn)
summary(testmod_mcmn)


cpt.grp$mcmn_pred<-predict(testmod_mcmn)
cpt.grp$mcmn_resid<-residuals(testmod_mcmn)


mcmn_fit_plot<-ggplot(cpt.grp, aes(x=age, y=log.mass, color=treatment))
mcmn_fit_plot+geom_point(
)+geom_line(aes(y=mcmn_pred),
            size=3
)+facet_wrap(~temp)


mcmn_fitres_plot<-ggplot(cpt.grp, aes(x=mcmn_pred, y=mcmn_resid, color=treatment))
mcmn_fitres_plot+geom_point()



#trying to rescale data so that it starts at 0

cpt.grp$resc_lm<-rescale(cpt.grp$log.mass, to=c(0,1))

#get initial starting values
initVals_mcmn_resc<-getInitial(resc_lm ~ SSmicmen(age2, Vm, K), data=cpt.grp)

#using the initial values I got with getInitial and SSlogis, just removing the xmid parameter
initVals_mcmn_resc2<-c(0.9978,0,0,0,0,0,141.805,0,0,0,0,0)

#no random effect atm
testmod_mcmn2<-nlme(resc_lm ~ SSmicmen(age, Vm, K),
                   data=cpt.grp,
                   fixed=Vm + K ~ temp*treatment,
                   random=Vm ~ 1,
                   start=initVals_mcmn_resc2)


anova(testmod_mcmn2) #some diff from not rescaled data, look at coef between models


cpt.grp$mcmn_pred_resc<-predict(testmod_mcmn2)
cpt.grp$mcmn_resid_resc<-residuals(testmod_mcmn2)


mcmn_fit_plot2<-ggplot(cpt.grp, aes(x=age, y=resc_lm, color=treatment))
mcmn_fit_plot2+geom_point(
)+geom_line(aes(y=mcmn_pred_resc),
            size=3
)+facet_wrap(~temp)

mcmn_fitres_plot2<-ggplot(cpt.grp, aes(x=mcmn_pred_resc, y=mcmn_resid_resc, color=treatment))
mcmn_fitres_plot2+geom_point()



#------------------

#STUFF THAT DIDN'T WORK

#trying non linear model with no random effects (nls)

# rN(1-N/K)

f1 <- log.mass ~ r*N*(1-N/K)

n1 <-nls(f1,
         data=cpt.cl,
         start=list(r=0.1, K=10),
         control = list(maxiter = 500),
         trace = TRUE)


#Example: fm1 <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), data = Chick.1)

#This runs!
n2 <- nls(log.mass ~ SSlogis(age, Asym, xmid, scal), data=cpt.cl)
summary(n2)



#Example: logisticModel <- nls(population~K/(1+exp(Po+r*time)), start=list(Po=5, r=2, K=5))

n3 <- nls(log.mass ~ r*N*(1-N/K), 
          start=list(N=0.1, r=1, K=10),
          data=cpt.cl)


#Example: wilson<-nls(mass~phi1/(1+exp(-(phi2+phi3*days.since.birth))),
                      #start=list(phi1=100,phi2=-1.096,phi3=.002),data=data,trace=TRUE)

#coef(lm(logit(mass/100)~days.since.birth,data=data))--estimate some starting values

coef(lm(logit(log.mass/10)~age, data=cpt.cl))

#this works, but has no temperture or parasitization
dogmod<-nls(log.mass ~ phi1/(1+exp(-(phi2+phi3*age))),
            start=list(phi1=10, phi2=-0.1637, phi3=0.0042),
            data=cpt.cl,
            trace = TRUE)

summary(dogmod)


#testing model fit (data lumped together, no temp or para)
dogphi1<-coef(dogmod)[1]
dogphi2<-coef(dogmod)[2]
dogphi3<-coef(dogmod)[3]

x<-c(min(cpt.cl$age, na.rm = TRUE):max(cpt.cl$age, na.rm = TRUE)) #construct a range of x values bounded by the data
y<-phi1/(1+exp(-(phi2+phi3*x))) #predicted mass

predict<-data.frame(x,y) #create the prediction data frame#And add a nice plot (I cheated and added the awesome inset jpg in another program)
ggplot(data=cpt.cl,aes(x=age,y=log.mass))+
  geom_point(color='blue',size=5)+theme_bw()+
  labs(x='Age [hours]',y='Log Mas')+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=24))+
  geom_line(data=predict,aes(x=x,y=y), size=1)



#Example: fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
                      #data = Loblolly,
                      #fixed = Asym + R0 + lrc ~ 1,
                      #random = Asym ~ 1,
                      #start = c(Asym = 103, R0 = -8.5, lrc = -3.3))

#Doesn't work, I think because the nlme doc. says the model needs to have all variables named in the data
  ##frame, and that's not how the nls worked
fm1<-nlme(log.mass ~ phi1/(1+exp(-(phi2+phi3*age))),
          data = cpt.cl,
          fixed = temp + treatment ~ 1,
          start=list(phi1=10, phi2=-0.1637, phi3=0.0042))


#Example: lF<-formula(Abundance~K*n0*exp(r*Time)/(K+n0*(exp(r*Time)-1)) | Treatment)
#fit the model
#(m <- nlsList(lF,data=dat,start=list(K=150,n0=10,r=0.5)))

#subset to only columns in model, and remove rows with NAs
cpt.sub<-select(cpt.cl, bug.id, temp, treatment, log.mass, age)
cpt.sub<-na.omit(cpt.sub)

#This runs!!
lf<-formula(log.mass ~ phi1/(1+exp(-(phi2+phi3*age))) | temp)

lfmod<-nlsList(lf, data=cpt.sub, 
               start=list(phi1=10, phi2=-0.1637, phi3=0.0042),
               na.action = na.omit)
lfmod


#plot predicted and actual values
cpt.sub$pred<-predict(lfmod)

fit.plot<-ggplot(cpt.sub, aes(x=age, y=log.mass, color=temp))
fit.plot+geom_point(
)+geom_line(aes(y=pred),
            size=3)


#looking at residuals against age and against fitted data
cpt.sub$resid<-residuals(lfmod)


fitres.plot<-ggplot(cpt.sub, aes(x=pred, y=resid, color=temp))
fitres.plot+geom_point()

resage.plot<-ggplot(cpt.sub, aes(x=age, y=resid, color=temp))
resage.plot+geom_point()


lf_null<-formula(log.mass ~ phi1/(1+exp(-(phi2+phi3*age))))

lfmod_null<-nls(lf_null, data=cpt.sub,
                start=list(phi1=10, phi2=-0.1637, phi3=0.0042),
                na.action = na.omit)




anova_nlslist(lfmod, lfmod_null)

coef(lm(logit(log.mass/10)~age, data=cpt.sub))


lf2<-formula(log.mass ~ phi1/(1+exp(-(phi2+phi3*age))) | treatment)

lfmod2<-nlsList(lf2, data=cpt.sub,
               start=list(phi1=10, phi2=-0.1637, phi3=0.0042),
               na.action = na.omit)
lfmod2


#plot predicted and actual values
cpt.sub$pred2<-predict(lfmod2)

#this did a weird thing
fit.plot2<-ggplot(cpt.sub, aes(x=age, y=log.mass, color=treatment))
fit.plot2+geom_point(
)+geom_line(aes(y=pred2),
            size=3
)+facet_wrap(~temp)


lmgage.plot<-ggplot(cpt.sub, aes(x=age, y=log.mass, color=treatment))
lmgage.plot+geom_point(
)+facet_wrap(~temp)

cpt.sub$resid2<-residuals(lfmod2)

fitres.plot2<-ggplot(cpt.sub, aes(x=pred2, y=resid2, color=treatment))
fitres.plot2+geom_point(
)+facet_wrap(~temp)

resage.plot2<-ggplot(cpt.sub, aes(x=age, y=resid2, color=treatment))
resage.plot2+geom_point()

#so far, cannot figure out how to run a model with multiple predictors using nlsList


#trying nlme and SSlogis

#example: data <- groupedData(y ~ t | UID, data=data) ## not strictly necessary
#initVals <- getInitial(y ~ SSlogis(t, Asym, xmid, scal), data = data)
#baseModel<- nlme(y ~ SSlogis(t, Asym, xmid, scal),
#                 data = data,
#                 fixed = list(Asym ~ 1, xmid ~ 1, scal ~ 1),
#                 random = Asym + xmid + scal ~ 1|UID,
#                 start = initVals
#)

# nestedModel <- update(baseModel, fixed=list(Asym ~ var1, xmid ~ 1, scal ~ 1), 
#start = c(fixef(baseModel)[1], 0, fixef(baseModel)[2], fixef(baseModel)[3]))

#making a grouped data object
cpt.grp<-groupedData(log.mass ~ temp|treatment, data=cpt.sub)


#initVals<-getInitial(log.mass ~ SSlogis(age, Asym, xmid, scal), data=cpt.grp)

initVals<-c(8.2,0,0,0,0,0,4.2,0,0,0,0,0,115.6,0,0,0,0,0)

testmod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
              data=cpt.grp,
              fixed=Asym + xmid + scal ~ temp*treatment,
              random=Asym ~ 1,
              start=initVals)

anova(testmod)
coef(testmod)

#check on how the predict funtion is working with my model (line is squiggly with treatment)
cpt.grp$pred3<-predict(testmod)
cpt.grp$resid3<-residuals(testmod)


fit.plot3<-ggplot(cpt.grp, aes(x=age, y=log.mass, group=interaction(temp, treatment), color=treatment))
fit.plot3+geom_point(
)+geom_line(aes(y=pred3),
            size=3
)+facet_wrap(~temp)

fitres.plot3<-ggplot(cpt.grp, aes(x=pred3, y=resid3, color=treatment))
fitres.plot3+geom_point()

resage.plot3<-ggplot(cpt.grp, aes(x=age, y=resid3, color=treatment))
resage.plot3+geom_point()


#drop all interactions and compare the 2 models with an anova
#maybe try using other function than SSlogis
  ##maybe try michaelis menten fnct




#----------------------------

#discussing with Geoff:

library(deSolve)

logistic<-function(t, y, param){
  with(as.list(c(y,param)),{
    dN <- r * N * (1 - N/k)
    list(c(dN))
  })
}

#y <- c(N=......)
#param<-c(r=..., k=....)

init<-c(N=100)
param<-c(r=.2, k=300)
times<-seq(from=0, to=100, by=0.01)

out <- lsoda(y = init,
             times = times,
             func = logistic,
             parms = param)
head(out)


#fake data
datatimes<-seq(0, 40, by=4)
datatimes

#select times that are in the time frame we want (r part of curve)
data<-out[which(out[,1] %in% datatimes),]
head(data)

#add some noise to data--looked at each row in column 2, from a random normal dist, take the mean and add 
  ## sd of 5 to make some noisy fake data
data[,2] <- rnorm(n=nrow(data), mean=data[,2], sd=5)
data<-cbind(data, rnorm(n=nrow(data), mean=data[,2], sd=5))

colnames(data)[3:5]<-"N"

head(data)


leastsquares <- function(param, data){
  ndata<-nrow(data)
  stor <- dim(4)
  for(i in 2:5){
  init.temp <- c(data[1, i])
    out.temp <- lsoda(y=init.temp,
                      times=seq(0, data[nrow(data), 1], by=0.1),
                      func=logistic,
                      parms=param)
    out2<-out.temp[which(out.temp[, 1] %in% data[, 1]), ]
    stor[i - 1] <- sum((out2[ , 2] - data[ , i]) ^2)
  }
   return(sum(stor))
}

leastsquares(param=c(r=.4, k=200), data=data)


#optimization

fit<-optim(par = c(r=.4, k=300),
      fn=leastsquares,
      data=data,
      control=list(trace=1))

fit

# $value is the least squared error

#for different temps and para, may have to 

