#Lab meeting nonlinear models 4-9-19


#load libraries
library(Rmisc)
library(scales)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(nlme)
library(cowplot)
library(car)


#-----------------------

#load data
cpt <- read_csv("data files/cpt.gr.long.load.csv", 
                col_types = cols(temp = col_factor(levels = c("20", "25", "30")), 
                                 treatment = col_factor(levels = c("control", "para"))))

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
cpt.cl<-subset(cpt.cl, log.cnsmp>=0)


#---------------------------

#Non-linear mixed effects model using a self starting logistic function

#subset to only columns in model, and remove rows with NAs
cpt.sub<-select(cpt.cl, bug.id, temp, treatment, log.mass, age)
cpt.sub<-na.omit(cpt.sub)

#making a grouped data object
cpt.grp<-groupedData(log.mass ~ temp|treatment, data=cpt.sub)

cpt.grp2<-groupedData(log.mass ~ age|bug.id, data=cpt.sub)

#Null model--no fixed effects or random effects

#get initial values
getInitial(log.mass ~ SSlogis(age, Asym, xmid, scal), data=cpt.grp2)

#make an object with initial values for all parameter/fixed effect combinations (here will only be parameters)
initVals_null<-c(8.2,4.2,115.6)

#Because data is grouped, model is estimating coefficients for treatment, even though it is not specified in model (??)
null_FERE_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                     data=cpt.grp2,
                     fixed=Asym + xmid + scal ~ 1,
                     random=Asym ~ 1,
                     start=initVals_null)

anova(null_FERE_mod)
summary(null_FERE_mod)
coef(null_FERE_mod)

#create a new dataframe to use for plotting fit and residuals
cpt_nullmod<-cpt.grp2

#find predicted values and residuals from the model
cpt_nullmod$pred<-predict(null_FERE_mod, level=0)
cpt_nullmod$resid<-residuals(null_FERE_mod, level=0)

#plot the predicted values against the raw data
nullfit_plot<-ggplot(cpt_nullmod, aes(x=age, y=log.mass, group=interaction(temp, treatment), color=treatment))
nullfit_plot+geom_point(
)+geom_line(aes(y=pred),
            size=3
)+facet_wrap(~temp)

#plot the residuals against the predicted values
nullres_plot<-ggplot(cpt_nullmod, aes(x=pred, y=resid, color=treatment))
nullres_plot+geom_point(
)+geom_hline(aes(yintercept=0),
             size=1)

#plot the residuals against age
nullresage_plot<-ggplot(cpt_nullmod, aes(x=age, y=resid, color=treatment))
nullresage_plot+geom_point(
)+geom_hline(aes(yintercept=0),
             size=1)



#Model with fixed effects with interactions, but no specified random effects

getInitial(log.mass ~ SSlogis(age, Asym, xmid, scal), data=cpt.grp)

#the 0s are place holders for start values for each combination of fixed effect and parameter
initVals<-c(8.2,0,0,0,0,0,4.2,0,0,0,0,0,115.6,0,0,0,0,0)
initVals2

int_nullRE_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
              data=cpt.grp2,
              fixed=Asym + xmid + scal ~ temp*treatment,
              random=Asym ~ 1,
              start=initVals)

summary(int_nullRE_mod)
anova(int_nullRE_mod)
coef(int_nullRE_mod)


#create a new dataframe to use for plotting fit and residuals
cpt_intnull<-cpt.grp2

#find predicted values and residuals from the model
cpt_intnull$pred_f<-predict(int_nullRE_mod, level=0)
cpt_intnull$resid_f<-residuals(int_nullRE_mod, level=0)


#plot the predicted values against the raw data
int_nullfit_plot<-ggplot(cpt_intnull, aes(x=age, y=log.mass, group=interaction(temp, treatment), color=treatment))
int_nullfit_plot+geom_point(
)+geom_line(aes(y=pred_f),
            size=3
)+facet_wrap(~temp)

#plot the residuals against the predicted values
int_nullres_plot<-ggplot(cpt_intnull, aes(x=pred_f, y=resid_f, color=treatment))
int_nullres_plot+geom_point(
)+geom_hline(aes(yintercept=0),
             size=1)

#plot the residuals against age
int_nullresage_plot<-ggplot(cpt_intnull, aes(x=age, y=resid_f, color=treatment))
int_nullresage_plot+geom_point(
)+geom_hline(aes(yintercept=0),
             size=1
)+facet_wrap(~temp)


#plot the random effects
int_nullRE_plot<-ggplot(subset(cpt_intnull, temp==20), aes(x=age, y=log.mass))
int_nullRE_plot+geom_point(
)+geom_line(data=subset(cpt_intnull, temp==20), 
            aes(y=pred, color=treatment,
                group=interaction(bug.id, treatment)),
                size=1, alpha=.7
)+facet_wrap(~treatment)


#compare the 2 models 
anova(null_FERE_mod, int_nullRE_mod)


#specifying starting values (from coefficients of the int_nullRE_mod)

initVals_spec<-c(11.09284, 0.18099, -0.20637, -0.83697, -0.53427, -0.59800,
                 136.53585, -58.48587, -88.37025, 5.71493, -17.64969, -15.91212,
                 263.25185, -124.67561, -158.91683, 28.96370, -2.00971, -22.63849)


int_nullRE_mod2<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                     data=cpt.grp2,
                     fixed=Asym + xmid + scal ~ temp*treatment,
                     random=Asym ~ 1,
                     start=initVals_spec)


anova(int_nullRE_mod, int_nullRE_mod2) #doesn't seem to change the model at all

#----------------------

#add random effects to interaction models

#run the full model with all fixed effects and interactions, random effect of individual on scal
lf_scal_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                  data=cpt.grp2,
                  fixed=Asym + xmid + scal ~ temp*treatment,
                  random=scal ~ 1|bug.id,
                  start=initVals)



#run the full model with all fixed effects and interactions, random effect of individual on xmid
lf_xmid_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                  data=cpt.grp2,
                  fixed=Asym + xmid + scal ~ temp*treatment,
                  random=xmid ~ 1|bug.id,
                  start=initVals)

#run the full model with all fixed effects and interactions, random effect of individual on Asym
lf_Asym_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                  data=cpt.grp2,
                  fixed=Asym + xmid + scal ~ temp*treatment,
                  random=Asym ~ 1|bug.id,
                  start=initVals)

#run the full model with all fixed effects and interactions, random effect of individual on scal and Asym
lf_as_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                data=cpt.grp2,
                fixed=Asym + xmid + scal ~ temp*treatment,
                random=scal + Asym ~ 1|bug.id,
                start=initVals)

#run the full model with all fixed effects and interactions, random effect of individual on scal and xmid
##does not run without opt="nlm"--but not sure what this does
lf_xs_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                data=cpt.grp2,
                fixed=Asym + xmid + scal ~ temp*treatment,
                random=xmid + scal ~ 1|bug.id,
                start=initVals,
                control=nlmeControl(opt="nlm"))

#run the full model with all fixed effects and interactions, random effect of individual on xmid and Asym
lf_xa_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                data=cpt.grp2,
                fixed=Asym + xmid + scal ~ temp*treatment,
                random=xmid + Asym ~ 1|bug.id,
                start=initVals)


initVals_spec<-c(11.09284, 0.18099, -0.20637, -0.83697, -0.53427, -0.59800,
                 136.53585, -58.48587, -88.37025, 5.71493, -17.64969, -15.91212,
                 263.25185, -124.67561, -158.91683, 28.96370, -2.00971, -22.63849)

lf_xas_mod<-nlme(log.mass ~ SSlogis(age, Asym, xmid, scal),
                data=cpt.grp2,
                fixed=Asym + xmid + scal ~ temp*treatment,
                random=xmid + Asym + scal ~ 1|bug.id,
                start=initVals_spec,
                control = nlmeControl(opt="nlminb", msMaxIter = 1000))


#test all models
anova(null_FERE_mod, int_nullRE_mod, lf_Asym_mod, lf_scal_mod, lf_xmid_mod, lf_as_mod, lf_xa_mod, lf_xs_mod)


#fit and residual plot of the best fitting models with RE of 1 and 2 parameters

#random effect on asymptote only
cpt_asym<-cpt.grp2
cpt_asym$pred_f<-predict(lf_Asym_mod, level=0)
cpt_asym$resid_f<-residuals(lf_Asym_mod, level=0)


#plot the predicted values against the raw data
REasym_plot<-ggplot(cpt_asym, aes(x=age, y=log.mass, group=interaction(temp, treatment), color=treatment))
REasym_plot+geom_point(
)+geom_line(aes(y=pred_f),
            size=3
)+facet_wrap(~temp)

#plot the residuals against the predicted values
REasym_res_plot<-ggplot(cpt_asym, aes(x=pred_f, y=resid_f, color=treatment))
REasym_res_plot+geom_point(
)+geom_hline(aes(yintercept=0),
             size=1)

#plot the residuals against age
REasym_resage_plot<-ggplot(cpt_asym, aes(x=age, y=resid_f, color=treatment))
REasym_resage_plot+geom_point(
)+geom_hline(aes(yintercept=0),
             size=1)





#random effect on xmid and scal
cpt_xs<-cpt.grp2
cpt_xs$pred_f<-predict(lf_xs_mod, level=0)
cpt_xs$resid_f<-residuals(lf_xs_mod, level=0)


#plot the predicted values against the raw data
RExs_plot<-ggplot(cpt_xs, aes(x=age, y=log.mass, group=interaction(temp, treatment), color=treatment))
RExs_plot+geom_point(
)+geom_line(aes(y=pred_f),
            size=3
)+facet_wrap(~temp)

#plot the residuals against the predicted values
RExs_res_plot<-ggplot(cpt_xs, aes(x=pred_f, y=resid_f, color=treatment))
RExs_res_plot+geom_point(
)+geom_hline(aes(yintercept=0),
             size=1)

#plot the residuals against age
RExs_resage_plot<-ggplot(cpt_xs, aes(x=age, y=resid_f, color=treatment))
RExs_resage_plot+geom_point(
)+geom_hline(aes(yintercept=0),
             size=1
)+facet_wrap(~temp)




