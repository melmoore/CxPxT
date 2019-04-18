#Testing nonlinear mixed effects models for CPT--FOR CONSUMPTION ONLY!!!

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
library(mgcv)


#-------------------

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



#-------------------------------

#model of consumption

#subset to only columns in model, and remove rows with NAs--for mass
cpt.sub<-select(cpt.cl, bug.id, temp, treatment, log.mass, age)
cpt.sub<-na.omit(cpt.sub)

#subset to only columns in model, and remove rows with NAs--for cnsmp
cpt.sub2<-select(cpt.cl, bug.id, temp, treatment, log.cnsmp, age)
cpt.sub2<-na.omit(cpt.sub2)

#making a grouped data object
cpt.grp3<-groupedData(log.cnsmp ~ age|bug.id, data=cpt.sub2)


getInitial(log.cnsmp ~ SSlogis(age, Asym, xmid, scal), data=cpt.grp3)

invalnull_cnsmp<-c(6.98, 16.08, 62.36)

null_cnsmp_mod<-nlme(log.cnsmp ~ SSlogis(age, Asym, xmid, scal),
                     data=cpt.grp3,
                     fixed=Asym + xmid + scal ~ 1,
                     random=Asym ~ 1,
                     start=invalnull_cnsmp)


inval_cnsmp<-c(6.98, 0,0,0,0,0, 16.08, 0,0,0,0,0, 62.36, 0,0,0,0,0)

lf_asymcnsmp_mod<-nlme(log.cnsmp ~ SSlogis(age, Asym, xmid, scal),
                       data=cpt.grp3,
                       fixed=Asym + xmid + scal ~ temp*treatment,
                       random=Asym ~ 1,
                       start=inval_cnsmp)

summary(lf_asymcnsmp_mod)
anova(lf_asymcnsmp_mod)

anova(null_cnsmp_mod, lf_asymcnsmp_mod)


cpt_ca<-cpt.grp3

cpt_ca$pred_r<-predict(lf_asymcnsmp_mod)
cpt_ca$resid_r<-residuals(lf_asymcnsmp_mod)
cpt_ca$pred_f<-predict(lf_asymcnsmp_mod, level=0)
cpt_ca$resid_f<-residuals(lf_asymcnsmp_mod, level=0)

ca_fit_mod<-ggplot(cpt_ca, aes(x=age, y=log.cnsmp, color=treatment))
ca_fit_mod+geom_point(
)+geom_line(aes(y=pred_f),
            size=1
)+facet_wrap(~temp)

cnsmp<-ggplot(cpt.cl, aes(x=age, y=log.cnsmp, color=treatment))
cnsmp+geom_point(
)+facet_wrap(~temp)


cnsmp<-ggplot(cpt_t0, aes(x=age, y=log.cnsmp, color=treatment))
cnsmp+geom_point(
)+facet_wrap(~temp)


ca_resfit_plot<-ggplot(cpt_ca, aes(x=pred_f, y=resid_f, color=treatment))
ca_resfit_plot+geom_point(
)+geom_hline(aes(yintercept=0), color="black"
)+facet_wrap(~temp)

ca_resage_plot<-ggplot(cpt_ca, aes(x=age, y=resid_f, color=treatment))
ca_resage_plot+geom_point(
)+geom_hline(aes(yintercept=0), color="black"
)+facet_wrap(~temp)


#model with scal RE

lf_scalcnsmp_mod<-nlme(log.cnsmp ~ SSlogis(age, Asym, xmid, scal),
                       data=cpt.grp3,
                       fixed=Asym + xmid + scal ~ temp*treatment,
                       random=scal ~ 1,
                       start=inval_cnsmp)


#model with xmid RE

lf_xmidcnsmp_mod<-nlme(log.cnsmp ~ SSlogis(age, Asym, xmid, scal),
                       data=cpt.grp3,
                       fixed=Asym + xmid + scal ~ temp*treatment,
                       random=xmid ~ 1,
                       start=inval_cnsmp)


#model with xmid and asymp

lf_xacnsmp_mod<-nlme(log.cnsmp ~ SSlogis(age, Asym, xmid, scal),
                     data=cpt.grp3,
                     fixed=Asym + xmid + scal ~ temp*treatment,
                     random=Asym +xmid ~ 1,
                     start=inval_cnsmp,
                     control = nlmeControl(msMaxIter = 1000))

#model with asym and scal

lf_ascnsmp_mod<-nlme(log.cnsmp ~ SSlogis(age, Asym, xmid, scal),
                     data=cpt.grp3,
                     fixed=Asym + xmid + scal ~ temp*treatment,
                     random=Asym + scal ~ 1,
                     start=inval_cnsmp,
                     control = nlmeControl(msMaxIter = 1000))

#model with xmid and scal

lf_xscnsmp_mod<-nlme(log.cnsmp ~ SSlogis(age, Asym, xmid, scal),
                     data=cpt.grp3,
                     fixed=Asym + xmid + scal ~ temp*treatment,
                     random=xmid + scal ~ 1,
                     start=inval_cnsmp,
                     control = nlmeControl(msMaxIter = 1000))


#model with all param as RE

lf_asxcnsmp_mod<-nlme(log.cnsmp ~ SSlogis(age, Asym, xmid, scal),
                      data=cpt.grp3,
                      fixed=Asym + xmid + scal ~ temp*treatment,
                      random=Asym + scal + xmid ~ 1,
                      start=inval_cnsmp,
                      control = nlmeControl(opt="nlm", msMaxIter = 1000))



anova(null_cnsmp_mod, lf_asymcnsmp_mod, lf_scalcnsmp_mod, lf_xmidcnsmp_mod, lf_ascnsmp_mod, lf_xacnsmp_mod, lf_xscnsmp_mod)

#--------------------

#trying michaelis menten function

#making 0s in age equal .001 so that the getInitial will work
cpt.grp3$age<-ifelse(cpt.grp3$age==0, 0.001, cpt.grp3$age)

#can't get getInitial to work for some reason--throws an error about NA/NAN/INF in my data, but I cannot find any
#getInitial(log.cnsmp ~ SSmicmen(age, Vm, K), data=cpt.sub2)

#using coefficient values from the ssLogis function
inval_cnsmp2<-c(7.12, 36.18)

null_cnsmp_mod2<-nlme(log.cnsmp ~ SSmicmen(age, Vm, K),
                     data=cpt.grp3,
                     fixed = Vm + K ~ 1,
                     random = Vm ~ 1,
                     start = inval_cnsmp2)

summary(null_cnsmp_mod2)


inval_cnsmp_int<-c(7.81, 0, 0, 0, 0, 0, 33.00, 0, 0, 0, 0, 0)

mm_vm_mod<-nlme(log.cnsmp ~ SSmicmen(age, Vm, K),
                data=cpt.grp3,
                fixed = Vm + K ~ temp*treatment,
                random = Vm ~ 1,
                start = inval_cnsmp_int)
summary(mm_vm_mod)

anova(null_cnsmp_mod2, mm_vm_mod)


mm_k_mod<-nlme(log.cnsmp ~ SSmicmen(age, Vm, K),
                data=cpt.grp3,
                fixed = Vm + K ~ temp*treatment,
                random = K ~ 1,
                start = inval_cnsmp_int)
summary(mm_k_mod)


mm_vmk_mod<-nlme(log.cnsmp ~ SSmicmen(age, Vm, K),
                 data=cpt.grp3,
                 fixed = Vm + K ~ temp*treatment,
                 random = Vm + K ~ 1,
                 start = inval_cnsmp_int,
                 control = nlmeControl(opt = "nlm", msMaxIter = 10000))

anova(null_cnsmp_mod2, mm_vm_mod, mm_k_mod)


cpt_mm<-cpt.grp3

cpt_mm$pred_r<-predict(mm_vm_mod)
cpt_mm$pred_f<-predict(mm_vm_mod, level=0)
cpt_mm$resid_f<-residuals(mm_vm_mod, level=0)

ca_vmfit_plot<-ggplot(cpt_mm, aes(x=age, y=log.cnsmp, color=treatment))
ca_vmfit_plot+geom_point(
)+geom_line(aes(y=pred_f),
            size=1
)+facet_wrap(~temp)

cnsmp<-ggplot(cpt.cl, aes(x=age, y=log.cnsmp, color=treatment))
cnsmp+geom_point(
)+facet_wrap(~temp)


cnsmp<-ggplot(cpt_t0, aes(x=age, y=log.cnsmp, color=treatment))
cnsmp+geom_point(
)+facet_wrap(~temp)


ca_resfit_plot<-ggplot(cpt_ca, aes(x=pred_f, y=resid_f, color=treatment))
ca_resfit_plot+geom_point(
)+geom_hline(aes(yintercept=0), color="black"
)+facet_wrap(~temp)


#trying my original model for the heck of it to look at fit and residuals

linc_mod<-lme(log.cnsmp~(age+I(age^2)):(temp*treatment)+temp,random=~age|bug.id,
             data=cpt.cl,na.action=na.exclude,method="ML")

linc_add_mod<-lme(log.cnsmp ~ (age+I(age^2)):(temp+treatment), random = ~age|bug.id, 
               data=cpt.cl,na.action=na.exclude,method="ML")

anova(linc_add_mod, linc_mod)


cpt_ln<-cpt.cl
cpt_ln$pred_f<-predict(linc_mod, level=0)
cpt_ln$resid_f<-residuals(linc_mod, level=0)


linc_fit_plot<-ggplot(cpt_ln, aes(x=age, y=log.cnsmp, color=treatment))
linc_fit_plot+geom_point(
)+geom_line(aes(y=pred_f),
            size=1
)+facet_wrap(~temp)


linc_respred_plot<-ggplot(cpt_ln, aes(x=pred_f, y=resid_f, color=treatment))
linc_respred_plot+geom_point(
)+geom_hline(aes(yintercept=0),
             size=1, color="black"
)+facet_wrap(~temp)


#trying age as 3rd order

linc2_mod<-lme(log.cnsmp~(age+I(age^2)):(temp*treatment)+temp,random=~age|bug.id,
              data=cpt.cl,na.action=na.exclude,method="ML")

linc3_mod<-lme(log.cnsmp~(age+I(age^2)+I(age^3)):(temp*treatment)+temp,random=~age|bug.id,
               data=cpt.cl,na.action=na.exclude,method="ML",
               control = lmeControl(opt="optim"))

summary(linc3_mod)

cpt_lin3<-cpt.cl

cpt_lin3$resid<-residuals(linc3_mod)
cpt_lin3$pred_f<-predict(linc3_mod, level=0)
cpt_lin3$pred_q<-predict(linc2_mod, level=0)
cpt_lin3$resid_q<-residuals(linc2_mod, level=0)

anova(linc2_mod, linc3_mod)

linc_resage_plot3<-ggplot(cpt_lin3, aes(x=age, y=resid, color=treatment))
linc_resage_plot3+geom_point(
)+facet_wrap(~temp)

linc_fit_plot<-ggplot(cpt_lin3, aes(x=age, y=log.cnsmp, color=treatment))
linc_fit_plot+geom_point(
)+geom_line(aes(y=pred_f),
            size=1
)+facet_wrap(~temp)


linc_resage_plot4<-ggplot(cpt_lin3, aes(x=age, y=resid_q, color=treatment))
linc_resage_plot4+geom_point(
)+facet_wrap(~temp)

linc_fit_plot<-ggplot(cpt_lin3, aes(x=age, y=log.cnsmp, color=treatment))
linc_fit_plot+geom_point(
)+geom_line(aes(y=pred_f),
            size=1
)+facet_wrap(~temp)



lina2_mod<-lme(log.mass~(age+I(age^2)):(temp*treatment)+temp,random=~age|bug.id,
               data=cpt.cl,na.action=na.exclude,method="ML")

lina3_mod<-lme(log.mass~(age+I(age^2)+I(age^3)):(temp*treatment)+temp,random=~age|bug.id,
               data=cpt.cl,na.action=na.exclude,method="ML",
               control = lmeControl(opt="optim"))


anova(lina2_mod, lina3_mod)


cpt_lina<-cpt.cl

cpt_lina$resid<-residuals(lina3_mod)
cpt_lina$pred_f<-predict(lina3_mod, level=0)

lina_resage_plot3<-ggplot(cpt_lina, aes(x=age, y=resid, color=treatment))
lina_resage_plot3+geom_point(
)+facet_wrap(~temp)


lina_fit_plot<-ggplot(cpt_lina, aes(x=age, y=log.mass, color=treatment))
lina_fit_plot+geom_point(
)+geom_line(aes(y=pred_f), size=1
)+facet_wrap(~temp)


#trying a linear model without the quadratic effect of age

linc_linage_mod<-lme(log.cnsmp ~ age*temp*treatment, random=~age|bug.id,
                     data=cpt.cl,na.action=na.exclude,method="ML",
                     control=lmeControl(opt="optim"))

null_linc_add_mod<-lme(log.cnsmp ~ age+temp+treatment, random=~age|bug.id,
                   data=cpt.cl,na.action=na.exclude,method="ML",
                   control=lmeControl(opt="optim"))
anova(null_linc_add_mod, linc_linage_mod)


cpt_ln2<-cpt.cl
cpt_ln2$pred_f<-predict(linc_linage_mod, level=0)
cpt_ln2$resid_f<-residuals(linc_linage_mod, level=0)

linc_fit_plot2<-ggplot(cpt_ln2, aes(x=age, y=log.cnsmp, color=treatment))
linc_fit_plot2+geom_point(
)+geom_line(aes(y=pred_f),
            size=1
)+facet_wrap(~temp)


linc_respred_plot2<-ggplot(cpt_ln2, aes(x=pred_f, y=resid_f, color=treatment))
linc_respred_plot2+geom_point(
)+geom_hline(aes(yintercept=0),
             size=1, color="black"
)+facet_wrap(~temp)


#-------------------

library(gam)

cpt_cnsmp<-cpt.sub2

gam_mod<-gamm(log.cnsmp ~ s(age, by = interaction(treatment,temp),k=10) + treatment * temp , random = list(bug.id=~1), 
              data=cpt_cnsmp, na.action = na.omit)
summary(gam_mod)
anova(gam_mod)

gam_mod2<-gamm(log.cnsmp ~ s(age, by = treatment, k=10) + s(age, by=temp, k=10) + treatment + temp, random = list(bug.id=~1), data=cpt.sub2, na.action = na.omit)

anova(gam_mod, gam_mod2, test="Chisq")

AIC(gam_mod, gam_mod2)


gam_temp_mod<-gamm(log.cnsmp ~ s(age, by = interaction(temp),k=10) + treatment * temp, random = list(bug.id=~1), data=cpt.sub2, na.action = na.omit)

anova(gam_mod, gam_temp_mod, test="Chisq")

gam_mass_mod<-gam(log.mass ~ s(age, by = interaction(treatment,temp),k=10) + treatment * temp, random = ~(1|bug.id), data=cpt.sub, na.action = na.omit)
summary(gam_mod)


cpt_gam<-cpt_cnsmp
cpt_gam$resid<-residuals(gam_mod, level=0)
cpt_gam$pred<-predict(gam_mod,level=0)
cpt_gam$pred2<-predict(gam_mod2, level=0)
cpt_gam$resid2<-residuals(gam_mod2, level=0)

gam_resid_plot<-ggplot(cpt_gam, aes(x=age, y=resid, color=treatment))
gam_resid_plot+geom_point(
)+facet_wrap(~temp)

gam_resid_plot2<-ggplot(cpt_gam, aes(x=age, y=resid2, color=treatment))
gam_resid_plot2+geom_point(
)+facet_wrap(~temp)

gam_fit_plot<-ggplot(cpt_gam, aes(x=age, y=log.cnsmp, color=treatment))
gam_fit_plot+geom_point(alpha=.2, shape=1
) + 
geom_line(aes(y=pred),            size=1)+
facet_wrap(~temp) #+geom_smooth(method="gam", formula = s(x))

gam_fit_plot2<-ggplot(cpt_gam, aes(x=age, y=log.cnsmp, color=treatment))
gam_fit_plot2+geom_point(alpha=.2, shape=1
) + 
  geom_line(aes(y=pred2),            size=1)+
  facet_wrap(~temp)


cpt_gamms<-cpt.sub
cpt_gamms$resid<-residuals(gam_mass_mod, level=0)
cpt_gamms$pred<-predict(gam_mass_mod,level=0)

gamms_resid_plot<-ggplot(cpt_gamms, aes(x=age, y=resid, color=treatment))
gamms_resid_plot+geom_point(
)+facet_wrap(~temp)

gamms_fit_plot<-ggplot(cpt_gamms, aes(x=age, y=log.mass, color=treatment))
gamms_fit_plot+geom_point(alpha=.2, shape=1
) + 
  geom_line(aes(y=pred),            size=1)+
  facet_wrap(~temp) #+geom_smooth(method="gam", formula = s(x))




cpt.sub<-select(cpt.cl, bug.id, temp, treatment, log.mass, age)
cpt.sub<-na.omit(cpt.sub)

gam_lmss_mod<-gam(log.mass ~ s(age) * treatment * temp, data=cpt.sub, na.action = na.omit)
summary(gam_lmss_mod)

cpt_mgam<-cpt.sub
cpt_mgam$resid<-residuals(gam_lmss_mod)
cpt_mgam$pred<-predict(gam_lmss_mod)

gamlm_fit_plot<-ggplot(cpt_mgam, aes(x=age, y=log.mass, color=treatment))
gamlm_fit_plot+geom_point(
)+geom_line(aes(y=pred),
            size=1
)+facet_wrap(~temp)

gamlm_resid_plot<-ggplot(cpt_mgam, aes(x=age, y=resid, color=treatment))
gamlm_resid_plot+geom_point(
)+facet_wrap(~temp)




gamm_mod<-gam(log.cnsmp)


#--------------------

#James GAMM edits

cpt.sub2$bug.id<-as.factor(cpt.sub2$bug.id)

gam_cnsmp_mod<-gam(log.cnsmp ~ s(age, by= interaction(treatment,temp, k=10,bs="ts")) + s(bug.id,bs="re") + treatment * temp,
             method="ML", data=cpt.sub2, na.action = na.omit)
anova(gam_cnsmp_mod)
summary(gam_cnsmp_mod)


#basically a null model (saying temp and treatment have effects, but don't interact with age to affect the curve)
gam_cnsmp_mod2<-gam(log.cnsmp ~ s(age, k=10,bs="ts") + s(bug.id,bs="fs") + treatment * temp,
                   method="ML", data=cpt.sub2, na.action = na.omit)
anova(gam_cnsmp_mod2)
summary(gam_cnsmp_mod)

anova(gam_cnsmp_mod2, gam_cnsmp_mod, test="Chisq")



cpt.sub$bug.id<-as.factor(cpt.sub$bug.id)

gam_mass_mod<-gam(log.mass ~ s(age, by= interaction(treatment,temp, k=10,bs="ts")) + s(bug.id,bs="re") + treatment * temp,
             method="ML", data=cpt.sub, na.action = na.omit)
anova(gam_mass_mod)


gam_mass_mod2<-gam(log.mass ~ s(age, k=10,bs="ts") + s(bug.id,bs="fs") + treatment * temp,
                    method="ML", data=cpt.sub, na.action = na.omit)

anova(gam_mass_mod2, gam_mass_mod, test="Chisq")

cpt.sub2$pred<-predict(gam_cnsmp_mod, level=0)
cpt.sub2$resid<-residuals(gam_cnsmp_mod, level=0)

cpt.sub$pred<-predict(gam_mass_mod, level=0)
cpt.sub$resid<-residuals(gam_mass_mod, level=0)


cnsmp_gam_fit<-ggplot(cpt.sub2, aes(x=age, y=log.cnsmp, color=treatment))
cnsmp_gam_fit+geom_point(shape=1, alpha=.4
)+geom_line(aes(y=pred, group=interaction(bug.id, treatment)),
            size=1
)+facet_wrap(~temp)


cnsmp_gam_rp<-ggplot(cpt.sub2, aes(x=pred, y=resid, color=treatment))
cnsmp_gam_rp+geom_point(
)+facet_wrap(~temp)

cnsmp_gam_ra<-ggplot(cpt.sub2, aes(x=age, y=resid, color=treatment))
cnsmp_gam_ra+geom_point(
)+facet_wrap(~temp)


mass_gam_fit<-ggplot(cpt.sub, aes(x=age, y=log.mass, color=treatment))
mass_gam_fit+geom_point(shape=1, alpha=.4
)+geom_line(aes(y=pred, group=interaction(bug.id, treatment)),
            size=1
)+facet_wrap(~temp)


mass_gam_rp<-ggplot(cpt.sub, aes(x=pred, y=resid, color=treatment))
mass_gam_rp+geom_point(
)+facet_wrap(~temp)

mass_gam_ra<-ggplot(cpt.sub, aes(x=age, y=resid, color=treatment))
mass_gam_ra+geom_point(
)+facet_wrap(~temp)


#-------------------

#GAMM model for effects of load on mass and consumption

cpt_pm<-subset(cpt.cl, treatment=="para")
cpt_pm<-select(cpt_pm, bug.id, temp, load, log.mass, age)
cpt_pm<-na.omit(cpt_pm)

cpt_pm$bug.id<-as.factor(cpt_pm$bug.id)
cpt_pm$load<-as.numeric(cpt_pm$load)

gam_ml_mod_null<-gam(log.mass ~ s(age, k=10,bs="ts") + s(bug.id,bs="re") + load * temp,
                  method="ML", data=cpt_pm, na.action = na.omit)
anova(gam_ml_mod_null)

gam_mass_mod<-gam(log.mass ~ s(age, by= interaction(load,temp, k=10,bs="ts")) + s(bug.id,bs="re") + load * temp,
                  method="ML", data=cpt_pm, na.action = na.omit)


gam_mass_mod2<-gam(log.mass ~ s(age, by=temp, k=10,bs="ts") + s(bug.id,bs="re") + load * temp,
                  method="ML", data=cpt_pm, na.action = na.omit)
anova(gam_mass_mod2)


gam_mass_mod3<-gam(log.mass ~ s(age, by=temp, k=10,bs="ts") + s(age, by=load, k=10,bs="ts") + s(bug.id,bs="re") + load * temp,
                   method="ML", data=cpt_pm, na.action = na.omit)
anova(gam_mass_mod3)



