#CxPxT Analysis--General additive mixed effects models (GAMM) for temperature, treatment and load on mass and consumption 
  ##Per round 1 of review comments


#load libraries
library(Rmisc)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(nlme)
library(cowplot)
library(viridis)
library(car)
library(mgcv)


#------------------------

#load data--CxPxT long format
cpt <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt.gr.long.load.csv", 
                col_types = cols(temp = col_factor(levels = c("20","25", "30")),
                                 treatment = col_factor(levels = c("control", "para"))))


#make some data adjustments (log mass, make a column with day age)
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

#--------------------------------

#GAMM model of log mass of temp and treat

#subset to only columns in model, and remove rows with NAs (so that predicted and fitted values can be
#added to the dataframe easily)
cpt_mass<-select(cpt.cl, bug.id, temp, treatment, log.mass, age)
cpt_mass<-na.omit(cpt_mass)

#make bug.id a factor so it will work as a random effect in the GAMM model
cpt_mass$bug.id<-as.factor(cpt_mass$bug.id)


#run a full GAMM model (where the smooth of age is also affected by the interaction of temp and treat)
gam_mass_mod<-gam(log.mass ~ s(age, by= interaction(treatment,temp, k=10,bs="ts")) + s(bug.id,bs="re") + treatment * temp,
                  method="ML", data=cpt_mass, na.action = na.omit)
anova(gam_mass_mod)
summary(gam_mass_mod)


#run a "null" GAMM model where the smooth of age is not affected by temp and treat
gam_mnull_mod<-gam(log.mass ~ s(age, k=10,bs="ts") + s(bug.id,bs="fs") + treatment * temp,
                   method="ML", data=cpt_mass, na.action = na.omit)

anova(gam_mnull_mod, gam_mass_mod, test="Chisq")
AIC(gam_mnull_mod, gam_mass_mod)


#make columns with predicted and residual values for plotting
cpt_mass$pred<-predict(gam_mass_mod, level=0)
cpt_mass$resid<-residuals(gam_mass_mod, level=0)

#plot the residuals by age
mass_gam_ra<-ggplot(cpt_mass, aes(x=age, y=resid, color=treatment))
mass_gam_ra<-mass_gam_ra+geom_point(shape=1, size=2, alpha=.6
)+scale_color_manual(values=c("black", "#DA8E03"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized")
)+geom_hline(aes(yintercept=0), 
             color="black", linetype="dashed", size=1.5
)+labs(x="Age [hours]", y="Model Residuals"
)+facet_wrap(~temp
)+ theme(axis.line.x=element_line(colour = 'black', size = 1),
         axis.line.y=element_line(colour = 'black', size = 1),
         axis.ticks = element_line(colour = 'black', size = 1),
         axis.ticks.length = unit(2, "mm"),
         axis.text.x = element_text(size = 18),
         axis.text.y = element_text(size = 18),
         axis.title.x = element_text(size = 18),
         axis.title.y = element_text(size = 18),
         strip.background = element_rect(color="black", fill="white", linetype="solid"),
         strip.text.x = element_text(size=18))

mass_gam_ra

#-------------------------------

#GAMM model of consumption by temp and treat

#subset to only columns in model, and remove rows with NAs--for cnsmp
cpt_cnsmp<-select(cpt.cl, bug.id, temp, treatment, log.cnsmp, age)
cpt_cnsmp<-na.omit(cpt_cnsmp)

#make bug.id a factor so it will work as a random effect in the GAMM model
cpt_cnsmp$bug.id<-as.factor(cpt_cnsmp$bug.id)

#run a full GAMM model (where the smooth of age is also affected by the interaction of temp and treat)
gam_cnsmp_mod<-gam(log.cnsmp ~ s(age, by= interaction(treatment,temp, k=10,bs="ts")) + s(bug.id,bs="re") + treatment * temp,
                   method="ML", data=cpt_cnsmp, na.action = na.omit)
anova(gam_cnsmp_mod)
summary(gam_cnsmp_mod)


#run a "null" GAMM model where the smooth of age is not affected by temp and treat
gam_cnull_mod<-gam(log.cnsmp ~ s(age, k=10,bs="ts") + s(bug.id,bs="fs") + treatment * temp,
                    method="ML", data=cpt_cnsmp, na.action = na.omit)

anova(gam_cnsmp_mod, gam_cnull_mod, test="Chisq")
AIC(gam_cnsmp_mod, gam_cnull_mod)


#make columns with predicted and residual values for plotting
cpt_cnsmp$pred<-predict(gam_cnsmp_mod, level=0)
cpt_cnsmp$resid<-residuals(gam_cnsmp_mod, level=0)

#plot the residuals by age
cnsmp_gam_ra<-ggplot(cpt_cnsmp, aes(x=age, y=resid, color=treatment))
cnsmp_gam_ra+geom_point(shape=1, size=2, alpha=.6
)+scale_color_manual(values=c("black", "#DA8E03"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized")
)+geom_hline(aes(yintercept=0), 
             color="black", linetype="dashed", size=1.5
)+labs(x="Age [hours]", y="Model Residuals"
)+facet_wrap(~temp
)+ theme(axis.line.x=element_line(colour = 'black', size = 1),
         axis.line.y=element_line(colour = 'black', size = 1),
         axis.ticks = element_line(colour = 'black', size = 1),
         axis.ticks.length = unit(2, "mm"),
         axis.text.x = element_text(size = 18),
         axis.text.y = element_text(size = 18),
         axis.title.x = element_text(size = 18),
         axis.title.y = element_text(size = 18),
         strip.background = element_rect(color="black", fill="white", linetype="solid"),
         strip.text.x = element_text(size=18))


#-------------------------------------

#GAMM model for effects of load on mass and consumption of parasitized caterpillars

#subset to only parasitized individuals and remove individuals with more than 3 ovp 
  ##num.ovp was not included when making the long data set, so using load as a proxy (by checking wide data set, 
  ##only ind para 3 or more times had loads greater than 350)
cpt_pm<-subset(cpt.cl, treatment=="para")
cpt_pm$load<-as.numeric(cpt_pm$load)
cpt_pm<-subset(cpt_pm, load<350)

#subset to only columns necessary for models, and remove rows with NAs
cpt_pm<-select(cpt_pm, bug.id, temp, load, log.mass, age)
cpt_pm<-na.omit(cpt_pm)

#make bug.id a factor so that it will work as a random effect in the model
cpt_pm$bug.id<-as.factor(cpt_pm$bug.id)

#run a GAMM model of mass, where the smooth is determined by age and load, with an interaction of temperature
  ##basically creates a 3D surface where one axis is age, one is load, and the other temperature
gam_ml_mod<-gam(log.mass ~ s(age, load, by=temp, bs="ts") + s(bug.id,bs="re") + load * temp,
                   method="ML", data=cpt_pm, na.action = na.omit)
anova(gam_ml_mod)
summary(gam_ml_mod)


#run a GAMM model of mass with smooth of age and load, specifying k=10 (like we did with the prev models)
gam_ml_kspec_mod<-gam(log.mass ~ s(age, load, by=temp, k=10, bs="ts") + s(bug.id,bs="re") + load * temp,
                method="ML", data=cpt_pm, na.action = na.omit)


#compare models with and without specifying knots--the unspecified one is much better
anova(gam_ml_kspec_mod, gam_ml_mod, test="Chisq")
AIC(gam_ml_kspec_mod, gam_ml_mod)


#run a GAMM with age and load as separate smooths--does not have an interaction with temp this way (2 2D surfaces, instead
  ## of one 3D surface)
gam_mL_noint_mod<-gam(log.mass ~ s(age, by=temp,bs="ts") + s(load, by=temp, bs="ts") + s(bug.id,bs="re") + load * temp,
                      method="ML", data=cpt_pm, na.action = na.omit)


#compare the interaction model with the no interaction model--the one with int is much better
anova(gam_ml_mod, gam_mL_noint_mod, test="Chisq")
AIC(gam_ml_mod, gam_mL_noint_mod)


#make columns with predicted and residual values for plotting
cpt_pm$pred<-predict(gam_ml_mod, level=0)
cpt_pm$resid<-residuals(gam_ml_mod, level=0)

#plot the residuals by age
ml_gam_ra<-ggplot(cpt_pm, aes(x=age, y=resid, color=load))
ml_gam_ra+geom_point(size=2, alpha=.6
)+scale_color_viridis(end=.9
)+geom_hline(aes(yintercept=0), 
             color="black", linetype="dashed", size=1.5
)+labs(x="Age [hours]", y="Model Residuals"
)+facet_wrap(~temp
)+ theme(axis.line.x=element_line(colour = 'black', size = 1),
         axis.line.y=element_line(colour = 'black', size = 1),
         axis.ticks = element_line(colour = 'black', size = 1),
         axis.ticks.length = unit(2, "mm"),
         axis.text.x = element_text(size = 18),
         axis.text.y = element_text(size = 18),
         axis.title.x = element_text(size = 18),
         axis.title.y = element_text(size = 18),
         strip.background = element_rect(color="black", fill="white", linetype="solid"),
         strip.text.x = element_text(size=18))



#----------------------------------------

#GAMM model of consumption for para cats by temp and load

#subset to only parasitized individuals and remove individuals with more than 3 ovp 
##num.ovp was not included when making the long data set, so using load as a proxy (by checking wide data set, 
##only ind para 3 or more times had loads greater than 350)
cpt_pc<-subset(cpt.cl, treatment=="para")
cpt_pc$load<-as.numeric(cpt_pc$load)
cpt_pc<-subset(cpt_pc, load<350)

#subset to only columns necessary for models, and remove rows with NAs
cpt_pc<-select(cpt_pc, bug.id, temp, load, log.cnsmp, age)
cpt_pc<-na.omit(cpt_pc)

#make bug.id a factor so that it will work as a random effect in the model
cpt_pc$bug.id<-as.factor(cpt_pc$bug.id)


#run a GAMM model of cnsmp, where the smooth is determined by age and load, with an interaction of temperature
##basically creates a 3D surface where one axis is age, one is load, and the other temperature
gam_cl_mod<-gam(log.cnsmp ~ s(age, load, by=temp, bs="ts") + s(bug.id,bs="re") + load * temp,
                     method="ML", data=cpt_pc, na.action = na.omit)
anova(gam_cl_mod)
summary(gam_cl_mod)


#run a GAMM model of cnsmp with smooth of age and load, specifying k=10 (like we did with the prev models)
gam_cl_kspec_mod2<-gam(log.cnsmp ~ s(age, load, k=10, by=temp, bs="ts") + s(bug.id,bs="re") + load * temp,
                      method="ML", data=cpt_pc, na.action = na.omit)


#compare GAMM models with and without specifying knots--model without specification is better
anova(gam_cl_kspec_mod2, gam_cl_mod, test="Chisq")
AIC(gam_cl_kspec_mod2, gam_cl_mod)


#run a GAMM with age and load as separate smooths--does not have an interaction with temp this way (2 2D surfaces, instead
## of one 3D surface)
gam_cl_noint_mod<-gam(log.cnsmp ~ s(age, by=temp,bs="ts") + s(load, by=temp, bs="ts") + s(bug.id,bs="re") + load * temp,
                      method="ML", data=cpt_pc, na.action = na.omit)


#compare the interaction model with the no interaction model--the one with int is much better
anova(gam_cl_mod, gam_cl_noint_mod, test="Chisq")
AIC(gam_ml_mod, gam_mL_noint_mod)


#make columns with predicted and residual values for plotting
cpt_pc$pred<-predict(gam_cl_mod, level=0)
cpt_pc$resid<-residuals(gam_cl_mod, level=0)

#plot the residuals by age
cl_gam_ra<-ggplot(cpt_pc, aes(x=age, y=resid, color=load))
cl_gam_ra+geom_point(size=2, alpha=.6
)+scale_color_viridis(end=.9
)+geom_hline(aes(yintercept=0), 
             color="black", linetype="dashed", size=1.5
)+labs(x="Age [hours]", y="Model Residuals"
)+facet_wrap(~temp
)+ theme(axis.line.x=element_line(colour = 'black', size = 1),
         axis.line.y=element_line(colour = 'black', size = 1),
         axis.ticks = element_line(colour = 'black', size = 1),
         axis.ticks.length = unit(2, "mm"),
         axis.text.x = element_text(size = 18),
         axis.text.y = element_text(size = 18),
         axis.title.x = element_text(size = 18),
         axis.title.y = element_text(size = 18),
         strip.background = element_rect(color="black", fill="white", linetype="solid"),
         strip.text.x = element_text(size=18))





