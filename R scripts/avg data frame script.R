library(Rmisc)
library(readr)
library(reshape2)
library(ggplot2)

wide <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt gr wide.csv", 
                 col_types = cols(temp = col_factor(levels = c("20", 
                 "25", "30")), treatment = col_factor(levels = c("control", 
                 "para"))))
View(wide)



avg <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt avg data.csv")
View(avg)
avg$temp<-as.factor(avg$temp)
avg$treat<-as.factor(avg$treat)


avg.long <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt avg long.csv")
View(avg.long)
avg.long$temp<-as.factor(avg.long$temp)
avg.long$treat<-as.factor(avg.long$treat)



age4.sum <- summarySE(wide, measurevar="age.4", 
                      groupvars=c("temp", "treatment"),na.rm=TRUE)
age4.sum



age5.sum <- summarySE(wide, measurevar="age.5", 
                      groupvars=c("temp", "treatment"),na.rm=TRUE)

age5.sum




agewand.sum<- summarySE(wide, measurevar="age.wander", 
                        groupvars=c("temp"),na.rm=TRUE)

agewand.sum




ageem.sum<- summarySE(wide, measurevar="age.em", 
                      groupvars=c("temp"),na.rm=TRUE)

ageem.sum




mass3.sum<-summarySE(wide, measurevar="mass.T0", 
                     groupvars=c("temp", "treatment"),na.rm=TRUE)
mass3.sum


wide$log.mass.3<-log(wide$mass.T0)
log.mass3.sum<-summarySE(wide, measurevar="log.mass.3", 
                         groupvars=c("temp", "treatment"),na.rm=TRUE)
log.mass3.sum



mass4.sum<-summarySE(wide, measurevar="mass.4", 
                     groupvars=c("temp", "treatment"),na.rm=TRUE)
mass4.sum


wide$log.mass.4<-log(wide$mass.4)
log.mass4.sum<-summarySE(wide, measurevar="log.mass.4", 
                         groupvars=c("temp", "treatment"),na.rm=TRUE)
log.mass4.sum



mass5.sum<-summarySE(wide, measurevar="mass.5", 
                     groupvars=c("temp", "treatment"),na.rm=TRUE)
mass5.sum


wide$log.mass.5<-log(wide$mass.5)
log.mass5.sum<-summarySE(wide, measurevar="log.mass.5", 
                         groupvars=c("temp", "treatment"),na.rm=TRUE)
log.mass5.sum



masswand.sum<-summarySE(wide, measurevar="mass.wander", 
                        groupvars=c("temp"),na.rm=TRUE)
masswand.sum


wide$log.mass.wand<-log(wide$mass.wander)
log.masswand.sum<-summarySE(wide, measurevar="log.mass.wand", 
                            groupvars=c("temp"),na.rm=TRUE)
log.masswand.sum



massem.sum<-summarySE(wide, measurevar="mass.befem", 
                      groupvars=c("temp"),na.rm=TRUE)

massem.sum


wide$log.mass.em<-log(wide$mass.befem)
log.massem.sum<-summarySE(wide, measurevar="log.mass.em", 
                          groupvars=c("temp"),na.rm=TRUE)
log.massem.sum



cnsmp4.sum<-summarySE(wide, measurevar="cnsmp.4", 
                      groupvars=c("temp","treatment"),na.rm=TRUE)
cnsmp4.sum


wide$log.cnsmp.4<-log(wide$cnsmp.4)
log.cnsmp4.sum<-summarySE(wide, measurevar="log.cnsmp.4", 
                          groupvars=c("temp","treatment"),na.rm=TRUE)
log.cnsmp4.sum



cnsmp5.sum<-summarySE(wide, measurevar="cnsmp.5", 
                      groupvars=c("temp","treatment"),na.rm=TRUE)
cnsmp5.sum


wide$log.cnsmp.5<-log(wide$cnsmp.5)
log.cnsmp5.sum<-summarySE(wide, measurevar="log.cnsmp.5", 
                          groupvars=c("temp","treatment"),na.rm=TRUE)
log.cnsmp5.sum



cnsmpwan.sum<-summarySE(wide, measurevar="cnsmp.wan", 
                     groupvars=c("temp"),na.rm=TRUE)
cnsmpwan.sum


wide$log.cnsmp.wan<-log(wide$cnsmp.wan)
log.cnsmpwan.sum<-summarySE(wide, measurevar="log.cnsmp.wan", 
                            groupvars=c("temp"),na.rm=TRUE)
log.cnsmpwan.sum



cnsmpem.sum<-summarySE(wide, measurevar="cnsmp.em", 
                       groupvars=c("temp"),na.rm=TRUE)
cnsmpem.sum



wide$log.cnsmp.em<-log(wide$cnsmp.em)
log.cnsmpem.sum<-summarySE(wide, measurevar="log.cnsmp.em", 
                           groupvars=c("temp"),na.rm=TRUE)
log.cnsmpem.sum






massgain4.sum<-summarySE(wide, measurevar="mass.gain.4", 
                         groupvars=c("temp","treatment"),na.rm=TRUE)


wide$log.mg.4<-log(wide$mass.gain.4)
log.mg4.sum<-summarySE(wide, measurevar="log.mg.4", 
                       groupvars=c("temp","treatment"),na.rm=TRUE)




massgain5.sum<-summarySE(wide, measurevar="mass.gain.5", 
                         groupvars=c("temp","treatment"),na.rm=TRUE)


wide$log.mg.5<-log(wide$mass.gain.5)
log.mg5.sum<-summarySE(wide, measurevar="log.mg.5", 
                       groupvars=c("temp","treatment"),na.rm=TRUE)




mgwan.sum<-summarySE(wide, measurevar="mass.gain.wan", 
                     groupvars=c("temp"),na.rm=TRUE)



wide$log.mg.wan<-log(wide$mass.gain.wan)
log.mgwan.sum<-summarySE(wide, measurevar="log.mg.wan", 
                         groupvars=c("temp"),na.rm=TRUE)





mgem.sum<-summarySE(wide, measurevar="mass.gain.em", 
                    groupvars=c("temp"),na.rm=TRUE)




wide$log.mg.em<-log(wide$mass.gain.em)
log.mgem.sum<-summarySE(wide, measurevar="log.mg.em", 
                        groupvars=c("temp"),na.rm=TRUE)



###Constructing a datafrme to look at average mass by average age

#Making vectors and creating dataframe


age.4 <- age4.sum[,4]
age.4se<-age4.sum[,6]
age.5<-age5.sum[,4]
age.5se<-age5.sum[,6]

mass.3<-mass3.sum[,4]
mass.3se<-mass3.sum[,6]
log.mass3<-log.mass3.sum[,4]
log.mass3.se<-log.mass3.sum[,6]

mass.4<-mass4.sum[,4]
mass.4se<-mass4.sum[,6]
log.mass4<-log.mass4.sum[,4]
log.mass4.se<-log.mass4.sum[,6]

mass.5<-mass5.sum[,4]
mass.5se<-mass5.sum[,6]
log.mass5<-log.mass5.sum[,4]
log.mass5.se<-log.mass5.sum[,6]

cnsmp.4<-cnsmp4.sum[,4]
cnsmp.4se<-cnsmp4.sum[,6]
log.cnsmp4<-log.cnsmp4.sum[,4]
log.cnsmp4.se<-log.cnsmp4.sum[,6]

cnsmp.5<-cnsmp5.sum[,4]
cnsmp.5se<-cnsmp5.sum[,6]
log.cnsmp5<-log.cnsmp5.sum[,4]
log.cnsmp5.se<-log.cnsmp5.sum[,6]


mass.gain.4<-massgain4.sum[,4]
mass.gain.4se<-massgain4.sum[,6]
log.mass.gain.4<-log.mg4.sum[,4]
log.mass.gain.4se<-log.mg4.sum[,6]


mass.gain.5<-massgain5.sum[,4]
mass.gain.5se<-massgain5.sum[,6]
log.mass.gain.5<-log.mg5.sum[,4]
log.mass.gain.5se<-log.mg5.sum[,6]



temp<-mass5.sum[,1]
treat<-mass5.sum[,2]



age.stop<-c(564.8136,530.9295,307.0614,323.1815,227.5927,262.5587)
age.stse<-c(9.934995,10.978871,5.337777,4.325319,4.692271,3.412339)

mass.stop<-c(10371.99,3079.293,11525.68,3239.798,8870.53,3563.364)
mass.stse<-c(222.0034,250.9695,196.8093,231.1751,184.4539,251.3386)
log.mass.stop<-c(9.236519,7.836183,9.346436,7.906531,9.081652,8.040838)
log.mass.stse<-c(0.02227472,0.09813605,0.01723102,0.10040220,0.02118177,0.08065983)

cnsmp.stop<-c(4521.435,1407.616,5320.226,1885.833,4166.484,1818.381)
cnsmp.stse<-c(154.6848,94.13993,139.7783,96.84869,87.7183,89.37030)
log.cnsmp.stop<-c(8.392793,7.131207,8.565933,7.475401,8.325474,7.444764)
log.cnsmp.stse<-c(0.03683668,0.07476028,0.02616584,0.05820187,0.02196705,0.05233605)


mass.gain.stop<-c(10324.985,3040.969,11470.715,3189.098,8805.928,3508.838)
mass.gain.stse<-c(221.9219,251.1335,196.2354,230.5902,185.0160,250.8792)
log.mass.gain.stop<-c(9.231888,7.816265,9.341629,7.883179,9.074137,8.020392)
log.mass.gain.stse<-c(0.02237196,0.10029755,0.01727234,0.10303471,0.02144074,0.08245197)


age.3<-c(0,0,0,0,0,0)
age.3se<-c(0,0,0,0,0,0)
cnsmp.3<-c(0,0,0,0,0,0)
cnsmp.3se<-c(0,0,0,0,0,0)
log.cnsmp3<-c(0,0,0,0,0,0)
log.cnsmp3.se<-c(0,0,0,0,0,0)
mass.gain.3<-c(0,0,0,0,0,0)
mass.gain.3se<-c(0,0,0,0,0,0)
log.mass.gain.3<-c(0,0,0,0,0,0)
log.mass.gain.3se<-c(0,0,0,0,0,0)

avg<-data.frame(temp,treat,age.3,age.3se,age.4,age.4se,age.5,age.5se,age.stop,age.stse,mass.3,log.mass3,mass.3se,log.mass3.se,
                mass.4,mass.4se,log.mass4,log.mass4.se,mass.5,mass.5se,log.mass5,log.mass5.se,mass.stop,mass.stse,log.mass.stop,
                log.mass.stse,cnsmp.3,cnsmp.3se,log.cnsmp3,log.cnsmp3.se,cnsmp.4,log.cnsmp4,log.cnsmp4.se,cnsmp.4se,cnsmp.5,
                cnsmp.5se,log.cnsmp5,log.cnsmp5.se,cnsmp.stop,cnsmp.stse,log.cnsmp.stop,log.cnsmp.stse,mass.gain.3,mass.gain.3se,
                log.mass.gain.3,log.mass.gain.3se,mass.gain.4,mass.gain.4se,log.mass.gain.4,log.mass.gain.4se,mass.gain.5,mass.gain.5se,
                log.mass.gain.5,log.mass.gain.5se,mass.gain.stop,mass.gain.stse,log.mass.gain.stop,log.mass.gain.stse)

avg$id<- c("20_c", "20_p", "25_c","25_p","30_c","30_p")
View(avg) #This isn't in the right format for the graph I want--need long format??


write.csv(avg,"cpt avg data.csv",row.names = FALSE)



#Making long format??

data_age<- melt(avg, id.vars=c("id","temp","treat"), measure.vars=c("age.3", "age.4", "age.5", "age.stop"), variable.name="instar", value.name="age")
View(data_age)
data_age$instar<- gsub("age.", "",data_age$instar)


data_age_se<- melt(avg, id.vars=c("id","temp","treat"), measure.vars=c("age.3se", "age.4se", "age.5se", "age.stse"), variable.name="instar", value.name="age.se")
View(data_age_se)
data_age_se$instar<- gsub("age.", "",data_age_se$instar)
data_age_se$instar<- gsub("se", "",data_age_se$instar)
data_age_se$instar<- gsub("st", "stop",data_age_se$instar)


data_mass<-melt(avg,id.vars=c("id","temp","treat"), measure.vars=c("mass.3","mass.4","mass.5","mass.stop"),variable.name="instar", value.name="mass")
View(data_mass)
data_mass$instar<-gsub("mass.","",data_mass$instar)


data_mass_se<- melt(avg, id.vars=c("id","temp","treat"), measure.vars=c("mass.3se", "mass.4se", "mass.5se", "mass.stse"), variable.name="instar", value.name="mass.se")
View(data_mass_se)
data_mass_se$instar<- gsub("mass.", "",data_mass_se$instar)
data_mass_se$instar<- gsub("se", "",data_mass_se$instar)
data_mass_se$instar<- gsub("st", "stop",data_mass_se$instar)


data_logmass<-melt(avg,id.vars=c("id","temp","treat"), measure.vars=c("log.mass3","log.mass4","log.mass5","log.mass.stop"),variable.name="instar", value.name="log.mass")
View(data_logmass)
data_logmass$instar<- gsub("log.mass", "",data_logmass$instar)
data_logmass$instar<- gsub(".stop", "stop",data_logmass$instar)


data_logmass_se<-melt(avg,id.vars=c("id","temp","treat"), measure.vars=c("log.mass3.se","log.mass4.se","log.mass5.se","log.mass.stse"),variable.name="instar", value.name="log.mass.se")
View(data_logmass_se)
data_logmass_se$instar<- gsub("log.mass", "",data_logmass_se$instar)
data_logmass_se$instar<- gsub(".se", "",data_logmass_se$instar)
data_logmass_se$instar<- gsub(".s", "stop",data_logmass_se$instar)


data_cnsmp<-melt(avg,id.vars=c("id","temp","treat"), measure.vars=c("cnsmp.3","cnsmp.4","cnsmp.5","cnsmp.stop"),variable.name="instar", value.name="cnsmp")
data_cnsmp$instar<-gsub("cnsmp.","",data_cnsmp$instar)
View(data_cnsmp)


data_cnsmp_se<- melt(avg, id.vars=c("id","temp","treat"), measure.vars=c("cnsmp.3se", "cnsmp.4se", "cnsmp.5se", "cnsmp.stse"), variable.name="instar", value.name="cnsmp.se")
View(data_cnsmp_se)
data_cnsmp_se$instar<- gsub("cnsmp.", "",data_cnsmp_se$instar)
data_cnsmp_se$instar<- gsub("se", "",data_cnsmp_se$instar)
data_cnsmp_se$instar<- gsub("st", "stop",data_cnsmp_se$instar)


data_logcnsmp<-melt(avg,id.vars=c("id","temp","treat"), measure.vars=c("log.cnsmp3","log.cnsmp4","log.cnsmp5","log.cnsmp.stop"),variable.name="instar", value.name="log.cnsmp")
View(data_logcnsmp)
data_logcnsmp$instar<- gsub("log.cnsmp", "",data_logcnsmp$instar)
data_logcnsmp$instar<- gsub(".stop", "stop",data_logcnsmp$instar)


data_logcnsmp_se<-melt(avg,id.vars=c("id","temp","treat"), measure.vars=c("log.cnsmp3.se","log.cnsmp4.se","log.cnsmp5.se","log.cnsmp.stse"),variable.name="instar", value.name="log.cnsmp.se")
View(data_logcnsmp_se)
data_logcnsmp_se$instar<- gsub("log.cnsmp", "",data_logcnsmp_se$instar)
data_logcnsmp_se$instar<- gsub(".se", "",data_logcnsmp_se$instar)
data_logcnsmp_se$instar<- gsub(".s", "stop",data_logcnsmp_se$instar)


data_mg<-melt(avg,id.vars=c("id","temp","treat"), measure.vars=c("mass.gain.3","mass.gain.4","mass.gain.5","mass.gain.stop"),variable.name="instar", value.name="mass.gain")
View(data_mg)
data_mg$instar<-gsub("mass.gain.","",data_mg$instar)


data_mg_se<-melt(avg,id.vars=c("id","temp","treat"), measure.vars=c("mass.gain.3se","mass.gain.4se","mass.gain.5se","mass.gain.stse"),variable.name="instar", value.name="mass.gain.se")
View(data_mg_se)
data_mg_se$instar<-gsub("mass.gain.","",data_mg_se$instar)
data_mg_se$instar<-gsub("se","",data_mg_se$instar)
data_mg_se$instar<-gsub("st","stop",data_mg_se$instar)


data_logmg<-melt(avg,id.vars=c("id","temp","treat"), measure.vars=c("log.mass.gain.3","log.mass.gain.4","log.mass.gain.5","log.mass.gain.stop"),variable.name="instar", value.name="log.mg")
View(data_logmg)
data_logmg$instar<-gsub("log.mass.gain.","",data_logmg$instar)




data_logmg_se<-melt(avg,id.vars=c("id","temp","treat"), measure.vars=c("log.mass.gain.3se","log.mass.gain.4se","log.mass.gain.5se","log.mass.gain.stse"),variable.name="instar", value.name="log.mg.se")
View(data_logmg_se)
data_logmg_se$instar<-gsub("mass.gain.","",data_logmg_se$instar)
data_logmg_se$instar<-gsub("se","",data_logmg_se$instar)
data_logmg_se$instar<-gsub("st","stop",data_logmg_se$instar)
data_logmg_se$instar<-gsub("log.","",data_logmg_se$instar)



data1<-merge(data_age,data_age_se)
data2<-merge(data_mass,data_mass_se)
data3<-merge(data_cnsmp,data_cnsmp_se)

data4<-merge(data_logmass,data_logmass_se)
data5<-merge(data_logcnsmp,data_logcnsmp_se)

data6<-merge(data_mg,data_mg_se)
data7<-merge(data_logmg,data_logmg_se)

data8<-merge(data1,data2)
data9<-merge(data3,data4)
data10<-merge(data6,data7)
data11<-merge(data8,data9)
data12<-merge(data10,data11)


avg.long<-merge(data5,data12)
View(avg.long)

write.csv(avg.long,"cpt avg long.csv",row.names = FALSE)



avg.30<-subset(avg.long,temp=="30")
avg.20<-subset(avg.long,temp=="20")
avg.25<-subset(avg.long,temp=="25")


#Plotting avg mass X avg age

theme_set(theme_classic(base_size=14))


#Facet by temp

avg.plot <- ggplot(avg.long, aes(x=age,y=log(mass),colour=treat,group=treat))
avg.plot<-avg.plot+geom_point()+geom_errorbar(aes(ymin=log.mass-log.mass.se,ymax=log.mass+log.mass.se),size=.5
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



avg.plot



#Facet by treatment
avg.plot <- ggplot(avg.long, aes(x=age,y=log.mass,colour=temp,group=temp))
avg.plot<-avg.plot+geom_point()+geom_errorbar(aes(ymin=log.mass-log.mass.se,ymax=log.mass+log.mass.se),width=.2
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



avg.plot








#Plotting average log mass gain (since 3rd instar) by average age (since 3rd instar)

#Facet by treatment
avg.plot <- ggplot(avg.long, aes(x=age,y=log.mg,colour=temp,group=temp))
avg.plot<-avg.plot+geom_point()+geom_errorbar(aes(ymin=log.mg-log.mg.se,ymax=log.mg+log.mg.se),width=.2
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

avg.plot



#Facet by temp

avg.plot <- ggplot(avg.long, aes(x=age,y=log.mg,colour=treat,group=treat))
avg.plot<-avg.plot+geom_point(size=1.8)+geom_errorbar(aes(ymin=log.mg-log.mg.se,ymax=log.mg+log.mg.se),width=3
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



avg.plot









#Plotting avg cnsmp by avg age



#Facet by temp

avg.plot <- ggplot(avg.long, aes(x=age,y=log.cnsmp,colour=treat,group=treat))
avg.plot<-avg.plot+geom_point(size=1.8)+geom_errorbar(aes(ymin=log.cnsmp-log.cnsmp.se,ymax=log.cnsmp+log.cnsmp.se),width=1
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

avg.plot



#cnsmp not logged

avg.plot <- ggplot(avg.long, aes(x=age,y=cnsmp,colour=treat,group=treat))
avg.plot<-avg.plot+geom_point()+geom_errorbar(aes(ymin=cnsmp-cnsmp.se,ymax=cnsmp+cnsmp.se),size=.5
)+geom_errorbarh(aes(xmin=age-age.se,xmax=age+age.se),size=.5
)+geom_line(aes(linetype=treat,color=treat),size=1
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+labs(title="Avg cnsmp X avg age",x="avg age [hour]",
       y="Avg cnsmp [mg]"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1)
)+facet_wrap(~temp)



avg.plot







#Facet by treatment
avg.plot <- ggplot(avg.long, aes(x=age,y=log.cnsmp,colour=temp,group=temp))
avg.plot<-avg.plot+geom_point()+geom_errorbar(aes(ymin=log.cnsmp-log.cnsmp.se,ymax=log.cnsmp+log.cnsmp.se),width=.2
)+geom_errorbarh(aes(xmin=age-age.se,xmax=age+age.se)
)+geom_line(aes(linetype=temp,color=temp),size=1
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"), 
                     name="Temperature [C]",breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Temperature [C]",breaks=c("20","25","30"),
                          labels=c("20","25","30")
#)+coord_cartesian(ylim=c(3, 9)
)+labs(title="Avg log(cnsmp) X avg age",x="avg age [hour]",
       y="Avg log(cnsmp) [mg]"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1)
)+facet_wrap(~treat)

avg.plot










avg.plot <- ggplot(avg.long, aes(x=age,y=log(cnsmp),colour=temp,group=temp))
avg.plot<-avg.plot+geom_point(#)+geom_errorbar(aes(ymin=log(cnsmp)-log(cnsmp.se),ymax=log(cnsmp)+log(cnsmp.se)),width=.2
#)+geom_errorbarh(aes(xmin=age-age.se,xmax=age+age.se)
)+geom_line(aes(linetype=temp,color=temp),size=1
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"), 
                     name="Temperature [C]",breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Temperature [C]",breaks=c("20","25","30"),
                          labels=c("20","25","30")
#)+scale_y_continuous(limits=c(0,9),breaks = c(0,2,4,6,8)
)+labs(title="Avg log(cnsmp) X avg age",x="avg age [hour]",
       y="Avg log(cnsmp) [mg]"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1)
)+facet_wrap(~treat)

avg.plot







#Csnmp not logged

avg.plot <- ggplot(avg.long, aes(x=age,y=cnsmp,colour=temp,group=temp))
avg.plot<-avg.plot+geom_point()+geom_errorbar(aes(ymin=cnsmp-cnsmp.se,ymax=cnsmp+cnsmp.se),width=.2
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

avg.plot




#Mass by consumption

#Facet by treatment
avg.plot <- ggplot(avg.long, aes(x=log.cnsmp,y=log.mass,colour=temp,group=temp))
avg.plot<-avg.plot+geom_point()+geom_errorbar(aes(ymin=log.mass-log.mass.se,ymax=log.mass+log.mass.se),width=.2
)+geom_errorbarh(aes(xmin=log.cnsmp-log.cnsmp.se,xmax=log.cnsmp+log.cnsmp.se)
)+geom_line(aes(linetype=temp,color=temp),size=1
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"), 
                     name="Temperature [C]",breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Temperature [C]",breaks=c("20","25","30"),
                          labels=c("20","25","30")
                          #)+coord_cartesian(ylim=c(3, 9)
)+labs(title="Avg log(mass) X avg log(mass)",x="avg log(cnsmp)",
       y="Avg log(mass)"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1)
)+facet_wrap(~treat)

avg.plot





#Facet by temperature

avg.plot <- ggplot(avg.long, aes(x=log.cnsmp,y=log.mass,colour=treat,group=treat))
avg.plot<-avg.plot+geom_point()+geom_errorbar(aes(ymin=log.mass-log.mass.se,ymax=log.mass+log.mass.se),width=.2
)+geom_errorbarh(aes(xmin=log.cnsmp-log.cnsmp.se,xmax=log.cnsmp+log.cnsmp.se)
)+geom_line(aes(linetype=treat,color=treat),size=1
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+labs(title="Avg log(mass) X avg log(mass)",x="avg log(cnsmp)",
       y="Avg log(mass)"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1)
)+facet_wrap(~temp)

avg.plot




#Mass gain by consumption

#Facet by treatment
avg.plot <- ggplot(avg.long, aes(x=log.cnsmp,y=log.mg,colour=temp,group=temp))
avg.plot<-avg.plot+geom_point(size=2)+geom_errorbar(aes(ymin=log.mg-log.mg.se,ymax=log.mg+log.mg.se),width=.5
)+geom_errorbarh(aes(xmin=log.cnsmp-log.cnsmp.se,xmax=log.cnsmp+log.cnsmp.se)
)+geom_line(aes(linetype=temp,color=temp),size=1.5
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"), 
                     name="Temperature [C]",breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Temperature [C]",breaks=c("20","25","30"),
                          labels=c("20","25","30")
                          #)+coord_cartesian(ylim=c(3, 9)
)+labs(title="Avg mass gain X avg consumption",x="avg log(cnsmp)",
       y="Avg log(mass gain)"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1)
)+facet_wrap(~treat)

avg.plot




#Facet by temp
avg.plot <- ggplot(avg.long, aes(x=log.cnsmp,y=log.mg,colour=treat,group=treat))
avg.plot<-avg.plot+geom_point(size=2)+geom_errorbar(aes(ymin=log.mg-log.mg.se,ymax=log.mg+log.mg.se),width=.5
)+geom_errorbarh(aes(xmin=log.cnsmp-log.cnsmp.se,xmax=log.cnsmp+log.cnsmp.se)
)+geom_line(aes(linetype=treat,color=treat),size=1.5
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasizited"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
                          #)+coord_cartesian(ylim=c(3, 9)
)+labs(title="Avg mass gain X avg consumption",x="avg log(cnsmp)",
       y="Avg log(mass gain)"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1)
)+facet_wrap(~temp)

avg.plot






#Testing to see if age/mass at em affected diff by temp
#Do this with avg or all data?


avg.para<-subset(avg,treat=="para")
View(avg.para)


data.para<-subset(data,treatment=="para")


p.mod<-lm(mass.befem~temp,data=data.para) 
anova(p.mod)

p.mod1<-lm(age.em~temp,data=data.para)
anova(p.mod1)


p.mod2<-lm(mass.befem~temp*num.em,data=data.para)  #interaction b/t temp and num.em not sig when temp is factor
anova(p.mod2)


p.mod3<-lm(mass.befem~temp+num.em,data=data.para)
anova(p.mod3)


#Plotting mass at em vs num.em scatterplot

em.plot <- ggplot(data.para, aes(x=num.em,y=log(mass.befem),colour=as.factor(temp),
                                 group=as.factor(temp)))
em.plot<-em.plot+geom_point(
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"), 
                     name="Temp",breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+labs(title="Em log(mass) X num.em",x="Number emerged",
       y="log(mass)"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1))


em.plot



#Plotting log(mass)Xnum.em w/lines

em.plot <- ggplot(data.para, aes(x=num.em,y=mass.befem,colour=as.factor(temp),
                                 group=as.factor(temp)))
em.plot<-em.plot+geom_point(
)+geom_smooth(se=FALSE
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"), 
                     name="Temp",breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+labs(title="Em mass X num.em",x="Number emerged",
       y="mass [mm]"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1))


em.plot



#Plotting age at em vs num.em scatterplot

em.plot <- ggplot(data.para, aes(x=num.em,y=age.em,colour=as.factor(temp),
                                 group=as.factor(temp)))
em.plot<-em.plot+geom_point(
)+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"), 
                     name="Temp",breaks=c("20","25","30"),
                     labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+labs(title="Em age X num.em",x="Number emerged",
       y="age.em"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1))


em.plot


#Modelling age X num.em

age.mod<-lm(age.em~num.em*temp,data=data.para)
anova(age.mod)
summary(age.mod)

#All preds sig when temp is factor--num.em, temp, and num.em:temp



devavg.plot<-ggplot(avg,aes(x=temp,y=age.stop,group=treat,color=treat))
devavg.plot+geom_point(size=2)+geom_line(aes(linetype=treat,color=treat),size=1.5
          )+geom_errorbar(aes(ymin=age.stop-age.stse,ymax=age.stop+age.stse),width=.3
          )+scale_color_manual(values=c("black","red"), 
                       name="Treatment",breaks=c("control","para"),
                       labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
          )+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                                     labels=c("Control","Parasitized")
          )+labs(title="Age at wandering or emergence",x="Temperature [C]",
                  y="Average age [hour]"
          )+theme(axis.title.y=element_text(vjust=1.5),
                  axis.title.x=element_text(vjust=-.35),
                  plot.title=element_text(face="bold",vjust=1))

