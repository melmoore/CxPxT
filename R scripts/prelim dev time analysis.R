#Model of time to 3rd instar


tt3.mod<-lm(timeto3~temp*treatment,data=para,na.action = na.omit)
anova(tt3.mod)



#Model of time to 4th instar

#tt4.mod<-lme(timeto4~temp*treatment*num.em,random=~1|bug.id,data=para,method='ML',na.action = na.omit)

tt4.mod<-lm(timeto4~temp*treatment*num.em,data=para,na.action = na.omit)
anova(tt4.mod)

tt4.mod2<-lm(timeto4~temp*treatment+num.em,data=para,na.action = na.omit)
anova(tt4.mod2)


anova(tt4.mod,tt4.mod2)





#Avg tt4 plot by temp


tt4.sum<-summarySE(para, measurevar="timeto4", 
                   groupvars=c("temp","treatment"),na.rm=TRUE)
tt4.sum


tt4plot <- ggplot(tt4.sum, aes(x=temp,y=timeto4,group=treatment,color=treatment))
tt4plot<-tt4plot+geom_point()+geom_errorbar(aes(ymin=timeto4-se,ymax=timeto4+se),width=.2
)+geom_line(aes(linetype=treatment,color=treatment)
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+labs(title="Dev time to 4th instar",x="Temperature [C]",
       y="Caterpillar dev [days]"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1))

tt4plot




tt4plot<-ggplot(para,aes(x=num.em,y=timeto4,group=temp,color=temp))
tt4plot<-tt4plot+geom_point()+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"), 
                                                 name="Temperature [C]",breaks=c("20","25","30"),
                                                 labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+geom_jitter()+geom_smooth(method=lm,se=FALSE)
tt4plot




#time to 5 model

tt5.mod<-lm(timeto5~temp*treatment*num.em,data=para,na.action = na.omit)
anova(tt5.mod)

tt5.mod2<-lm(timeto5~temp*treatment+num.em,data=para,na.action = na.omit)
anova(tt5.mod2)


anova(tt5.mod,tt5.mod2)



#Avg dev time to 5th instar plot

tt5.sum<-summarySE(para, measurevar="timeto5", 
                   groupvars=c("temp","treatment"),na.rm=TRUE)
tt5.sum


tt5plot <- ggplot(tt5.sum, aes(x=temp,y=timeto5,group=treatment,color=treatment))
tt5plot<-tt5plot+geom_point()+geom_errorbar(aes(ymin=timeto5-se,ymax=timeto5+se),width=.2
)+geom_line(aes(linetype=treatment,color=treatment)
)+scale_color_manual(values=c("black","red"), 
                     name="Treatment",breaks=c("control","para"),
                     labels=c("Control","Parasitized"),guide=guide_legend(keywidth=1.8)
)+scale_linetype_discrete(name="Treatment",breaks=c("control","para"),
                          labels=c("Control","Parasitized")
)+labs(title="Dev time to 5th instar",x="Temperature [C]",
       y="Caterpillar dev [days]"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1))

tt5plot



#Dev time to wander model

control<-subset(para,treatment=="control")

ttw.mod<-lm(ttwand~temp,data=control,na.action = na.omit)
anova(ttw.mod)


ttw.plot<-ggplot(control,aes(x=temp,y=ttwand))
ttw.plot<-ttw.plot+geom_boxplot()
ttw.plot



#wasp dev time model:

wm.mod<-lm(int.wasp.dev~temp*num.em,data=para,na.action = na.omit)
anova(wm.mod)
summary(wm.mod)


#Plotting wasp dev 

wm.sum<-summarySE(para, measurevar="int.wasp.dev", 
                  groupvars=c("temp"),na.rm=TRUE)
wm.sum


#Avg plot

wmplot <- ggplot(wm.sum, aes(x=temp,y=int.wasp.dev,group=1))
wmplot<-wmplot+geom_point()+geom_errorbar(aes(ymin=int.wasp.dev-se,ymax=int.wasp.dev+se),width=.2
)+geom_line(
)+labs(title="Wasp dev",x="Temperature [C]",
       y="wasp dev [days]"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1))

wmplot



#Box plot

wmplot <- ggplot(para, aes(x=temp,y=int.wasp.dev))
wmplot<-wmplot+geom_boxplot(#)+geom_errorbar(aes(ymin=int.wasp.dev-int.wasp.dev.se,ymax=int.wasp.dev+int.wasp.dev.se),width=.2
  #)+geom_errorbarh(aes(xmin=temp-temp.se,xmax=temp+temp.se)
)+labs(title="wasp dev time",x="temperature",
       y="Wasp dev time [days]"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1))
wmplot


#scatterplot wasp dev by num.em (load proxy)

wmplot<-ggplot(para,aes(x=num.em,y=int.wasp.dev,group=temp,color=temp))
wmplot<-wmplot+geom_point()+scale_color_manual(values=c("#56B4E9","#000000","#E69F00"), 
                                               name="Temperature [C]",breaks=c("20","25","30"),
                                               labels=c("20","25","30"),guide=guide_legend(keywidth=1.8)
)+geom_jitter()+geom_smooth(method=lm,se=FALSE)
wmplot


#Avg wasp dev by avg num em

numem.sum<-summarySE(para, measurevar="num.em", 
                     groupvars=c("temp"),na.rm=TRUE)
numem.sum

wm.sum$num.em<-numem.sum$num.em
wm.sum$num.em.se<-numem.sum$se


wmplot <- ggplot(wm.sum, aes(x=num.em,y=int.wasp.dev,group=temp))
wmplot<-wmplot+geom_point()+geom_boxplot(
)+labs(title="avg wasp dev by avg num em",x="avg num em",
       y="avg wasp dev [days]"
)+theme(axis.title.y=element_text(vjust=1.5),
        axis.title.x=element_text(vjust=-.35),
        plot.title=element_text(face="bold",vjust=1))

wmplot  #Not working, deal with later







#stage specific late larval mortality model

paratrt<-subset(para,treatment=="para")

stsllmort.mod<-lm(stsp.llmort~temp*num.em,data=paratrt,na.action = na.omit)
anova(stsllmort.mod)
summary(stsllmort.mod)

ssllm.plot<-ggplot(paratrt,aes(x=temp,y=stsp.llmort))
ssllm.plot<-ssllm.plot+geom_boxplot()
ssllm.plot





