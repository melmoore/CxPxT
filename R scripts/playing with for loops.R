library(datasets)
library(dplyr)
chickwts


feeds<-unique(chickwts$feed)

output<-data.frame(meanfeed=NULL,cat=NULL)

for(f in feeds){
  meanfeed<-mean(chickwts$weight[chickwts$feed==f])
  cat=f
  temp=data.frame(meanfeed=meanfeed, cat=cat)
  output=rbind(output,temp)
}


cpt_gr_long[is.na(cpt_gr_long)] = 0


time<-unique(cpt_gr_long$Timepoint)
treatments<-unique(cpt_gr_long$treatment)
temps<-unique(cpt_gr_long$temp)

tr_tempmass<-data.frame(mean_mass=NULL,t=NULL,tr=NULL,temp=NULL)

for(t in time){
  for(tr in treatments){
    for(C in temps){
  mean_mass<-mean(cpt_gr_long$mass)
  df<-data.frame(mean_mass=mean_mass,t=t,tr=treatment,temp=C)
  tr_tempmass<-rbind(df,tr_tempmass)
}}}

head(tr_tempmass)
View(tr_tempmass)





















































