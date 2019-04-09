#Here’s the data file for manduca short-term growth experiment.  The datasheet you want (cleaned and renamed, in wide form) is labeled WideData1 .  What we want is:

#1. Put data for larval measurements in long format.  There are fields Date.zh, Time.zh, Mass.zh, Fed.zh, where z is a number (integer) from 0 to 144. I’d like to create a new variable Hours whose value = z, and put the data in long form, i.e.
# BugID    Temp.rear           Temp.test           Hours    Date      Time      Fed        Mass  (and either repeat or drop the rest of the fields for now—let’s figure these out later)
# 2.Plot ln(Mass) vs Hours for each individual (maybe include mean line as well) for each Temp.test as a separate panel.

library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(gridExtra)


data<- read.csv("Msexta.GRE.Summer2016.under2g.csv", header=TRUE, stringsAsFactors = FALSE)
names(data)
with(data,plot(Temp.test,Mass.0h))
head(data)

#Reshaping each column and getting hour into numeric
data_Date<- melt(data, id.vars=c(  "BugID","Temp.rear",   "Temp.test"), measure.vars=c("Date.0h", "Date.6h", "Date.24h", "Date.30h", "Date.48h", "Date.54h", "Date.72h", "Date.96h", "Date.120h", "Date.144h"), variable.name="Hour", value.name="Date")
data_Date$Hour<- gsub("Date.", "",data_Date$Hour)
data_Date$Hour<- as.numeric(gsub("h", "", data_Date$Hour)) 

data_Time<- melt(data, id.vars=c(  "BugID","Temp.rear",   "Temp.test"), measure.vars=c("Time.0h", "Time.6h", "Time.24h", "Time.30h", "Time.48h", "Time.54h", "Time.72h", "Time.96h", "Time.120h", "Time.144h"), variable.name="Hour", value.name="Time")
data_Time$Hour<- gsub("Time.", "",data_Time$Hour)
data_Time$Hour<- as.numeric(gsub("h", "", data_Time$Hour))

data_Fed<- melt(data, id.vars=c(  "BugID", "Temp.rear",   "Temp.test"), measure.vars=c("Fed.0h","Fed.6h","Fed.24h", "Fed.30h", "Fed.48h", "Fed.54h", "Fed.72h", "Fed.96h", "Fed.120h", "Fed.144h"), variable.name="Hour", value.name="Fed")
data_Fed$Hour<- gsub("Fed.", "",data_Fed$Hour)
data_Fed$Hour<- as.numeric(gsub("h", "", data_Fed$Hour))
data_Fed$Fed<- factor(data_Fed$Fed)

data_Mass<- melt(data, id.vars=c("BugID", "Temp.rear",   "Temp.test"), measure.vars=c("Mass.0h", "Mass.6h", "Mass.24h", "Mass.30h", "Mass.48h", "Mass.54h", "Mass.72h", "Mass.96h", "Mass.120h", "Mass.144h"), variable.name="Hour", value.name="Mass")
data_Mass$Hour<- gsub("Mass.", "",data_Mass$Hour)
data_Mass$Hour<- as.numeric(gsub("h", "", data_Mass$Hour))
data_Mass$Mass<- as.numeric(data_Mass$Mass)

#Merging the data back together into one data.frame
data_1<- merge(data_Date, data_Time)
data_2<- merge(data_Fed, data_Mass)

data_full<- merge(data_1, data_2)
head(data_full)
summary(data_full)
data_full$Temp.test<- factor(data_full$Temp.test)
write.csv(data_full, "longform_Msexta_growth.Summer2016.smallonly.csv", row.names = FALSE, quote = FALSE)


##omit large initial 
#Making the plot
## 2.Plot ln(Mass) vs Hours for each individual (maybe include mean line as well) for each Temp.test as a separate panel.


k<- ggplot(data_full, aes(Hour, log(Mass), group=interaction(BugID, Temp.test), color=factor(Temp.test)))
k+  geom_line()+
  theme_bw() +  
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.border= element_blank(), axis.line.x = element_line(color="black", size = 1.5),axis.line.y = element_line(color="black", size = 1.5),text = element_text(size = 15))+
  scale_color_manual(values=c( "lightblue","darkblue", "magenta", "red", "orange"),guide=guide_legend(title="Test temperature"))+
  scale_x_continuous(breaks=c(0,6,24,30,48,54,72,96,120,144))

#make a version of this with 6 panels (all with the same y and x ranges):  one panel for each test temperature (individual curve), plus the last panel with mean curves for the different test temps.

#Making means
names(data_full)

data_means<- data_full %>% 
  group_by(Temp.test, Hour) %>% 
  summarize(Means=mean(Mass, na.rm=TRUE))

data_means$Mass<- data_means$Means


head(data_full)
data_full.1<- bind_rows(data_full, data_means)
names(data_full.1)

plot1<-k+facet_wrap(~Temp.test)+
  geom_line()+
  theme_bw() +  
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.border= element_blank(), axis.line.x = element_line(color="black", size = 1.5),axis.line.y = element_line(color="black", size = 1.5),text = element_text(size = 15))+
  scale_color_manual(values=c( "lightblue","darkblue", "magenta", "red", "orange"),guide=guide_legend(title="Test temperature"))+
  scale_x_continuous(breaks=c(0,6,24,30,48,54,72,96,120,144))+
  ylim(-.5,2.5)
plot1

l<- ggplot(data_means, aes(x=Hour, y=log(Mass), color=Temp.test))
plot2<-l+geom_line()+
  theme_bw() +  
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.border= element_blank(), axis.line.x = element_line(color="black", size = 1.5),axis.line.y = element_line(color="black", size = 1.5),text = element_text(size = 15))+
  scale_color_manual(values=c( "lightblue","darkblue", "magenta", "red", "orange"),guide=guide_legend(title="Test temperature"))+
  scale_x_continuous(breaks=c(0,6,24,30,48,54,72,96,120,144))+
  ylim(-.5,2.5)
plot2

#Still working on this
grid.newpage()
pushViewport(viewport(layout = grid.layout(3,2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(plot1, vp = vplayout(1:3, 1:2))  # key is to define vplayout
print(plot2, vp = vplayout(3, 2))



