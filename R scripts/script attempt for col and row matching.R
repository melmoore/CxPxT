library(dplyr)
library(readr)

df$value <- df[-ncol(df)][cbind(1:nrow(df), match(df$column_name, colnames(df)))]
df$value


wide <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt gr wide.csv", 
                 col_types = cols(temp = col_factor(levels = c("20", 
                 "25", "30")), treatment = col_factor(levels = c("control", 
                 "para"))))
View(wide)


#Can do all of this with select from dplyr--keeps only the variables you specify, while renaming them

keepvars<-c("num","bug.id","timepoint.4","dry.totcnsmp.T1","dry.totcnsmp.T2","dry.totcnsmp.T3","dry.totcnsmp.T4","dry.totcnsmp.T5","dry.totcnsmp.T6","dry.totcnsmp.T7","dry.totcnsmp.T8","dry.totcnsmp.T9","dry.totcnsmp.T10","dry.totcnsmp.T11","dry.totcnsmp.T12","dry.totcnsmp.T13","dry.totcnsmp.T14","dry.totcnsmp.T15","dry.totcnsmp.T16","dry.totcnsmp.T17","dry.totcnsmp.T18","dry.totcnsmp.T19","dry.totcnsmp.T20","dry.totcnsmp.T21","dry.totcnsmp.T22","dry.totcnsmp.T23","dry.totcnsmp.T24","dry.totcnsmp.T25","dry.totcnsmp.T26","dry.totcnsmp.T27","dry.totcnsmp.T28","dry.totcnsmp.T29","dry.totcnsmp.T30","dry.totcnsmp.T31","dry.totcnsmp.T32","dry.totcnsmp.T33","dry.totcnsmp.T34","dry.totcnsmp.T35","dry.totcnsmp.T36","dry.totcnsmp.T37")
sub<-wide[keepvars]
View(sub)
sub<-as.data.frame(sub)


sub<-rename(sub, T1=dry.totcnsmp.T1,T2=dry.totcnsmp.T2,T3=dry.totcnsmp.T3,T4=dry.totcnsmp.T4,T5=dry.totcnsmp.T5,T6=dry.totcnsmp.T6,T7=dry.totcnsmp.T7,T8=dry.totcnsmp.T8,T9=dry.totcnsmp.T9,T10=dry.totcnsmp.T10,T11=dry.totcnsmp.T11,T12=dry.totcnsmp.T12,T13=dry.totcnsmp.T13,T14=dry.totcnsmp.T14,T15=dry.totcnsmp.T15,T16=dry.totcnsmp.T16,T17=dry.totcnsmp.T17,T18=dry.totcnsmp.T18,T19=dry.totcnsmp.T19,T20=dry.totcnsmp.T20,T21=dry.totcnsmp.T21,T22=dry.totcnsmp.T22,T23=dry.totcnsmp.T23,T24=dry.totcnsmp.T24,T25=dry.totcnsmp.T25,T26=dry.totcnsmp.T26,T27=dry.totcnsmp.T27,T28=dry.totcnsmp.T28,T29=dry.totcnsmp.T29,T30=dry.totcnsmp.T30,T31=dry.totcnsmp.T31,T32=dry.totcnsmp.T32,T33=dry.totcnsmp.T33,T34=dry.totcnsmp.T34,T35=dry.totcnsmp.T35,T36=dry.totcnsmp.T36,T37=dry.totcnsmp.T37)

sub$cnsmp.4 <- sub[][cbind(1:nrow(sub), match(sub$timepoint.4, colnames(sub)))]


keepvars2<-c("num","bug.id","cnsmp.4")
sub2<-sub[keepvars2]
sub2<-as.data.frame(sub2)


wide<-merge(wide,sub2,by=c("num","bug.id"))

wide<-arrange(wide, num)

wide[c("num", "cnsmp.4")]
sub2[c("num", "cnsmp.4")]




keepvars3<-c("num","bug.id","timepoint.5","dry.totcnsmp.T1","dry.totcnsmp.T2","dry.totcnsmp.T3","dry.totcnsmp.T4","dry.totcnsmp.T5","dry.totcnsmp.T6","dry.totcnsmp.T7","dry.totcnsmp.T8","dry.totcnsmp.T9","dry.totcnsmp.T10","dry.totcnsmp.T11","dry.totcnsmp.T12","dry.totcnsmp.T13","dry.totcnsmp.T14","dry.totcnsmp.T15","dry.totcnsmp.T16","dry.totcnsmp.T17","dry.totcnsmp.T18","dry.totcnsmp.T19","dry.totcnsmp.T20","dry.totcnsmp.T21","dry.totcnsmp.T22","dry.totcnsmp.T23","dry.totcnsmp.T24","dry.totcnsmp.T25","dry.totcnsmp.T26","dry.totcnsmp.T27","dry.totcnsmp.T28","dry.totcnsmp.T29","dry.totcnsmp.T30","dry.totcnsmp.T31","dry.totcnsmp.T32","dry.totcnsmp.T33","dry.totcnsmp.T34","dry.totcnsmp.T35","dry.totcnsmp.T36","dry.totcnsmp.T37")
sub3<-wide[keepvars3]
sub3<-as.data.frame(sub3)

sub3<-rename(sub3, T1=dry.totcnsmp.T1,T2=dry.totcnsmp.T2,T3=dry.totcnsmp.T3,T4=dry.totcnsmp.T4,T5=dry.totcnsmp.T5,T6=dry.totcnsmp.T6,T7=dry.totcnsmp.T7,T8=dry.totcnsmp.T8,T9=dry.totcnsmp.T9,T10=dry.totcnsmp.T10,T11=dry.totcnsmp.T11,T12=dry.totcnsmp.T12,T13=dry.totcnsmp.T13,T14=dry.totcnsmp.T14,T15=dry.totcnsmp.T15,T16=dry.totcnsmp.T16,T17=dry.totcnsmp.T17,T18=dry.totcnsmp.T18,T19=dry.totcnsmp.T19,T20=dry.totcnsmp.T20,T21=dry.totcnsmp.T21,T22=dry.totcnsmp.T22,T23=dry.totcnsmp.T23,T24=dry.totcnsmp.T24,T25=dry.totcnsmp.T25,T26=dry.totcnsmp.T26,T27=dry.totcnsmp.T27,T28=dry.totcnsmp.T28,T29=dry.totcnsmp.T29,T30=dry.totcnsmp.T30,T31=dry.totcnsmp.T31,T32=dry.totcnsmp.T32,T33=dry.totcnsmp.T33,T34=dry.totcnsmp.T34,T35=dry.totcnsmp.T35,T36=dry.totcnsmp.T36,T37=dry.totcnsmp.T37)

sub3$cnsmp.5 <- sub3[][cbind(1:nrow(sub3), match(sub3$timepoint.5, colnames(sub3)))]


keepvars3.5<-c("num","bug.id","cnsmp.5")
sub3.5<-sub3[keepvars3.5]
sub3.5<-as.data.frame(sub3.5)


wide<-merge(wide,sub3.5,by=c("num","bug.id"))

wide<-arrange(wide, num)

wide[c("num", "cnsmp.5")]
sub3[c("num", "cnsmp.5")]




keepvars4<-c("num","bug.id","timepoint.wander","dry.totcnsmp.T1","dry.totcnsmp.T2","dry.totcnsmp.T3","dry.totcnsmp.T4","dry.totcnsmp.T5","dry.totcnsmp.T6","dry.totcnsmp.T7","dry.totcnsmp.T8","dry.totcnsmp.T9","dry.totcnsmp.T10","dry.totcnsmp.T11","dry.totcnsmp.T12","dry.totcnsmp.T13","dry.totcnsmp.T14","dry.totcnsmp.T15","dry.totcnsmp.T16","dry.totcnsmp.T17","dry.totcnsmp.T18","dry.totcnsmp.T19","dry.totcnsmp.T20","dry.totcnsmp.T21","dry.totcnsmp.T22","dry.totcnsmp.T23","dry.totcnsmp.T24","dry.totcnsmp.T25","dry.totcnsmp.T26","dry.totcnsmp.T27","dry.totcnsmp.T28","dry.totcnsmp.T29","dry.totcnsmp.T30","dry.totcnsmp.T31","dry.totcnsmp.T32","dry.totcnsmp.T33","dry.totcnsmp.T34","dry.totcnsmp.T35","dry.totcnsmp.T36","dry.totcnsmp.T37")
sub4<-wide[keepvars4]
sub4<-as.data.frame(sub4)

sub4<-rename(sub4, T1=dry.totcnsmp.T1,T2=dry.totcnsmp.T2,T3=dry.totcnsmp.T3,T4=dry.totcnsmp.T4,T5=dry.totcnsmp.T5,T6=dry.totcnsmp.T6,T7=dry.totcnsmp.T7,T8=dry.totcnsmp.T8,T9=dry.totcnsmp.T9,T10=dry.totcnsmp.T10,T11=dry.totcnsmp.T11,T12=dry.totcnsmp.T12,T13=dry.totcnsmp.T13,T14=dry.totcnsmp.T14,T15=dry.totcnsmp.T15,T16=dry.totcnsmp.T16,T17=dry.totcnsmp.T17,T18=dry.totcnsmp.T18,T19=dry.totcnsmp.T19,T20=dry.totcnsmp.T20,T21=dry.totcnsmp.T21,T22=dry.totcnsmp.T22,T23=dry.totcnsmp.T23,T24=dry.totcnsmp.T24,T25=dry.totcnsmp.T25,T26=dry.totcnsmp.T26,T27=dry.totcnsmp.T27,T28=dry.totcnsmp.T28,T29=dry.totcnsmp.T29,T30=dry.totcnsmp.T30,T31=dry.totcnsmp.T31,T32=dry.totcnsmp.T32,T33=dry.totcnsmp.T33,T34=dry.totcnsmp.T34,T35=dry.totcnsmp.T35,T36=dry.totcnsmp.T36,T37=dry.totcnsmp.T37)

sub4$timepoint.wander<-gsub("T", "",sub4$timepoint.wander)
sub4$timepoint.wander<-as.numeric(sub4$timepoint.wander)-1
sub4$timepoint.wander<-gsub("^", "T",sub4$timepoint.wander)


sub4$cnsmp.wan <- sub4[][cbind(1:nrow(sub4), match(sub4$timepoint.wander, colnames(sub4)))]


keepvars4.5<-c("num","bug.id","cnsmp.wan")
sub4.5<-sub4[keepvars4.5]
sub4.5<-as.data.frame(sub4.5)


wide<-merge(wide,sub4.5,by=c("num","bug.id"))

wide<-arrange(wide, num)

wide[c("num", "cnsmp.wan")]
sub4[c("num", "cnsmp.wan")]




keepvars5<-c("num","bug.id","timepoint.em","dry.totcnsmp.T1","dry.totcnsmp.T2","dry.totcnsmp.T3","dry.totcnsmp.T4","dry.totcnsmp.T5","dry.totcnsmp.T6","dry.totcnsmp.T7","dry.totcnsmp.T8","dry.totcnsmp.T9","dry.totcnsmp.T10","dry.totcnsmp.T11","dry.totcnsmp.T12","dry.totcnsmp.T13","dry.totcnsmp.T14","dry.totcnsmp.T15","dry.totcnsmp.T16","dry.totcnsmp.T17","dry.totcnsmp.T18","dry.totcnsmp.T19","dry.totcnsmp.T20","dry.totcnsmp.T21","dry.totcnsmp.T22","dry.totcnsmp.T23","dry.totcnsmp.T24","dry.totcnsmp.T25","dry.totcnsmp.T26","dry.totcnsmp.T27","dry.totcnsmp.T28","dry.totcnsmp.T29","dry.totcnsmp.T30","dry.totcnsmp.T31","dry.totcnsmp.T32","dry.totcnsmp.T33","dry.totcnsmp.T34","dry.totcnsmp.T35","dry.totcnsmp.T36","dry.totcnsmp.T37")
sub5<-wide[keepvars5]
sub5<-as.data.frame(sub5)

sub5<-rename(sub5, T1=dry.totcnsmp.T1,T2=dry.totcnsmp.T2,T3=dry.totcnsmp.T3,T4=dry.totcnsmp.T4,T5=dry.totcnsmp.T5,T6=dry.totcnsmp.T6,T7=dry.totcnsmp.T7,T8=dry.totcnsmp.T8,T9=dry.totcnsmp.T9,T10=dry.totcnsmp.T10,T11=dry.totcnsmp.T11,T12=dry.totcnsmp.T12,T13=dry.totcnsmp.T13,T14=dry.totcnsmp.T14,T15=dry.totcnsmp.T15,T16=dry.totcnsmp.T16,T17=dry.totcnsmp.T17,T18=dry.totcnsmp.T18,T19=dry.totcnsmp.T19,T20=dry.totcnsmp.T20,T21=dry.totcnsmp.T21,T22=dry.totcnsmp.T22,T23=dry.totcnsmp.T23,T24=dry.totcnsmp.T24,T25=dry.totcnsmp.T25,T26=dry.totcnsmp.T26,T27=dry.totcnsmp.T27,T28=dry.totcnsmp.T28,T29=dry.totcnsmp.T29,T30=dry.totcnsmp.T30,T31=dry.totcnsmp.T31,T32=dry.totcnsmp.T32,T33=dry.totcnsmp.T33,T34=dry.totcnsmp.T34,T35=dry.totcnsmp.T35,T36=dry.totcnsmp.T36,T37=dry.totcnsmp.T37)

sub5$timepoint.em<-gsub("T", "",sub5$timepoint.em)
sub5$timepoint.em<-as.numeric(sub5$timepoint.em)-1
sub5$timepoint.em<-gsub("^", "T",sub5$timepoint.em)


sub5$cnsmp.em <- sub5[][cbind(1:nrow(sub5), match(sub5$timepoint.em, colnames(sub5)))]



keepvars5.5<-c("num","bug.id","cnsmp.em")
sub5.5<-sub5[keepvars5.5]
sub5.5<-as.data.frame(sub5.5)


wide<-merge(wide,sub5.5,by=c("num","bug.id"))

wide<-arrange(wide, num)

wide[c("num", "cnsmp.em")]
sub5[c("num", "cnsmp.em")]



write.csv(wide, "cpt gr wide.csv", row.names = FALSE, quote = FALSE)






