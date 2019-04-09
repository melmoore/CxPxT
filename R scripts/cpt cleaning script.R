#Cleaning CxPxT data

library(readr)
library(lubridate)
library(dplyr)

para <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/raw cpt.csv")
View(para)


#Remove comments column--messes up following columns

para$comments<-NULL



#Gets rid of sacrifice bugs

para<-para[!(para$sacrifice==1),]  #I think this actually just made the 1s equal 0. Come back to this and fix



#Parsing dates

para$date.hatch<-parse_date_time(para$date.hatch,orders='mdy')
para$date.died<-parse_date_time(para$date.died,orders='mdy')
para$date.3<-parse_date_time(para$date.3,orders='mdy')
para$date.ovp<-parse_date_time(para$date.ovp,orders='mdy')
para$date.4<-parse_date_time(para$date.4,orders='mdy')
para$date.5<-parse_date_time(para$date.5,orders='mdy')
para$date.wander<-parse_date_time(para$date.wander,orders='mdy')
para$date.em<-parse_date_time(para$date.em,orders='mdy')
para$date.T1<-parse_date_time(para$date.T1,orders='mdy')
para$date.T2<-parse_date_time(para$date.T2,orders='mdy')
para$date.T3<-parse_date_time(para$date.T3,orders='mdy')
para$date.T4<-parse_date_time(para$date.T4,orders='mdy')
para$date.T5<-parse_date_time(para$date.T5,orders='mdy')
para$date.T6<-parse_date_time(para$date.T6,orders='mdy')
para$date.T7<-parse_date_time(para$date.T7,orders='mdy')
para$date.T8<-parse_date_time(para$date.T8,orders='mdy')
para$date.T9<-parse_date_time(para$date.T9,orders='mdy')
para$date.T10<-parse_date_time(para$date.T10,orders='mdy')
para$date.T11<-parse_date_time(para$date.T11,orders='mdy')
para$date.T12<-parse_date_time(para$date.T12,orders='mdy')
para$date.T13<-parse_date_time(para$date.T13,orders='mdy')
para$date.T14<-parse_date_time(para$date.T14,orders='mdy')
para$date.T15<-parse_date_time(para$date.T15,orders='mdy')
para$date.T16<-parse_date_time(para$date.T16,orders='mdy')
para$date.T17<-parse_date_time(para$date.T17,orders='mdy')
para$date.T18<-parse_date_time(para$date.T18,orders='mdy')
para$date.T19<-parse_date_time(para$date.T19,orders='mdy')
para$date.T20<-parse_date_time(para$date.T20,orders='mdy')
para$date.T21<-parse_date_time(para$date.T21,orders='mdy')
para$date.T22<-parse_date_time(para$date.T22,orders='mdy')
para$date.T23<-parse_date_time(para$date.T23,orders='mdy')
para$date.T24<-parse_date_time(para$date.T24,orders='mdy')
para$date.T25<-parse_date_time(para$date.T25,orders='mdy')
para$date.T26<-parse_date_time(para$date.T26,orders='mdy')
para$date.T27<-parse_date_time(para$date.T27,orders='mdy')
para$date.T28<-parse_date_time(para$date.T28,orders='mdy')
para$date.T29<-parse_date_time(para$date.T29,orders='mdy')
para$date.T30<-parse_date_time(para$date.T30,orders='mdy')
para$date.T31<-parse_date_time(para$date.T31,orders='mdy')
para$date.T32<-parse_date_time(para$date.T32,orders='mdy')
para$date.T33<-parse_date_time(para$date.T33,orders='mdy')
para$date.T34<-parse_date_time(para$date.T34,orders='mdy')
para$date.T35<-parse_date_time(para$date.T35,orders='mdy')
para$date.T36<-parse_date_time(para$date.T36,orders='mdy')
para$date.T37<-parse_date_time(para$date.T37,orders='mdy')
para$date.T38<-parse_date_time(para$date.T38,orders='mdy')


#Converting to Julian days


para$date.ovp<-format(para$date.ovp, "%j")
para$date.ovp<-as.numeric(para$date.ovp)

para$date.em<-format(para$date.em, "%j")
para$date.em<-as.numeric(para$date.em)

para$date.hatch<-format(para$date.hatch,"%j")
para$date.hatch<-as.numeric(para$date.hatch)

para$date.died<-format(para$date.died,"%j")
para$date.died<-as.numeric(para$date.died)

para$date.3<-format(para$date.3,"%j")
para$date.3<-as.numeric(para$date.3)

para$date.4<-format(para$date.4,"%j")
para$date.4<-as.numeric(para$date.4)

para$date.5<-format(para$date.5,"%j")
para$date.5<-as.numeric(para$date.5)

para$date.wander<-format(para$date.wander,"%j")
para$date.wander<-as.numeric(para$date.wander)

para$date.T1<-format(para$date.T1,"%j")
para$date.T1<-as.numeric(para$date.T1)

para$date.T2<-format(para$date.T2,"%j")
para$date.T2<-as.numeric(para$date.T2)

para$date.T3<-format(para$date.T3,"%j")
para$date.T3<-as.numeric(para$date.T3)

para$date.T4<-format(para$date.T4,"%j")
para$date.T4<-as.numeric(para$date.T4)

para$date.T5<-format(para$date.T5,"%j")
para$date.T5<-as.numeric(para$date.T5)

para$date.T6<-format(para$date.T6,"%j")
para$date.T6<-as.numeric(para$date.T6)

para$date.T7<-format(para$date.T7,"%j")
para$date.T7<-as.numeric(para$date.T7)

para$date.T8<-format(para$date.T8,"%j")
para$date.T8<-as.numeric(para$date.T8)

para$date.T9<-format(para$date.T9,"%j")
para$date.T9<-as.numeric(para$date.T9)

para$date.T10<-format(para$date.T10,"%j")
para$date.T10<-as.numeric(para$date.T10)

para$date.T11<-format(para$date.T11,"%j")
para$date.T11<-as.numeric(para$date.T11)

para$date.T12<-format(para$date.T12,"%j")
para$date.T12<-as.numeric(para$date.T12)

para$date.T13<-format(para$date.T13,"%j")
para$date.T13<-as.numeric(para$date.T13)

para$date.T14<-format(para$date.T14,"%j")
para$date.T14<-as.numeric(para$date.T14)

para$date.T15<-format(para$date.T15,"%j")
para$date.T15<-as.numeric(para$date.T15)

para$date.T16<-format(para$date.T16,"%j")
para$date.T16<-as.numeric(para$date.T16)

para$date.T17<-format(para$date.T17,"%j")
para$date.T17<-as.numeric(para$date.T17)

para$date.T18<-format(para$date.T18,"%j")
para$date.T18<-as.numeric(para$date.T18)

para$date.T19<-format(para$date.T19,"%j")
para$date.T19<-as.numeric(para$date.T19)

para$date.T20<-format(para$date.T20,"%j")
para$date.T20<-as.numeric(para$date.T20)

para$date.T21<-format(para$date.T21,"%j")
para$date.T21<-as.numeric(para$date.T21)

para$date.T22<-format(para$date.T22,"%j")
para$date.T22<-as.numeric(para$date.T22)

para$date.T23<-format(para$date.T23,"%j")
para$date.T23<-as.numeric(para$date.T23)

para$date.T24<-format(para$date.T24,"%j")
para$date.T24<-as.numeric(para$date.T24)

para$date.T25<-format(para$date.T25,"%j")
para$date.T25<-as.numeric(para$date.T25)

para$date.T26<-format(para$date.T26,"%j")
para$date.T26<-as.numeric(para$date.T26)

para$date.T27<-format(para$date.T27,"%j")
para$date.T27<-as.numeric(para$date.T27)

para$date.T28<-format(para$date.T28,"%j")
para$date.T28<-as.numeric(para$date.T28)

para$date.T29<-format(para$date.T29,"%j")
para$date.T29<-as.numeric(para$date.T29)

para$date.T30<-format(para$date.T30,"%j")
para$date.T30<-as.numeric(para$date.T30)

para$date.T31<-format(para$date.T31,"%j")
para$date.T31<-as.numeric(para$date.T31)

para$date.T32<-format(para$date.T32,"%j")
para$date.T32<-as.numeric(para$date.T32)

para$date.T33<-format(para$date.T33,"%j")
para$date.T33<-as.numeric(para$date.T33)

para$date.T34<-format(para$date.T34,"%j")
para$date.T34<-as.numeric(para$date.T34)

para$date.T35<-format(para$date.T35,"%j")
para$date.T35<-as.numeric(para$date.T35)

para$date.T36<-format(para$date.T36,"%j")
para$date.T36<-as.numeric(para$date.T36)

para$date.T37<-format(para$date.T37,"%j")
para$date.T37<-as.numeric(para$date.T37)

para$date.T38<-format(para$date.T38,"%j")
para$date.T38<-as.numeric(para$date.T38)


#Calculating development times

para$int.wasp.dev<-para$date.em-para$date.ovp

para$timeto3<-para$date.3-para$date.hatch

para$int.3<-para$date.4-para$date.3

para$timeto4<-para$date.4-para$date.hatch

para$int.4<-para$date.5-para$date.4

para$timeto5<-para$date.5-para$date.hatch

para$int.5.p<-para$date.em-para$date.5
  
para$int.5.c<-para$date.wander-para$date.5
para$int.5.c[para$int.5.c<0]<-NA


para$int.5.c[is.na(para$int.5.c)] <- 0
para$int.5.p[is.na(para$int.5.p)] <- 0

para$int.5<-para$int.5.c+para$int.5.p

para$int.5[para$int.5==0]<-NA


para$ttwand<-para$date.wander-para$date.hatch


#Calculating wasp mortality metrics

para$stsp.llmort<-para$num.fail.spin/para$num.em






#Create new columns with initial caterpillar diet wet mass * the slope of the initial regression line (.227)


para$dry.cat.in.T0<-(para$diet.cat.in.T0*.227)
para$dry.cat.in.T1<-(para$diet.cat.in.T1*.227)
para$dry.cat.in.T2<-(para$diet.cat.in.T2*.227)
para$dry.cat.in.T3<-(para$diet.cat.in.T3*.227)
para$dry.cat.in.T4<-(para$diet.cat.in.T4*.227)
para$dry.cat.in.T5<-(para$diet.cat.in.T5*.227)
para$dry.cat.in.T6<-(para$diet.cat.in.T6*.227)
para$dry.cat.in.T7<-(para$diet.cat.in.T7*.227)
para$dry.cat.in.T8<-(para$diet.cat.in.T8*.227)
para$dry.cat.in.T9<-(para$diet.cat.in.T9*.227)
para$dry.cat.in.T10<-(para$diet.cat.in.T10*.227)
para$dry.cat.in.T11<-(para$diet.cat.in.T11*.227)
para$dry.cat.in.T12<-(para$diet.cat.in.T12*.227)
para$dry.cat.in.T13<-(para$diet.cat.in.T13*.227)
para$dry.cat.in.T14<-(para$diet.cat.in.T14*.227)
para$dry.cat.in.T15<-(para$diet.cat.in.T15*.227)
para$dry.cat.in.T16<-(para$diet.cat.in.T16*.227)
para$dry.cat.in.T17<-(para$diet.cat.in.T17*.227)
para$dry.cat.in.T18<-(para$diet.cat.in.T18*.227)
para$dry.cat.in.T19<-(para$diet.cat.in.T19*.227)
para$dry.cat.in.T20<-(para$diet.cat.in.T20*.227)
para$dry.cat.in.T21<-(para$diet.cat.in.T21*.227)
para$dry.cat.in.T22<-(para$diet.cat.in.T22*.227)
para$dry.cat.in.T23<-(para$diet.cat.in.T23*.227)
para$dry.cat.in.T24<-(para$diet.cat.in.T24*.227)
para$dry.cat.in.T25<-(para$diet.cat.in.T25*.227)
para$dry.cat.in.T26<-(para$diet.cat.in.T26*.227)
para$dry.cat.in.T27<-(para$diet.cat.in.T27*.227)
para$dry.cat.in.T28<-(para$diet.cat.in.T28*.227)
para$dry.cat.in.T29<-(para$diet.cat.in.T29*.227)
para$dry.cat.in.T30<-(para$diet.cat.in.T30*.227)
para$dry.cat.in.T31<-(para$diet.cat.in.T31*.227)
para$dry.cat.in.T32<-(para$diet.cat.in.T32*.227)
para$dry.cat.in.T33<-(para$diet.cat.in.T33*.227)
para$dry.cat.in.T34<-(para$diet.cat.in.T34*.227)
para$dry.cat.in.T35<-(para$diet.cat.in.T35*.227)
para$dry.cat.in.T36<-(para$diet.cat.in.T36*.227)
para$dry.cat.in.T37<-(para$diet.cat.in.T37*.227)



#Create new columns with initial control diet wet mass * the slope of the initial regression line (.227)


para$dry.cont.in.T0<-(para$diet.cont.in.T0*.227)
para$dry.cont.in.T1<-(para$diet.cont.in.T1*.227)
para$dry.cont.in.T2<-(para$diet.cont.in.T2*.227)
para$dry.cont.in.T3<-(para$diet.cont.in.T3*.227)
para$dry.cont.in.T4<-(para$diet.cont.in.T4*.227)
para$dry.cont.in.T5<-(para$diet.cont.in.T5*.227)
para$dry.cont.in.T6<-(para$diet.cont.in.T6*.227)
para$dry.cont.in.T7<-(para$diet.cont.in.T7*.227)
para$dry.cont.in.T8<-(para$diet.cont.in.T8*.227)
para$dry.cont.in.T9<-(para$diet.cont.in.T9*.227)
para$dry.cont.in.T10<-(para$diet.cont.in.T10*.227)
para$dry.cont.in.T11<-(para$diet.cont.in.T11*.227)
para$dry.cont.in.T12<-(para$diet.cont.in.T12*.227)
para$dry.cont.in.T13<-(para$diet.cont.in.T13*.227)
para$dry.cont.in.T14<-(para$diet.cont.in.T14*.227)
para$dry.cont.in.T15<-(para$diet.cont.in.T15*.227)
para$dry.cont.in.T16<-(para$diet.cont.in.T16*.227)
para$dry.cont.in.T17<-(para$diet.cont.in.T17*.227)
para$dry.cont.in.T18<-(para$diet.cont.in.T18*.227)
para$dry.cont.in.T19<-(para$diet.cont.in.T19*.227)
para$dry.cont.in.T20<-(para$diet.cont.in.T20*.227)
para$dry.cont.in.T21<-(para$diet.cont.in.T21*.227)
para$dry.cont.in.T22<-(para$diet.cont.in.T22*.227)
para$dry.cont.in.T23<-(para$diet.cont.in.T23*.227)
para$dry.cont.in.T24<-(para$diet.cont.in.T24*.227)
para$dry.cont.in.T25<-(para$diet.cont.in.T25*.227)
para$dry.cont.in.T26<-(para$diet.cont.in.T26*.227)
para$dry.cont.in.T27<-(para$diet.cont.in.T27*.227)
para$dry.cont.in.T28<-(para$diet.cont.in.T28*.227)
para$dry.cont.in.T29<-(para$diet.cont.in.T29*.227)
para$dry.cont.in.T30<-(para$diet.cont.in.T30*.227)
para$dry.cont.in.T31<-(para$diet.cont.in.T31*.227)
para$dry.cont.in.T32<-(para$diet.cont.in.T32*.227)
para$dry.cont.in.T33<-(para$diet.cont.in.T33*.227)
para$dry.cont.in.T34<-(para$diet.cont.in.T34*.227)
para$dry.cont.in.T35<-(para$diet.cont.in.T35*.227)
para$dry.cont.in.T36<-(para$diet.cont.in.T36*.227)
para$dry.cont.in.T37<-(para$diet.cont.in.T37*.227)




#Create new columns with final caterpillar diet wet mass * the slope of the final regression line (.237)


para$dry.cat.out.T0<-(para$diet.cat.out.T0*.237)
para$dry.cat.out.T1<-(para$diet.cat.out.T1*.237)
para$dry.cat.out.T2<-(para$diet.cat.out.T2*.237)
para$dry.cat.out.T3<-(para$diet.cat.out.T3*.237)
para$dry.cat.out.T4<-(para$diet.cat.out.T4*.237)
para$dry.cat.out.T5<-(para$diet.cat.out.T5*.237)
para$dry.cat.out.T6<-(para$diet.cat.out.T6*.237)
para$dry.cat.out.T7<-(para$diet.cat.out.T7*.237)
para$dry.cat.out.T8<-(para$diet.cat.out.T8*.237)
para$dry.cat.out.T9<-(para$diet.cat.out.T9*.237)
para$dry.cat.out.T10<-(para$diet.cat.out.T10*.237)
para$dry.cat.out.T11<-(para$diet.cat.out.T11*.237)
para$dry.cat.out.T12<-(para$diet.cat.out.T12*.237)
para$dry.cat.out.T13<-(para$diet.cat.out.T13*.237)
para$dry.cat.out.T14<-(para$diet.cat.out.T14*.237)
para$dry.cat.out.T15<-(para$diet.cat.out.T15*.237)
para$dry.cat.out.T16<-(para$diet.cat.out.T16*.237)
para$dry.cat.out.T17<-(para$diet.cat.out.T17*.237)
para$dry.cat.out.T18<-(para$diet.cat.out.T18*.237)
para$dry.cat.out.T19<-(para$diet.cat.out.T19*.237)
para$dry.cat.out.T20<-(para$diet.cat.out.T20*.237)
para$dry.cat.out.T21<-(para$diet.cat.out.T21*.237)
para$dry.cat.out.T22<-(para$diet.cat.out.T22*.237)
para$dry.cat.out.T23<-(para$diet.cat.out.T23*.237)
para$dry.cat.out.T24<-(para$diet.cat.out.T24*.237)
para$dry.cat.out.T25<-(para$diet.cat.out.T25*.237)
para$dry.cat.out.T26<-(para$diet.cat.out.T26*.237)
para$dry.cat.out.T27<-(para$diet.cat.out.T27*.237)
para$dry.cat.out.T28<-(para$diet.cat.out.T28*.237)
para$dry.cat.out.T29<-(para$diet.cat.out.T29*.237)
para$dry.cat.out.T30<-(para$diet.cat.out.T30*.237)
para$dry.cat.out.T31<-(para$diet.cat.out.T31*.237)
para$dry.cat.out.T32<-(para$diet.cat.out.T32*.237)
para$dry.cat.out.T33<-(para$diet.cat.out.T33*.237)
para$dry.cat.out.T34<-(para$diet.cat.out.T34*.237)
para$dry.cat.out.T35<-(para$diet.cat.out.T35*.237)
para$dry.cat.out.T36<-(para$diet.cat.out.T36*.237)



#Create new columns with final control diet wet mass * the slope of the final regression line (.237)

para$dry.cont.out.T0<-(para$diet.cont.out.T0*.237)
para$dry.cont.out.T1<-(para$diet.cont.out.T1*.237)
para$dry.cont.out.T2<-(para$diet.cont.out.T2*.237)
para$dry.cont.out.T3<-(para$diet.cont.out.T3*.237)
para$dry.cont.out.T4<-(para$diet.cont.out.T4*.237)
para$dry.cont.out.T5<-(para$diet.cont.out.T5*.237)
para$dry.cont.out.T6<-(para$diet.cont.out.T6*.237)
para$dry.cont.out.T7<-(para$diet.cont.out.T7*.237)
para$dry.cont.out.T8<-(para$diet.cont.out.T8*.237)
para$dry.cont.out.T9<-(para$diet.cont.out.T9*.237)
para$dry.cont.out.T10<-(para$diet.cont.out.T10*.237)
para$dry.cont.out.T11<-(para$diet.cont.out.T11*.237)
para$dry.cont.out.T12<-(para$diet.cont.out.T12*.237)
para$dry.cont.out.T13<-(para$diet.cont.out.T13*.237)
para$dry.cont.out.T14<-(para$diet.cont.out.T14*.237)
para$dry.cont.out.T15<-(para$diet.cont.out.T15*.237)
para$dry.cont.out.T16<-(para$diet.cont.out.T16*.237)
para$dry.cont.out.T17<-(para$diet.cont.out.T17*.237)
para$dry.cont.out.T18<-(para$diet.cont.out.T18*.237)
para$dry.cont.out.T19<-(para$diet.cont.out.T19*.237)
para$dry.cont.out.T20<-(para$diet.cont.out.T20*.237)
para$dry.cont.out.T21<-(para$diet.cont.out.T21*.237)
para$dry.cont.out.T22<-(para$diet.cont.out.T22*.237)
para$dry.cont.out.T23<-(para$diet.cont.out.T23*.237)
para$dry.cont.out.T24<-(para$diet.cont.out.T24*.237)
para$dry.cont.out.T25<-(para$diet.cont.out.T25*.237)
para$dry.cont.out.T26<-(para$diet.cont.out.T26*.237)
para$dry.cont.out.T27<-(para$diet.cont.out.T27*.237)
para$dry.cont.out.T28<-(para$diet.cont.out.T28*.237)
para$dry.cont.out.T29<-(para$diet.cont.out.T29*.237)
para$dry.cont.out.T30<-(para$diet.cont.out.T30*.237)
para$dry.cont.out.T31<-(para$diet.cont.out.T31*.237)
para$dry.cont.out.T32<-(para$diet.cont.out.T32*.237)
para$dry.cont.out.T33<-(para$diet.cont.out.T33*.237)
para$dry.cont.out.T34<-(para$diet.cont.out.T34*.237)
para$dry.cont.out.T35<-(para$diet.cont.out.T35*.237)
para$dry.cont.out.T36<-(para$diet.cont.out.T36*.237)








#Calculating stand alone and running total consumption for each time point/time period


#Create a new column with consumption of first time period (cat. initial - cat. final: in.T0-out.T0)

para$dry.cnsmp.T0_T1<-(para$dry.cat.in.T0-para$dry.cat.out.T0)



#Create a new column with running total of consumption for the first time period--will be the same value as the stand alone consumption for the first time period

para$dry.totcnsmp.T1<-(para$dry.cat.in.T0-para$dry.cat.out.T0)



#Create a new column with stand alone consumption for the 2nd time period (in.T1-out.T1)

para$dry.cnsmp.T1_T2<-(para$dry.cat.in.T1-para$dry.cat.out.T1)




#Create a new column with the running total of consumption for 2nd time period--will be the runnng total from the 1st tp + the stand alone total from the 2nd tp (dry.totcnsmp.T1 + dry.cnsmp.T1-T2)  

para$dry.totcnsmp.T2<-(para$dry.totcnsmp.T1+para$dry.cnsmp.T1_T2)



#3rd time period stand alone (in.T2-out.T2)

para$dry.cnsmp.T2_T3<-(para$dry.cat.in.T2-para$dry.cat.out.T2)

#3rd time period running total (dry.totcnsmp.T2 + dry.cnsmp.T2_T3)

para$dry.totcnsmp.T3<-(para$dry.totcnsmp.T2+para$dry.cnsmp.T2_T3)

#4th time period stand alone (in.T3-out.T3)

para$dry.cnsmp.T3_T4<-(para$dry.cat.in.T3-para$dry.cat.out.T3)

#4th time period running total (dry.totcnsmp.T3 + dry.cnsmp.T3_T4)

para$dry.totcnsmp.T4<-(para$dry.totcnsmp.T3+para$dry.cnsmp.T3_T4)

#5th time period stand alone (in.T4-out.T4)

para$dry.cnsmp.T4_T5<-(para$dry.cat.in.T4-para$dry.cat.out.T4)

#5th time period running total (dry.totcnsmp.T4 + dry.cnsmp.T4_T5)

para$dry.totcnsmp.T5<-(para$dry.cnsmp.T4_T5+para$dry.totcnsmp.T4)

#6th time period stand alone (in.T5-out.T5)

para$dry.cnsmp.T5_T6<-(para$dry.cat.in.T5-para$dry.cat.out.T5)

#6th time period running total (dry.totcnsmp.T5 + dry.cnsmp.T5_T6)

para$dry.totcnsmp.T6<-(para$dry.cnsmp.T5_T6+para$dry.totcnsmp.T5)

#7th time period stand alone (in.T6-out.T6)

para$dry.cnsmp.T6_T7<-(para$dry.cat.in.T6-para$dry.cat.out.T6)

#7th time period running total (dry.cnsmp.T6_T7 + dry.tot.cnsmp.T6)

para$dry.totcnsmp.T7<-(para$dry.cnsmp.T6_T7+para$dry.totcnsmp.T6)

#8th time period stand alone (in.T7-out.T7)

para$dry.cnsmp.T7_T8<-(para$dry.cat.in.T7-para$dry.cat.out.T7)

#8th time period running total (dry.cnsmp.T7_T8 + dry.tot.cnsmp.T7)

para$dry.totcnsmp.T8<-(para$dry.cnsmp.T7_T8+para$dry.totcnsmp.T7)

#9th time period stand alone (in.T8-out.T8)

para$dry.cnsmp.T8_T9<-(para$dry.cat.in.T8-para$dry.cat.out.T8)

#9th time period running total (T8_T9 + tot.T8)

para$dry.totcnsmp.T9<-(para$dry.cnsmp.T8_T9+para$dry.totcnsmp.T8)

#10th time period stand alone (in.T9-out.T9)

para$dry.cnsmp.T9_T10<-(para$dry.cat.in.T9-para$dry.cat.out.T9)

#10th time period running total (T9_T10 + tot.T9)

para$dry.totcnsmp.T10<-(para$dry.cnsmp.T9_T10+para$dry.totcnsmp.T9)

#11th time period stand alone (in.T10-out.T10)

para$dry.cnsmp.T10_T11<-(para$dry.cat.in.T10-para$dry.cat.out.T10)

#11th time period running total (T10_T11 + tot.T10)

para$dry.totcnsmp.T11<-(para$dry.cnsmp.T10_T11+para$dry.totcnsmp.T10)

#12th time period stand alone (in.T11-out.T11)

para$dry.cnsmp.T11_T12<-(para$dry.cat.in.T11-para$dry.cat.out.T11)

#12th time period running total (T11_T12 + tot.T11)

para$dry.totcnsmp.T12<-(para$dry.cnsmp.T11_T12+para$dry.totcnsmp.T11)

#13th time period stand alone (in.T12-out.T12)

para$dry.cnsmp.T12_T13<-(para$dry.cat.in.T12-para$dry.cat.out.T12)

#13th time period running total (T12_T13 + tot.T12)

para$dry.totcnsmp.T13<-(para$dry.cnsmp.T12_T13+para$dry.totcnsmp.T12)

#14th time period stand alone (in.T13-out.T13)

para$dry.cnsmp.T13_T14<-(para$dry.cat.in.T13-para$dry.cat.out.T13)

#14th time period running total (T13_T14 + tot.T13)

para$dry.totcnsmp.T14<-(para$dry.cnsmp.T13_T14+para$dry.totcnsmp.T13)

#15th time period stand alone (in.T14-out.T14)

para$dry.cnsmp.T14_T15<-(para$dry.cat.in.T14-para$dry.cat.out.T14)

#15th time period running total (T14_T15 + tot.T14)

para$dry.totcnsmp.T15<-(para$dry.cnsmp.T14_T15+para$dry.totcnsmp.T14)

#16th time period stand alone (in.T15-out.T15)

para$dry.cnsmp.T15_T16<-(para$dry.cat.in.T15-para$dry.cat.out.T15)

#16th time period running total (T15_T16 + tot.T15)

para$dry.totcnsmp.T16<-(para$dry.cnsmp.T15_T16+para$dry.totcnsmp.T15)

#17th time period stand alone (in.T16-out.T16)

para$dry.cnsmp.T16_T17<-(para$dry.cat.in.T16-para$dry.cat.out.T16)

#17th time period running total (T16_T16 + tot.T16)

para$dry.totcnsmp.T17<-(para$dry.cnsmp.T16_T17+para$dry.totcnsmp.T16)

#18th time period stand alone (in.T17-out.T17)

para$dry.cnsmp.T17_T18<-(para$dry.cat.in.T17-para$dry.cat.out.T17)

#18th time period running total (T17_T18 + tot.T17)

para$dry.totcnsmp.T18<-(para$dry.cnsmp.T17_T18+para$dry.totcnsmp.T17)

#19th time period stand alone (in.T18-out.T18)

para$dry.cnsmp.T18_T19<-(para$dry.cat.in.T18-para$dry.cat.out.T18)

#19th time period running total (T18_T19 + tot.T18)

para$dry.totcnsmp.T19<-(para$dry.cnsmp.T18_T19+para$dry.totcnsmp.T18)

#20th time period stand alone (in.T19-out.T19)

para$dry.cnsmp.T19_T20<-(para$dry.cat.in.T19-para$dry.cat.out.T19)

#20th time period running total (T19_T20 + tot.T19)

para$dry.totcnsmp.T20<-(para$dry.cnsmp.T19_T20+para$dry.totcnsmp.T19)

#21st time period stand alone (in.T20-out.T20)

para$dry.cnsmp.T20_T21<-(para$dry.cat.in.T20-para$dry.cat.out.T20)

#21st time period running total (T20_T21 + tot.T20)

para$dry.totcnsmp.T21<-(para$dry.cnsmp.T20_T21+para$dry.totcnsmp.T20)

#22nd time period stand alone (in.T21-out.T21)

para$dry.cnsmp.T21_T22<-(para$dry.cat.in.T21-para$dry.cat.out.T21)

#22nd time period runnint total (T21_T22 + tot.T21)

para$dry.totcnsmp.T22<-(para$dry.cnsmp.T21_T22+para$dry.totcnsmp.T21)

#23rd time period stand alone (in.T22-out.T22)

para$dry.cnsmp.T22_T23<-(para$dry.cat.in.T22-para$dry.cat.out.T22)

#23rd time period running total (T22_T23 + tot.T22)

para$dry.totcnsmp.T23<-(para$dry.cnsmp.T22_T23+para$dry.totcnsmp.T22)

#24th time period stand alone (in.T23-out.T23)

para$dry.cnsmp.T23_T24<-(para$dry.cat.in.T23-para$dry.cat.out.T23)

#24th time period running total (T23_T24 + tot.T23)

para$dry.totcnsmp.T24<-(para$dry.cnsmp.T23_T24+para$dry.totcnsmp.T23)

#25th time period stand alone (in.T24-out.T24)

para$dry.cnsmp.T24_T25<-(para$dry.cat.in.T24-para$dry.cat.out.T24)

#25th time period running total (T24_T25 + tot.T24)

para$dry.totcnsmp.T25<-(para$dry.cnsmp.T24_T25+para$dry.totcnsmp.T24)

#26th time period stand alone (in.T25-out.T25)

para$dry.cnsmp.T25_T26<-(para$dry.cat.in.T25-para$dry.cat.out.T25)

#26th time period running total (T25_T26 + tot.T25)

para$dry.totcnsmp.T26<-(para$dry.cnsmp.T25_T26+para$dry.totcnsmp.T25)

#27th time period stand alone (in.T26-out.T26)

para$dry.cnsmp.T26_T27<-(para$dry.cat.in.T26-para$dry.cat.out.T26)

#27th time period running total (T26_T27 + tot.T26)

para$dry.totcnsmp.T27<-(para$dry.cnsmp.T26_T27+para$dry.totcnsmp.T26)

#28th time period stand alone (in.T27-out.T27)

para$dry.cnsmp.T27_T28<-(para$dry.cat.in.T27-para$dry.cat.out.T27)

#28th time period running total (T27_T28 + tot.T27)

para$dry.totcnsmp.T28<-(para$dry.cnsmp.T27_T28+para$dry.totcnsmp.T27)

#29th time period stand alone (in.T28-out.T28)

para$dry.cnsmp.T28_T29<-(para$dry.cat.in.T28-para$dry.cat.out.T28)

#29th time period running total (T28_T29 + tot.T28)

para$dry.totcnsmp.T29<-(para$dry.cnsmp.T28_T29+para$dry.totcnsmp.T28)

#30th time period stand alone (in.T29-out.T29)

para$dry.cnsmp.T29_T30<-(para$dry.cat.in.T29-para$dry.cat.out.T29)

#30th time period running total (T29_T30 + tot.T29)

para$dry.totcnsmp.T30<-(para$dry.cnsmp.T29_T30+para$dry.totcnsmp.T29)

#31st time period stand alone (in.T30-out.T30)

para$dry.cnsmp.T30_T31<-(para$dry.cat.in.T30-para$dry.cat.out.T30)

#31st time period running total (T30_T31 + tot.T30)

para$dry.totcnsmp.T31<-(para$dry.cnsmp.T30_T31+para$dry.totcnsmp.T30)

#32nd time period stand alone (in.T31-out.T31)

para$dry.cnsmp.T31_T32<-(para$dry.cat.in.T31-para$dry.cat.out.T31)

#32nd time period running total (T31_T32 + tot.T31)

para$dry.totcnsmp.T32<-(para$dry.cnsmp.T31_T32+para$dry.totcnsmp.T31)

#33rd time period stand alone (in.T32-out.T32)

para$dry.cnsmp.T32_T33<-(para$dry.cat.in.T32-para$dry.cat.out.T32)

#33rd time period running total (T32_T33 + tot.T32)

para$dry.totcnsmp.T33<-(para$dry.cnsmp.T32_T33+para$dry.totcnsmp.T32)

#34th time period stand alone (in.T33-out.T33)

para$dry.cnsmp.T33_T34<-(para$dry.cat.in.T33-para$dry.cat.out.T33)

#34th time period running total (T33_T34 + tot.T33)

para$dry.totcnsmp.T34<-(para$dry.cnsmp.T33_T34+para$dry.totcnsmp.T33)

#35th time period stand alone (in.T34-out.T34)

para$dry.cnsmp.T34_T35<-(para$dry.cat.in.T34-para$dry.cat.out.T34)

#35th time period running total (T34_T35 + tot.T34)

para$dry.totcnsmp.T35<-(para$dry.cnsmp.T34_T35+para$dry.totcnsmp.T34)

#36th time period stand alone (in.T35-out.T35)

para$dry.cnsmp.T35_T36<-(para$dry.cat.in.T35-para$dry.cat.out.T35)

#36th time period running total (T35_T36 + tot.T35)

para$dry.totcnsmp.T36<-(para$dry.cnsmp.T35_T36+para$dry.totcnsmp.T35)

#37th time period stand alone (in.T36-out.T36)

para$dry.cnsmp.T36_T37<-(para$dry.cat.in.T36-para$dry.cat.out.T36)

#37th time period running total (T36_T37 + tot.T36)

para$dry.totcnsmp.T37<-(para$dry.cnsmp.T36_T37+para$dry.totcnsmp.T36)







#Making negative values = 1, so that models and graphs with log(tot.cnsmp) will run 

para$dry.cnsmp.T0_T1[para$dry.cnsmp.T0_T1 < 0] <- 1
para$dry.cnsmp.T1_T2[para$dry.cnsmp.T1_T2 < 0] <- 1

para$dry.totcnsmp.T1[para$dry.totcnsmp.T1 < 0] <- 1
para$dry.totcnsmp.T2[para$dry.totcnsmp.T2 < 0] <- 1



para$dry.cnsmp.T2_T3[para$dry.cnsmp.T2_T3 < 0] <- 1
para$dry.cnsmp.T3_T4[para$dry.cnsmp.T3_T4 < 0] <- 1
para$dry.cnsmp.T4_T5[para$dry.cnsmp.T4_T5 < 0] <- 1
para$dry.cnsmp.T5_T6[para$dry.cnsmp.T5_T6 < 0] <- 1
para$dry.cnsmp.T6_T7[para$dry.cnsmp.T6_T7 < 0] <- 1
para$dry.cnsmp.T7_T8[para$dry.cnsmp.T7_T8 < 0] <- 1
para$dry.cnsmp.T8_T9[para$dry.cnsmp.T8_T9 < 0] <- 1
para$dry.cnsmp.T9_T10[para$dry.cnsmp.T9_T10 < 0] <- 1
para$dry.cnsmp.T10_T11[para$dry.cnsmp.T10_T11 < 0] <- 1
para$dry.cnsmp.T11_T12[para$dry.cnsmp.T11_T12 < 0] <- 1
para$dry.cnsmp.T12_T13[para$dry.cnsmp.T12_T13 < 0] <- 1
para$dry.cnsmp.T13_T14[para$dry.cnsmp.T13_T14 < 0] <- 1
para$dry.cnsmp.T14_T15[para$dry.cnsmp.T14_T15 < 0] <- 1
para$dry.cnsmp.T15_T16[para$dry.cnsmp.T15_T16 < 0] <- 1
para$dry.cnsmp.T16_T17[para$dry.cnsmp.T16_T17 < 0] <- 1
para$dry.cnsmp.T17_T18[para$dry.cnsmp.T17_T18 < 0] <- 1
para$dry.cnsmp.T18_T19[para$dry.cnsmp.T18_T19 < 0] <- 1
para$dry.cnsmp.T19_T20[para$dry.cnsmp.T19_T20 < 0] <- 1
para$dry.cnsmp.T20_T21[para$dry.cnsmp.T20_T21 < 0] <- 1
para$dry.cnsmp.T21_T22[para$dry.cnsmp.T21_T22 < 0] <- 1
para$dry.cnsmp.T22_T23[para$dry.cnsmp.T22_T23 < 0] <- 1
para$dry.cnsmp.T23_T24[para$dry.cnsmp.T23_T24 < 0] <- 1
para$dry.cnsmp.T24_T25[para$dry.cnsmp.T24_T25 < 0] <- 1
para$dry.cnsmp.T25_T26[para$dry.cnsmp.T25_T26 < 0] <- 1
para$dry.cnsmp.T26_T27[para$dry.cnsmp.T26_T27 < 0] <- 1
para$dry.cnsmp.T27_T28[para$dry.cnsmp.T27_T28 < 0] <- 1
para$dry.cnsmp.T28_T29[para$dry.cnsmp.T28_T29 < 0] <- 1
para$dry.cnsmp.T29_T30[para$dry.cnsmp.T29_T30 < 0] <- 1
para$dry.cnsmp.T30_T31[para$dry.cnsmp.T30_T31 < 0] <- 1
para$dry.cnsmp.T31_T32[para$dry.cnsmp.T31_T32 < 0] <- 1
para$dry.cnsmp.T32_T33[para$dry.cnsmp.T32_T33 < 0] <- 1
para$dry.cnsmp.T33_T34[para$dry.cnsmp.T33_T34 < 0] <- 1
para$dry.cnsmp.T34_T35[para$dry.cnsmp.T34_T35 < 0] <- 1
para$dry.cnsmp.T35_T36[para$dry.cnsmp.T35_T36 < 0] <- 1
para$dry.cnsmp.T36_T37[para$dry.cnsmp.T36_T37 < 0] <- 1




para$dry.totcnsmp.T3[para$dry.totcnsmp.T3 < 0] <- 1
para$dry.totcnsmp.T4[para$dry.totcnsmp.T4 < 0] <- 1
para$dry.totcnsmp.T5[para$dry.totcnsmp.T5 < 0] <- 1
para$dry.totcnsmp.T6[para$dry.totcnsmp.T6 < 0] <- 1
para$dry.totcnsmp.T7[para$dry.totcnsmp.T7 < 0] <- 1
para$dry.totcnsmp.T8[para$dry.totcnsmp.T8 < 0] <- 1
para$dry.totcnsmp.T9[para$dry.totcnsmp.T9 < 0] <- 1
para$dry.totcnsmp.T10[para$dry.totcnsmp.T10 < 0] <- 1
para$dry.totcnsmp.T11[para$dry.totcnsmp.T11 < 0] <- 1
para$dry.totcnsmp.T12[para$dry.totcnsmp.T12 < 0] <- 1
para$dry.totcnsmp.T13[para$dry.totcnsmp.T13 < 0] <- 1
para$dry.totcnsmp.T14[para$dry.totcnsmp.T14 < 0] <- 1
para$dry.totcnsmp.T15[para$dry.totcnsmp.T15 < 0] <- 1
para$dry.totcnsmp.T16[para$dry.totcnsmp.T16 < 0] <- 1
para$dry.totcnsmp.T17[para$dry.totcnsmp.T17 < 0] <- 1
para$dry.totcnsmp.T18[para$dry.totcnsmp.T18 < 0] <- 1
para$dry.totcnsmp.T19[para$dry.totcnsmp.T19 < 0] <- 1
para$dry.totcnsmp.T20[para$dry.totcnsmp.T20 < 0] <- 1
para$dry.totcnsmp.T21[para$dry.totcnsmp.T21 < 0] <- 1
para$dry.totcnsmp.T22[para$dry.totcnsmp.T22 < 0] <- 1
para$dry.totcnsmp.T23[para$dry.totcnsmp.T23 < 0] <- 1
para$dry.totcnsmp.T24[para$dry.totcnsmp.T24 < 0] <- 1
para$dry.totcnsmp.T25[para$dry.totcnsmp.T25 < 0] <- 1
para$dry.totcnsmp.T26[para$dry.totcnsmp.T26 < 0] <- 1
para$dry.totcnsmp.T27[para$dry.totcnsmp.T27 < 0] <- 1
para$dry.totcnsmp.T28[para$dry.totcnsmp.T28 < 0] <- 1
para$dry.totcnsmp.T29[para$dry.totcnsmp.T29 < 0] <- 1
para$dry.totcnsmp.T30[para$dry.totcnsmp.T30 < 0] <- 1
para$dry.totcnsmp.T31[para$dry.totcnsmp.T31 < 0] <- 1
para$dry.totcnsmp.T32[para$dry.totcnsmp.T32 < 0] <- 1
para$dry.totcnsmp.T33[para$dry.totcnsmp.T33 < 0] <- 1
para$dry.totcnsmp.T34[para$dry.totcnsmp.T34 < 0] <- 1
para$dry.totcnsmp.T35[para$dry.totcnsmp.T35 < 0] <- 1
para$dry.totcnsmp.T36[para$dry.totcnsmp.T36 < 0] <- 1
para$dry.totcnsmp.T37[para$dry.totcnsmp.T37 < 0] <- 1





#Making cnsmp.4, cnsmp.5, cnsmp.em and cnsmp.wan columns



#Making cnmp.4 column 

keepvars<-c("num","bug.id","timepoint.4","dry.totcnsmp.T1","dry.totcnsmp.T2","dry.totcnsmp.T3","dry.totcnsmp.T4","dry.totcnsmp.T5","dry.totcnsmp.T6","dry.totcnsmp.T7","dry.totcnsmp.T8","dry.totcnsmp.T9","dry.totcnsmp.T10","dry.totcnsmp.T11","dry.totcnsmp.T12","dry.totcnsmp.T13","dry.totcnsmp.T14","dry.totcnsmp.T15","dry.totcnsmp.T16","dry.totcnsmp.T17","dry.totcnsmp.T18","dry.totcnsmp.T19","dry.totcnsmp.T20","dry.totcnsmp.T21","dry.totcnsmp.T22","dry.totcnsmp.T23","dry.totcnsmp.T24","dry.totcnsmp.T25","dry.totcnsmp.T26","dry.totcnsmp.T27","dry.totcnsmp.T28","dry.totcnsmp.T29","dry.totcnsmp.T30","dry.totcnsmp.T31","dry.totcnsmp.T32","dry.totcnsmp.T33","dry.totcnsmp.T34","dry.totcnsmp.T35","dry.totcnsmp.T36","dry.totcnsmp.T37")
sub<-para[keepvars]
View(sub)
sub<-as.data.frame(sub)


sub<-rename(sub, T1=dry.totcnsmp.T1,T2=dry.totcnsmp.T2,T3=dry.totcnsmp.T3,T4=dry.totcnsmp.T4,T5=dry.totcnsmp.T5,T6=dry.totcnsmp.T6,T7=dry.totcnsmp.T7,T8=dry.totcnsmp.T8,T9=dry.totcnsmp.T9,T10=dry.totcnsmp.T10,T11=dry.totcnsmp.T11,T12=dry.totcnsmp.T12,T13=dry.totcnsmp.T13,T14=dry.totcnsmp.T14,T15=dry.totcnsmp.T15,T16=dry.totcnsmp.T16,T17=dry.totcnsmp.T17,T18=dry.totcnsmp.T18,T19=dry.totcnsmp.T19,T20=dry.totcnsmp.T20,T21=dry.totcnsmp.T21,T22=dry.totcnsmp.T22,T23=dry.totcnsmp.T23,T24=dry.totcnsmp.T24,T25=dry.totcnsmp.T25,T26=dry.totcnsmp.T26,T27=dry.totcnsmp.T27,T28=dry.totcnsmp.T28,T29=dry.totcnsmp.T29,T30=dry.totcnsmp.T30,T31=dry.totcnsmp.T31,T32=dry.totcnsmp.T32,T33=dry.totcnsmp.T33,T34=dry.totcnsmp.T34,T35=dry.totcnsmp.T35,T36=dry.totcnsmp.T36,T37=dry.totcnsmp.T37)

sub$cnsmp.4 <- sub[][cbind(1:nrow(sub), match(sub$timepoint.4, colnames(sub)))]


keepvars2<-c("num","bug.id","cnsmp.4")
sub2<-sub[keepvars2]
sub2<-as.data.frame(sub2)


para<-merge(para,sub2,by=c("num","bug.id"))

para<-arrange(para, num)

para$cnsmp.4 %in% sub2$cnsmp.4



#Making cnsmp.5 column

keepvars3<-c("num","bug.id","timepoint.5","dry.totcnsmp.T1","dry.totcnsmp.T2","dry.totcnsmp.T3","dry.totcnsmp.T4","dry.totcnsmp.T5","dry.totcnsmp.T6","dry.totcnsmp.T7","dry.totcnsmp.T8","dry.totcnsmp.T9","dry.totcnsmp.T10","dry.totcnsmp.T11","dry.totcnsmp.T12","dry.totcnsmp.T13","dry.totcnsmp.T14","dry.totcnsmp.T15","dry.totcnsmp.T16","dry.totcnsmp.T17","dry.totcnsmp.T18","dry.totcnsmp.T19","dry.totcnsmp.T20","dry.totcnsmp.T21","dry.totcnsmp.T22","dry.totcnsmp.T23","dry.totcnsmp.T24","dry.totcnsmp.T25","dry.totcnsmp.T26","dry.totcnsmp.T27","dry.totcnsmp.T28","dry.totcnsmp.T29","dry.totcnsmp.T30","dry.totcnsmp.T31","dry.totcnsmp.T32","dry.totcnsmp.T33","dry.totcnsmp.T34","dry.totcnsmp.T35","dry.totcnsmp.T36","dry.totcnsmp.T37")
sub3<-para[keepvars3]
sub3<-as.data.frame(sub3)

sub3<-rename(sub3, T1=dry.totcnsmp.T1,T2=dry.totcnsmp.T2,T3=dry.totcnsmp.T3,T4=dry.totcnsmp.T4,T5=dry.totcnsmp.T5,T6=dry.totcnsmp.T6,T7=dry.totcnsmp.T7,T8=dry.totcnsmp.T8,T9=dry.totcnsmp.T9,T10=dry.totcnsmp.T10,T11=dry.totcnsmp.T11,T12=dry.totcnsmp.T12,T13=dry.totcnsmp.T13,T14=dry.totcnsmp.T14,T15=dry.totcnsmp.T15,T16=dry.totcnsmp.T16,T17=dry.totcnsmp.T17,T18=dry.totcnsmp.T18,T19=dry.totcnsmp.T19,T20=dry.totcnsmp.T20,T21=dry.totcnsmp.T21,T22=dry.totcnsmp.T22,T23=dry.totcnsmp.T23,T24=dry.totcnsmp.T24,T25=dry.totcnsmp.T25,T26=dry.totcnsmp.T26,T27=dry.totcnsmp.T27,T28=dry.totcnsmp.T28,T29=dry.totcnsmp.T29,T30=dry.totcnsmp.T30,T31=dry.totcnsmp.T31,T32=dry.totcnsmp.T32,T33=dry.totcnsmp.T33,T34=dry.totcnsmp.T34,T35=dry.totcnsmp.T35,T36=dry.totcnsmp.T36,T37=dry.totcnsmp.T37)

sub3$cnsmp.5 <- sub3[][cbind(1:nrow(sub3), match(sub3$timepoint.5, colnames(sub3)))]


keepvars3.5<-c("num","bug.id","cnsmp.5")
sub3.5<-sub3[keepvars3.5]
sub3.5<-as.data.frame(sub3.5)


para<-merge(para,sub3.5,by=c("num","bug.id"))

para<-arrange(para, num)

para$cnsmp.5 %in% sub3.5$cnsmp.5



#Making cnsmp.wan column

keepvars4<-c("num","bug.id","timepoint.wander","dry.totcnsmp.T1","dry.totcnsmp.T2","dry.totcnsmp.T3","dry.totcnsmp.T4","dry.totcnsmp.T5","dry.totcnsmp.T6","dry.totcnsmp.T7","dry.totcnsmp.T8","dry.totcnsmp.T9","dry.totcnsmp.T10","dry.totcnsmp.T11","dry.totcnsmp.T12","dry.totcnsmp.T13","dry.totcnsmp.T14","dry.totcnsmp.T15","dry.totcnsmp.T16","dry.totcnsmp.T17","dry.totcnsmp.T18","dry.totcnsmp.T19","dry.totcnsmp.T20","dry.totcnsmp.T21","dry.totcnsmp.T22","dry.totcnsmp.T23","dry.totcnsmp.T24","dry.totcnsmp.T25","dry.totcnsmp.T26","dry.totcnsmp.T27","dry.totcnsmp.T28","dry.totcnsmp.T29","dry.totcnsmp.T30","dry.totcnsmp.T31","dry.totcnsmp.T32","dry.totcnsmp.T33","dry.totcnsmp.T34","dry.totcnsmp.T35","dry.totcnsmp.T36","dry.totcnsmp.T37")
sub4<-para[keepvars4]
sub4<-as.data.frame(sub4)

sub4<-rename(sub4, T1=dry.totcnsmp.T1,T2=dry.totcnsmp.T2,T3=dry.totcnsmp.T3,T4=dry.totcnsmp.T4,T5=dry.totcnsmp.T5,T6=dry.totcnsmp.T6,T7=dry.totcnsmp.T7,T8=dry.totcnsmp.T8,T9=dry.totcnsmp.T9,T10=dry.totcnsmp.T10,T11=dry.totcnsmp.T11,T12=dry.totcnsmp.T12,T13=dry.totcnsmp.T13,T14=dry.totcnsmp.T14,T15=dry.totcnsmp.T15,T16=dry.totcnsmp.T16,T17=dry.totcnsmp.T17,T18=dry.totcnsmp.T18,T19=dry.totcnsmp.T19,T20=dry.totcnsmp.T20,T21=dry.totcnsmp.T21,T22=dry.totcnsmp.T22,T23=dry.totcnsmp.T23,T24=dry.totcnsmp.T24,T25=dry.totcnsmp.T25,T26=dry.totcnsmp.T26,T27=dry.totcnsmp.T27,T28=dry.totcnsmp.T28,T29=dry.totcnsmp.T29,T30=dry.totcnsmp.T30,T31=dry.totcnsmp.T31,T32=dry.totcnsmp.T32,T33=dry.totcnsmp.T33,T34=dry.totcnsmp.T34,T35=dry.totcnsmp.T35,T36=dry.totcnsmp.T36,T37=dry.totcnsmp.T37)

sub4$timepoint.wander<-gsub("T", "",sub4$timepoint.wander)
sub4$timepoint.wander<-as.numeric(sub4$timepoint.wander)-1
sub4$timepoint.wander<-gsub("^", "T",sub4$timepoint.wander)


sub4$cnsmp.wan <- sub4[][cbind(1:nrow(sub4), match(sub4$timepoint.wander, colnames(sub4)))]


keepvars4.5<-c("num","bug.id","cnsmp.wan")
sub4.5<-sub4[keepvars4.5]
sub4.5<-as.data.frame(sub4.5)


para<-merge(para,sub4.5,by=c("num","bug.id"))

para<-arrange(para, num)

para$cnsmp.wan %in% sub4.5$cnsmp.wan




#Making cnsmp.em column

keepvars5<-c("num","bug.id","timepoint.em","dry.totcnsmp.T1","dry.totcnsmp.T2","dry.totcnsmp.T3","dry.totcnsmp.T4","dry.totcnsmp.T5","dry.totcnsmp.T6","dry.totcnsmp.T7","dry.totcnsmp.T8","dry.totcnsmp.T9","dry.totcnsmp.T10","dry.totcnsmp.T11","dry.totcnsmp.T12","dry.totcnsmp.T13","dry.totcnsmp.T14","dry.totcnsmp.T15","dry.totcnsmp.T16","dry.totcnsmp.T17","dry.totcnsmp.T18","dry.totcnsmp.T19","dry.totcnsmp.T20","dry.totcnsmp.T21","dry.totcnsmp.T22","dry.totcnsmp.T23","dry.totcnsmp.T24","dry.totcnsmp.T25","dry.totcnsmp.T26","dry.totcnsmp.T27","dry.totcnsmp.T28","dry.totcnsmp.T29","dry.totcnsmp.T30","dry.totcnsmp.T31","dry.totcnsmp.T32","dry.totcnsmp.T33","dry.totcnsmp.T34","dry.totcnsmp.T35","dry.totcnsmp.T36","dry.totcnsmp.T37")
sub5<-para[keepvars5]
sub5<-as.data.frame(sub5)

sub5<-rename(sub5, T1=dry.totcnsmp.T1,T2=dry.totcnsmp.T2,T3=dry.totcnsmp.T3,T4=dry.totcnsmp.T4,T5=dry.totcnsmp.T5,T6=dry.totcnsmp.T6,T7=dry.totcnsmp.T7,T8=dry.totcnsmp.T8,T9=dry.totcnsmp.T9,T10=dry.totcnsmp.T10,T11=dry.totcnsmp.T11,T12=dry.totcnsmp.T12,T13=dry.totcnsmp.T13,T14=dry.totcnsmp.T14,T15=dry.totcnsmp.T15,T16=dry.totcnsmp.T16,T17=dry.totcnsmp.T17,T18=dry.totcnsmp.T18,T19=dry.totcnsmp.T19,T20=dry.totcnsmp.T20,T21=dry.totcnsmp.T21,T22=dry.totcnsmp.T22,T23=dry.totcnsmp.T23,T24=dry.totcnsmp.T24,T25=dry.totcnsmp.T25,T26=dry.totcnsmp.T26,T27=dry.totcnsmp.T27,T28=dry.totcnsmp.T28,T29=dry.totcnsmp.T29,T30=dry.totcnsmp.T30,T31=dry.totcnsmp.T31,T32=dry.totcnsmp.T32,T33=dry.totcnsmp.T33,T34=dry.totcnsmp.T34,T35=dry.totcnsmp.T35,T36=dry.totcnsmp.T36,T37=dry.totcnsmp.T37)

sub5$timepoint.em<-gsub("T", "",sub5$timepoint.em)
sub5$timepoint.em<-as.numeric(sub5$timepoint.em)-1
sub5$timepoint.em<-gsub("^", "T",sub5$timepoint.em)


sub5$cnsmp.em <- sub5[][cbind(1:nrow(sub5), match(sub5$timepoint.em, colnames(sub5)))]



keepvars5.5<-c("num","bug.id","cnsmp.em")
sub5.5<-sub5[keepvars5.5]
sub5.5<-as.data.frame(sub5.5)


para<-merge(para,sub5.5,by=c("num","bug.id"))

para<-arrange(para, num)

para$cnsmp.em %in% sub5$cnsmp.em




#Making mass gain (since start of 3rd instar) columns

para$mass.gain.T0<-para$mass.T0-para$mass.T0
para$mass.gain.T1<-para$mass.T1-para$mass.T0
para$mass.gain.T2<-para$mass.T2-para$mass.T0
para$mass.gain.T3<-para$mass.T3-para$mass.T0
para$mass.gain.T4<-para$mass.T4-para$mass.T0
para$mass.gain.T5<-para$mass.T5-para$mass.T0
para$mass.gain.T6<-para$mass.T6-para$mass.T0
para$mass.gain.T7<-para$mass.T7-para$mass.T0
para$mass.gain.T8<-para$mass.T8-para$mass.T0
para$mass.gain.T9<-para$mass.T9-para$mass.T0
para$mass.gain.T10<-para$mass.T10-para$mass.T0
para$mass.gain.T11<-para$mass.T11-para$mass.T0
para$mass.gain.T12<-para$mass.T12-para$mass.T0
para$mass.gain.T13<-para$mass.T13-para$mass.T0
para$mass.gain.T14<-para$mass.T14-para$mass.T0
para$mass.gain.T15<-para$mass.T15-para$mass.T0
para$mass.gain.T16<-para$mass.T16-para$mass.T0
para$mass.gain.T17<-para$mass.T17-para$mass.T0
para$mass.gain.T18<-para$mass.T18-para$mass.T0
para$mass.gain.T19<-para$mass.T19-para$mass.T0
para$mass.gain.T20<-para$mass.T20-para$mass.T0
para$mass.gain.T21<-para$mass.T21-para$mass.T0
para$mass.gain.T22<-para$mass.T22-para$mass.T0
para$mass.gain.T23<-para$mass.T23-para$mass.T0
para$mass.gain.T24<-para$mass.T24-para$mass.T0
para$mass.gain.T25<-para$mass.T25-para$mass.T0
para$mass.gain.T26<-para$mass.T26-para$mass.T0
para$mass.gain.T27<-para$mass.T27-para$mass.T0
para$mass.gain.T28<-para$mass.T28-para$mass.T0
para$mass.gain.T29<-para$mass.T29-para$mass.T0
para$mass.gain.T30<-para$mass.T30-para$mass.T0
para$mass.gain.T31<-para$mass.T31-para$mass.T0
para$mass.gain.T32<-para$mass.T32-para$mass.T0
para$mass.gain.T33<-para$mass.T33-para$mass.T0
para$mass.gain.T34<-para$mass.T34-para$mass.T0
para$mass.gain.T35<-para$mass.T35-para$mass.T0
para$mass.gain.T36<-para$mass.T36-para$mass.T0
para$mass.gain.T37<-para$mass.T37-para$mass.T0




#Making mass.gain at 4th instar column

keepvars6<-c("num","bug.id","timepoint.4","mass.gain.T1","mass.gain.T2","mass.gain.T3","mass.gain.T4","mass.gain.T5","mass.gain.T6","mass.gain.T7","mass.gain.T8","mass.gain.T9","mass.gain.T10","mass.gain.T11","mass.gain.T12","mass.gain.T13","mass.gain.T14","mass.gain.T15","mass.gain.T16","mass.gain.T17","mass.gain.T18","mass.gain.T19","mass.gain.T20","mass.gain.T21","mass.gain.T22","mass.gain.T23","mass.gain.T24","mass.gain.T25","mass.gain.T26","mass.gain.T27","mass.gain.T28","mass.gain.T29","mass.gain.T30","mass.gain.T31","mass.gain.T32","mass.gain.T33","mass.gain.T34","mass.gain.T35","mass.gain.T36","mass.gain.T37")
sub6<-para[keepvars6]
sub6<-as.data.frame(sub6)

sub6<-rename(sub6, T1=mass.gain.T1,T2=mass.gain.T2,T3=mass.gain.T3,T4=mass.gain.T4,T5=mass.gain.T5,T6=mass.gain.T6,T7=mass.gain.T7,T8=mass.gain.T8,T9=mass.gain.T9,T10=mass.gain.T10,T11=mass.gain.T11,T12=mass.gain.T12,T13=mass.gain.T13,T14=mass.gain.T14,T15=mass.gain.T15,T16=mass.gain.T16,T17=mass.gain.T17,T18=mass.gain.T18,T19=mass.gain.T19,T20=mass.gain.T20,T21=mass.gain.T21,T22=mass.gain.T22,T23=mass.gain.T23,T24=mass.gain.T24,T25=mass.gain.T25,T26=mass.gain.T26,T27=mass.gain.T27,T28=mass.gain.T28,T29=mass.gain.T29,T30=mass.gain.T30,T31=mass.gain.T31,T32=mass.gain.T32,T33=mass.gain.T33,T34=mass.gain.T34,T35=mass.gain.T35,T36=mass.gain.T36,T37=mass.gain.T37)

sub6$mass.gain.4 <- sub6[][cbind(1:nrow(sub6), match(sub6$timepoint.4, colnames(sub6)))]


keepvars5.5<-c("num","bug.id","mass.gain.4")
sub6.5<-sub6[keepvars5.5]
sub6.5<-as.data.frame(sub6.5)


para<-merge(para,sub6.5,by=c("num","bug.id"))

para<-arrange(para, num)

para$mass.gain.4 %in% sub6$mass.gain.4



#Making a mass gain at 5th instar column

keepvars7<-c("num","bug.id","timepoint.5","mass.gain.T1","mass.gain.T2","mass.gain.T3","mass.gain.T4","mass.gain.T5","mass.gain.T6","mass.gain.T7","mass.gain.T8","mass.gain.T9","mass.gain.T10","mass.gain.T11","mass.gain.T12","mass.gain.T13","mass.gain.T14","mass.gain.T15","mass.gain.T16","mass.gain.T17","mass.gain.T18","mass.gain.T19","mass.gain.T20","mass.gain.T21","mass.gain.T22","mass.gain.T23","mass.gain.T24","mass.gain.T25","mass.gain.T26","mass.gain.T27","mass.gain.T28","mass.gain.T29","mass.gain.T30","mass.gain.T31","mass.gain.T32","mass.gain.T33","mass.gain.T34","mass.gain.T35","mass.gain.T36","mass.gain.T37")
sub7<-para[keepvars7]
sub7<-as.data.frame(sub7)


sub7<-rename(sub7, T1=mass.gain.T1,T2=mass.gain.T2,T3=mass.gain.T3,T4=mass.gain.T4,T5=mass.gain.T5,T6=mass.gain.T6,T7=mass.gain.T7,T8=mass.gain.T8,T9=mass.gain.T9,T10=mass.gain.T10,T11=mass.gain.T11,T12=mass.gain.T12,T13=mass.gain.T13,T14=mass.gain.T14,T15=mass.gain.T15,T16=mass.gain.T16,T17=mass.gain.T17,T18=mass.gain.T18,T19=mass.gain.T19,T20=mass.gain.T20,T21=mass.gain.T21,T22=mass.gain.T22,T23=mass.gain.T23,T24=mass.gain.T24,T25=mass.gain.T25,T26=mass.gain.T26,T27=mass.gain.T27,T28=mass.gain.T28,T29=mass.gain.T29,T30=mass.gain.T30,T31=mass.gain.T31,T32=mass.gain.T32,T33=mass.gain.T33,T34=mass.gain.T34,T35=mass.gain.T35,T36=mass.gain.T36,T37=mass.gain.T37)

sub7$mass.gain.5 <- sub7[][cbind(1:nrow(sub7), match(sub7$timepoint.5, colnames(sub7)))]


keepvars7.5<-c("num","bug.id","mass.gain.5")
sub7.5<-sub7[keepvars7.5]
sub7.5<-as.data.frame(sub7.5)


para<-merge(para,sub7.5,by=c("num","bug.id"))

para<-arrange(para, num)

para$mass.gain.5 %in% sub7$mass.gain.5





#Making a mass gain at wandering column

keepvars8<-c("num","bug.id","timepoint.wander","mass.gain.T1","mass.gain.T2","mass.gain.T3","mass.gain.T4","mass.gain.T5","mass.gain.T6","mass.gain.T7","mass.gain.T8","mass.gain.T9","mass.gain.T10","mass.gain.T11","mass.gain.T12","mass.gain.T13","mass.gain.T14","mass.gain.T15","mass.gain.T16","mass.gain.T17","mass.gain.T18","mass.gain.T19","mass.gain.T20","mass.gain.T21","mass.gain.T22","mass.gain.T23","mass.gain.T24","mass.gain.T25","mass.gain.T26","mass.gain.T27","mass.gain.T28","mass.gain.T29","mass.gain.T30","mass.gain.T31","mass.gain.T32","mass.gain.T33","mass.gain.T34","mass.gain.T35","mass.gain.T36","mass.gain.T37")
sub8<-para[keepvars8]
sub8<-as.data.frame(sub8)


sub8<-rename(sub8, T1=mass.gain.T1,T2=mass.gain.T2,T3=mass.gain.T3,T4=mass.gain.T4,T5=mass.gain.T5,T6=mass.gain.T6,T7=mass.gain.T7,T8=mass.gain.T8,T9=mass.gain.T9,T10=mass.gain.T10,T11=mass.gain.T11,T12=mass.gain.T12,T13=mass.gain.T13,T14=mass.gain.T14,T15=mass.gain.T15,T16=mass.gain.T16,T17=mass.gain.T17,T18=mass.gain.T18,T19=mass.gain.T19,T20=mass.gain.T20,T21=mass.gain.T21,T22=mass.gain.T22,T23=mass.gain.T23,T24=mass.gain.T24,T25=mass.gain.T25,T26=mass.gain.T26,T27=mass.gain.T27,T28=mass.gain.T28,T29=mass.gain.T29,T30=mass.gain.T30,T31=mass.gain.T31,T32=mass.gain.T32,T33=mass.gain.T33,T34=mass.gain.T34,T35=mass.gain.T35,T36=mass.gain.T36,T37=mass.gain.T37)

sub8$mass.gain.wan <- sub8[][cbind(1:nrow(sub8), match(sub8$timepoint.wander, colnames(sub8)))]


keepvars8.5<-c("num","bug.id","mass.gain.wan")
sub8.5<-sub8[keepvars8.5]
sub8.5<-as.data.frame(sub8.5)


para<-merge(para,sub8.5,by=c("num","bug.id"))

para<-arrange(para, num)

para$mass.gain.wan %in% sub8$mass.gain.wan



#Making a mass gain at emergence (technically before emergence)

keepvars9<-c("num","bug.id","timepoint.em","mass.gain.T1","mass.gain.T2","mass.gain.T3","mass.gain.T4","mass.gain.T5","mass.gain.T6","mass.gain.T7","mass.gain.T8","mass.gain.T9","mass.gain.T10","mass.gain.T11","mass.gain.T12","mass.gain.T13","mass.gain.T14","mass.gain.T15","mass.gain.T16","mass.gain.T17","mass.gain.T18","mass.gain.T19","mass.gain.T20","mass.gain.T21","mass.gain.T22","mass.gain.T23","mass.gain.T24","mass.gain.T25","mass.gain.T26","mass.gain.T27","mass.gain.T28","mass.gain.T29","mass.gain.T30","mass.gain.T31","mass.gain.T32","mass.gain.T33","mass.gain.T34","mass.gain.T35","mass.gain.T36","mass.gain.T37")
sub9<-para[keepvars9]
sub9<-as.data.frame(sub9)


sub9<-rename(sub9, T1=mass.gain.T1,T2=mass.gain.T2,T3=mass.gain.T3,T4=mass.gain.T4,T5=mass.gain.T5,T6=mass.gain.T6,T7=mass.gain.T7,T8=mass.gain.T8,T9=mass.gain.T9,T10=mass.gain.T10,T11=mass.gain.T11,T12=mass.gain.T12,T13=mass.gain.T13,T14=mass.gain.T14,T15=mass.gain.T15,T16=mass.gain.T16,T17=mass.gain.T17,T18=mass.gain.T18,T19=mass.gain.T19,T20=mass.gain.T20,T21=mass.gain.T21,T22=mass.gain.T22,T23=mass.gain.T23,T24=mass.gain.T24,T25=mass.gain.T25,T26=mass.gain.T26,T27=mass.gain.T27,T28=mass.gain.T28,T29=mass.gain.T29,T30=mass.gain.T30,T31=mass.gain.T31,T32=mass.gain.T32,T33=mass.gain.T33,T34=mass.gain.T34,T35=mass.gain.T35,T36=mass.gain.T36,T37=mass.gain.T37)

sub9$timepoint.em<-gsub("T", "",sub9$timepoint.em)
sub9$timepoint.em<-as.numeric(sub9$timepoint.em)-1
sub9$timepoint.em<-gsub("^", "T",sub9$timepoint.em)

sub9$mass.gain.em <- sub9[][cbind(1:nrow(sub9), match(sub9$timepoint.em, colnames(sub9)))]


keepvars9.5<-c("num","bug.id","mass.gain.em")
sub9.5<-sub9[keepvars9.5]
sub9.5<-as.data.frame(sub9.5)


para<-merge(para,sub9.5,by=c("num","bug.id"))

para<-arrange(para, num)

para$mass.gain.em %in% sub9$mass.gain.em






#Write into cpt gr wide csv

write.csv(para,"cpt gr wide.csv", row.names = FALSE, quote = FALSE)













