#CxPxT cleaning script---REVISED


#Load libraries

library(readr)
library(lubridate)
library(dplyr)


#Load data set

cpt_raw <- read_csv("~/Manduca expts/Summer+Fall 2016/CxPxT/data files/cpt_raw.csv", 
                    col_types = cols(temp = col_factor(levels = c("20", 
                                                                  "25", "30"))))
View(cpt_raw)


#---------------------------------------------
  

#removing the empty rows at the bottom of the data frame
  ##Using ID as the subsetting column, since all individuals should have an NA

#Making NAs in the ID column == 0
  
cpt_raw$ID[is.na(cpt_raw$ID)]<-0

#Subsetting cpt_raw so that all ID's are greater than 0

cpt_raw<-subset(cpt_raw,ID>0)
  

#-------------------------------------------
  
#Excluding those that died
  ##creating an ifelse statement that creates a sorting column ("died")

cpt_raw$died<-ifelse(cpt_raw$date.died=="", "N", "Y")
cpt_raw<-subset(cpt_raw,!(died %in% "Y"))



#Excluding those that were sacrificed

cpt_raw<-subset(cpt_raw,sacrifice=="0")




#------------------------------------------

#Renaming columns that accidentally had duplicated names:

colnames(cpt_raw)

cpt_raw<-dplyr::rename(cpt_raw, diet.cat.out.T33=diet.cat.out.T34, 
                       diet.cont.out.T33=diet.cont.out.T34)

cpt_raw<-dplyr::rename(cpt_raw, diet.cat.out.T34=diet.cat.out.T34_1, 
                       diet.cont.out.T34=diet.cont.out.T34_1)


#------------------------------------------


#Converting dates to Julian using own functions

#Found an error where some dates in December were written as January (written as 1 instead of 12)--replacing erroneous dates

cpt_raw$date.T19<-gsub("1/15/16","12/15/16",cpt_raw$date.T19)



##Converts x into julian date

j.date<-function(x){
  strptime(x, "%m/%d")$yday+1
}


##Takes all columns that have "date." in the name, and converts contents to Julian day using j.date function. Renames columns (adds a 
##j to end of column name), and binds the out put julian day columns to the original data set

lapj.date<-function(df){
  date.j<-lapply(df[,grep("date.",colnames(df))],j.date)
  date.j<-as.data.frame(date.j)
  colnames(date.j)<-paste(colnames(date.j), "j", sep = ".")
  output.df<-cbind(df,date.j)
  output.df
}

cpt_raw<-lapj.date(cpt_raw)



#-----------------------------------------------------------

#Converting time columns to decimal time for age calculations


#Because of the format of the data, there are several pre-steps that need to be taken before running functions:

#First, change the names of odd numbered time point columns (1-11). This way they can be differentiated from the rest of the time point
##columns. These were afternoon time points, and therefor need to be handled differently (there were no am/pm designations in data)
cpt_raw<-dplyr::rename(cpt_raw,T1.odd=time.T1,
                       T3.odd=time.T3,
                       T5.odd=time.T5,
                       T7.odd=time.T7,
                       T9.odd=time.T9,
                       T11.odd=time.T11)

#replacing column names that have "timepoint" with "tp", so that grep will only return columns with time variables 

colnames(cpt_raw)<-gsub("timepoint", "tp", colnames(cpt_raw))



#Now use two different functions, depending on the type of time point column (odd or the rest)
  ##These take the input, turn it into a character for strsplit, divides by the : in time, and then adds the two numbers together and
  ##divides by 60 to get decimal time. If they were morning time points (not odd in 1-11), 12 is added to the time if the number is 
  ##less than 7 (none should have been weighed earlier in the morning than 7 am)--this puts the afternoon times in 24:00 format
  ##For the odd columns, which were all weighed in the afternoon, all outputs have 12 added to them

dec.time<-function(x) {
  x<-as.character(x)
  sapply(strsplit(x,":"),function(x){
    x <- as.numeric(x)
    y<-x[1]+x[2]/60
    ifelse(y<6.8, y+12, y)
    
  })
}


dec.time.odd<-function(x) {
  x<-as.character(x)
  sapply(strsplit(x,":"),function(x){
    x <- as.numeric(x)
    y<-x[1]+x[2]/60
    y<-y+12
  })
}



#Now use 2 more functions (that incorporate the above functions) to add the decimal time columns to the data set. Be sure to use the 
##output from the 1st function as the input for the 2nd, so that both odd and other columns are added to the data frame


dec.time.col<-function(df){
  dct<-lapply(df[,grep("time.",colnames(df))],dec.time)
  dct<-as.data.frame(dct)
  colnames(dct)<-paste(colnames(dct), "dec", sep = ".")
  output.df<-cbind(df,dct)
  output.df
}


dec.time.col.odd<-function(df){
  dct<-lapply(df[,grep(".odd",colnames(df))],dec.time.odd)
  dct<-as.data.frame(dct)
  colnames(dct)<-paste(colnames(dct), "dec", sep = ".")
  output.df<-cbind(df,dct)
  output.df
}



cpt_raw<-dec.time.col(cpt_raw)  
cpt_raw<-dec.time.col.odd(cpt_raw)


#Renaming the odd time columns so they match the other time columns

cpt_raw<-dplyr::rename(cpt_raw, time.T1.dec=T1.odd.dec,
              time.T3.dec=T3.odd.dec,
              time.T5.dec=T5.odd.dec,
              time.T7.dec=T7.odd.dec,
              time.T9.dec=T9.odd.dec,
              time.T11.dec=T11.odd.dec)





#This function takes julian days and decimal time columns performs the following equation:
  ## ((Day2-Day1)*24)+(Time2-Time1)
  ##The results must be done individually for each pair of columns and then added to the dataframe

hour.calc<-function(w,x,y,z){
  hour<-((w-x)*24)+(y-z)
}


#Creating columns with the number of hours between each measurement time point (delta time)
  ##names are delta.time.X, so that delta.time.1==T0-T1, delta.time.2==T1-T2, etc



cpt_raw$delta.time.1<-hour.calc(cpt_raw$date.T1.j,
                                cpt_raw$date.3.j,
                                cpt_raw$time.T1.dec,
                                cpt_raw$time.T0.dec)


cpt_raw$delta.time.2<-hour.calc(cpt_raw$date.T2.j,
                                cpt_raw$date.T1.j,
                                cpt_raw$time.T2.dec,
                                cpt_raw$time.T1.dec)


cpt_raw$delta.time.3<-hour.calc(cpt_raw$date.T3.j,
                                cpt_raw$date.T2.j,
                                cpt_raw$time.T3.dec,
                                cpt_raw$time.T2.dec)


cpt_raw$delta.time.4<-hour.calc(cpt_raw$date.T4.j,
                                cpt_raw$date.T3.j,
                                cpt_raw$time.T4.dec,
                                cpt_raw$time.T3.dec)


cpt_raw$delta.time.5<-hour.calc(cpt_raw$date.T5.j,
                                cpt_raw$date.T4.j,
                                cpt_raw$time.T5.dec,
                                cpt_raw$time.T4.dec)


cpt_raw$delta.time.6<-hour.calc(cpt_raw$date.T6.j,
                                cpt_raw$date.T5.j,
                                cpt_raw$time.T6.dec,
                                cpt_raw$time.T5.dec)


cpt_raw$delta.time.7<-hour.calc(cpt_raw$date.T7.j,
                                cpt_raw$date.T6.j,
                                cpt_raw$time.T7.dec,
                                cpt_raw$time.T6.dec)


cpt_raw$delta.time.8<-hour.calc(cpt_raw$date.T8.j,
                                cpt_raw$date.T7.j,
                                cpt_raw$time.T8.dec,
                                cpt_raw$time.T7.dec)


cpt_raw$delta.time.9<-hour.calc(cpt_raw$date.T9.j,
                                cpt_raw$date.T8.j,
                                cpt_raw$time.T9.dec,
                                cpt_raw$time.T8.dec)


cpt_raw$delta.time.10<-hour.calc(cpt_raw$date.T10.j,
                                cpt_raw$date.T9.j,
                                cpt_raw$time.T10.dec,
                                cpt_raw$time.T9.dec)


cpt_raw$delta.time.11<-hour.calc(cpt_raw$date.T11.j,
                                 cpt_raw$date.T10.j,
                                 cpt_raw$time.T11.dec,
                                 cpt_raw$time.T10.dec)


cpt_raw$delta.time.12<-hour.calc(cpt_raw$date.T12.j,
                                 cpt_raw$date.T11.j,
                                 cpt_raw$time.T12.dec,
                                 cpt_raw$time.T11.dec)


cpt_raw$delta.time.13<-hour.calc(cpt_raw$date.T13.j,
                                 cpt_raw$date.T12.j,
                                 cpt_raw$time.T13.dec,
                                 cpt_raw$time.T12.dec)


cpt_raw$delta.time.14<-hour.calc(cpt_raw$date.T14.j,
                                 cpt_raw$date.T13.j,
                                 cpt_raw$time.T14.dec,
                                 cpt_raw$time.T13.dec)


cpt_raw$delta.time.15<-hour.calc(cpt_raw$date.T15.j,
                                 cpt_raw$date.T14.j,
                                 cpt_raw$time.T15.dec,
                                 cpt_raw$time.T14.dec)


cpt_raw$delta.time.16<-hour.calc(cpt_raw$date.T16.j,
                                 cpt_raw$date.T15.j,
                                 cpt_raw$time.T16.dec,
                                 cpt_raw$time.T15.dec)


cpt_raw$delta.time.17<-hour.calc(cpt_raw$date.T17.j,
                                 cpt_raw$date.T16.j,
                                 cpt_raw$time.T17.dec,
                                 cpt_raw$time.T16.dec)


cpt_raw$delta.time.18<-hour.calc(cpt_raw$date.T18.j,
                                 cpt_raw$date.T17.j,
                                 cpt_raw$time.T18.dec,
                                 cpt_raw$time.T17.dec)


cpt_raw$delta.time.19<-hour.calc(cpt_raw$date.T19.j,
                                 cpt_raw$date.T18.j,
                                 cpt_raw$time.T19.dec,
                                 cpt_raw$time.T18.dec)


cpt_raw$delta.time.20<-hour.calc(cpt_raw$date.T20.j,
                                 cpt_raw$date.T19.j,
                                 cpt_raw$time.T20.dec,
                                 cpt_raw$time.T19.dec)


cpt_raw$delta.time.21<-hour.calc(cpt_raw$date.T21.j,
                                 cpt_raw$date.T20.j,
                                 cpt_raw$time.T21.dec,
                                 cpt_raw$time.T20.dec)


cpt_raw$delta.time.22<-hour.calc(cpt_raw$date.T22.j,
                                 cpt_raw$date.T21.j,
                                 cpt_raw$time.T22.dec,
                                 cpt_raw$time.T21.dec)


cpt_raw$delta.time.23<-hour.calc(cpt_raw$date.T23.j,
                                 cpt_raw$date.T22.j,
                                 cpt_raw$time.T23.dec,
                                 cpt_raw$time.T22.dec)


cpt_raw$delta.time.24<-hour.calc(cpt_raw$date.T24.j,
                                 cpt_raw$date.T23.j,
                                 cpt_raw$time.T24.dec,
                                 cpt_raw$time.T23.dec)


cpt_raw$delta.time.25<-hour.calc(cpt_raw$date.T25.j,
                                 cpt_raw$date.T24.j,
                                 cpt_raw$time.T25.dec,
                                 cpt_raw$time.T24.dec)


cpt_raw$delta.time.26<-hour.calc(cpt_raw$date.T26.j,
                                 cpt_raw$date.T25.j,
                                 cpt_raw$time.T26.dec,
                                 cpt_raw$time.T25.dec)


cpt_raw$delta.time.27<-hour.calc(cpt_raw$date.T27.j,
                                 cpt_raw$date.T26.j,
                                 cpt_raw$time.T27.dec,
                                 cpt_raw$time.T26.dec)


cpt_raw$delta.time.28<-hour.calc(cpt_raw$date.T28.j,
                                 cpt_raw$date.T27.j,
                                 cpt_raw$time.T28.dec,
                                 cpt_raw$time.T27.dec)


cpt_raw$delta.time.29<-hour.calc(cpt_raw$date.T29.j,
                                 cpt_raw$date.T28.j,
                                 cpt_raw$time.T29.dec,
                                 cpt_raw$time.T28.dec)


cpt_raw$delta.time.30<-hour.calc(cpt_raw$date.T30.j,
                                 cpt_raw$date.T29.j,
                                 cpt_raw$time.T30.dec,
                                 cpt_raw$time.T29.dec)


cpt_raw$delta.time.31<-hour.calc(cpt_raw$date.T31.j,
                                 cpt_raw$date.T30.j,
                                 cpt_raw$time.T31.dec,
                                 cpt_raw$time.T30.dec)


cpt_raw$delta.time.32<-hour.calc(cpt_raw$date.T32.j,
                                 cpt_raw$date.T31.j,
                                 cpt_raw$time.T32.dec,
                                 cpt_raw$time.T31.dec)


cpt_raw$delta.time.33<-hour.calc(cpt_raw$date.T33.j,
                                 cpt_raw$date.T32.j,
                                 cpt_raw$time.T33.dec,
                                 cpt_raw$time.T32.dec)


cpt_raw$delta.time.34<-hour.calc(cpt_raw$date.T34.j,
                                 cpt_raw$date.T33.j,
                                 cpt_raw$time.T34.dec,
                                 cpt_raw$time.T33.dec)


cpt_raw$delta.time.35<-hour.calc(cpt_raw$date.T35.j,
                                 cpt_raw$date.T34.j,
                                 cpt_raw$time.T35.dec,
                                 cpt_raw$time.T34.dec)


cpt_raw$delta.time.36<-hour.calc(cpt_raw$date.T36.j,
                                 cpt_raw$date.T35.j,
                                 cpt_raw$time.T36.dec,
                                 cpt_raw$time.T35.dec)


#One T37 is in January, 2017, so it calculates the J day as 1. Instead, adding 365 to 1, so that the age will be calculated
##correctly

cpt_raw$date.T37.j<-ifelse(cpt_raw$date.T37.j=="1", cpt_raw$date.T37.j+365, cpt_raw$date.T37.j)

cpt_raw$delta.time.37<-hour.calc(cpt_raw$date.T37.j,
                                 cpt_raw$date.T36.j,
                                 cpt_raw$time.T37.dec,
                                 cpt_raw$time.T36.dec)


cpt_raw$delta.time.38<-hour.calc(cpt_raw$date.T38.j,
                                 cpt_raw$date.T37.j,
                                 cpt_raw$time.T38.dec,
                                 cpt_raw$time.T37.dec)



#Calculating a running total of age since the start of the experiment using the function below:

#This function calculates running total of age from the delta.time columns. If the output is less than 0 or greater than 
  ##1000, function puts "error" in that row. Can be used to find errors in other columns, or to weed out rows with vital
  ##missing data
  ##This function must be run individually for all delta time columns. Input 1 (x) should be the previous age.T column, and 
  ##input 2 (y) should be the next delta.time column

age.calc<-function(x,y){
  age<-x+y
  ifelse(age<0 | age>1000, "error",age)
}


cpt_raw$age.T0<-0


cpt_raw$age.T1<-age.calc(cpt_raw$age.T0, cpt_raw$delta.time.1)
grep("error",cpt_raw$age.T1)

cpt_raw$age.T2<-age.calc(cpt_raw$age.T1, cpt_raw$delta.time.2)
grep("error",cpt_raw$age.T2)

cpt_raw$age.T3<-age.calc(cpt_raw$age.T2, cpt_raw$delta.time.3)
grep("error",cpt_raw$age.T3)

cpt_raw$age.T4<-age.calc(cpt_raw$age.T3, cpt_raw$delta.time.4)
grep("error",cpt_raw$age.T4)

cpt_raw$age.T5<-age.calc(cpt_raw$age.T4, cpt_raw$delta.time.5)
grep("error",cpt_raw$age.T5)

cpt_raw$age.T6<-age.calc(cpt_raw$age.T5, cpt_raw$delta.time.6)
grep("error",cpt_raw$age.T6)

cpt_raw$age.T7<-age.calc(cpt_raw$age.T6, cpt_raw$delta.time.7)
grep("error",cpt_raw$age.T7)

cpt_raw$age.T8<-age.calc(cpt_raw$age.T7, cpt_raw$delta.time.8)
grep("error",cpt_raw$age.T8)

cpt_raw$age.T9<-age.calc(cpt_raw$age.T8, cpt_raw$delta.time.9)
grep("error",cpt_raw$age.T9)

cpt_raw$age.T10<-age.calc(cpt_raw$age.T9, cpt_raw$delta.time.10)
grep("error",cpt_raw$age.T10)

cpt_raw$age.T11<-age.calc(cpt_raw$age.T10, cpt_raw$delta.time.11)
grep("error",cpt_raw$age.T11)

cpt_raw$age.T12<-age.calc(cpt_raw$age.T11, cpt_raw$delta.time.12)
grep("error",cpt_raw$age.T12)

cpt_raw$age.T13<-age.calc(cpt_raw$age.T12, cpt_raw$delta.time.13)
grep("error",cpt_raw$age.T13)

cpt_raw$age.T14<-age.calc(cpt_raw$age.T13, cpt_raw$delta.time.14)
grep("error",cpt_raw$age.T14)

cpt_raw$age.T15<-age.calc(cpt_raw$age.T14, cpt_raw$delta.time.15)
grep("error",cpt_raw$age.T15)

cpt_raw$age.T16<-age.calc(cpt_raw$age.T15, cpt_raw$delta.time.16)
grep("error",cpt_raw$age.T16)

cpt_raw$age.T17<-age.calc(cpt_raw$age.T16, cpt_raw$delta.time.17)
grep("error",cpt_raw$age.T17)

cpt_raw$age.T18<-age.calc(cpt_raw$age.T17, cpt_raw$delta.time.18)
grep("error",cpt_raw$age.T18)

cpt_raw$age.T19<-age.calc(cpt_raw$age.T18, cpt_raw$delta.time.19)
grep("error",cpt_raw$age.T19)

cpt_raw$age.T20<-age.calc(cpt_raw$age.T19, cpt_raw$delta.time.20)
grep("error",cpt_raw$age.T20)

cpt_raw$age.T21<-age.calc(cpt_raw$age.T20, cpt_raw$delta.time.21)
grep("error",cpt_raw$age.T21)

cpt_raw$age.T22<-age.calc(cpt_raw$age.T21, cpt_raw$delta.time.22)
grep("error",cpt_raw$age.T22)

cpt_raw$age.T23<-age.calc(cpt_raw$age.T22, cpt_raw$delta.time.23)
grep("error",cpt_raw$age.T23)

cpt_raw$age.T24<-age.calc(cpt_raw$age.T23, cpt_raw$delta.time.24)
grep("error",cpt_raw$age.T24)

cpt_raw$age.T25<-age.calc(cpt_raw$age.T24, cpt_raw$delta.time.25)
grep("error",cpt_raw$age.T25)

cpt_raw$age.T26<-age.calc(cpt_raw$age.T25, cpt_raw$delta.time.26)
grep("error",cpt_raw$age.T26)

cpt_raw$age.T27<-age.calc(cpt_raw$age.T26, cpt_raw$delta.time.27)
grep("error",cpt_raw$age.T27)

cpt_raw$age.T28<-age.calc(cpt_raw$age.T27, cpt_raw$delta.time.28)
grep("error",cpt_raw$age.T28)

cpt_raw$age.T29<-age.calc(cpt_raw$age.T28, cpt_raw$delta.time.29)
grep("error",cpt_raw$age.T29)

cpt_raw$age.T30<-age.calc(cpt_raw$age.T29, cpt_raw$delta.time.30)
grep("error",cpt_raw$age.T30)

cpt_raw$age.T31<-age.calc(cpt_raw$age.T30, cpt_raw$delta.time.31)
grep("error",cpt_raw$age.T31)

cpt_raw$age.T32<-age.calc(cpt_raw$age.T31, cpt_raw$delta.time.32)
grep("error",cpt_raw$age.T32)

cpt_raw$age.T33<-age.calc(cpt_raw$age.T32, cpt_raw$delta.time.33)
grep("error",cpt_raw$age.T33)

cpt_raw$age.T34<-age.calc(cpt_raw$age.T33, cpt_raw$delta.time.34)
grep("error",cpt_raw$age.T34)

cpt_raw$age.T35<-age.calc(cpt_raw$age.T34, cpt_raw$delta.time.35)
grep("error",cpt_raw$age.T35)

cpt_raw$age.T36<-age.calc(cpt_raw$age.T35, cpt_raw$delta.time.36)
grep("error",cpt_raw$age.T36)

cpt_raw$age.T37<-age.calc(cpt_raw$age.T36, cpt_raw$delta.time.37)
grep("error",cpt_raw$age.T37)

cpt_raw$age.T38<-age.calc(cpt_raw$age.T37, cpt_raw$delta.time.38)
grep("error",cpt_raw$age.T38)



