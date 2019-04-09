#FUNCTION TESTING

library(readr)
library(plyr)
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



#---------------------------------------------


#Writing pair of functions that will convert dates in "date" columns to julian days, rename them, then add them to the origianl df


#attempting to write a function that will convert all dates in "date" columns to julian days

test<-cpt_raw[,grep("date.",colnames(cpt_raw))]

j.date<-function(x){
  strptime(x, "%m/%d")$yday+1
}


j.date(cpt_raw$date.hatch)

test1<-lapply(cpt_raw[,grep("date.",colnames(cpt_raw))],j.date)
View(test1)




#This one works, but still doesn't add the columns to the dataframe--won't work when I try to take out cpt_raw and specificy df in 
#function call
j.date2<-function(x){
  newcol<-strptime(x, "%m/%d")$yday+1;
  cpt_raw[["date.x.j"]]<-newcol
  cpt_raw$date.x.j
}


j.date2(cpt_raw$date.hatch)


test1.5<-lapply(cpt_raw[,grep("date.",colnames(cpt_raw))],j.date2)




#Used this as a template (https://stackoverflow.com/questions/20713739/using-a-function-to-add-a-column-in-r-data-frame/20714393#20714393)
lump <- function(db, spp.list, new.spp) { #input spp.list as a c('spp.a', 'spp.b', ...), and new.spp must be in quotes (e.g. 'new.spp')
  mini.db <- subset(db, select=spp.list);
  newcol <- as.vector(apply(mini.db, 1, max, na.rm=T));
  newcol[newcol==-Inf] <- NA;
  db[new.spp] <- newcol;
  db <- db[, !names(db) %in% spp.list];
  return(as.data.frame(db));
}


#This one works, at least for one column
j.date3<-function(df,x, new.date){
  newcol<-strptime(x, "%m/%d")$yday+1;
  df[new.date]<-newcol;
  return(as.data.frame(df))
}

test3<-j.date3(cpt_raw,cpt_raw$date.hatch,"date.hatch.j")

test4<-lapply(cpt_raw[,grep("date.",colnames(cpt_raw))],j.date3(cpt_raw,cpt_raw$date.hatch,"date.hatch.j"))

#this doesn't work either--something about a list (the grep?) not being able to be coerced to type logical
test5<-ifelse(cpt_raw[,grep("date.",colnames(cpt_raw))], j.date(cpt_raw),cpt_raw)


j.date4<-function(df,x){
  newcol<-strptime(df[[x]], "%m/%d")$yday+1;
  df[[x]]<-newcol
  df[[x]]
}

j.date4(cpt_raw,"date.hatch")




#Trying to make a set of functions, one of which has lapply within it

j.date<-function(x){
  strptime(x, "%m/%d")$yday+1
}


lapj.date0<-function(df){
  date.j<-lapply(df[,grep("date.",colnames(df))],j.date)
  date.j
}


test7<-lapj.date(cpt_raw)



lapj.date<-function(df){
  date.j<-lapply(df[,grep("date.",colnames(df))],j.date)
  date.j<-as.data.frame(date.j)
  colnames(date.j)<-paste(colnames(date.j), "j", sep = ".")
  output.df<-cbind(df,date.j)
  output.df
}

cpt_raw1<-cpt_raw

test8<-lapj.date(cpt_raw1)


test.name<-j.date(test8$date.hatch)

test8$date.hatch.j %in% test.name

#maybe useful for making new column names
files <- lapply(1:10, function(x){paste0("myfile", x, ".csv")})
colnames(m2) <- paste("Sub", colnames(m2), sep = "_")


colnames(test8)<-paste(colnames(test8), "j", sep = ".")



#----------------------------------

#Converting time columns to decimal for age computations

#Example from (https://stackoverflow.com/questions/5186972/how-to-convert-time-mmss-to-decimal-form-in-r)

minPerGame <- c("4:30","2:20","34:10")

sapply(strsplit(minPerGame,":"),
       function(x) {
         x <- as.numeric(x)
         x[1]+x[2]/60
       }
)



test<-cpt_raw

test$time.ovp<-as.character(test$time.ovp)

sapply(strsplit(test$time.ovp,":"),dec.time)



#Removing time and adding "odd" for odd time points 1-11 so that I can sort them when converting to decimal time. Odd times were weighed
  ##in the afternoon and into the evening, so they have to be converted separately (no am/pm designation)
    ###all other time points were weighed in the morning (none earlier than 7am), and while some extended into the afternoon, none went
      ####into the evening
  
test<-dplyr::rename(test,T1.odd=time.T1,
                    T3.odd=time.T3,
                    T5.odd=time.T5,
                    T7.odd=time.T7,
                    T9.odd=time.T9,
                    T11.odd=time.T11)

#replacing column names that have "timepoint" with "tp", so that grep will only return columns with time variables 

colnames(test)<-gsub("timepoint", "tp", colnames(test))


gtest<-test[,grep("time.",colnames(test))]




#Function that takes input x, turns into numeric, adds components together and divides by 60, and then adds 12 if x<7 (to make afternoon)
  ##times be in 24:00 syntax--

#Found an error when calculating ages--there was one bug weighed at 6:59 in the morning. Adjusting function to be less than 
  ##6.8, rather than 7

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



test2<-sapply(test[,grep(".even",colnames(test))],dec.time.even)
View(test2)


test3<-sapply(test[,grep(".odd",colnames(test))],dec.time.odd)
View(test3)


test4<-sapply(test[,grep("time.",colnames(test))],dec.time.even)
View(test4)

check<-cpt_raw[,c("time.T1","time.T3","time.T5","time.T7")]




dec.time.col<-function(df){
  dct<-lapply(df[,grep("time.",colnames(df))],dec.time)
  dct<-as.data.frame(dct)
  colnames(dct)<-paste(colnames(dct), "dec", sep = ".")
  output.df<-cbind(df,dct)
  output.df
}


test2<-dec.time.col(test)




dec.time.col.odd<-function(df){
  dct<-lapply(df[,grep(".odd",colnames(df))],dec.time.odd)
  dct<-as.data.frame(dct)
  colnames(dct)<-paste(colnames(dct), "dec", sep = ".")
  output.df<-cbind(df,dct)
  output.df
}


test2<-dec.time.col.odd(test2)


dec.time.col2<-function(df){
  dct<-sapply(df[,grep("time.",colnames(df))],dec.time)
  dct
}


dec.time.col2(test)
dec.time(test$time.ovp)



#-----------------------------------------------------------


#Writing a function that will add julian date columns to decimal time columns, so I can calculate age
  ##Have to run the j.date and dec.time.col functions before doing this practice


#Should follow this formula: ((Day2-Day1)*24)+(time2-time1)


#Can do it separately for each column
test1<-(test$date.T1.j*24)
test2<-test$date.T2.j*24

test1.5<-test1+test$time.T1.dec
test2.5<-test2+test$time.T2.dec

age.test<-test2.5-test1.5


#or can combine the steps into one command

age.test2<-((test$date.T2.j-test$date.T1.j)*24)+(test$time.T2.dec-test$time.T1.dec)

age<-data.frame(age.test,age.test2)


#not sure which way is best for function, since everything needs to be done in order

#Maybe separate into multiple functions, one where it multiplies date.j columns by 24, one where it adds matching timepoint
  ##date and time columns together, and one where it subtracts them from the previous one. Or, depending on how long it takes
  ##to figure out, I could do the first two with functions, and then do the last one manually. Not sure how I would do the 
  ##subtraction in the order it needs to be done 


date.j.24<-function(df){
  j.24<-lapply(df[,grep(".j",colnames(df))],function(x) x*24)
  j.24<-as.data.frame(j.24)
  colnames(j.24)<-paste(colnames(j.24), "24", sep = ".")
  output.df<-cbind(df,j.24)
  output.df
}

test<-date.j.24(test)

test.day<-test[,grep("T1.j.24",colnames(test))]
test.hour<-test[,grep("T1.dec",colnames(test))]

test.tot.time<-test.day+test.hour


#This does not work, it won't seem to accept any argument to x to define string match parameters. Returns a dataframe with 0
  ##entries
day.hour.add<-function(df,x){
  day<-df[,grep("x.j.24",colnames(df))]
  hour<-df[,grep("x.dec",colnames(df))]
  day+hour
}

test.add<-day.hour.add(test,"T1")






T1<-match(test[grep("T1.j.24",colnames(test))],test)




#Use this function if you have not created j.24 columns with the date.j.24 function

hour.calc<-function(w,x,y,z){
 hour<-((w-x)*24)+(y-z)
}



test1<-hour.calc(test$date.T2.j, test$date.T1.j, test$time.T2.dec, test$time.T1.dec)

test0<-hour.calc(test$date.T1.j, test$date.T0.j, test$time.T1.dec, test$time.T.dec)



age<-data.frame(age.test,age.test2,test1)


test2<-lapply(c(test$date.T2.j, test$date.T1.j, test$time.T2.dec, test$time.T1.dec), hour.calc)



T1<-test$date.T1.j.24+test$time.T1.dec


#--------------------------------------

#Writing a function that calculated a running total of age from the delta time columns created using the hour.calc function

cpt_raw$age.T0<-0

test<-cpt_raw

age.calc<-function(x,y){
  age<-x+y
  ifelse(age<0 | age>1000, "error",age)
}



test$age.T1<-age.calc(test$age.T0,test$delta.time.1)

check<-test[,c("ID","date.died.j","time.T0.dec","delta.time.1","age.T1")]

#Found a row with an error--time.T0 was 6:59 in the morning. Need to adjust time.dec functions to be less than 6.8 rather
  ##than less than 7

x341<-test[269,]
check.x341<-x341[,c("date.3.j","date.T1.j","time.T0","time.T0.dec","time.T1.dec","delta.time.1","age.T0","age.T1")]

#Checking a few other time points for other possible errors

test$age.T2<-age.calc(test$age.T1,test$delta.time.2)

check2<-test[,c("ID","date.died.j","time.T1.dec","delta.time.2","age.T1","age.T2")]


test$age.T3<-age.calc(test$age.T2, test$delta.time.3)

check3<-test[,c("ID","date.died.j","time.T2.dec","delta.time.3","age.T2","age.T3")]


#----------------------------------------

#Writing a function that inputs the instar at each time point by checking date of timepoint against date of molt

test.instar<-ifelse(test$date.T1.j<=test$date.3.j, "3",
                    ifelse(test$date.T1.j>=test$date.4.j & test$date.T1.j<=test$date.5.j, "4",
                           ifelse(test$date.T1.j>=test$date.5.j & test$date.T1.j<test$date.wander.j, "5",
                                  ifelse(test$date.T1.j==test$date.wander.j, w, 0))))


#This mostly works, but throws an NA if a para caterpillar had wasp em during the 4th instar (date.5==NA)

test.instar10<-ifelse(test$date.T10.j<=test$date.3.j, "3",
                    ifelse(test$date.T10.j>=test$date.4.j & test$date.T10.j<=test$date.5.j, "4",
                           ifelse(test$date.T10.j>=test$date.5.j & test$date.T10.j<test$date.wander.j, "5",
                                  ifelse(test$date.T10.j==test$date.wander.j, w,
                                         ifelse(test$date.T10.j==test$date.em.j, "em", 0)))))


check.instar10<-test[,c("ID","treatment","date.T10.j","date.3.j","date.4.j","date.5.j","date.wander.j","date.em.j")]
check.instar10$test.inst10<-test.instar10


#-----------------------------------

#Writing a function that will calculate change in mass from each time point to the next
  ##Like delta.time, delta.mass column will be in the format of T0-T1==delta.mass.1, T1-T2==delta.mass.2, etc
  ##Some of the values are negative, and I don't think they are errors. I just want to avoid big mistakes. 

#It is very difficult to capture errors across time, as the amount the caterpillars eat and gain mass changes drastically
  ##as they age. I think I will try making 2 conservative functions (small and large), plot it, and see how many errors slip 
  ##through


dm.calc.small.1_3<-function(x,y){
  d.mass<-y-x
  ifelse(d.mass< -20 | d.mass>200, "error", d.mass)
}


dm.calc.small.4_7<-function(x,y){
  d.mass<-y-x
  ifelse(d.mass< -50 | d.mass>500, "error", d.mass)
}


dm.calc.large<-function(x,y){
  d.mass<-y-x
  ifelse(d.mass< -50 | d.mass>1000, "error", d.mass)
}


dm.calc.blank<-function(x,y){
  d.mass<-y-x
}

#Found 2 rows that have a delta.mass.1 that are obviously too high (>400mg). Probably a decimal place mistake. Not sure the 
##best way to handle this--for now, am changing to what looks like a possibly correct weight

test[197,38]<-65.3
test[267,38]<-51.2



test$mg.T0<-0

test$delta.mass.1<-dm.calc.small.1_3(test$mass.T0,test$mass.T1)
grep("error", test$delta.mass.1)


check.dm<-test[,c("ID","mass.T0","mass.T1","delta.mass.1")]
which(test$delta.mass.1>100)



test$delta.mass.2<-dm.calc.small.1_3(test$mass.T1,test$mass.T2)
grep("error", test$delta.mass.2)


test$delta.mass.3<-dm.calc.small.1_3(test$mass.T2,test$mass.T3)
grep("error", test$delta.mass.3)


#Found several caterpillars with ~ -25 and ~200 delta mass. Adjusted the error conditions for timepoints 4 onward

test$delta.mass.4<-dm.calc.small.4_7(test$mass.T3,test$mass.T4)
grep("error", test$delta.mass.4)

check.mass4<-test[,c("ID","treatment","temp","fed.T4","mass.T3","mass.T4","delta.mass.4")]


test$delta.mass.5<-dm.calc.small.4_7(test$mass.T4,test$mass.T5)
grep("error", test$delta.mass.5)

#Row 115, ID 149 has an error--goes from 97.53 (mass.T4) to 19.87 (mass.T5)--if it's an entry error, not sure what. 198.7?
  ##that seems like a large increase for 20 C

check.mass5<-test[,c("ID","treatment","temp","fed.T5","mass.T4","mass.T5","delta.mass.5")]


test$delta.mass.6<-dm.calc.small.4_7(test$mass.T5,test$mass.T6)
grep("error", test$delta.mass.6)

#Rows 42, 117 and 119 had mass changes of >500mg, all at 30C. Not sure if that is accurate? but possible over 18 hours

check.mass6<-test[,c("ID","treatment","temp","fed.T6","mass.T5","mass.T6","delta.mass.6")]


test$delta.mass.7<-dm.calc.small.4_7(test$mass.T6,test$mass.T7)
grep("error", test$delta.mass.7)


#At this time point, changes over 500 seem to become common. Writing another function

test$delta.mass.8<-dm.calc.large(test$mass.T7,test$mass.T8)
grep("error", test$delta.mass.8)

#row 2, 42 and 106 have mass change more than -50, not sure if error or not

check.mass8<-test[,c("ID","treatment","temp","fed.T8","mass.T7","mass.T8","delta.mass.8")]



test$delta.mass.9<-dm.calc.large(test$mass.T8,test$mass.T9)
grep("error", test$delta.mass.9)

#row 41, 111 and 187 have mass change more than -50, not sure if error or not

check.mass9<-test[,c("ID","treatment","temp","fed.T9","mass.T8","mass.T9","delta.mass.9")]


test$delta.mass.10<-dm.calc.large(test$mass.T9,test$mass.T10)
grep("error", test$delta.mass.10)

check.mass10<-test[,c("ID","treatment","temp","instar.T10","fed.T10","mass.T9","mass.T10","delta.mass.10")]








#------------------------------------

#Writing a function that will calculate a running total of mass gain from the start of the experiment


mass.gain.calc<-function(x,y){
  mg<-x+y
  ifelse(mg<0 | mg>20000, "error", mg)
}



#----------------------------------


####FINAL FUNCTIONS:


#Convert dates into Julian date:

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





#Changing times to a decimal format:

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




#This function takes julian days and decimal time columns performs the following equation:
  ## ((Day2-Day1)*24)+(Time2-Time1)
  ##The results must be done individually for each pair of columns and then added to the dataframe

hour.calc<-function(w,x,y,z){
  hour<-((w-x)*24)+(y-z)
}




#This function calculates running total of age from the delta.time columns. If the output is less than 0 or greater than 
  ##1000, function puts "error" in that row. Can be used to find errors in other columns, or to weed out rows with vital
  ##missing data
  ##This function must be run individually for all delta time columns. Input 1 (x) should be the previous age.T column, and 
  ##input 2 (y) should be the next delta.time column

age.calc<-function(x,y){
  age<-x+y
  ifelse(age<0 | age>1000, "error",age)
}





