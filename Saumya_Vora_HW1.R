# Name:Saumya Vora, Section:01 ,Course ID:IE 6600.18004.202010
library(stringr)
library(dplyr)
library(tidyverse)
library(lubridate)
# Question 1
#imporitng dataset
nycollision<-read.csv("NYC Collision Data.csv",stringsAsFactors = FALSE)
nycollision<-na.omit(nycollision)

#Unique Borough
distborough<-unique(nycollision$BOROUGH)

#For every unique borough getting sum of number of accidents
print("NO. of people injured")
for(i in distborough)
{
  a<-which(nycollision$BOROUGH==i)
  mydata<-nycollision[a,c(3,11)]
  sum<-sum(mydata$NUMBER.OF.PERSONS.INJURED,na.rm=TRUE)
  cat(i,"-", sum,'\n')
  rm(a)
  rm(mydata)
}

# Question 2
#For every unique borough getting sum of people got killed
for(i in distborough)
{
  a<-which(nycollision$BOROUGH==i)
  mydata<-nycollision[a,c(3,12)]
  sum<-sum(mydata$NUMBER.OF.PERSONS.KILLED,na.rm=TRUE)
  cat(i,"-",sum,'\n')
  rm(a)
  rm(mydata)
}

#Question 3

#nycollision dataset where Borough = BRONX
Bronx<-subset(nycollision,BOROUGH=="BRONX")
#Calculating total injury, adding No. of persons injured, no. of pedestrians injured, number of cyclist injured and no. of motorist injured
Bronx$Total_Injury<-Bronx$NUMBER.OF.PERSONS.INJURED+Bronx$NUMBER.OF.PEDESTRIANS.INJURED+Bronx$NUMBER.OF.CYCLIST.INJURED+Bronx$NUMBER.OF.MOTORIST.INJURED
#dataframe with On street name and total injured people
sum_injured<-summarise(group_by(Bronx,ON.STREET.NAME),injured=sum(Total_Injury,na.rm=TRUE))
#Arranging in descending order
sum_injured<-arrange(sum_injured,desc(injured))
#Top 10 values
sum_injured<-sum_injured[1:10,]

#Question 4
#Combining values of BOROUGH, ZIP Code, On Street Name and storing in Address Column
nycollision$Address=str_c(nycollision$BOROUGH,",",nycollision$ZIP.CODE,",",nycollision$ON.STREET.NAME)

#Question 5
typeof(nycollision$TIME)
nycollision$TIME <- factor(nycollision$TIME)

# parse date
nycollision$TIME1 <- hms(as.character(nycollision$TIME))

# get hours#
nycollision$hour<-hour(nycollision$TIME1)

#dataframe for Manhattan
df3<-subset(nycollision,BOROUGH=="MANHATTAN")

#Particular hourwise number of persons injured
df4<-summarise(group_by(df3,hour),Injured=mean(NUMBER.OF.PERSONS.INJURED,na.rm=TRUE))

#Ordering data frame in decreasing order to get the first top 10 hours when accidents are maximum
df4<-df4[order(df4$Injured,decreasing=TRUE),]
typeof(df4$Injured)

#Getting first 10 rows
df5<-df4[1:10,1:2]

#Question 6
typeof(nycollision$DATE)
#Extracting Year out of the whole date

nycollision$YEAR<-substring(nycollision$DATE,7,10)

#Sumarise BOROUGH and YEAR with sum of the number of persons injured
A<-summarise(group_by(nycollision,BOROUGH,YEAR),Injured=sum(NUMBER.OF.PERSONS.INJURED,na.rm=TRUE))

#Sumarise BOROUGH and YEAR with sum of the number of persons killed
B<-summarise(group_by(nycollision,BOROUGH,YEAR),Killed=sum(NUMBER.OF.PERSONS.KILLED,na.rm=TRUE))

#Question 7
Boston_Crime<-read.csv("Boston Crime.csv")
Boston_Crime<-na.omit(Boston_Crime)
na.exclude(Boston_Crime)
coc_matrix1<-xtabs(~OFFENSE_CODE_GROUP+DISTRICT,Boston_Crime)
coc_matrix1

