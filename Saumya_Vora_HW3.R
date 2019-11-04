# Name:Saumya Vora, Section:01 ,Course ID:IE 6600.18004.202010
library(ggplot2)
library(dplyr)
air_delay<-read.csv("airline_delay.csv")
air_delay<-na.omit(air_delay)
#1
#Getting Total Arrival delay by airlines
total_delay<-summarise(group_by(air_delay,carrier_name),arr_delay=sum(arr_delay,na.rm=TRUE))

#Bar plot
ggplot(total_delay,aes(x=reorder(carrier_name,arr_delay),y=arr_delay))+
geom_bar(stat="identity")+
coord_flip()+
labs(x = "Carrier Name",y="Total Arrival Delay")

#2
total_Delay2<-summarise(group_by(air_delay,year,carrier_name),arr_Delay=sum(arr_delay))
#Stack bar plot
ggplot(total_Delay2,aes(x=reorder(carrier_name,arr_Delay),y=arr_Delay,fill=factor(year)))+
  geom_bar(stat = "identity") +
  theme_bw()+
  guides(fill = guide_legend()) +
  coord_flip()+
  labs(x="Carrier Name",y="Total Arrival Delay",fill="Year")

#3
total_Delay3<-summarise(group_by(air_delay,year,airport),arr_delay=sum(arr_delay))
total_Delay3<-subset(total_Delay3,airport=="SFO"|airport=="ORD"|airport=="LGA"|airport=="LAX"|airport=="JFK"|airport=="EWR"|airport=="DFW"|airport=="DEN"|airport=="BOS"|airport=="ATL")
ggplot(total_Delay3, aes(x=factor(year), y=factor(airport), fill=arr_delay)) + geom_tile()+ labs(x="Year",y="Airport Name",fill="Total Arrival Delay")

#4
cldelay<-summarise(group_by(air_delay,year),Carrier_Delay=sum(carrier_delay),Late_Aircraft_Delay=sum(late_aircraft_delay),Total_delay=sum(arr_delay))
j<-c("Carrier_Delay","Late_Aircraft_Delay")

ggplot(cldelay,aes(x=year)) +
  geom_line(aes(y=Carrier_Delay,colour="Carrier Delay"))+
  geom_line(aes(y=Late_Aircraft_Delay,colour="Late Aircraft Delay"))+
  geom_point(aes(y=Carrier_Delay,colour="Carrier Delay"))+
  geom_point(aes(y=Late_Aircraft_Delay,colour="Late Aircraft Delay"))+
  labs(x="Year",y="Mins",colour="Delay_Type")

#5
#Getting the data for three airports
three_airports<-subset(air_delay,airport=="JFK"|airport=="ATL"|airport=="BOS")
#Box plot
ggplot(three_airports,aes(x=airport,y=arr_delay))+geom_boxplot()+labs(x="Airport",y="Arrival Delay in Mins")

#Violin plot
ggplot(three_airports,aes(x=airport,y=arr_delay))+geom_violin()+labs(x="Airport",y="Arrival Delay in Mins")

#Density Plot
ggplot(three_airports,aes(x=arr_delay,fill=airport))+geom_density()+labs(x="Arrival Delay (mins)",y="Density")

#6
library(readxl)
#a
#Read xlsx file
enplanement <- read_excel("cy17-commercial-service-enplanements.xlsx")
#change 9th column name to "enplanements"
names(enplanement)[9]<-"enplanements"
# sum of enplanements by Locid
z<-summarise(group_by(enplanement,Locid),Enplanement=sum(enplanements))
z<-na.omit(z)
#Change name first column to "Airport"
names(z)[1]<-"Airport"
#Change name first column to "Airport"
names(air_delay)[5]<-"Airport"
#Sum of arrival delay by Airport names.
sum_total_delay1<-summarise(group_by(air_delay,Airport),Arrival_Delay=sum(arr_delay,na.rm=TRUE))
#Joining two dataframes
Joined<-merge(z,sum_total_delay1)
#Changing the name of third column to "Arrival Delay(mins)"
names(Joined)[3]<-"Arrival Delay(mins)"
Joined

#b
names(Joined)[3]<-"Arrival_Delay"
ggplot(Joined,aes(x=Enplanement,y=Arrival_Delay))+geom_point()+xlab("Enplacement")+ylab("Arrival Delay")

#c
ggplot(Joined,aes(x=log(Enplanement),y=log(Arrival_Delay)))+geom_point()+xlab("Enplacement")+ylab("Log Arrival Delay")

