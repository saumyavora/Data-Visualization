library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape)
#Read the Dataset
FMI<-read.csv("farmers_market_info.csv")#omitting NAs in the dataset
FMI[FMI==""]<-NA
FMI[FMI=="-"]<-NA

#1
#How many farmers markets are in each states?
#States Counts
State<-data.frame(table(FMI$State))
#Barplot 
ggplot(State,aes(x =reorder(Var1,Freq) ,y=Freq,fill=Var1))+
  geom_bar(stat = "identity") + 
  theme(legend.position = 'right')+
  coord_flip()+
  labs(x="States",y="No.of Farmers Market")+ theme(legend.position='none')+
  geom_text(aes(label=Freq),hjust = 1.5, size=3.2)

#How many farmers markets are there in each region?
#Region wise distribution of farmers Market
FMI_Regions<-FMI %>% select(FMID,MarketName,State,city) 
Regions<-read.csv("Regions.csv")
names(Regions)[1]<-"State"
FMI_Regions<-merge(x=FMI_Regions,y=Regions,by='State')

#Bar plot
FMI_Regions<- FMI_Regions %>% group_by(Region) %>% tally()
ggplot(FMI_Regions,aes(x=reorder(Region,n),y=n,fill=Region))+
  geom_bar(stat="identity")+
  labs(x="Regions of USA",y="No.of Farmer Markets",fill="Regions")+
  coord_flip()+theme(legend.title = element_text(color = "black", size = 15),
                    legend.text = element_text(color = "black"),
                    legend.background = element_rect(color="black",fill = "darkgray"),
                    legend.key = element_rect(fill = "lightblue", color = NA),
                    legend.key.size = unit(1.5, "cm"),
                    legend.key.width = unit(0.5,"cm"),
                    axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"))+
                    geom_text(aes(label=n),hjust = 1.5, size=5)
                    
#2
#Give time based insights (trends) that show the evolution of farmers market.

FMI2<-FMI %>% select(Season1Date,Season2Date,Season3Date,Season4Date)
FMI2[c('Season1_Start','Season1_End')] <- colsplit(FMI2$Season1Date,' to ',c('a','b'))
FMI2[c('Season2_Start','Season2_End')] <- colsplit(FMI2$Season2Date,' to ',c('a','b'))
FMI2[c('Season3_Start','Season3_End')] <- colsplit(FMI2$Season3Date,' to ',c('a','b'))
FMI2[c('Season4_Start','Season4_End')] <- colsplit(FMI2$Season4Date,' to ',c('a','b'))
FMI_S1<-FMI2[,c(5,6)]
FMI_S2<-FMI2[,c(7,8)]
FMI_S3<-FMI2[,c(9,10)]
FMI_S4<-FMI2[,c(11,12)]

typeof(FMI_S1$Season1_Start)
FMI_S1$Season1_Start<-as.character(FMI_S1$Season1_Start)
typeof(FMI_S1$Season1_End)
FMI_S1$Season1_End<-as.character(FMI_S1$Season1_End)

sum(is.na(FMI_S1))

FMI_S1[which(FMI_S1$Season1_Start=="January"),]<-"01"
FMI_S1[which(FMI_S1$Season1_Start=="February"),]<-"02"
FMI_S1[which(FMI_S1$Season1_Start=="March"),]<-"03"
FMI_S1[which(FMI_S1$Season1_Start=="April"),]<-"04"
FMI_S1[which(FMI_S1$Season1_Start=="May"),]<-"05"
FMI_S1[which(FMI_S1$Season1_Start=="June"),]<-"06"
FMI_S1[which(FMI_S1$Season1_Start=="July"),]<-"07"
FMI_S1[which(FMI_S1$Season1_Start=="August"),]<-"08"
FMI_S1[which(FMI_S1$Season1_Start=="September"),]<-"09"
FMI_S1[which(FMI_S1$Season1_Start=="October"),]<-"10"
FMI_S1[which(FMI_S1$Season1_Start=="November"),]<-"11"
FMI_S1[which(FMI_S1$Season1_Start=="December"),]<-"12"

FMI_S1[which(FMI_S1$Season1_End=="January"),]<-"01"
FMI_S1[which(FMI_S1$Season1_End=="February"),]<-"02"
FMI_S1[which(FMI_S1$Season1_End=="March"),]<-"03"
FMI_S1[which(FMI_S1$Season1_End=="April"),]<-"04"
FMI_S1[which(FMI_S1$Season1_End=="May"),]<-"05"
FMI_S1[which(FMI_S1$Season1_End=="June"),]<-"06"
FMI_S1[which(FMI_S1$Season1_End=="July"),]<-"07"
FMI_S1[which(FMI_S1$Season1_End=="August"),]<-"08"
FMI_S1[which(FMI_S1$Season1_End=="September"),]<-"09"
FMI_S1[which(FMI_S1$Season1_End=="October"),]<-"10"
FMI_S1[which(FMI_S1$Season1_End=="November"),]<-"11"
FMI_S1[which(FMI_S1$Season1_End=="December"),]<-"12"

FMI_S1[FMI_S1==""]<-NA
FMI_S1<-na.omit(FMI_S1)

FMI_S1$Season1_Start_year<-substr(FMI_S1$Season1_Start,7,10)
FMI_S1$Season1_End_year<-substr(FMI_S1$Season1_End,7,10)
FMI_S1$Season1_Start<-substr(FMI_S1$Season1_Start,1,2)
FMI_S1$Season1_End<-substr(FMI_S1$Season1_End,1,2)
FMI_S1$Season1_Start<-as.factor(FMI_S1$Season1_Start)

FMI_S1y<-data.frame(table(FMI_S1$Season1_Start_year))
FMI_S1y<-FMI_S1y[9:19,]
FMI_S<-data.frame(table(FMI_S1$Season1_Start))
FMI_SE<-data.frame(table(FMI_S1$Season1_End))
FMI_S<-FMI_S[1:12,c(1,2)]
FMI_SE<-FMI_SE[1:12,c(1,2)]

#Showing trend of Yearwise Farmers Market
ggplot(FMI_S1y,aes(x=Var1,y=Freq,group=1))+
  geom_line(color="red",size=2)+geom_point(color="blue",size=2)+
  ggtitle("Yearwise Trend of Farmers Market")+
  labs(x="Years",y="No. of Markets")+
  theme(axis.line = element_line(color = "darkblue",size = 1, linetype = "solid"))+
  geom_text(aes(label=Freq),hjust = 1.5, size=3.2)
 
#Showing the trend of starting of season months.
ggplot(FMI_S,aes(x=as.numeric(Var1),y=Freq))+
  geom_line(color="red",size=2)+geom_point(color="blue",size=2)+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  ggtitle("Markets Started in Season 1 ")+
  labs(x="Months",y="No. of Markets")+
  theme(axis.line = element_line(color = "darkblue",size = 1, linetype = "solid"))+
  geom_text(aes(label=Freq),hjust = 1.5, size=3.2)

#Showing the trend of ending of season months.
ggplot(FMI_SE,aes(x=as.numeric(Var1),y=Freq))+geom_line(color="red",size=2)+geom_point(color="blue",size=2)+
scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  ggtitle("Markets Ended in Season 1 ")+theme_minimal()  +
  labs(x="Months",y="No. of Markets")+ 
  geom_text(aes(label=Freq),hjust = 1.5, size=3.2)+
  theme(plot.title=element_text(size=18, face="bold",color="black",lineheight=1.2, hjust = 0.4),axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"))

#2-2
typeof(FMI_S2$Season2_Start)
FMI_S2$Season2_Start<-as.character(FMI_S2$Season2_Start)
typeof(FMI_S2$Season2_End)
FMI_S2$Season2_End<-as.character(FMI_S2$Season2_End)

sum(is.na(FMI_S2))
FMI_S2[which(FMI_S2$Season2_Start=="January"),]<-"01"
FMI_S2[which(FMI_S2$Season2_Start=="February"),]<-"02"
FMI_S2[which(FMI_S2$Season2_Start=="March"),]<-"03"
FMI_S2[which(FMI_S2$Season2_Start=="April"),]<-"04"
FMI_S2[which(FMI_S2$Season2_Start=="May"),]<-"05"
FMI_S2[which(FMI_S2$Season2_Start=="June"),]<-"06"
FMI_S2[which(FMI_S2$Season2_Start=="July"),]<-"07"
FMI_S2[which(FMI_S2$Season2_Start=="August"),]<-"08"
FMI_S2[which(FMI_S2$Season2_Start=="September"),]<-"09"
FMI_S2[which(FMI_S2$Season2_Start=="October"),]<-"10"
FMI_S2[which(FMI_S2$Season2_Start=="November"),]<-"11"
FMI_S2[which(FMI_S2$Season2_Start=="December"),]<-"12"

FMI_S2[which(FMI_S2$Season2_End=="January"),]<-"01"
FMI_S2[which(FMI_S2$Season2_End=="February"),]<-"02"
FMI_S2[which(FMI_S2$Season2_End=="March"),]<-"03"
FMI_S2[which(FMI_S2$Season2_End=="April"),]<-"04"
FMI_S2[which(FMI_S2$Season2_End=="May"),]<-"05"
FMI_S2[which(FMI_S2$Season2_End=="June"),]<-"06"
FMI_S2[which(FMI_S2$Season2_End=="July"),]<-"07"
FMI_S2[which(FMI_S2$Season2_End=="August"),]<-"08"
FMI_S2[which(FMI_S2$Season2_End=="September"),]<-"09"
FMI_S2[which(FMI_S2$Season2_End=="October"),]<-"10"
FMI_S2[which(FMI_S2$Season2_End=="November"),]<-"11"
FMI_S2[which(FMI_S2$Season2_End=="December"),]<-"12"

FMI_S2[FMI_S2==""]<-NA
FMI_S2<-na.omit(FMI_S2)

FMI_S2$Season2_Start<-substr(FMI_S2$Season2_Start,1,2)
FMI_S2$Season2_End<-substr(FMI_S2$Season2_End,1,2)
FMI_S2$Season2_Start<-as.factor(FMI_S2$Season2_Start)
FMI_SS<-data.frame(table(FMI_S2$Season2_Start))
FMI_SSE<-data.frame(table(FMI_S2$Season2_End))
FMI_SS<-FMI_SS[1:12,c(1,2)]
FMI_SSE<-FMI_SSE[1:12,c(1,2)]
#Showing the trend of starting of season months.
ggplot(FMI_SS,aes(x=as.numeric(Var1),y=Freq))+geom_line(color="red",size=2)+geom_point(color="blue",size=2)+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  ggtitle("Markets Started in Season 2 ")+theme_minimal()  +
  labs(x="Months",y="No. of Markets")+  geom_text(aes(label=Freq),hjust = 1.5, size=3.2)+
  theme(plot.title=element_text(size=18, face="bold",color="black",lineheight=1.2, hjust = 0.4),axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"))

#Showing the trend of ending of season months.
ggplot(FMI_SSE,aes(x=as.numeric(Var1),y=Freq))+geom_line(color="red",size=2)+geom_point(color="blue",size=2)+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  ggtitle("Markets Ended in Season 2 ")+theme_minimal()  +
  labs(x="Months",y="No. of Markets")+  geom_text(aes(label=Freq),hjust = 1.5, size=3.2)+
  theme(plot.title=element_text(size=18, face="bold",color="black",lineheight=1.2, hjust = 0.4),axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"))


#2-3
typeof(FMI_S3$Season3_Start)
FMI_S3$Season3_Start<-as.character(FMI_S3$Season3_Start)
typeof(FMI_S3$Season3_End)
FMI_S3$Season3_End<-as.character(FMI_S3$Season3_End)

sum(is.na(FMI_S3))
FMI_S3[which(FMI_S3$Season3_Start=="January"),]<-"01"
FMI_S3[which(FMI_S3$Season3_Start=="February"),]<-"02"
FMI_S3[which(FMI_S3$Season3_Start=="March"),]<-"03"
FMI_S3[which(FMI_S3$Season3_Start=="April"),]<-"04"
FMI_S3[which(FMI_S3$Season3_Start=="May"),]<-"05"
FMI_S3[which(FMI_S3$Season3_Start=="June"),]<-"06"
FMI_S3[which(FMI_S3$Season3_Start=="July"),]<-"07"
FMI_S3[which(FMI_S3$Season3_Start=="August"),]<-"08"
FMI_S3[which(FMI_S3$Season3_Start=="September"),]<-"09"
FMI_S3[which(FMI_S3$Season3_Start=="October"),]<-"10"
FMI_S3[which(FMI_S3$Season3_Start=="November"),]<-"11"
FMI_S3[which(FMI_S3$Season3_Start=="December"),]<-"12"

FMI_S3[which(FMI_S3$Season3_End=="January"),]<-"01"
FMI_S3[which(FMI_S3$Season3_End=="February"),]<-"02"
FMI_S3[which(FMI_S3$Season3_End=="March"),]<-"03"
FMI_S3[which(FMI_S3$Season3_End=="April"),]<-"04"
FMI_S3[which(FMI_S3$Season3_End=="May"),]<-"05"
FMI_S3[which(FMI_S3$Season3_End=="June"),]<-"06"
FMI_S3[which(FMI_S3$Season3_End=="July"),]<-"07"
FMI_S3[which(FMI_S3$Season3_End=="August"),]<-"08"
FMI_S3[which(FMI_S3$Season3_End=="September"),]<-"09"
FMI_S3[which(FMI_S3$Season3_End=="October"),]<-"10"
FMI_S3[which(FMI_S3$Season3_End=="November"),]<-"11"
FMI_S3[which(FMI_S3$Season3_End=="December"),]<-"12"

FMI_S3[FMI_S3==""]<-NA
FMI_S3<-na.omit(FMI_S3)

FMI_S3$Season3_Start<-substr(FMI_S3$Season3_Start,1,2)
FMI_S3$Season3_End<-substr(FMI_S3$Season3_End,1,2)
FMI_S3$Season3_Start<-as.factor(FMI_S3$Season3_Start)
FMI_SSS<-data.frame(table(FMI_S3$Season3_Start))
FMI_SSSE<-data.frame(table(FMI_S3$Season3_End))
FMI_SSS<-FMI_SSS[1:12,c(1,2)]
FMI_SSSE<-data.frame(table(FMI_S3$Season3_End))
#Showing the trend of starting of season months.
ggplot(FMI_SSS,aes(x=as.numeric(Var1),y=Freq))+geom_line(color="red",size=2)+geom_point(color="blue",size=2)+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  ggtitle("Markets Started in Season 3 ")+theme_minimal()  +
  labs(x="Months",y="No. of Markets")+  geom_text(aes(label=Freq),hjust = 1.5, size=3.2)+
  theme(plot.title=element_text(size=18, face="bold",color="black",lineheight=1.2, hjust = 0.4),axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"))

#Showing the trend of ending of season months.
ggplot(FMI_SSSE,aes(x=as.numeric(Var1),y=Freq))+geom_line(color="red",size=2)+geom_point(color="blue",size=2)+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  ggtitle("Markets Ended in Season 3 ")+theme_minimal()  +
  labs(x="Months",y="No. of Markets")+  geom_text(aes(label=Freq),hjust = 1.5, size=3.2)+
  theme(plot.title=element_text(size=18, face="bold",color="black",lineheight=1.2, hjust = 0.4),axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"))

#2-4
typeof(FMI_S4$Season4_Start)
FMI_S4$Season4_Start<-as.character(FMI_S4$Season4_Start)
typeof(FMI_S4$Season4_End)
FMI_S4$Season4_End<-as.character(FMI_S4$Season4_End)

sum(is.na(FMI_S4))
FMI_S4[which(FMI_S4$Season4_Start=="January"),]<-"01"
FMI_S4[which(FMI_S4$Season4_Start=="February"),]<-"02"
FMI_S4[which(FMI_S4$Season4_Start=="March"),]<-"03"
FMI_S4[which(FMI_S4$Season4_Start=="April"),]<-"04"
FMI_S4[which(FMI_S4$Season4_Start=="May"),]<-"05"
FMI_S4[which(FMI_S4$Season4_Start=="June"),]<-"06"
FMI_S4[which(FMI_S4$Season4_Start=="July"),]<-"07"
FMI_S4[which(FMI_S4$Season4_Start=="August"),]<-"08"
FMI_S4[which(FMI_S4$Season4_Start=="September"),]<-"09"
FMI_S4[which(FMI_S4$Season4_Start=="October"),]<-"10"
FMI_S4[which(FMI_S4$Season4_Start=="November"),]<-"11"
FMI_S4[which(FMI_S4$Season4_Start=="December"),]<-"12"

FMI_S4[which(FMI_S4$Season4_End=="January"),]<-"01"
FMI_S4[which(FMI_S4$Season4_End=="February"),]<-"02"
FMI_S4[which(FMI_S4$Season4_End=="March"),]<-"03"
FMI_S4[which(FMI_S4$Season4_End=="April"),]<-"04"
FMI_S4[which(FMI_S4$Season4_End=="May"),]<-"05"
FMI_S4[which(FMI_S4$Season4_End=="June"),]<-"06"
FMI_S4[which(FMI_S4$Season4_End=="July"),]<-"07"
FMI_S4[which(FMI_S4$Season4_End=="August"),]<-"08"
FMI_S4[which(FMI_S4$Season4_End=="September"),]<-"09"
FMI_S4[which(FMI_S4$Season4_End=="October"),]<-"10"
FMI_S4[which(FMI_S4$Season4_End=="November"),]<-"11"
FMI_S4[which(FMI_S4$Season4_End=="December"),]<-"12"

FMI_S4[FMI_S4==""]<-NA
FMI_S4<-na.omit(FMI_S4)

FMI_S4$Season4_Start<-substr(FMI_S4$Season4_Start,1,2)
FMI_S4$Season4_End<-substr(FMI_S4$Season4_End,1,2)
FMI_S4$Season4_Start<-as.factor(FMI_S4$Season4_Start)
FMI_SSSS<-data.frame(table(FMI_S4$Season4_Start))
FMI_SSSSE<-data.frame(table(FMI_S4$Season4_End))
FMI_SSSS<-FMI_SSSS[1:12,c(1,2)]
FMI_SSSSE<-FMI_SSSSE[1:12,c(1,2)]
#Showing the trend of starting of season months.
ggplot(FMI_SSSS,aes(x=as.numeric(Var1),y=Freq))+geom_line(color="red",size=2)+geom_point(color="blue",size=2)+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  ggtitle("Markets Started in Season 4 ")+theme_minimal()  +
  labs(x="Months",y="No. of Markets")+  geom_text(aes(label=Freq),hjust = 1.5, size=3.2)+
  theme(plot.title=element_text(size=18, face="bold",color="black",lineheight=1.2, hjust = 0.4),axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"))

#Showing the trend of ending of season months.
ggplot(FMI_SSSSE,aes(x=as.numeric(Var1),y=Freq))+geom_line(color="red",size=2)+geom_point(color="blue",size=2)+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+
  ggtitle("Markets Ended in Season 4 ")+theme_minimal()  +
  labs(x="Months",y="No. of Markets")+  geom_text(aes(label=Freq),hjust = 1.5, size=3.2)+
  theme(plot.title=element_text(size=18, face="bold",color="black",lineheight=1.2, hjust = 0.4),axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"))

#3
#How does the count vary for each product type?
FMI3<-FMI[,c(29:59)]
FMI3<-na.omit(FMI3)
#How does the count vary for each product type?
#Organic
Organic<-data.frame(table(FMI3$Organic))
names(Organic)[2]<-"Organic"

#Backedgoods
Backedgoods<-data.frame(table(FMI3$Bakedgoods))
names(Backedgoods)[2]<-"Backedgoods"

#Cheese
Cheese<-data.frame(table(FMI3$Cheese))
names(Cheese)[2]<-"Cheese"

#Craftes
Crafts<-data.frame(table(FMI3$Crafts))
names(Crafts)[2]<-"Crafts"

#Flowers
Flowers<-data.frame(table(FMI3$Flowers))
names(Flowers)[2]<-"Flowers"

#Eggs
Eggs<-data.frame(table(FMI3$Eggs))
names(Eggs)[2]<-"Eggs"

#Seafood
Seafood<-data.frame(table(FMI3$Seafood))
names(Seafood)[2]<-"Seafood"

#Herbs
Herbs<-data.frame(table(FMI3$Herbs))
names(Herbs)[2]<-"Herbs"

#Vegetables
Vegetables<-data.frame(table(FMI3$Vegetables))
names(Vegetables)[2]<-"Vegetables"

#Honey
Honey<-data.frame(table(FMI3$Honey))
names(Honey)[2]<-"Honey"

#Jams
Jams<-data.frame(table(FMI3$Jams))
names(Jams)[2]<-"Jams"

#Maple
Maple<-data.frame(table(FMI3$Maple))
names(Maple)[2]<-"Maple"

#Meat
Meat<-data.frame(table(FMI3$Meat))
names(Meat)[2]<-"Meat"

#Nursery
Nursery<-data.frame(table(FMI3$Nursery))
names(Nursery)[2]<-"Nursery"

#Nuts
Nuts<-data.frame(table(FMI3$Nuts))
names(Nuts)[2]<-"Nuts"

#Plants
Plants<-data.frame(table(FMI3$Plants))
names(Plants)[2]<-"Plants"

#Poultry
Poultry<-data.frame(table(FMI3$Poultry))
names(Poultry)[2]<-"Poultry"

#Prepared
Prepared<-data.frame(table(FMI3$Prepared))
names(Prepared)[2]<-"Prepared"

#Soap
Soap<-data.frame(table(FMI3$Soap))
names(Soap)[2]<-"Soap"

#Trees
Trees<-data.frame(table(FMI3$Trees))
names(Trees)[2]<-"Trees"

#Wine
Wine<-data.frame(table(FMI3$Wine))
names(Wine)[2]<-"Wine"

#Coffee
Coffee<-data.frame(table(FMI3$Coffee))
names(Coffee)[2]<-"Coffee"

#Fruits
Fruits<-data.frame(table(FMI3$Fruits))
names(Fruits)[2]<-"Fruits"

#Grains
Grains<-data.frame(table(FMI3$Grains))
names(Grains)[2]<-"Grains"

#Juices
Juices<-data.frame(table(FMI3$Juices))
names(Juices)[2]<-"Juices"

MyMerge <- function(x, y){
  df <- merge(x, y, by= "Var1")
  return(df)
}
new <- data.frame(Reduce(MyMerge, list(Organic,Backedgoods,Cheese,Crafts,Flowers,Eggs,Seafood,Herbs,Vegetables,Honey,Jams,Maple,Meat,Nursery,Nuts,Plants,Poultry,Prepared,Soap,Trees,Wine,Coffee,Fruits,Grains,Juices)))
new<-data.frame(t(new))
new<-tibble::rownames_to_column(new,"Products")
new= new[-1, ]
colnames(new)[2]<-"No"
colnames(new)[3]<-"Yes"
new<-as.data.frame(new)
new$Yes=as.numeric(levels(new$Yes))[new$Yes]
new$No=as.numeric(levels(new$No))[new$No]
p1<-ggplot(new,aes(x=Products,y=Yes,fill=Products))+geom_bar(stat="identity")+coord_flip()+
  labs(x="Products",y="No. of Yes")+theme(legend.position='none')+
  geom_text(aes(label=Yes),hjust = 1.5, size=3.2)

p2<-ggplot(new,aes(x=Products,y=No,fill=Products))+geom_bar(stat="identity")+coord_flip()+
  labs(x="Products",y="No. of No")+theme(legend.position='none')+
  geom_text(aes(label=No),hjust = 1.5, size=3.2)

grid.arrange(p1,p2,ncol=2)

#4

FMI4<-FMI[,c(11,24:28)]
credit<-data.frame(table(FMI4$Credit))
names(credit)[2]<-"Credit"
wic<-data.frame(table(FMI4$WIC))
names(wic)[2]<-"WIC"
wiccash<-data.frame(table(FMI4$WICcash))
names(wiccash)[2]<-"wiccash"
SFMNP<-data.frame(table(FMI4$SFMNP))
names(SFMNP)[2]<-"SFMNP"
SNAP<-data.frame(table(FMI4$SNAP))
names(SNAP)[2]<-"SNAP"

new1 <- data.frame(Reduce(MyMerge,list(credit,wic,wiccash,SFMNP,SNAP)))
new1<-data.frame(t(new1))
new1 <- data.frame(Type = row.names(new1), new1)
new1<-new1[-1,]
colnames(new1)[2]<-"N"
colnames(new1)[3]<-"Y"
new1$Type<-as.factor(new1$Type)
new1$Y=as.numeric(levels(new1$Y))[new1$Y]
new1$N=as.numeric(levels(new1$N))[new1$N]
p1<-ggplot(new1,aes(x=Type,y=Y,fill=Type))+geom_bar(stat="identity")+theme_classic()+
  coord_flip()+
  labs(x="Payment Methods",y="Number of Farmers Market")+ggtitle("Payment Types in Midwest")+
  geom_text(aes(label=Y),hjust = 1.5, size=3.2)+
  theme(legend.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black"),
        legend.background = element_rect(color="black",fill = "darkgray"),
        legend.key = element_rect(fill = "lightblue", color = NA),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(0.5,"cm"),
        axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"))


p2<-ggplot(new1, aes(x=Type, y = N, fill = Type)) + geom_bar(stat = "identity")+ theme_classic()+
  ylab("Number of Farmers Markets")+ggtitle("Not allowed Payment Types in Midwest")+coord_flip()+
  geom_text(aes(label=N),hjust = 1.5, size=3.2)+
  theme(legend.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black"),
        legend.background = element_rect(color="black",fill = "darkgray"),
        legend.key = element_rect(fill = "lightblue", color = NA),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(0.5,"cm"),
        axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"))


grid.arrange(p1,p2,ncol=2)

#5
# Which payment mode is most offered in northwest part of the country?
FMI_Regions<-FMI %>% select(FMID,MarketName,State,city,Credit,WIC,WICcash,SFMNP,SNAP) 
FMI_Regions<-merge(x=FMI_Regions,y=Regions,by='State')
a<-subset(FMI_Regions,Region=="Midwest")
Credit<-as.data.frame(table(a$Credit))
names(Credit)[2]<-"Credit"

Wic<-as.data.frame(table(a$WIC))
names(Wic)[2]<-"WIC"

Wiccash<-as.data.frame(table(a$WICcash))
names(Wiccash)[2]<-"Wiccash"

SFMNP<-as.data.frame(table(a$SFMNP))
names(SFMNP)[2]<-"SFMNP"

SNAP<-as.data.frame(table(a$SNAP))
names(SNAP)[2]<-"SNAP"

b <- data.frame(Reduce(MyMerge,list(Credit,Wic,Wiccash,SFMNP,SNAP)))
b<-data.frame(t(b))
b <- data.frame(Type = row.names(b), b)
b<-b[-1,]
colnames(b)[2]<-"N"
colnames(b)[3]<-"Y"
b$Y=as.numeric(levels(b$Y))[b$Y]
b$N=as.numeric(levels(b$N))[b$N]
p1<-ggplot(b,aes(x=Type,y=Y,fill=Type))+geom_bar(stat="identity")+theme_classic()+coord_flip()+
  labs(x="Payment Methods",y="Number of Farmers Market")+ggtitle("Payment Types in Midwest")+
  geom_text(aes(label=Y),hjust = 1.5, size=3.2)+
  theme(legend.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black"),
        legend.background = element_rect(color="black",fill = "darkgray"),
        legend.key = element_rect(fill = "lightblue", color = NA),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(0.5,"cm"),
        axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"))


p2<-ggplot(b,aes(x=Type,y=N,fill=Type))+geom_bar(stat="identity")+theme_classic()+coord_flip()+
  labs(x="Payment Methods",y="Number of Farmers Market")+ggtitle("Not allowed Payment Types in Midwest")+
  geom_text(aes(label=N),hjust = 1.5, size=3.2)+
  theme(legend.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black"),
        legend.background = element_rect(color="black",fill = "darkgray"),
        legend.key = element_rect(fill = "lightblue", color = NA),
        legend.key.size = unit(1, "cm"),
        legend.key.width = unit(0.5,"cm"),
        axis.line = element_line(colour = "darkblue",size = 1, linetype = "solid"))


grid.arrange(p1,p2,ncol=2)
