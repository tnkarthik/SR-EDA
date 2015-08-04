setwd("C:/Users/Karthik/Documents/R/Karthik's projects/datagov")
getwd()

sf_crime<-read.csv("Map__Crime_Incidents_-_from_1_Jan_2003.csv")

library(dplyr)
glimpse(sf_crime)

sf_crime$Date<-strptime(sf_crime$Date,format="%m/%d/%Y %H:%M:%S")
sf_crime$DayofMonth<-sf_crime$Date$mday
sf_crime$year<-sf_crime$Date$year+1900 ## Year uses 1900 as ref
sf_crime$Month<-sf_crime$Date$mon+1    ## Month is indexed with 0

##Reorder levels of DayOfWeek to run from Monday to Sunday
sf_crime$DayOfWeek <- factor(sf_crime$DayOfWeek,levels(sf_crime$DayOfWeek)[c(2,6,7,5,1,3,4)])
glimpse(sf_crime)

##Generate plots to explore data
library(ggplot2)


## Crime incidence in different neighborhoods
jpeg("p1.jpg",width=960,height=480)
p1<-ggplot(data=sf_crime,aes(x=PdDistrict))+
  geom_bar(fill="lightgreen", colour="darkgreen")+
  xlab("Neighborhood")+
  ylab("# Incidents")
p1
dev.off()


## Top 3 crimes for different neighborhoods
sf_crime1=mutate(sf_crime,Date=NULL) ## dplyr cannot seem to handle formatted Date data
sf_crime1$Category=as.character(sf_crime1$Category)
temp1=filter(sf_crime1,Category!="OTHER OFFENSES")
sf_crime.byyearsummary<-temp1%>%
  group_by(PdDistrict,Category)%>%
  summarise(n=n())

top3<-top_n(sf_crime.byyearsummary, 3)
top3<-arrange(top3,n)
glimpse(top3)


## How did the incidence of different crimes change with year
## Removing year 2015 as the data is available only for half of the year at this point! (It is still July)

sf_crime.byyearsummary<-sf_crime1%>%
  group_by(year,PdDistrict)%>%
  summarise(n=n())

glimpse(sf_crime.byyearsummary)  
  
jpeg("p2.jpg",width=960,height=480)
p2<-ggplot(data=subset(sf_crime.byyearsummary,year!=2015),aes(x=year,y=n))+
  geom_point(aes(colour=PdDistrict),size=2)+
  geom_smooth(aes(colour=PdDistrict),size=2)+
  scale_x_continuous(breaks=2003:2014)+
  xlab("Year")+
  ylab("# of incidents")
p2

dev.off()
## Lets pick DUI incidents to investigate further
sf_crime.dui=subset(sf_crime,Category=="DRIVING UNDER THE INFLUENCE" )

## What neighborhoods have the highest rates of drunken driving

jpeg("p3.jpg",width=960,height=480)
p3<-ggplot(sf_crime.dui,aes(x=PdDistrict))+
  scale_y_continuous()+
  geom_bar(fill="lightgreen", colour="darkgreen")+
  xlab("Neighborhood")+
  ylab("# DUI Incidents")

p3
dev.off()
## DUI reports are expected to be higher on weekends. 
## Plot DUI reports vs day of week for diff neighborhoods to verify 
jpeg("p4.jpg",width=1200,height=800)
p4<-ggplot(sf_crime.dui,aes(x=DayOfWeek))+
  scale_y_continuous()+
  geom_bar(fill="lightgreen", colour="darkgreen")+
  facet_wrap(~PdDistrict,scales="free_y",ncol=3)+
  xlab("Day of the week")+
  ylab("DUI Incidents")

p4
dev.off()

## How about Month of the year
sf_crime.dui$Month=as.character(sf_crime.dui$Month)


lut<-c("1"="Jan","2"="Feb","3"="March",
       "4"="Apr","5"="May","6"="June",
       "7"="Jul","8"="Aug","9"="Sept",
       "10"="Oct","11"="Nov","12"="Dec")
sf_crime.dui$Month=lut[sf_crime.dui$Month]
  
  ## Factor Month
  sf_crime.dui$Month=factor(sf_crime.dui$Month)
  levels(sf_crime.dui$Month)
  sf_crime.dui$Month<- factor(sf_crime.dui$Month,levels(sf_crime.dui$Month)[c(5,4,8,1,9,7,6,2,12,11,10,3)])
  
  ## Plot vs. month
  jpeg("p5.jpg",width=1400,height=480)
  p5<-ggplot(sf_crime.dui,aes(x=Month))+
    geom_bar(fill="lightgreen", colour="darkgreen")+
    facet_wrap(~PdDistrict,scales="free_y")+
    xlab("Month")+
    
    ylab("DUI Incidents")
  
  p5
  
  dev.off()
    
## How about time of the day. 
  ## need help with reordering X-axis date so it runs from say 8am to 7:59am
  sf_crime.dui$Time1=strptime(sf_crime.dui$Time,format="%H:%M")
  
  jpeg("p6.jpg",width=960,height=550)
  p6<-ggplot(sf_crime.dui, aes(x=Time1))+
    geom_bar(fill="lightgreen", colour="darkgreen",binwidth=3600)+
    facet_wrap(~PdDistrict,scales="free_y",ncol=3)+
    xlab("Time")+
    ylab("DUI Incidents")
  p6
  dev.off()
  
temp=subset(sf_crime1,year!=2015)  
sf_crime.byyearcatsummary<-temp%>%
  group_by(Category,year)%>%
  summarise(count=n())

  
  ## Crime vs. year for different crimes
  p7<-ggplot(sf_crime.byyearcatsummary, aes(x=year,y=count))+
    geom_line(size=1)+
    facet_wrap(~Category,ncol=6,scales="free_y")
  p7
  
  ## Crimes vs. day of Month
  p7<-ggplot(subset(sf_crime1,year=2007), aes(x=DayofMonth))+
    geom_bar()+
    facet_wrap(~Month)
  
  p7
  
  sf_crime.dui$Time2=factor(substr(as.character(sf_crime.dui$Time),1,2))
  sf_crime.dui$Time2=factor(sf_crime.dui$Time2,levels(sf_crime.dui$Time2)[c(9:24,1:8)])
  
  p6<-ggplot(sf_crime.dui, aes(x=Time2))+
    geom_bar(fill="lightgreen", colour="darkgreen",binwidth=3600)+
    xlab("Time")+
    ylab("DUI Incidents")
  p6
  