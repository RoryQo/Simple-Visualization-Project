## ----setup, include=FALSE------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 


## ----Data load-----------------------------------------------------
library(dplyr)
Dailyshowguests<- read.csv("C:/Users/roryq/Downloads/daily_show_guests.csv")
head(Dailyshowguests,3)
is.data.frame(Dailyshowguests)



## ------------------------------------------------------------------

library(tidyr)
Percent <- Dailyshowguests %>% group_by(YEAR) %>% count(Group) %>% mutate(Percent = n / sum(n)*100)
Percent<- data.frame(Percent)
head(Percent,3)


## ------------------------------------------------------------------
Media<- Dailyshowguests %>% group_by(YEAR) %>% count(Group) %>% mutate(Percent=n / sum(n)*100) %>% filter(Group== "Media")
year<- c(1999,2000,2001,2001,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)
Media<-data.frame(year,Media)


## ------------------------------------------------------------------
A<- Dailyshowguests %>% group_by(YEAR) %>% count(Group) %>% mutate(Percent= n/sum(n)*100) %>% filter(Group== "Acting")
head(A,3)


## ------------------------------------------------------------------
C<- Dailyshowguests %>% group_by(YEAR) %>% count(Group) %>% mutate(Percent= n/sum(n)*100) %>% filter(Group== "Comedy")
head(C,3)


## ------------------------------------------------------------------
M<- Dailyshowguests %>% group_by(YEAR) %>% count(Group) %>% mutate(Percent= n/sum(n)*100) %>% filter(Group== "Musician")
head(M,3)


## ------------------------------------------------------------------
A1<-A$Percent
M1<-M$Percent
C1<-C$Percent
AMC<-A1+C1+M1
AMC
year<- c(1999,2000,2001,2001,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)
AMCLine<-data.frame(year,AMC)
head(AMCLine,3)


## ------------------------------------------------------------------
G<- Dailyshowguests %>% group_by(YEAR) %>% count(Group) %>% mutate(Percent= n/sum(n)*100) %>% filter(Group== "Government")
head(G,3)


## ------------------------------------------------------------------
P<- Dailyshowguests %>% group_by(YEAR) %>% count(Group) %>% mutate(Percent= n/sum(n)*100) %>% filter(Group== "Politician")
head(P,3)


## ------------------------------------------------------------------
PA<- Dailyshowguests %>% group_by(YEAR) %>% count(Group) %>% mutate(Percent= n/sum(n)*100) %>% filter(Group== "Political Aide")
head(PA,3)


## ------------------------------------------------------------------
G1<-as.numeric(G$Percent)
P1<-as.numeric(P$Percent)
PA1<-as.numeric(PA$Percent)
Gov<-G1+P1+PA1
head(Gov,3)
year<- c(1999,2000,2001,2001,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)
GovLine<-data.frame(year,Gov)
head(GovLine,3)


## ------------------------------------------------------------------
library(tidyverse)
plot<- ggplot(Percent,mapping=aes(x=year,y=Percent))
plot<-plot+geom_line(data=Media,mapping=aes(x=year,y=Percent), color= "darkorchid",size=1.5, na.rm=TRUE)



## ------------------------------------------------------------------
plot<-plot+geom_line(data=Media,mapping=aes(x=year,y=Percent), color= "darkorchid",size=1.5, na.rm=TRUE)


## ------------------------------------------------------------------
plot<- plot+geom_line(data=AMCLine, mapping=aes(x=year,y=AMC),color="cornflowerblue",size=1.5, na.rm=TRUE)


## ------------------------------------------------------------------
plot<-plot+geom_line(data=GovLine, mapping=aes(x=year,y=Gov), color="darkorange2", size=1.5)
plot


## ------------------------------------------------------------------
plot<-plot+ylim(0,100)


## ------------------------------------------------------------------
plot<- plot+geom_text(x=2008,y=53, label="Media", colour="darkorchid",size=4.5,fontface="bold")
plot<- plot+ geom_text(x=2003.5,y=80, label="Acting, Comedy & Music", size=4.5, fontface="bold", color="cornflowerblue")
plot<- plot+geom_text(x=2012,y=5,label="Government and Politics", size=4.5, fontface="bold", color="darkorange2")


## ------------------------------------------------------------------
plot<-plot+ggtitle("Who Got To Be On 'The Daily Show'?", subtitle= "Occupation of guests, by year")
plot<-plot+ theme(plot.title=element_text(face="bold", size=15, family= "CM Roman"))
plot<-plot+theme(plot.subtitle=element_text(family="CM Roman",size=13))


## ------------------------------------------------------------------
plot<-plot+ylab(label=NULL)+xlab(label=NULL)



## ------------------------------------------------------------------
plot<-plot + scale_x_continuous(breaks=c(2000, 2004, 2008,2012))
plot<-plot+ scale_x_continuous(labels=c("2000" = "2000", "2004" = "'04",
                              "2008" = "'08", "2012"="'12"))



## ------------------------------------------------------------------
plot<-plot + theme(axis.text.x = element_text(size=10, family="Serif", face="bold"))


## ------------------------------------------------------------------
plot<-plot + theme(axis.text.y = element_text(size=10, family="Serif", face= "bold"))


## ------------------------------------------------------------------
plot<-plot + scale_y_continuous(limit = c(0,100), 
                         breaks = c(0,25,50, 75, 100), 
                         labels = c("0","25","50","75", "100%"))


## ------------------------------------------------------------------
plot

