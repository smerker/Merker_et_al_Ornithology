# nest dates
library(chron)
library(lubridate)
nest.dates<-read.csv("./Nest_dates.csv")
save(nest.dates, file="nest_dates.gzip")
load("nest_dates.gzip")
nest.dates$Treatment<-as.factor(nest.dates$Treatment)
nest.dates$yday<-yday(as.Date(nest.dates$CC_Natural, format="%m/%d/%Y"))

date.aov<-t.test(yday~Treatment, data=nest.dates[1:43,], paired=F)
