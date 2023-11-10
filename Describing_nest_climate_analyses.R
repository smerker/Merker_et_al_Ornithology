#### Load data for analyses ####

load("Nest_Climate_Data.gzip")
#save(night.sum.stats, night.out.sum.stats, day.sum.stats, day.out.sum.stats, file="D:/Dissertation/Dissertation/Egg Swapping/Writing/Functional Ecology/EggTrans_Ornith_R_proj/Nest_Climate_Data_new.gzip")



#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#### Night time Temperature and Humidity Analysis ####


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

library(multcomp)

##Night time temperature inside nests.

## species only
lm.sp.t.in<-lm(meanT~species,data=night.sum.stats)
summary(lm.sp.t.in)

## elevation only
lm.elev.t.in<-lm(meanT~elevation, data=night.sum.stats)
summary(lm.elev.t.in)

## elevation + species
lm.elev.sp.t.in<-lm(meanT~elevation+species, data=night.sum.stats)
summary(anova(lm.elev.sp.t.in))

# effect size
mean(night.sum.stats$meanT[night.sum.stats$species=="HOWA"])-mean(night.sum.stats$meanT[night.sum.stats$species=="BTBW"])

## interaction
lm.t.in.X<-lm(meanT~elevation*species, data=night.sum.stats)
summary(lm.t.in.X)
summary(step(lm.t.in.X))

## model selection
t.AIC<-AIC(lm.sp.t.in, lm.elev.t.in, lm.elev.sp.t.in, lm.t.in.X)
str(t.AIC)
t.AICdff<-t.AIC[2]-min(t.AIC[,2])
w <- exp(-0.5*t.AICdff)/sum(exp(-0.5*t.AICdff))

t.AIC.df<-data.frame(t.AIC,t.AICdff, w)
colnames(t.AIC.df)<-c("K", "AIC", "deltaAIC", "weight")

t.AIC.df.ord<-round(t.AIC.df[order(t.AICdff$AIC, decreasing =F),],3)
t.AIC.df.ord

rownames(t.AIC.df.ord)<-c("Mean Internal Temp ~ Elevation", 
                          "Mean Internal Temp ~ Species",
                          "Mean Internal Temp ~ Species  X Elevation",
                          "Mean Internal Temp ~ Elevation + Species")

write.csv(t.AIC.df.ord,"Nightime_Temp_Inside_Nests.csv")

#
##make predictions from model with species + elevation
#

#new data frame for predictions
new.dat.in.temp<-data.frame(elevation=seq(1200, 1519, by=10), species="BTBW")#, height=c(.78,.99))#average nest height for each species
howa.temp<-data.frame(elevation=seq(735, 1200, by=10), species="HOWA")
new.dat.in.temp<-rbind(new.dat.in.temp,howa.temp)

#make predictions
pred.temp.in3<-predict(lm.elev.sp.t.in, newdata=new.dat.in.temp, se.fit=TRUE, type="response", interval="confidence")
summary(lm.elev.sp.t.in)

new.dat.in.temp$pred.fit<-pred.temp.in3$fit[,1]
new.dat.in.temp$pred.CI.lwr<-pred.temp.in3$fit[,2]
new.dat.in.temp$pred.CI.upr<-pred.temp.in3$fit[,3]




#
#
## ibuttons out of the nest #
#
## temperature
#
#

#make species a factor for Tukey
night.out.sum.stats$species<-as.factor(night.out.sum.stats$species)

#make species a factor for Tukey
night.sum.stats$species<-as.factor(night.sum.stats$species)

##analysis

##elevation
lm.elev.t.out<-lm(meanT~elevation, data=night.out.sum.stats)
summary(lm.elev.t.out)


lm.elev.sp.t.out<-lm(meanT~elevation+species, data=night.out.sum.stats)
summary(lm.elev.sp.t.out)

# effect size
#mean(night.out.sum.stats$meanT[night.out.sum.stats$species=="HOWA"])-mean(night.out.sum.stats$meanT[night.out.sum.stats$species=="BTBW"])

AIC(lm.elev.t.out,lm.elev.sp.t.out)
#new data frame for predictions
new.dat.out.temp<-data.frame(elevation=seq(735, 1519, by=10))

new.dat.in.temp.out<-data.frame(elevation=seq(1200, 1519, by=10), species="BTBW")#, height=c(.78,.99))#average nest height for each species
howa.temp.out<-data.frame(elevation=seq(735, 1200, by=10), species="HOWA")
new.dat.in.temp.out<-rbind(new.dat.in.temp.out,howa.temp)



pred.temp.out.e.sp<-predict(lm.elev.sp.t.out, newdata=new.dat.in.temp.out, se.fit=TRUE, type="response", interval="confidence")
new.dat.in.temp.out$pred.fit<-pred.temp.out.e.sp$fit[,1]
new.dat.in.temp.out$pred.CI.lwr<-pred.temp.out.e.sp$fit[,2]
new.dat.in.temp.out$pred.CI.upr<-pred.temp.out.e.sp$fit[,3]

plot(pred.fit~elevation, data=subset(new.dat.in.temp.out, species=="BTBW"), type='l', ylim=c(13, 20), xlim=c(700, 1550))
lines(pred.fit~elevation, data=subset(new.dat.in.temp.out, species=="HOWA"), col='red')

#make predictions
pred.temp.out3<-predict(lm.elev.t.out, newdata=new.dat.out.temp, se.fit=TRUE, type="response", interval="confidence")


#model predictions
new.dat.out.temp$pred.fit<-pred.temp.out3$fit[,1]

#model average lwr CI
new.dat.out.temp$pred.CI.lwr<-pred.temp.out3$fit[,2]

#model averaged upr CI
new.dat.out.temp$pred.CI.upr<-pred.temp.out3$fit[,3]


new.dat.out.temp




#### Humidity Analysis ###
library(LaplacesDemon)
##inside nest analysis

# species only
log.sp.hu.in<-lm(logit(meanHprop)~species,data=night.sum.stats)
summary(log.sp.hu.in)

## elevation
log.elev.hu.in<-lm(logit(meanHprop)~elevation, data=night.sum.stats)
summary(log.elev.hu.in)


# elevation + species
log.elev.sp.hu.in<-lm(logit(meanHprop)~elevation+species, data=night.sum.stats)
summary(log.elev.sp.hu.in)

# interaction
log.hu.in.X<-lm(logit(meanHprop)~elevation*species, data=night.sum.stats)
summary(log.hu.in.X)
coef(log.hu.in.X)

# model selection
logh.AIC<-AIC(log.sp.hu.in, log.elev.hu.in, log.elev.sp.hu.in,log.hu.in.X)
str(logh.AIC)
logh.AICdff<-logh.AIC[2]-min(logh.AIC[,2])
w.logh <- exp(-0.5*logh.AICdff)/sum(exp(-0.5*logh.AICdff))

logh.AIC.df<-data.frame(logh.AIC,logh.AICdff, w.logh)
colnames(logh.AIC.df)<-c("K", "AIC", "deltaAIC", "weight")
logh.AIC.df.ord<-round(logh.AIC.df[order(logh.AICdff$AIC),],3)
logh.AIC.df<-logh.AIC.df.ord
logh.AIC.df

rownames(logh.AIC.df)<-c("Mean Internal Humid ~ Species  X Elevation", 
                         "Mean Internal Humid ~ Species",
                         "Mean Internal Humid ~ Elevation",
                         "Mean Internal Humid ~ Elevation + Species")

write.csv(logh.AIC.df,"Nightime_Humid_Inside_Nests.csv")


#
##make predictions
#
new.dat.loghumid<-data.frame(elevation=seq(1200, 1519, by=10), species="BTBW")
howa.loghumid<-data.frame(elevation=seq(735, 1200, by=10), species="HOWA")
new.dat.loghumid<-rbind(new.dat.loghumid,howa.loghumid)

#make predictions
pred.loghumid.inX<-predict(log.hu.in.X, newdata=new.dat.loghumid, se.fit=TRUE, type="response", interval="confidence")

#model averaged fit
new.dat.loghumid$pred.fit<-plogis(pred.loghumid.inX$fit[,1])


#model averaged lwr CI
new.dat.loghumid$pred.CI.lwr<-plogis(pred.loghumid.inX$fit[,2])

#model averaged upr CI
new.dat.loghumid$pred.CI.upr<-plogis(pred.loghumid.inX$fit[,3])

new.dat.loghumid



#### outer humidity analysis ####

log.elev.hu.out<-lm(logit(meanHprop)~elevation, data=night.out.sum.stats)
summary(log.elev.hu.out)

summary(log.elev.hu.out)$coefficients[, "Std. Error"]
#new data frame for predictions
new.dat.loghumid.out<-data.frame(elevation=seq(735, 1519, by=10)) #average nest height for each species

pred.loghumid.out3<-predict(log.elev.hu.out, newdata=new.dat.loghumid.out, se.fit=TRUE, type="response", interval="confidence")

#prediction
new.dat.loghumid.out$pred.fit<-plogis(pred.loghumid.out3$fit[,1])

# averaged lower CI
new.dat.loghumid.out$pred.CI.lwr<-plogis(pred.loghumid.out3$fit[,2])

# upperr CI
new.dat.loghumid.out$pred.CI.upr<-plogis(pred.loghumid.out3$fit[,3])





#### Daytime Temperature and Humidity Analysis ####

# inner daytime temperature

day.in.T.mod.sp<-lm(meanT~species, data=day.sum.stats)
day.in.T.mod.x<-lm(meanT~elevation*species, data=day.sum.stats)
day.in.T.mod.s.e<-lm(meanT~species+elevation, data=day.sum.stats)
day.in.T.mod.e<-lm(meanT~elevation, data=day.sum.stats)


summary(day.in.T.mod.sp)
summary(day.in.T.mod.x)
summary(day.in.T.mod.s.e)
summary(day.in.T.mod.e)


#model selection
t.AIC.day<-AIC(day.in.T.mod.sp, day.in.T.mod.x, day.in.T.mod.s.e, day.in.T.mod.e)
str(t.AIC.day)
t.AICdff.day<-t.AIC.day[2]-min(t.AIC.day[,2])
w.day <- exp(-0.5*t.AICdff.day)/sum(exp(-0.5*t.AICdff.day))

t.AIC.df.day<-data.frame(t.AIC.day,t.AICdff.day, w.day)
colnames(t.AIC.df.day)<-c("K", "AIC", "deltaAIC", "weight")

t.AIC.df.day.ord<-round(t.AIC.df.day[order(t.AICdff.day$AIC),],3)
t.AIC.df.day.ord

rownames(t.AIC.df.day.ord)<-c("Mean Internal Temp ~ Elevation", 
                              "Mean Internal Temp ~ Species",
                              "Mean Internal Temp ~ Elevation X Species",
                              "Mean Internal Temp ~ Elevation + Species")

write.csv(t.AIC.df.day.ord,"Daytime_Temp_Inside_Nests.csv")


min(day.sum.stats$elevation[day.sum.stats$species=="BTBW"], na.rm=T)

#new data frame for predictions
new.dat.in.temp.day<-data.frame(elevation=seq(1200, 1530, by=10), species="BTBW")#, height=c(.78,.99))#average nest height for each species
howa.temp.day<-data.frame(elevation=seq(735, 1200, by=10), species="HOWA")
new.dat.in.temp.day<-rbind(new.dat.in.temp.day,howa.temp.day)

#make predictions
pred.temp.in1.day<-predict(day.in.mod.sp, newdata=new.dat.in.temp.day, se.fit=TRUE, type="response", interval="confidence")
pred.temp.in2.day<-predict(day.in.mod.x, newdata=new.dat.in.temp.day, se.fit=TRUE, type="response", interval="confidence")
pred.temp.in3.day<-predict(day.in.mod.s.e, newdata=new.dat.in.temp.day, se.fit=TRUE, type="response", interval="confidence")
pred.temp.in4.day<-predict(dayu.in.mod.e, newdata=new.dat.in.temp.day, se.fit=TRUE, type="response", interval="confidence")
#model averaged fit
pred.fit.t.in.day<-cbind(pred.temp.in1.day$fit[,1],pred.temp.in2.day$fit[,1],pred.temp.in3.day$fit[,1],pred.temp.in4.day$fit[,1])
new.dat.in.temp.day$pred.fit<-pred.fit.t.in.day%*%t.AIC.df.day$weight

#model averaged lwr CI
pred.CI.lwr.t.in.day<-cbind(pred.temp.in1.day$fit[,2],pred.temp.in2.day$fit[,2],pred.temp.in3.day$fit[,2],pred.temp.in4.day$fit[,2])
new.dat.in.temp.day$pred.CI.lwr<-pred.CI.lwr.t.in.day%*%t.AIC.df.day$weight

#model averaged upr CI
pred.CI.upr.t.in.day<-cbind(pred.temp.in1.day$fit[,3],pred.temp.in2.day$fit[,3],pred.temp.in3.day$fit[,3],pred.temp.in4.day$fit[,3])
new.dat.in.temp.day$pred.CI.upr<-pred.CI.upr.t.in.day%*%t.AIC.df.day$weight



#outer daytime temperature/humidity

##temp~elevation
lm.elev.dayt.out<-lm(meanT~elevation, data=day.out.sum.stats)
summary(lm.elev.dayt.out)

#humid~elevation
lm.elev.dayh.out<-lm(logit(meanHprop)~elevation, data=day.out.sum.stats)
summary(lm.elev.dayh.out)
summary(lm.elev.dayh.out)$coefficients[, "Std. Error"]
#new data frame for predictions
new.dat.out.daytemp<-data.frame(elevation=seq(739, 1519, by=10))

#make predictions
pred.daytemp.out3<-predict(lm.elev.dayt.out, newdata=new.dat.out.daytemp, se.fit=TRUE, type="response", interval="confidence")
pred.dayhumid.out3<-predict(lm.elev.dayh.out, newdata=new.dat.out.daytemp, se.fit=TRUE, type="response", interval="confidence")


#model averaged fit
new.dat.out.daytemp$pred.fit.T<-pred.daytemp.out3$fit[,1]
new.dat.out.daytemp$pred.fit.H<-plogis(pred.dayhumid.out3$fit[,1])

#lower CI
new.dat.out.daytemp$pred.CI.T.lwr<-pred.daytemp.out3$fit[,2]
new.dat.out.daytemp$pred.CI.H.lwr<-plogis(pred.dayhumid.out3$fit[,2])

#upper CI
new.dat.out.daytemp$pred.CI.T.upr<-pred.daytemp.out3$fit[,3]
new.dat.out.daytemp$pred.CI.H.upr<-plogis(pred.dayhumid.out3$fit[,3])



## daytime interior humidity


day.in.mod.sp.h<-lm(logit(meanH/100)~species, data=day.sum.stats)
day.in.mod.x.h<-lm(logit(meanH/100)~elevation*species, data=day.sum.stats)
day.in.mod.s.e.h<-lm(logit(meanH/100)~species+elevation, data=day.sum.stats)
day.in.mod.e.h<-lm(logit(meanH/100)~elevation, data=day.sum.stats)


summary(day.in.mod.sp.h)
summary(day.in.mod.x.h)
summary(day.in.mod.s.e.h)
summary(day.in.mod.e.h)


#which one was better
h.AIC.day<-AIC(day.in.mod.sp.h, day.in.mod.x.h, day.in.mod.s.e.h, day.in.mod.e.h)
str(h.AIC.day)
h.AICdff.day<-h.AIC.day[2]-min(h.AIC.day[,2])
hw.day <- exp(-0.5*h.AICdff.day)/sum(exp(-0.5*h.AICdff.day))

h.AIC.df.day<-data.frame(h.AIC.day,h.AICdff.day, hw.day)
colnames(h.AIC.df.day)<-c("K", "AIC", "deltaAIC", "weight")

h.AIC.df.day.ord<-round(h.AIC.df.day[order(h.AICdff.day$AIC),],3)
h.AIC.df.day.ord


rownames(h.AIC.df.day.ord)<-c("Mean Internal Humid ~ Elevation", 
                              "Mean Internal Humid ~ Species",
                              "Mean Internal Humid ~ Elevation X Species",
                              "Mean Internal Humid ~ Elevation + Species")

write.csv(h.AIC.df.day.ord,"Daytime_Humid_Inside_Nests.csv")



#new data frame for predictions
new.dat.in.hum.day<-data.frame(elevation=seq(1200, 1530, by=10), species="BTBW")#
howa.hum.day<-data.frame(elevation=seq(735, 1200, by=10), species="HOWA")
new.dat.in.hum.day<-rbind(new.dat.in.hum.day,howa.hum.day)

#make predictions
pred.humid.in1.day<-predict(day.in.mod.sp.h, newdata=new.dat.in.hum.day, se.fit=TRUE, type="response", interval="confidence")
pred.humid.in2.day<-predict(day.in.mod.x.h, newdata=new.dat.in.hum.day, se.fit=TRUE, type="response", interval="confidence")
pred.humid.in3.day<-predict(day.in.mod.s.e.h, newdata=new.dat.in.hum.day, se.fit=TRUE, type="response", interval="confidence")
pred.humid.in4.day<-predict(dayu.in.mod.e.h, newdata=new.dat.in.hum.day, se.fit=TRUE, type="response", interval="confidence")
#model averaged fit
pred.fit.t.in.day<-cbind(pred.humid.in1.day$fit[,1],pred.humid.in2.day$fit[,1],pred.humid.in3.day$fit[,1],pred.humid.in4.day$fit[,1])
new.dat.in.hum.day$pred.fit<-pred.fit.t.in.day%*%t.AIC.df.day$weight

#model averaged lwr CI
pred.CI.lwr.h.in.day<-cbind(pred.humid.in1.day$fit[,2],pred.humid.in2.day$fit[,2],pred.humid.in3.day$fit[,2],pred.humid.in4.day$fit[,2])
new.dat.in.hum.day$pred.CI.lwr<-pred.CI.lwr.h.in.day%*%h.AIC.df.day$weight

#model averaged upr CI
pred.CI.upr.h.in.day<-cbind(pred.humid.in1.day$fit[,3],pred.humid.in2.day$fit[,3],pred.humid.in3.day$fit[,3],pred.humid.in4.day$fit[,3])
new.dat.in.hum.day$pred.CI.upr<-pred.CI.upr.h.in.day%*%h.AIC.df.day$weight

new.dat.in.hum.day$pred.fit<-plogis(pred.humid.in2.day$fit[,1])
new.dat.in.hum.day$pred.CI.lwr<-plogis(pred.humid.in2.day$fit[,2])
new.dat.in.hum.day$pred.CI.upr<-plogis(pred.humid.in2.day$fit[,3])


# exterior daytime humidity


lm.elev.dayh.out<-lm(logit(meanHprop)~elevation, data=day.out.sum.stats)
summary(lm.elev.dayh.out)



#new data frame for predictions
new.dat.out.daytemp<-data.frame(elevation=seq(739, 1519, by=10))#,each=50), species=c("BTBW", "HOWA"), height=c(.78,.99))#average nest height for each species

#make predictions
pred.daytemp.out3<-predict(lm.elev.dayt.out, newdata=new.dat.out.daytemp, se.fit=TRUE, type="response", interval="confidence")
pred.dayhumid.out3<-predict(lm.elev.dayh.out, newdata=new.dat.out.daytemp, se.fit=TRUE, type="response", interval="confidence")


#model averaged fit
new.dat.out.daytemp$pred.fit.T<-pred.daytemp.out3$fit[,1]
new.dat.out.daytemp$pred.fit.H<-plogis(pred.dayhumid.out3$fit[,1])

#model average lwr CI
new.dat.out.daytemp$pred.CI.T.lwr<-pred.daytemp.out3$fit[,2]
new.dat.out.daytemp$pred.CI.H.lwr<-plogis(pred.dayhumid.out3$fit[,2])

#model averaged upr CI
new.dat.out.daytemp$pred.CI.T.upr<-pred.daytemp.out3$fit[,3]
new.dat.out.daytemp$pred.CI.H.upr<-plogis(pred.dayhumid.out3$fit[,3])


