#### nighttime temp models ####

# read in the minimum and maximum data that any given nest experiences.

load("Minimum_maximum_Temp_Humid.gzip")
minmax.clim.data

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                                                                  #### Night Min Max Models ####

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# starting with minimum internal and external nighttime conditiosn

#night time min temp in nest
glm.night.T.in.min<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+night_T_in_min, family=binomial, data=minmax.clim.data)
summary(glm.night.T.in.min)

#night time min temp out of nest
glm.night.T.out.min<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+night_T_out_min, family=binomial, data=minmax.clim.data)
summary(glm.night.T.out.min)

#night humidity in
glm.night.H.in.min<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+night_H_in_min, family=binomial, data=minmax.clim.data)
summary(glm.night.H.in.min)

#night time min humid out of nest
glm.night.H.out.min<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+night_H_out_min, family=binomial, data=minmax.clim.data)
summary(glm.night.H.out.min)

# night temp in out
glm.night.T.inout.min<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+night_T_in_min+night_T_out_min, family=binomial, data=minmax.clim.data)
summary(glm.night.T.inout.min)

#night humid in out
glm.night.H.inout.min<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+night_H_in_min+night_H_out_min, family=binomial, data=minmax.clim.data)
summary(glm.night.H.inout.min)

# AIC

night.min.aic<-AIC(glm.night.T.in.min, glm.night.T.out.min, glm.night.T.inout.min)

night.min.aic.ord<-night.min.aic[order(night.min.aic$AIC),]
night.min.aic.ord

night.min.aicdff<-night.min.aic[2]-min(night.min.aic[,2])
w.min.night <- exp(-0.5*night.min.aicdff)/sum(exp(-0.5*night.min.aicdff))

AIC.df.night.min<-data.frame(night.min.aic, night.min.aicdff, round(w.min.night,3))
colnames(AIC.df.night.min)<-c("K", "AIC", "deltaAIC", "weight")
AIC.df.night.min<-AIC.df.night.min[order(AIC.df.night.min$weight, decreasing=T),]
AIC.df.night.min

# predictions
range(minmax.clim.data$night_T_in_min[minmax.clim.data$Species_eggs=="HOWA"], na.rm=T)
range(minmax.clim.data$night_T_in_min[minmax.clim.data$Species_eggs=="BTBW"], na.rm=T)

mean(minmax.clim.data$night_T_out_min, na.rm=T)


night.min.T.pred.data<-data.frame(night_T_in_min=c(seq(from=10.59, to=31.61, length=100),seq(from=15.05, to=33.08, length=100)), 
                                  Species_eggs=rep(c("HOWA", "BTBW"),each=100))


night.t.min.pred<-predict(glm.night.T.in.min, newdata = night.min.T.pred.data, se.fit=TRUE)
night.min.T.pred.data$prediction<-plogis(night.t.min.pred$fit)

night.min.T.pred.data

night.min.T.pred.data$night.min.lwr.T<-plogis(night.t.min.pred$fit-(1.96*night.t.min.pred$se.fit))
night.min.T.pred.data$night.min.upper.T<-plogis(night.t.min.pred$fit+(1.96*night.t.min.pred$se.fit))


x.ticks.minT.night<-seq(from=min(night.min.T.pred.data$night_T_in_min, na.rm=T), to=max(night.min.T.pred.data$night_T_in_min, na.rm=T), length=6)
x.labs.minT.night<-seq(from=min(night.min.T.pred.data$night_T_in_min, na.rm=T), to=max(night.min.T.pred.data$night_T_in_min, na.rm=T), length=6)

# nighttime in minimum

plot(prediction~night_T_in_min, type='l',data=subset(night.min.T.pred.data, Species_eggs=="HOWA"), xlim=c(11, 34),ylim=c(0,1), col="red", xaxt='n', 
     ylab="probability of hatching", xlab="minimum internal night time nest temperature(\u00B0C)")

polygon(x=c(min(subset(night.min.T.pred.data, Species_eggs=="HOWA", select=night_T_in_min)),
            min(subset(night.min.T.pred.data, Species_eggs=="HOWA", select=night_T_in_min)), 
            night.min.T.pred.data$night_T_in_min[1:100],max(subset(night.min.T.pred.data, Species_eggs=="HOWA", select=night_T_in_min)),
            max(subset(night.min.T.pred.data, Species_eggs=="HOWA", select=night_T_in_min)), 
            night.min.T.pred.data$night_T_in_min[100:1],min(subset(night.min.T.pred.data, Species_eggs=="HOWA", select=night_T_in_min))),
        y=c(min(subset(night.min.T.pred.data, Species_eggs=="HOWA", select=night.min.lwr.T)), 
            max(subset(night.min.T.pred.data, Species_eggs=="HOWA", select=night.min.upper.T)),   
            subset(night.min.T.pred.data, Species_eggs=="HOWA",select=night.min.upper.T, drop=TRUE),     
            max(subset(night.min.T.pred.data, Species_eggs=="HOWA",select=night.min.upper.T)),         
            min(subset(night.min.T.pred.data, Species_eggs=="HOWA", select=night.min.lwr.T)),
            rev(subset(night.min.T.pred.data, Species_eggs=="HOWA", select=night.min.lwr.T, drop=TRUE)),
            min(subset(night.min.T.pred.data, Species_eggs=="HOWA", select=night.min.lwr.T))), 
        col=rgb(0.5,0,0,.2), border=FALSE)

lines(prediction~night_T_in_min, col="blue",data=subset(night.min.T.pred.data, Species_eggs=="BTBW"))
polygon(x=c(min(subset(night.min.T.pred.data, Species_eggs=="BTBW", select=night_T_in_min)),
            min(subset(night.min.T.pred.data, Species_eggs=="BTBW", select=night_T_in_min)), 
            night.min.T.pred.data$night_T_in_min[101:200],max(subset(night.min.T.pred.data, Species_eggs=="BTBW", select=night_T_in_min)),
            max(subset(night.min.T.pred.data, Species_eggs=="BTBW", select=night_T_in_min)), 
            night.min.T.pred.data$night_T_in_min[200:101],min(subset(night.min.T.pred.data, Species_eggs=="BTBW", select=night_T_in_min))),
        y=c(min(subset(night.min.T.pred.data, Species_eggs=="BTBW", select=night.min.lwr.T)), 
            max(subset(night.min.T.pred.data, Species_eggs=="BTBW", select=night.min.upper.T)),   
            subset(night.min.T.pred.data, Species_eggs=="BTBW",select=night.min.upper.T, drop=TRUE),     
            max(subset(night.min.T.pred.data, Species_eggs=="BTBW",select=night.min.upper.T)),         
            min(subset(night.min.T.pred.data, Species_eggs=="BTBW", select=night.min.lwr.T)),
            rev(subset(night.min.T.pred.data, Species_eggs=="BTBW", select=night.min.lwr.T, drop=TRUE)),
            min(subset(night.min.T.pred.data, Species_eggs=="BTBW", select=night.min.lwr.T))), 
        col=rgb(0,0,0.5,.2), border=FALSE)
axis(1, at=x.ticks.minT.night, labels=round(x.labs.minT.night))






##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
#
                                                            # continuing with maximum night time temp ####
#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#night data

#night.data<-minmax.clim.data[,c("Species_eggs", "night_T_in_max", "night_T_out_max")]

#night time temp in nest
glm.night.T.in.max<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+night_T_in_max, family=binomial, data=minmax.clim.data, na.action=na.omit)
summary(glm.night.T.in.max)


#night time temp out nest
glm.night.T.out.max<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+night_T_out_max, family=binomial, data=minmax.clim.data, na.action=na.omit)
summary(glm.night.T.out.max)

#night humidity in
glm.night.H.in.max<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+night_H_in_max, family=binomial, data=minmax.clim.data, na.action=na.omit)
summary(glm.night.H.in.max)

#night humidity in
glm.night.H.out.max<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+night_H_out_max, family=binomial, data=minmax.clim.data, na.action=na.omit)
summary(glm.night.H.out.max)

# night temp in + out
glm.night.T.inout.max<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+night_T_in_max+night_T_out_max, family=binomial, data=minmax.clim.data, na.action=na.omit)
summary(glm.night.T.inout.max)

#night humid in + out
glm.night.H.inout.max<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+night_H_in_max+night_H_out_max, family=binomial, data=minmax.clim.data, na.action=na.omit)
summary(glm.night.H.inout.max)


night.max.aic<-AIC(glm.night.T.in.max, glm.night.T.out.max, glm.night.T.inout.max)

night.max.aic.ord<-night.max.aic[order(night.max.aic$AIC),]
night.max.aic.ord

night.max.aicdff<-night.max.aic[2]-min(night.max.aic[,2])
w.max.night <- exp(-0.5*night.max.aicdff)/sum(exp(-0.5*night.max.aicdff))

AIC.df.night.max<-data.frame(night.max.aic, night.max.aicdff, round(w.max.night,3))
colnames(AIC.df.night.max)<-c("K", "AIC", "deltaAIC", "weight")
AIC.df.night.max<-AIC.df.night.max[order(AIC.df.night.max$weight, decreasing=T),]
AIC.df.night.max


#night time maximum predictions

range(minmax.clim.data$night_T_in_max[minmax.clim.data$Species_eggs=="HOWA"], na.rm=T)
range(minmax.clim.data$night_T_in_max[minmax.clim.data$Species_eggs=="BTBW"], na.rm=T)

range(minmax.clim.data$night_T_out_max[minmax.clim.data$Species_eggs=="HOWA"], na.rm=T)
range(minmax.clim.data$night_T_out_max[minmax.clim.data$Species_eggs=="BTBW"], na.rm=T)


night.max.in.T.pred.data<-data.frame(night_T_in_max=c(seq(from=24.67, to=39.13, length=100),seq(from=23.58, to=38.57, length=100)), 
                                     Species_eggs=rep(c("HOWA", "BTBW"),each=100), 
                                     night_T_out_max=mean(minmax.clim.data$night_T_out_max, na.rm=T))

night.max.out.T.pred.data<-data.frame(night_T_in_max=mean(minmax.clim.data$night_T_in_max, na.rm=T),
                                      Species_eggs=rep(c("HOWA", "BTBW"),each=100), 
                                      night_T_out_max=c(seq(from=15.10, to=25.09, length=100),seq(from=16.11, to=22.15, length=100)))


night.t.max.in.pred<-predict(glm.night.T.inout.max, newdata = night.max.in.T.pred.data, se.fit=TRUE)
night.max.in.T.pred.data$prediction<-plogis(night.t.max.in.pred$fit)

night.max.in.T.pred.data$night.max.lwr.T<-plogis(night.t.max.in.pred$fit-(1.96*night.t.max.in.pred$se.fit))
night.max.in.T.pred.data$night.max.upper.T<-plogis(night.t.max.in.pred$fit+(1.96*night.t.max.in.pred$se.fit))



night.t.max.out.pred<-predict(glm.night.T.inout.max, newdata = night.max.out.T.pred.data, se.fit=TRUE)
night.max.out.T.pred.data$prediction<-plogis(night.t.max.out.pred$fit)

night.max.out.T.pred.data$night.max.lwr.T<-plogis(night.t.max.out.pred$fit-(1.96*night.t.max.out.pred$se.fit))
night.max.out.T.pred.data$night.max.upper.T<-plogis(night.t.max.out.pred$fit+(1.96*night.t.max.out.pred$se.fit))



x.ticks.maxT.night<-seq(from=min(night.max.in.T.pred.data$night_T_in_max, na.rm=T), to=max(night.max.in.T.pred.data$night_T_in_max, na.rm=T), length=6)
x.labs.maxT.night<-seq(from=min(night.max.in.T.pred.data$night_T_in_max, na.rm=T), to=max(night.max.in.T.pred.data$night_T_in_max, na.rm=T), length=6)

# plotting nighttime in max

plot(prediction~night_T_in_max, type='l',data=subset(night.max.in.T.pred.data, Species_eggs=="HOWA"), ylim=c(0,1), xlim=c(24,39),col="red", xaxt='n', 
     ylab="probability of hatching", xlab="Maximum internal night time nest temperature(\u00B0C)")
polygon(x=c(min(subset(night.max.in.T.pred.data, Species_eggs=="HOWA", select=night_T_in_max)),
            min(subset(night.max.in.T.pred.data, Species_eggs=="HOWA", select=night_T_in_max)), 
            night.max.in.T.pred.data$night_T_in_max[1:100],max(subset(night.max.in.T.pred.data, Species_eggs=="HOWA", select=night_T_in_max)),
            max(subset(night.max.in.T.pred.data, Species_eggs=="HOWA", select=night_T_in_max)), 
            night.max.in.T.pred.data$night_T_in_max[100:1],min(subset(night.max.in.T.pred.data, Species_eggs=="HOWA", select=night_T_in_max))),
        y=c(min(subset(night.max.in.T.pred.data, Species_eggs=="HOWA", select=night.max.lwr.T)), 
            max(subset(night.max.in.T.pred.data, Species_eggs=="HOWA", select=night.max.upper.T)),   
            subset(night.max.in.T.pred.data, Species_eggs=="HOWA",select=night.max.upper.T, drop=TRUE),     
            max(subset(night.max.in.T.pred.data, Species_eggs=="HOWA",select=night.max.upper.T)),         
            min(subset(night.max.in.T.pred.data, Species_eggs=="HOWA", select=night.max.lwr.T)),
            rev(subset(night.max.in.T.pred.data, Species_eggs=="HOWA", select=night.max.lwr.T, drop=TRUE)),
            min(subset(night.max.in.T.pred.data, Species_eggs=="HOWA", select=night.max.lwr.T))), 
        col=rgb(0.5,0,0,.2), border=FALSE)

lines(prediction~night_T_in_max, col="blue",data=subset(night.max.in.T.pred.data, Species_eggs=="BTBW"))
polygon(x=c(min(subset(night.max.in.T.pred.data, Species_eggs=="BTBW", select=night_T_in_max)),
            min(subset(night.max.in.T.pred.data, Species_eggs=="BTBW", select=night_T_in_max)), 
            night.max.in.T.pred.data$night_T_in_max[101:200],max(subset(night.max.in.T.pred.data, Species_eggs=="BTBW", select=night_T_in_max)),
            max(subset(night.max.in.T.pred.data, Species_eggs=="BTBW", select=night_T_in_max)), 
            night.max.in.T.pred.data$night_T_in_max[200:101],min(subset(night.max.in.T.pred.data, Species_eggs=="BTBW", select=night_T_in_max))),
        y=c(min(subset(night.max.in.T.pred.data, Species_eggs=="BTBW", select=night.max.lwr.T)), 
            max(subset(night.max.in.T.pred.data, Species_eggs=="BTBW", select=night.max.upper.T)),   
            subset(night.max.in.T.pred.data, Species_eggs=="BTBW",select=night.max.upper.T, drop=TRUE),     
            max(subset(night.max.in.T.pred.data, Species_eggs=="BTBW",select=night.max.upper.T)),         
            min(subset(night.max.in.T.pred.data, Species_eggs=="BTBW", select=night.max.lwr.T)),
            rev(subset(night.max.in.T.pred.data, Species_eggs=="BTBW", select=night.max.lwr.T, drop=TRUE)),
            min(subset(night.max.in.T.pred.data, Species_eggs=="BTBW", select=night.max.lwr.T))), 
        col=rgb(0,0,0.5,.2), border=FALSE)
axis(1, at=x.ticks.maxT.night, labels=round(x.labs.maxT.night))



# plotting nightime out max


plot(prediction~night_T_out_max, type='l',data=subset(night.max.out.T.pred.data, Species_eggs=="HOWA"), ylim=c(0,1), xlim=c(15,25),col="red", xaxt='n', 
     ylab="probability of hatching", xlab="Maximum internal night time nest temperature(\u00B0C)")
polygon(x=c(min(subset(night.max.out.T.pred.data, Species_eggs=="HOWA", select=night_T_out_max)),
            min(subset(night.max.out.T.pred.data, Species_eggs=="HOWA", select=night_T_out_max)), 
            night.max.out.T.pred.data$night_T_out_max[1:100],max(subset(night.max.out.T.pred.data, Species_eggs=="HOWA", select=night_T_out_max)),
            max(subset(night.max.out.T.pred.data, Species_eggs=="HOWA", select=night_T_out_max)), 
            night.max.out.T.pred.data$night_T_out_max[100:1],min(subset(night.max.out.T.pred.data, Species_eggs=="HOWA", select=night_T_out_max))),
        y=c(min(subset(night.max.out.T.pred.data, Species_eggs=="HOWA", select=night.max.lwr.T)), 
            max(subset(night.max.out.T.pred.data, Species_eggs=="HOWA", select=night.max.upper.T)),   
            subset(night.max.out.T.pred.data, Species_eggs=="HOWA",select=night.max.upper.T, drop=TRUE),     
            max(subset(night.max.out.T.pred.data, Species_eggs=="HOWA",select=night.max.upper.T)),         
            min(subset(night.max.out.T.pred.data, Species_eggs=="HOWA", select=night.max.lwr.T)),
            rev(subset(night.max.out.T.pred.data, Species_eggs=="HOWA", select=night.max.lwr.T, drop=TRUE)),
            min(subset(night.max.out.T.pred.data, Species_eggs=="HOWA", select=night.max.lwr.T))), 
        col=rgb(0.5,0,0,.2), border=FALSE)

lines(prediction~night_T_out_max, col="blue",data=subset(night.max.out.T.pred.data, Species_eggs=="BTBW"))
polygon(x=c(min(subset(night.max.out.T.pred.data, Species_eggs=="BTBW", select=night_T_out_max)),
            min(subset(night.max.out.T.pred.data, Species_eggs=="BTBW", select=night_T_out_max)), 
            night.max.out.T.pred.data$night_T_out_max[101:200],max(subset(night.max.out.T.pred.data, Species_eggs=="BTBW", select=night_T_out_max)),
            max(subset(night.max.out.T.pred.data, Species_eggs=="BTBW", select=night_T_out_max)), 
            night.max.out.T.pred.data$night_T_out_max[200:101],min(subset(night.max.out.T.pred.data, Species_eggs=="BTBW", select=night_T_out_max))),
        y=c(min(subset(night.max.out.T.pred.data, Species_eggs=="BTBW", select=night.max.lwr.T)), 
            max(subset(night.max.out.T.pred.data, Species_eggs=="BTBW", select=night.max.upper.T)),   
            subset(night.max.out.T.pred.data, Species_eggs=="BTBW",select=night.max.upper.T, drop=TRUE),     
            max(subset(night.max.out.T.pred.data, Species_eggs=="BTBW",select=night.max.upper.T)),         
            min(subset(night.max.out.T.pred.data, Species_eggs=="BTBW", select=night.max.lwr.T)),
            rev(subset(night.max.out.T.pred.data, Species_eggs=="BTBW", select=night.max.lwr.T, drop=TRUE)),
            min(subset(night.max.out.T.pred.data, Species_eggs=="BTBW", select=night.max.lwr.T))), 
        col=rgb(0,0,0.5,.2), border=FALSE)
axis(1, at=x.ticks.maxT.night, labels=round(x.labs.maxT.night))





### a model with both internal min and max temp ####

glm.night.T.in.minmax<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+night_T_in_max+night_T_in_min, family=binomial, data=minmax.clim.data, na.action=na.omit)
summary(glm.night.T.in.minmax)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



                                                                  #### Daytime Min Max Models ####



#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# starting with min

#day time temp in nest
glm.day.T.in.min<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+day_T_in_min, family=binomial, data=minmax.clim.data)
summary(glm.day.T.in.min)


#day humidity in
glm.day.H.in.min<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+day_H_in_min, family=binomial, data=minmax.clim.data)
summary(glm.day.H.in.min)



#day time temp out of nest
glm.day.T.out.min<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+day_T_out_min, family=binomial, data=minmax.clim.data)
summary(glm.day.T.out.min)


#day humidity out of nest
glm.day.H.out.min<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+day_H_out_min, family=binomial, data=minmax.clim.data)
summary(glm.day.H.out.min)


# night temp in + out
glm.day.T.inout.min<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+day_T_in_min+day_T_out_min, family=binomial, data=minmax.clim.data, na.action=na.omit)
summary(glm.day.T.inout.min)

#night humid in + out
glm.day.H.inout.min<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+day_H_in_min+day_H_out_min, family=binomial, data=minmax.clim.data, na.action=na.omit)
summary(glm.day.H.inout.min)



day.aic<-AIC(glm.day.T.in.min, glm.day.T.out.min, glm.day.H.in.min, glm.day.H.out.min, glm.day.T.inout.min, glm.day.H.inout.min)


day.aic.ord<-day.aic[order(day.aic$AIC),]
day.aic.ord

day.aicdff<-day.aic[2]-min(day.aic[,2])
w.day <- exp(-0.5*day.aicdff)/sum(exp(-0.5*day.aicdff))

AIC.df.day<-data.frame(day.aic,day.aicdff, round(w.day,3))
colnames(AIC.df.day)<-c("K", "AIC", "deltaAIC", "weight")
AIC.df.day<-AIC.df.day[order(AIC.df.day$weight, decreasing=T),]
AIC.df.day


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
#
                                                                        ## continuing with daytime maximum
#
#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

str(minmax.clim.data)

#maximum day time temp in nest
glm.day.T.in.max<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+day_T_in_max, family=binomial, data=minmax.clim.data, na.action = na.omit)
summary(glm.day.T.in.max)


#maximum day time temp in nest
glm.day.T.out.max<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+day_T_out_max, family=binomial, data=minmax.clim.data, na.action = na.omit)
summary(glm.day.T.out.max)


glm.day.T.out.max<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs*day_T_out_max, family=binomial, data=minmax.clim.data, na.action = na.omit)
summary(glm.day.T.out.max)


#day humidity in
glm.day.H.in.max<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+day_H_in_max, family=binomial, data=minmax.clim.data, na.action = na.omit)
summary(glm.day.H.in.max)

#day humidity in
glm.day.H.out.max<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+day_H_out_max, family=binomial, data=minmax.clim.data, na.action = na.omit)
summary(glm.day.H.in.max)



# maximum day temp in + out
glm.day.T.inout.max<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+day_T_in_max+day_T_out_max, family=binomial, data=minmax.clim.data, na.action = na.omit)
summary(glm.day.T.inout.max)

#day humid in + out
glm.day.H.inout.max<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs+day_H_in_max+day_H_out_max, family=binomial, data=minmax.clim.data, na.action = na.omit)
summary(glm.day.H.inout.max)




## Sam, I think this is the model that best reflects our hypothesis (that BTBW hatch rate would be more sensitive to temp than HOWA)
#day time temp in nest with interaction
glm.day.TxS.in.max<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs*day_T_in_max, family=binomial, data=minmax.clim.data)
summary(glm.day.TxS.in.max)

#day time temp out of nest with interaction
glm.day.TxS.out.max<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs*day_T_out_min, family=binomial, data=minmax.clim.data)
summary(glm.day.TxS.out.max)


#day time temp in nest with interaction
glm.day.HxS.in.max<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs*day_H_in_max, family=binomial, data=minmax.clim.data)
summary(glm.day.HxS.in.max)

#day time temp out of nest with interaction
glm.day.HxS.out.max<-glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs*day_H_out_min, family=binomial, data=minmax.clim.data)
summary(glm.day.HxS.in.max)




day.aic<-AIC(glm.day.T.in.max, glm.day.T.out.max, glm.day.H.in.max, glm.day.H.out.max, glm.day.T.inout.max, glm.day.H.inout.max, glm.day.TxS.in.max)


day.aic.ord<-day.aic[order(day.aic$AIC),]
day.aic.ord

day.aicdff<-day.aic[2]-min(day.aic[,2])
w.day <- exp(-0.5*day.aicdff)/sum(exp(-0.5*day.aicdff))

AIC.df.day<-data.frame(day.aic,day.aicdff, round(w.day,3))
colnames(AIC.df.day)<-c("K", "AIC", "deltaAIC", "weight")
AIC.df.day<-AIC.df.day[order(AIC.df.day$weight, decreasing=T),]
AIC.df.day


range(minmax.clim.data[minmax.clim.data$Species_Inc=="HOWA", 'day_T_in_max'], na.rm=T)
range(minmax.clim.data[minmax.clim.data$Species_Inc=="BTBW", 'day_T_in_max'], na.rm=T)


new.daytime.max.T.data<-data.frame(Species_eggs=rep(c("BTBW", "HOWA"), each=100), day_T_in_max=c(seq(from=28.08, to=40.04, length=100), seq(from=29.61, to=42.07, length=100)))
## max.t.in.pred<-predict(glm.day.T.in.max, newdata = new.daytime.max.T.data, type="link", se.fit=TRUE)
max.t.in.pred<-predict(glm.day.TxS.in.max, newdata = new.daytime.max.T.data, type="link", se.fit=TRUE)
max.t.in.pred

new.daytime.max.T.data$fit<-plogis(max.t.in.pred$fit)

new.daytime.max.T.data$day.lwr.max.T<-plogis(max.t.in.pred$fit-(1.96*max.t.in.pred$se.fit))
new.daytime.max.T.data$day.upper.max.T<-plogis(max.t.in.pred$fit+(1.96*max.t.in.pred$se.fit))


plot(fit~day_T_in_max, data=subset(new.daytime.max.T.data,Species_eggs=="BTBW"), type="l", col="blue", xlim=c(28, 43), ylim=c(0.45,1),
     xlab="Maximum internal daytime nest temperature(\u00B0C)", ylab="Probability of hatching")


polygon(x=c(min(subset(new.daytime.max.T.data, Species_eggs=="BTBW", select=day_T_in_max)),
            min(subset(new.daytime.max.T.data, Species_eggs=="BTBW", select=day_T_in_max)), 
            new.daytime.max.T.data$day_T_in_max[1:100],max(subset(new.daytime.max.T.data, Species_eggs=="BTBW", select=day_T_in_max)),
            max(subset(new.daytime.max.T.data, Species_eggs=="BTBW", select=day_T_in_max)), 
            new.daytime.max.T.data$day_T_in_max[100:1],min(subset(new.daytime.max.T.data, Species_eggs=="BTBW", select=day_T_in_max))),
        
        y=c(min(subset(new.daytime.max.T.data, Species_eggs=="BTBW", select=day.lwr.max.T)), 
            max(subset(new.daytime.max.T.data, Species_eggs=="BTBW", select=day.upper.max.T)),   
            subset(new.daytime.max.T.data, Species_eggs=="BTBW",select=day.upper.max.T, drop=TRUE),     
            max(subset(new.daytime.max.T.data, Species_eggs=="BTBW",select=day.lwr.max.T)),         
            min(subset(new.daytime.max.T.data, Species_eggs=="BTBW", select=day.lwr.max.T)),
            rev(subset(new.daytime.max.T.data, Species_eggs=="BTBW", select=day.lwr.max.T, drop=TRUE)),
            min(subset(new.daytime.max.T.data, Species_eggs=="BTBW", select=day.lwr.max.T))), 
        col=rgb(0,0,0.5,.2), border=FALSE)

lines(fit~day_T_in_max, data=subset(new.daytime.max.T.data,Species_eggs=="HOWA"), col="red")

polygon(x=c(min(subset(new.daytime.max.T.data, Species_eggs=="HOWA", select=day_T_in_max)),
            min(subset(new.daytime.max.T.data, Species_eggs=="HOWA", select=day_T_in_max)), 
            new.daytime.max.T.data$day_T_in_max[101:200],max(subset(new.daytime.max.T.data, Species_eggs=="HOWA", select=day_T_in_max)),
            max(subset(new.daytime.max.T.data, Species_eggs=="HOWA", select=day_T_in_max)), 
            new.daytime.max.T.data$day_T_in_max[200:101],min(subset(new.daytime.max.T.data, Species_eggs=="HOWA", select=day_T_in_max))),
        
        y=c(min(subset(new.daytime.max.T.data, Species_eggs=="HOWA", select=day.lwr.max.T)), 
            max(subset(new.daytime.max.T.data, Species_eggs=="HOWA", select=day.upper.max.T)),   
            subset(new.daytime.max.T.data, Species_eggs=="HOWA",select=day.upper.max.T, drop=TRUE),     
            max(subset(new.daytime.max.T.data, Species_eggs=="HOWA",select=day.lwr.max.T)),         
            min(subset(new.daytime.max.T.data, Species_eggs=="HOWA", select=day.lwr.max.T)),
            rev(subset(new.daytime.max.T.data, Species_eggs=="HOWA", select=day.lwr.max.T, drop=TRUE)),
            min(subset(new.daytime.max.T.data, Species_eggs=="HOWA", select=day.lwr.max.T))), 
        col=rgb(0.5,0,0,.2), border=FALSE)



