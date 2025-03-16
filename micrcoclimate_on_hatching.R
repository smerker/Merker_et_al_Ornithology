#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#### Analysis of microclimate on probability of hatching ####


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#bring in data
load("micro_climate_data.gzip")

str(micro_clim_data)

####models including temperature and humidity####

micro_clim_data$Species_eggs<-as.factor(micro_clim_data$Species_eggs)



## Ambient temperature
glm.climoutT <- glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs*Toutscl, family=binomial, data=micro_clim_data,
                    subset=!is.na(Toutscl))
summary(glm.climoutT)
glm.climoutT.best <- step(glm.climoutT)   ## Best model has species only
summary(glm.climoutT.best)

## Ambient humidity
glm.climoutH <- glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs*Houtscl, family=binomial, data=micro_clim_data,
                    subset=!is.na(Houtscl))
summary(glm.climoutH)
glm.climoutH.best <- step(glm.climoutH)   ## Best model has species only
summary(glm.climoutH.best)

## Inner temp, foraging
glm.TinOFF <- glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs*TinOFFscl, family=binomial, data=micro_clim_data,
                  subset=!is.na(TinOFFscl))
summary(glm.TinOFF)
glm.TinOFF.best <- step(glm.TinOFF)   ## Best model has additive species+temp
summary(glm.TinOFF.best)

## Inner temp, incubation
glm.TinON <- glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs*TinONscl, family=binomial, data=micro_clim_data,
                 subset=!is.na(TinONscl))
summary(glm.TinON)
glm.TinON.best <- step(glm.TinON)  ## Best model has additive species+temp
summary(glm.TinON.best)

## Inner humidity, foraging
glm.HinOFF <- glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs*HinOFFscl, family=binomial, data=micro_clim_data,
                  subset=!is.na(HinOFFscl))
summary(glm.HinOFF)
glm.HinOFF.best <- step(glm.HinOFF)  ## Best model has species only
summary(glm.HinOFF.best)

## Inner humidity, incubation
glm.HinON <- glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs*HinONscl, family=binomial, data=micro_clim_data,
                 subset=!is.na(HinONscl))
summary(glm.HinON)
glm.HinON.best <- step(glm.HinON)  ## Best model has species only
summary(glm.HinON.best)

## Prediction data
pred.t.in.off.howa <- seq(min(subset(micro_clim_data, Species_eggs=="HOWA", select=TinOFFscl), na.rm=TRUE),
                          max(subset(micro_clim_data, Species_eggs=="HOWA", select=TinOFFscl), na.rm=TRUE),
                          length.out=100)
pred.t.in.off.btbw <- seq(min(subset(micro_clim_data, Species_eggs=="BTBW", select=TinOFFscl), na.rm=TRUE), 
                          max(subset(micro_clim_data, Species_eggs=="BTBW", select=TinOFFscl), na.rm=TRUE),
                          length.out=100)
new.dat.clim <- data.frame(TinOFFscl=c(pred.t.in.off.howa, pred.t.in.off.btbw))


pred.t.in.on.howa <- seq(min(subset(micro_clim_data, Species_eggs=="HOWA", select=TinONscl), na.rm=T), 
                         max(subset(micro_clim_data, Species_eggs=="HOWA", select=TinONscl), na.rm=T),
                         length.out=100)
pred.t.in.on.btbw <- seq(min(subset(micro_clim_data, Species_eggs=="BTBW", select=TinONscl), na.rm=T), 
                         max(subset(micro_clim_data, Species_eggs=="BTBW", select=TinONscl), na.rm=T),
                         length.out=100)
new.dat.clim$TinONscl <- c(pred.t.in.on.howa, pred.t.in.on.btbw)

pred.h.in.on.howa <- seq(min(subset(micro_clim_data, Species_eggs=="HOWA", select=HinONscl), na.rm=T), 
                         max(subset(micro_clim_data, Species_eggs=="HOWA", select=HinONscl), na.rm=T),
                         length.out=100)
pred.h.in.on.btbw <- seq(min(subset(micro_clim_data, Species_eggs=="BTBW", select=HinONscl), na.rm=T), 
                         max(subset(micro_clim_data, Species_eggs=="BTBW", select=HinONscl), na.rm=T),
                         length.out=100)
new.dat.clim$HinONscl <- c(pred.h.in.on.howa, pred.h.in.on.btbw)


pred.h.in.off.howa <- seq(min(subset(micro_clim_data, Species_eggs=="HOWA", select=HinOFFscl), na.rm=T), 
                          max(subset(micro_clim_data, Species_eggs=="HOWA", select=HinOFFscl), na.rm=T),
                          length.out=100)
pred.h.in.off.btbw <- seq(min(subset(micro_clim_data, Species_eggs=="BTBW", select=HinOFFscl), na.rm=T), 
                          max(subset(micro_clim_data, Species_eggs=="BTBW", select=HinOFFscl), na.rm=T),
                          length.out=100)
new.dat.clim$HinOFFscl <- c(pred.h.in.off.howa, pred.h.in.off.btbw)


pred.t.out.howa <- seq(min(subset(micro_clim_data, Species_eggs=="HOWA", select=Toutscl), na.rm=T), 
                       max(subset(micro_clim_data, Species_eggs=="HOWA", select=Toutscl), na.rm=T),
                       length.out=100)
pred.t.out.btbw <- seq(min(subset(micro_clim_data, Species_eggs=="BTBW", select=Toutscl), na.rm=T), 
                       max(subset(micro_clim_data, Species_eggs=="BTBW", select=Toutscl), na.rm=T),
                       length.out=100)
new.dat.clim$Toutscl <- c(pred.t.out.howa, pred.t.out.btbw)


pred.h.out.howa <- seq(min(subset(micro_clim_data, Species_eggs=="HOWA", select=Houtscl), na.rm=T), 
                       max(subset(micro_clim_data, Species_eggs=="HOWA", select=Houtscl), na.rm=T),
                       length.out=100)
pred.h.out.btbw <- seq(min(subset(micro_clim_data, Species_eggs=="BTBW", select=Houtscl), na.rm=T), 
                       max(subset(micro_clim_data, Species_eggs=="BTBW", select=Houtscl), na.rm=T),
                       length.out=100)
new.dat.clim$Houtscl <- c(pred.h.out.howa, pred.h.out.btbw)

new.dat.clim$Species_eggs=rep(c("HOWA", "BTBW"), each=100)

str(new.dat.clim)


## Back-transform covariates to original scales (original values were in data)
load("in_sums.R") # object called 'insums'
load("out_sums.R") # object called 'outsums'

t.in.off.mean <- mean(insums$TinOFF)
t.in.off.sd <- sd(insums$TinOFF)
all( ((insums$TinOFF-t.in.off.mean)/t.in.off.sd) == insums$TinOFFscl)
new.dat.clim$TinOFF <- new.dat.clim$TinOFFscl*t.in.off.sd + t.in.off.mean
pred.t.in.off.howa.back <- pred.t.in.off.howa*t.in.off.sd + t.in.off.mean
pred.t.in.off.btbw.back <- pred.t.in.off.btbw*t.in.off.sd + t.in.off.mean

t.in.on.mean <- mean(insums$TinON)
t.in.on.sd <- sd(insums$TinON)
all( ((insums$TinON-t.in.on.mean)/t.in.on.sd) == insums$TinONscl)
new.dat.clim$TinON <- new.dat.clim$TinONscl*t.in.on.sd + t.in.on.mean
pred.t.in.on.howa.back <- pred.t.in.on.howa*t.in.on.sd + t.in.on.mean
pred.t.in.on.btbw.back <- pred.t.in.on.btbw*t.in.on.sd + t.in.on.mean

h.in.off.mean <- mean(insums$HinOFF)
h.in.off.sd <- sd(insums$HinOFF)
all( ((insums$HinOFF-h.in.off.mean)/h.in.off.sd) == insums$HinOFFscl)
new.dat.clim$HinOFF <- new.dat.clim$HinOFFscl*h.in.off.sd + h.in.off.mean
pred.h.in.off.howa.back <- pred.h.in.off.howa*h.in.off.sd + h.in.off.mean
pred.h.in.off.btbw.back <- pred.h.in.off.btbw*h.in.off.sd + h.in.off.mean

h.in.on.mean <- mean(insums$HinON)
h.in.on.sd <- sd(insums$HinON)
all( ((insums$HinON-h.in.on.mean)/h.in.on.sd) == insums$HinONscl)
new.dat.clim$HinON <- new.dat.clim$HinONscl*h.in.on.sd + h.in.on.mean
pred.h.in.on.howa.back <- pred.h.in.on.howa*h.in.on.sd + h.in.on.mean
pred.h.in.on.btbw.back <- pred.h.in.on.btbw*h.in.on.sd + h.in.on.mean


t.out.mean <- mean(outsums$tempOUT)
t.out.sd <- sd(outsums$tempOUT)
new.dat.clim$Tout <- new.dat.clim$Toutscl*t.out.sd + t.out.mean
pred.t.out.howa.back <- pred.t.out.howa*t.out.sd + t.out.mean
pred.t.out.btbw.back <- pred.t.out.btbw*t.out.sd + t.out.mean


h.out.mean <- mean(outsums$humidOUT)
h.out.sd <- sd(outsums$humidOUT)
new.dat.clim$Hout <- new.dat.clim$Houtscl*h.out.sd + h.out.mean
pred.h.out.howa.back <- pred.h.out.howa*h.out.sd + h.out.mean
pred.h.out.btbw.back <- pred.h.out.btbw*h.out.sd + h.out.mean


str(new.dat.clim)





## Predictions

## Effect of temperature during off bouts (foraging)
pred.t.off <- predict(glm.TinOFF.best, new.dat.clim, se.fit=TRUE)
new.dat.clim$predictionToFF<-plogis(pred.t.off$fit)
new.dat.clim$seToFF<-pred.t.off$se.fit ## Not on response scale
new.dat.clim$lwrToFF<-plogis(pred.t.off$fit-(1.96*new.dat.clim$seToF))
new.dat.clim$uprToFF<-plogis(pred.t.off$fit+(1.96*new.dat.clim$seToF))


## Effect of temperature during on bouts (incubation)
pred.t.on <- predict(glm.TinON.best, new.dat.clim, se.fit=TRUE)
new.dat.clim$predictionToN<-plogis(pred.t.on$fit)
new.dat.clim$seToN<-pred.t.on$se.fit ## Not on response scale
new.dat.clim$lwrToN<-plogis(pred.t.on$fit-(1.96*new.dat.clim$seToN))
new.dat.clim$uprToN<-plogis(pred.t.on$fit+(1.96*new.dat.clim$seToN))

## Effect of humidity during off bouts
pred.h.off<-predict(glm.HinOFF.best, new.dat.clim, se.fit=TRUE)
new.dat.clim$predictionHoFF<-plogis(pred.h.off$fit)
new.dat.clim$seHoFF<-pred.h.off$se.fit
new.dat.clim$lwrHoFF<-plogis(pred.h.off$fit-(1.96*new.dat.clim$seHoFF))
new.dat.clim$uprHoFF<-plogis(pred.h.off$fit+(1.96*new.dat.clim$seHoFF))


## Effect of humidity during on bouts
pred.h.on<-predict(glm.HinON.best, new.dat.clim, se.fit=TRUE)
new.dat.clim$predictionHoN<-plogis(pred.h.on$fit)
new.dat.clim$seHoN<-pred.h.on$se.fit
new.dat.clim$lwrHoN<-plogis(pred.h.on$fit-(1.96*new.dat.clim$seHoN))
new.dat.clim$uprHoN<-plogis(pred.h.on$fit+(1.96*new.dat.clim$seHoN))


## Effect of ambient temperature
pred.t.out<-predict(glm.climoutT.best, new.dat.clim, se.fit=TRUE)
new.dat.clim$predictionTout<-plogis(pred.t.out$fit)
new.dat.clim$seTout<-pred.t.out$se.fit
new.dat.clim$lwrTout<-plogis(pred.t.out$fit-(1.96*new.dat.clim$seTout))
new.dat.clim$uprTout<-plogis(pred.t.out$fit+(1.96*new.dat.clim$seTout))

## Effect of ambient humidity
pred.h.out<-predict(glm.climoutH.best, new.dat.clim, se.fit=TRUE)
new.dat.clim$predictionHout<-plogis(pred.h.out$fit)
new.dat.clim$seHout<-pred.h.out$se.fit
new.dat.clim$lwrHout<-plogis(pred.h.out$fit-(1.96*new.dat.clim$seHout))
new.dat.clim$uprHout<-plogis(pred.h.out$fit+(1.96*new.dat.clim$seHout))

str(new.dat.clim)




#### only "panel B" ####

png("Figure_5.png", res=600, units = "in", height=3.5, width=3.5)

plot(predictionToN~TinONscl, data=subset(new.dat.clim, Species_eggs=="HOWA"), col="red",type="l", lty=1,ylim=c(0,1), xlim=c(-2.0, 1.8), 
     xaxt='n',xlab=NA,ylab="Probablity of Hatching")


axis(1, at=round(seq(min(new.dat.clim$TinONscl), max(new.dat.clim$TinONscl),length=5),1),
     labels=round(seq(min(insums$TinON), max(insums$TinON),length=5),0), tick=TRUE, padj=-.75)


polygon(x=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=TinONscl), na.rm=T),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=TinONscl), na.rm=T), 
            new.dat.clim$TinONscl[1:100],max(subset(new.dat.clim, Species_eggs=="HOWA", select=TinONscl), na.rm=T), 
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=TinONscl), na.rm=T), 
            new.dat.clim$TinONscl[100:1],
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=TinONscl), na.rm=T)),
        y=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToN)), 
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=uprToN)),   
            subset(new.dat.clim, Species_eggs=="HOWA",select=uprToN, drop=TRUE),     
            max(subset(new.dat.clim, Species_eggs=="HOWA",select=uprToN)),         
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToN)),
            rev(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToN, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToN))), 
        col=rgb(0.5,0,0,.2), border=FALSE)

lines(predictionToN~TinONscl, data=subset(new.dat.clim, Species_eggs=="BTBW"), col="blue", lty=2)
polygon(x=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=TinONscl), na.rm=T),
            min(subset(new.dat.clim, Species_eggs=="BTBW", select=TinONscl), na.rm=T), 
            new.dat.clim$TinONscl[101:200],
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=TinONscl), na.rm=T),
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=TinONscl), na.rm=T), 
            new.dat.clim$TinONscl[200:101],min(subset(new.dat.clim, Species_eggs=="BTBW", select=TinONscl), na.rm=T)),
        y=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=lwrToN)), 
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=uprToN)),   
            subset(new.dat.clim,Species_eggs=="BTBW",select=uprToN, drop=TRUE),     
            max(subset(new.dat.clim,Species_eggs=="BTBW",select=uprToN)),         
            min(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrToN)),
            rev(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrToN, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="BTBW",select=lwrToN))), 
        col=rgb(0,0,0.5,.2), border=FALSE)
legend(-0.2, .25, c("Hooded", "Black-throated Blue"), col=c("red", "blue"), title="Warbler Species Eggs", lty=c(1,2), cex=.5)
#text(-1.9, .1, "B", font=2, cex=1.2)
mtext(side=3, "Effect of Inner Nest Temperature on Hatch Rates", font=2, cex=.7, padj=-3)
mtext(side=1, "Inner Temperature (\u00B0C)", cex=1, adj=.5, padj=4.7)

dev.off()
system("open Figure_5.png")




#
#
#
#
#
#
#
#
#
#
#
#





#### 6- panels ####

png("Appendix_2.png", res=800, units="in", width=6, height=8)
par(mfrow=c(3,2), mar=c(2,4,1,1), oma=c(1,1,1,1))
#A

plot(predictionToFF~TinOFFscl, data=subset(new.dat.clim, Species_eggs=="HOWA"), col="red",type="l", ylim=c(0,1), xlim=c(-2.0, 1.8), 
     xaxt='n',xlab=NA,ylab="Probability of Hatching", lty=1)


axis(1, at=round(seq(min(new.dat.clim$TinOFFscl), max(new.dat.clim$TinOFFscl),length=5),1),
     labels=round(seq(min(insums$TinOFF), max(insums$TinOFF),length=5),0), tick=TRUE, padj=-.75)


polygon(x=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=TinOFFscl), na.rm=T),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=TinOFFscl), na.rm=T), 
            new.dat.clim$TinOFFscl[1:100],max(subset(new.dat.clim, Species_eggs=="HOWA", select=TinOFFscl), na.rm=T), 
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=TinOFFscl), na.rm=T), 
            new.dat.clim$TinOFFscl[100:1],
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=TinOFFscl), na.rm=T)),
        y=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToFF)), 
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=uprToFF)),   
            subset(new.dat.clim, Species_eggs=="HOWA",select=uprToFF, drop=TRUE),     
            max(subset(new.dat.clim, Species_eggs=="HOWA",select=uprToFF)),         
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToFF)),
            rev(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToFF, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToFF))), 
        col=rgb(0.5,0,0,.2), border=FALSE)

lines(predictionToFF~TinOFFscl, data=subset(new.dat.clim, Species_eggs=="BTBW"), col="blue", lty=2)
polygon(x=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=TinOFFscl), na.rm=T),
            min(subset(new.dat.clim, Species_eggs=="BTBW", select=TinOFFscl), na.rm=T), 
            new.dat.clim$TinOFFscl[101:200],
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=TinOFFscl), na.rm=T),
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=TinOFFscl), na.rm=T), 
            new.dat.clim$TinOFFscl[200:101],min(subset(new.dat.clim, Species_eggs=="BTBW", select=TinOFFscl), na.rm=T)),
        y=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=lwrToFF)), 
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=uprToFF)),   
            subset(new.dat.clim,Species_eggs=="BTBW",select=uprToFF, drop=TRUE),     
            max(subset(new.dat.clim,Species_eggs=="BTBW",select=uprToFF)),         
            min(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrToFF)),
            rev(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrToFF, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="BTBW",select=lwrToFF))), 
        col=rgb(0,0,0.5,.2), border=FALSE)
#legend(0.2, .15, c("Hooded", "Black-throated Blue"), col=c("red", "blue"), title="Warbler Species Eggs", lty=1, cex=.7)
text(-1.9, .1, "A", font=2, cex=1.2)
#mtext(side=3, "Effect of Inner Nest Temperature on Hatch Rates", font=2, cex=.9, padj=-3)
mtext(side=1, "Inner Temperature (\u00B0C), Off Bouts", cex=.7, adj=.5, padj=1.95)




#D

plot(predictionHoFF~HinOFFscl, data=subset(new.dat.clim, Species_eggs=="HOWA"), col="red",type="l", ylim=c(0,1), xlim=c(-2.0, 2.4), 
     xaxt='n',xlab=NA,ylab="", lty=1)


axis(1, at=round(seq(min(new.dat.clim$HinOFFscl), max(new.dat.clim$HinOFFscl),length=6),1),
     labels=round(seq(min(insums$HinOFF), max(insums$HinOFF),length=6),0), tick=TRUE, padj=-.75)


polygon(x=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=HinOFFscl), na.rm=T),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=HinOFFscl), na.rm=T), 
            new.dat.clim$HinOFFscl[1:100],max(subset(new.dat.clim, Species_eggs=="HOWA", select=HinOFFscl), na.rm=T), 
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=HinOFFscl), na.rm=T), 
            new.dat.clim$HinOFFscl[100:1],
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=HinOFFscl), na.rm=T)),
        y=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrHoFF)), 
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=uprHoFF)),   
            subset(new.dat.clim, Species_eggs=="HOWA",select=uprHoFF, drop=TRUE),     
            max(subset(new.dat.clim, Species_eggs=="HOWA",select=uprHoFF)),         
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrHoFF)),
            rev(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrHoFF, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrHoFF))), 
        col=rgb(0.5,0,0,.2), border=FALSE)

lines(predictionHoFF~HinOFFscl, data=subset(new.dat.clim, Species_eggs=="BTBW"), col="blue", lty=2)
polygon(x=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=HinOFFscl), na.rm=T),
            min(subset(new.dat.clim, Species_eggs=="BTBW", select=HinOFFscl), na.rm=T), 
            new.dat.clim$TinOFFscl[101:200],
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=HinOFFscl), na.rm=T),
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=HinOFFscl), na.rm=T), 
            new.dat.clim$HinOFFscl[200:101],min(subset(new.dat.clim, Species_eggs=="BTBW", select=HinOFFscl), na.rm=T)),
        y=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=lwrHoFF)), 
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=uprHoFF)),   
            subset(new.dat.clim,Species_eggs=="BTBW",select=uprHoFF, drop=TRUE),     
            max(subset(new.dat.clim,Species_eggs=="BTBW",select=uprHoFF)),         
            min(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrHoFF)),
            rev(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrHoFF, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="BTBW",select=lwrHoFF))), 
        col=rgb(0,0,0.5,.2), border=FALSE)
#legend(0.2, .15, c("Hooded", "Black-throated Blue"), col=c("red", "blue"), title="Warbler Species", lty=1, cex=.7)
text(-1.9, .1, "D", font=2, cex=1.2)
#mtext(side=3, "Effect of Inner Nest Temperature on Hatch Rates", font=2, cex=.9, padj=-3)
mtext(side=1, "Inner Humidity, Off Bouts", cex=.7, adj=.5, padj=1.95)

#B


plot(predictionToN~TinONscl, data=subset(new.dat.clim, Species_eggs=="HOWA"), col="red",type="l", ylim=c(0,1), xlim=c(-2.0, 1.8), 
     xaxt='n',xlab=NA,ylab="Probability of Hatching", lty=1)


axis(1, at=round(seq(min(new.dat.clim$TinONscl), max(new.dat.clim$TinONscl),length=5),1),
     labels=round(seq(min(insums$TinON), max(insums$TinON),length=5),0), tick=TRUE, padj=-.75)


polygon(x=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=TinONscl), na.rm=T),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=TinONscl), na.rm=T), 
            new.dat.clim$TinONscl[1:100],max(subset(new.dat.clim, Species_eggs=="HOWA", select=TinONscl), na.rm=T), 
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=TinONscl), na.rm=T), 
            new.dat.clim$TinONscl[100:1],
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=TinONscl), na.rm=T)),
        y=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToN)), 
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=uprToN)),   
            subset(new.dat.clim, Species_eggs=="HOWA",select=uprToN, drop=TRUE),     
            max(subset(new.dat.clim, Species_eggs=="HOWA",select=uprToN)),         
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToN)),
            rev(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToN, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToN))), 
        col=rgb(0.5,0,0,.2), border=FALSE)

lines(predictionToN~TinONscl, data=subset(new.dat.clim, Species_eggs=="BTBW"), col="blue", lty=2)
polygon(x=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=TinONscl), na.rm=T),
            min(subset(new.dat.clim, Species_eggs=="BTBW", select=TinONscl), na.rm=T), 
            new.dat.clim$TinONscl[101:200],
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=TinONscl), na.rm=T),
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=TinONscl), na.rm=T), 
            new.dat.clim$TinONscl[200:101],min(subset(new.dat.clim, Species_eggs=="BTBW", select=TinONscl), na.rm=T)),
        y=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=lwrToN)), 
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=uprToN)),   
            subset(new.dat.clim,Species_eggs=="BTBW",select=uprToN, drop=TRUE),     
            max(subset(new.dat.clim,Species_eggs=="BTBW",select=uprToN)),         
            min(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrToN)),
            rev(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrToN, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="BTBW",select=lwrToN))), 
        col=rgb(0,0,0.5,.2), border=FALSE)
#legend(0.2, .15, c("Hooded", "Black-throated Blue"), col=c("red", "blue"), title="Warbler Species", lty=1, cex=.7)
text(-1.9, .1, "B", font=2, cex=1.2)
#mtext(side=3, "Effect of Inner Nest Temperature on Hatch Rates", font=2, cex=.9, padj=-3)
mtext(side=1, "Inner Temperature (\u00B0C), Incubating", cex=.7, adj=.5, padj=1.95)


# E
plot(predictionHoN~HinONscl, data=subset(new.dat.clim, Species_eggs=="HOWA"), col="red",type="l", ylim=c(0,1), xlim=c(-2.0, 1.8), 
     xaxt='n',xlab=NA,ylab="", lty=1)


axis(1, at=round(seq(min(new.dat.clim$HinONscl), max(new.dat.clim$HinONscl),length=5),1),
     labels=round(seq(min(insums$HinON), max(insums$HinON),length=5),0), tick=TRUE, padj=-.75)


polygon(x=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=HinONscl), na.rm=T),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=HinONscl), na.rm=T), 
            new.dat.clim$HinONscl[1:100],max(subset(new.dat.clim, Species_eggs=="HOWA", select=HinONscl), na.rm=T), 
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=HinONscl), na.rm=T), 
            new.dat.clim$HinONscl[100:1],
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=HinONscl), na.rm=T)),
        y=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrHoN)), 
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=uprHoN)),   
            subset(new.dat.clim, Species_eggs=="HOWA",select=uprHoN, drop=TRUE),     
            max(subset(new.dat.clim, Species_eggs=="HOWA",select=uprHoN)),         
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrHoN)),
            rev(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrHoN, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrHoN))), 
        col=rgb(0.5,0,0,.2), border=FALSE)

lines(predictionHoN~HinONscl, data=subset(new.dat.clim, Species_eggs=="BTBW"), col="blue", lty=2)
polygon(x=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=HinONscl), na.rm=T),
            min(subset(new.dat.clim, Species_eggs=="BTBW", select=HinONscl), na.rm=T), 
            new.dat.clim$HinONscl[101:200],
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=HinONscl), na.rm=T),
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=HinONscl), na.rm=T), 
            new.dat.clim$HinONscl[200:101],min(subset(new.dat.clim, Species_eggs=="BTBW", select=HinONscl), na.rm=T)),
        y=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=lwrHoN)), 
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=uprHoN)),   
            subset(new.dat.clim,Species_eggs=="BTBW",select=uprHoN, drop=TRUE),     
            max(subset(new.dat.clim,Species_eggs=="BTBW",select=uprHoN)),         
            min(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrHoN)),
            rev(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrHoN, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="BTBW",select=lwrHoN))), 
        col=rgb(0,0,0.5,.2), border=FALSE)
#legend(0.2, .15, c("Hooded", "Black-throated Blue"), col=c("red", "blue"), title="Warbler Species", lty=1, cex=.7)
text(-1.9, .1, "E", font=2, cex=1.2)
#mtext(side=3, "Effect of Inner Nest Temperature on Hatch Rates", font=2, cex=.9, padj=-3)
mtext(side=1, "Inner Humidity, Incubating", cex=.7, adj=.5, padj=1.95)



# C
plot(predictionTout~Toutscl, data=subset(new.dat.clim, Species_eggs=="HOWA"), col="red",type="l", ylim=c(0,1), xlim=c(-2.0, 1.8), 
     xaxt='n',xlab=NA,ylab="Probability of Hatching", lty=1)


axis(1, at=round(seq(min(new.dat.clim$Toutscl), max(new.dat.clim$Toutscl),length=5),1),
     labels=round(seq(min(new.dat.clim$Tout), max(new.dat.clim$Tout),length=5),0), tick=TRUE, padj=-.75)


polygon(x=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=Toutscl), na.rm=T),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=Toutscl), na.rm=T), 
            new.dat.clim$Toutscl[1:100],max(subset(new.dat.clim, Species_eggs=="HOWA", select=Toutscl), na.rm=T), 
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=Toutscl), na.rm=T), 
            new.dat.clim$Toutscl[100:1],
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=Toutscl), na.rm=T)),
        y=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrTout)), 
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=uprTout)),   
            subset(new.dat.clim, Species_eggs=="HOWA",select=uprTout, drop=TRUE),     
            max(subset(new.dat.clim, Species_eggs=="HOWA",select=uprTout)),         
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrTout)),
            rev(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrTout, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrTout))), 
        col=rgb(0.5,0,0,.2), border=FALSE)

lines(predictionTout~Toutscl, data=subset(new.dat.clim, Species_eggs=="BTBW"), col="blue", lty=2)
polygon(x=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=Toutscl), na.rm=T),
            min(subset(new.dat.clim, Species_eggs=="BTBW", select=Toutscl), na.rm=T), 
            new.dat.clim$Toutscl[101:200],
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=Toutscl), na.rm=T),
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=Toutscl), na.rm=T), 
            new.dat.clim$Toutscl[200:101],min(subset(new.dat.clim, Species_eggs=="BTBW", select=Toutscl), na.rm=T)),
        y=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=lwrTout)), 
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=uprTout)),   
            subset(new.dat.clim,Species_eggs=="BTBW",select=uprTout, drop=TRUE),     
            max(subset(new.dat.clim,Species_eggs=="BTBW",select=uprTout)),         
            min(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrTout)),
            rev(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrTout, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="BTBW",select=lwrTout))), 
        col=rgb(0,0,0.5,.2), border=FALSE)
#legend(0.2, .15, c("Tooded", "Black-throated Blue"), col=c("red", "blue"), title="Warbler Species", lty=1, cex=.7)
text(-1.9, .1, "C", font=2, cex=1.2)
#mtext(side=3, "Effect of Inner Nest Temperature on Tatch Rates", font=2, cex=.9, padj=-3)
mtext(side=1, "Ambient Temperature (\u00B0C)", cex=.7, adj=.5, padj=1.95)


# F
plot(predictionHout~Houtscl, data=subset(new.dat.clim, Species_eggs=="HOWA"), col="red",type="l", lty=1, ylim=c(0,1), xlim=c(-2.0, 1.1), 
     xaxt='n',xlab=NA,ylab="")


axis(1, at=round(seq(min(new.dat.clim$Houtscl), max(new.dat.clim$Houtscl),length=5),1),
     labels=round(seq(min(new.dat.clim$Hout), max(new.dat.clim$Hout),length=5),0), tick=TRUE, padj=-.75)


polygon(x=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=Houtscl), na.rm=T),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=Houtscl), na.rm=T), 
            new.dat.clim$Houtscl[1:100],max(subset(new.dat.clim, Species_eggs=="HOWA", select=Houtscl), na.rm=T), 
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=Houtscl), na.rm=T), 
            new.dat.clim$Houtscl[100:1],
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=Houtscl), na.rm=T)),
        y=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrHout)), 
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=uprHout)),   
            subset(new.dat.clim, Species_eggs=="HOWA",select=uprHout, drop=TRUE),     
            max(subset(new.dat.clim, Species_eggs=="HOWA",select=uprHout)),         
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrHout)),
            rev(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrHout, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrHout))), 
        col=rgb(0.5,0,0,.2), border=FALSE)

lines(predictionHout~Houtscl, data=subset(new.dat.clim, Species_eggs=="BTBW"), col="blue", lty=2)
polygon(x=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=Houtscl), na.rm=T),
            min(subset(new.dat.clim, Species_eggs=="BTBW", select=Houtscl), na.rm=T), 
            new.dat.clim$Houtscl[101:200],
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=Houtscl), na.rm=T),
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=Houtscl), na.rm=T), 
            new.dat.clim$Houtscl[200:101],min(subset(new.dat.clim, Species_eggs=="BTBW", select=Houtscl), na.rm=T)),
        y=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=lwrHout)), 
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=uprHout)),   
            subset(new.dat.clim,Species_eggs=="BTBW",select=uprHout, drop=TRUE),     
            max(subset(new.dat.clim,Species_eggs=="BTBW",select=uprHout)),         
            min(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrHout)),
            rev(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrHout, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="BTBW",select=lwrHout))), 
        col=rgb(0,0,0.5,.2), border=FALSE)
legend(-0.5, .25, c("Hooded", "Black-throated Blue"), col=c("red", "blue"), title="Warbler Species Eggs", lty=c(1,2), cex=.7)
text(-1.9, .1, "F", font=2, cex=1.2)
#mtext(side=3, "Effect of Inner Nest Temperature on Hatch Rates", font=2, cex=.9, padj=-3)
mtext(side=1, "Ambient Humidity", cex=.7, adj=.5, padj=1.95)


dev.off()
system("open Appendix_2.png")




