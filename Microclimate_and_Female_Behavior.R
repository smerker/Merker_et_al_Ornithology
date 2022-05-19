#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#### Analysis of microclimate on probability of hatching ####


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#bring in data
load("micro_clim_data.gzip")


####models including temperature and humidity####

micro_clim_data$Species_eggs<-as.factor(micro_clim_data$Species_eggs)



glm.climoutT <- glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs*Toutscl, family=binomial, data=micro_clim_data,
                    subset=!is.na(Toutscl))
summary(glm.climoutT0)
glm.climoutT.best <- step(glm.climoutT)   ## Best model has species only

glm.climoutH <- glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs*Houtscl, family=binomial, data=micro_clim_data,
                    subset=!is.na(Houtscl))
summary(glm.climoutH)
glm.climoutH.best <- step(glm.climoutH)   ## Best model has species only

glm.climsums <- glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs*TinOFFscl, family=binomial, data=micro_clim_data,
                    subset=!is.na(TinOFFscl))
summary(glm.climsums)
glm.climsums.best <- step(glm.climsums)   ## Best model has additive species+temp

glm.climsums1 <- glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs*TinONscl, family=binomial, data=micro_clim_data,
                     subset=!is.na(TinONscl))
summary(glm.climsums1)
glm.climsums1.best <- step(glm.climsums1)  ## Best model has additive species+temp

glm.climsums2 <- glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs*HinOFFscl, family=binomial, data=micro_clim_data,
                     subset=!is.na(HinOFFscl))
summary(glm.climsums2)
glm.climsums2.best <- step(glm.climsums2)  ## Best model has species only

glm.climsums3 <- glm(cbind(Eggs.Hatched,unhatched) ~ Species_eggs*HinONscl, family=binomial, data=micro_clim_data,
                     subset=!is.na(HinONscl))
summary(glm.climsums3)
glm.climsums2.best <- step(glm.climsums3)  ## Best model has species only

#new data frame for predicting
new.dat.clim <- data.frame(TinOFFscl=rep(seq(min(subset(micro_clim_data, Species_eggs=="HOWA", select=TinOFFscl), na.rm=T), 
                                             max(subset(micro_clim_data, Species_eggs=="HOWA", select=TinOFFscl), na.rm=T), length.out=100),2))

new.dat.clim$TinOFFscl[101:200]<-seq(min(subset(micro_clim_data, Species_eggs=="BTBW", select=TinOFFscl), na.rm=T), 
                                     max(subset(micro_clim_data, Species_eggs=="BTBW", select=TinOFFscl), na.rm=T), length.out=100)

new.dat.clim$TinONscl[1:100]<-seq(min(subset(micro_clim_data, Species_eggs=="HOWA", select=TinONscl), na.rm=T), 
                                  max(subset(micro_clim_data, Species_eggs=="HOWA", select=TinONscl), na.rm=T), length.out=100)

new.dat.clim$TinONscl[101:200]<-seq(min(subset(micro_clim_data, Species_eggs=="BTBW", select=TinONscl), na.rm=T), 
                                    max(subset(micro_clim_data, Species_eggs=="BTBW", select=TinONscl), na.rm=T), length.out=100)


new.dat.clim$HinONscl[1:100]<-seq(min(subset(micro_clim_data, Species_eggs=="HOWA", select=HinONscl), na.rm=T), 
                                  max(subset(micro_clim_data, Species_eggs=="HOWA", select=HinONscl), na.rm=T), length.out=100)

new.dat.clim$HinONscl[101:200]<-seq(min(subset(micro_clim_data, Species_eggs=="BTBW", select=HinONscl), na.rm=T), 
                                    max(subset(micro_clim_data, Species_eggs=="BTBW", select=HinONscl), na.rm=T), length.out=100)



new.dat.clim$HinOFFscl[1:100]<-seq(min(subset(micro_clim_data, Species_eggs=="HOWA", select=HinOFFscl), na.rm=T), 
                                   max(subset(micro_clim_data, Species_eggs=="HOWA", select=HinOFFscl), na.rm=T), length.out=100)

new.dat.clim$HinOFFscl[101:200]<-seq(min(subset(micro_clim_data, Species_eggs=="BTBW", select=HinOFFscl), na.rm=T), 
                                     max(subset(micro_clim_data, Species_eggs=="BTBW", select=HinOFFscl), na.rm=T), length.out=100)

new.dat.clim$Toutscl[1:100]<-seq(min(subset(micro_clim_data, Species_eggs=="HOWA", select=Toutscl), na.rm=T), 
                                 max(subset(micro_clim_data, Species_eggs=="HOWA", select=Toutscl), na.rm=T), length.out=100)

new.dat.clim$Toutscl[101:200]<-seq(min(subset(micro_clim_data, Species_eggs=="BTBW", select=Toutscl), na.rm=T), 
                                   max(subset(micro_clim_data, Species_eggs=="BTBW", select=Toutscl), na.rm=T), length.out=100)

new.dat.clim$Houtscl[1:100]<-seq(min(subset(micro_clim_data, Species_eggs=="HOWA", select=Houtscl), na.rm=T), 
                                 max(subset(micro_clim_data, Species_eggs=="HOWA", select=Houtscl), na.rm=T), length.out=100)

new.dat.clim$Houtscl[101:200]<-seq(min(subset(micro_clim_data, Species_eggs=="BTBW", select=Houtscl), na.rm=T), 
                                   max(subset(micro_clim_data, Species_eggs=="BTBW", select=Houtscl), na.rm=T), length.out=100)



new.dat.clim$Species_eggs=rep(c("HOWA", "BTBW"), each=100)



#using predict function including response 

#temperature during off bouts

pred.t.off<-predict(glm.climsums.best, new.dat.clim, se.fit=TRUE)
pred.t.off
#add predictions to new.dat 
new.dat.clim$predictionToFF<-plogis(pred.t.off$fit)
new.dat.clim$seToFF<-pred.t.off$se.fit

new.dat.clim$lwrToFF<-plogis(pred.t.off$fit-(1.96*new.dat.clim$seToF))
new.dat.clim$uprToFF<-plogis(pred.t.off$fit+(1.96*new.dat.clim$seToF))


#temperature during on bouts

pred.t.on<-predict(glm.climsums1.best, new.dat.clim, se.fit=TRUE)
pred.t.on
#add predictions to new.dat 
new.dat.clim$predictionToN<-plogis(pred.t.on$fit)
new.dat.clim$seToN<-pred.t.on$se.fit

new.dat.clim$lwrToN<-plogis(pred.t.on$fit-(1.96*new.dat.clim$seToN))
new.dat.clim$uprToN<-plogis(pred.t.on$fit+(1.96*new.dat.clim$seToN))

#humidity during off bouts

pred.h.off<-predict(glm.climsums2.best, new.dat.clim, se.fit=TRUE)
pred.h.off
#add predictions to new.dat 
new.dat.clim$predictionHoFF<-plogis(pred.h.off$fit)
new.dat.clim$seHoFF<-pred.h.off$se.fit

#
new.dat.clim$lwrHoFF<-plogis(pred.h.off$fit-(1.96*new.dat.clim$seHoFF))
new.dat.clim$uprHoFF<-plogis(pred.h.off$fit+(1.96*new.dat.clim$seHoFF))


#humidity during on bouts

pred.h.on<-predict(glm.climsums3.best, new.dat.clim, se.fit=TRUE)
pred.h.on

new.dat.clim$predictionHoN<-plogis(pred.h.on$fit)
new.dat.clim$seHoN<-pred.h.on$se.fit


new.dat.clim$lwrHoN<-plogis(pred.h.on$fit-(1.96*new.dat.clim$seHoN))
new.dat.clim$uprHoN<-plogis(pred.h.on$fit+(1.96*new.dat.clim$seHoN))




#temperature outside nest

pred.t.out<-predict(glm.climoutT.best, new.dat.clim, se.fit=TRUE)
pred.t.out
#add predictions to new.dat 
new.dat.clim$predictionTout<-plogis(pred.t.out$fit)
new.dat.clim$seTout<-pred.t.out$se.fit


new.dat.clim$lwrTout<-plogis(pred.t.out$fit-(1.96*new.dat.clim$seTout))
new.dat.clim$uprTout<-plogis(pred.t.out$fit+(1.96*new.dat.clim$seTout))

#humidity  outside nest

pred.h.out<-predict(glm.climoutH.best, new.dat.clim, se.fit=TRUE)
pred.h.out

new.dat.clim$predictionHout<-plogis(pred.h.out$fit)
new.dat.clim$seHout<-pred.h.out$se.fit
new.dat.clim

new.dat.clim$lwrHout<-plogis(pred.h.out$fit-(1.96*new.dat.clim$seHout))
new.dat.clim$uprHout<-plogis(pred.h.out$fit+(1.96*new.dat.clim$seHout))
new.dat.clim
#








load("newdatclim.R")
load("insums.R")







png("hatch_humidity.png", res=800, units="in", width=7, height=4.5)
## par(mfrow=c(2,2), mar=c(2,4,1,1), oma=c(1,1,1,1))
par(mfrow=c(1,2), mai=c(0.7, 0.5, 0.6, 0.1), omi=c(0, 0.3, 0, 0))

plot(predictionHoFF~HinOFFscl, data=subset(new.dat.clim, Species_eggs=="HOWA"), col="red",type="l", ylim=c(0,1), ##xlim=c(-2.1, 2),  
     xaxt='n', xlab="Inner Humidity", ylab="", main="Foraging", mgp=c(2,1,0)) #"Probablity of Hatching")
axis(1, at=round(seq(min(new.dat.clim$HinOFFscl), max(new.dat.clim$HinOFFscl),length=5),1),
     labels=round(seq(min(insums$HinOFF), max(insums$HinOFF),length=5),0), tick=TRUE, padj=-.75)
polygon(x=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=HinOFFscl)),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=HinOFFscl)), 
            new.dat.clim$HinOFFscl[1:100],max(subset(new.dat.clim, Species_eggs=="HOWA", select=HinOFFscl)),
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=HinOFFscl)), 
            new.dat.clim$HinOFFscl[100:1],min(subset(new.dat.clim, Species_eggs=="HOWA", select=HinOFFscl))),
        y=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrHoFF)), 
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=uprHoFF)),   
            subset(new.dat.clim, Species_eggs=="HOWA",select=uprHoFF, drop=TRUE),     
            max(subset(new.dat.clim, Species_eggs=="HOWA",select=uprHoFF)),         
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrHoFF)),
            rev(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrHoFF, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrHoFF))), 
        col=rgb(0.5,0,0,.2), border=FALSE)
lines(predictionHoFF~HinOFFscl, data=subset(new.dat.clim, Species_eggs=="BTBW"), col="blue")
polygon(x=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=HinOFFscl)),
            min(subset(new.dat.clim, Species_eggs=="BTBW", select=HinOFFscl)),
            new.dat.clim$HinOFFscl[101:200],max(subset(new.dat.clim, Species_eggs=="BTBW", select=HinOFFscl)), 
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=HinOFFscl)),
            new.dat.clim$HinOFFscl[200:101],
            min(subset(new.dat.clim, Species_eggs=="BTBW", select=HinOFFscl))),
        y=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=lwrHoFF)), 
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=uprHoFF)),   
            subset(new.dat.clim,Species_eggs=="BTBW",select=uprHoFF, drop=TRUE),     
            max(subset(new.dat.clim,Species_eggs=="BTBW",select=uprHoFF)),         
            min(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrHoFF)),
            rev(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrHoFF, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="BTBW",select=lwrHoFF))), 
        col=rgb(0,0,0.5,.2), border=FALSE)
text(-1.8, .1, "A", font=2, cex=1.2)
## mtext(side=3, "Foraging", font=2, cex=.9)
## mtext(side=1, "Inner Temperature (\u00B0C)", cex=.7, adj=.5, padj=2.7)
mtext("Probability of hatching", side=2, line=0, outer=TRUE)

plot(predictionHoN~HinONscl, data=subset(new.dat.clim, Species_eggs=="HOWA"), col="red",type="l", ylim=c(0,1), ##xlim=c(-2.0, 1.8), 
     xaxt='n', xlab="Inner Humidity", ylab="", main="Incubating", mgp=c(2,1,0)) ##"Probablity of Hatching")
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
lines(predictionHoN~HinONscl, data=subset(new.dat.clim, Species_eggs=="BTBW"), col="blue")
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
legend(-0.8, .2, c("Hooded", "Black-throated blue"), col=c("red", "blue"), title="Warbler Species", lty=1, cex=.8)
text(-1.9, .1, "B", font=2, cex=1.2)
##mtext(side=3, "Incubating", font=2, cex=.9)
##mtext(side=1, "Inner Humidity", cex=.7, adj=.5, padj=2.7)

dev.off()
system("gopen hatch_humidity.png")








png("hatch_temp.png", res=800, units="in", width=7, height=4.5)
## par(mfrow=c(2,2), mar=c(2,4,1,1), oma=c(1,1,1,1))
par(mfrow=c(1,2), mai=c(0.7, 0.5, 0.6, 0.1), omi=c(0, 0.3, 0, 0))

plot(predictionToFF~TinOFFscl, data=subset(new.dat.clim, Species_eggs=="HOWA"), col="red",type="l", ylim=c(0,1), ##xlim=c(-2.1, 2),  
     xaxt='n', xlab="Inner Temperature (\u00B0C)", ylab="", main="Foraging", mgp=c(2,1,0)) #"Probablity of Hatching")
axis(1, at=round(seq(min(new.dat.clim$TinOFFscl), max(new.dat.clim$TinOFFscl),length=5),1),
     labels=round(seq(min(insums$TinOFF), max(insums$TinOFF),length=5),0), tick=TRUE, padj=-.75)
polygon(x=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=TinOFFscl)),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=TinOFFscl)), 
            new.dat.clim$TinOFFscl[1:100],max(subset(new.dat.clim, Species_eggs=="HOWA", select=TinOFFscl)),
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=TinOFFscl)), 
            new.dat.clim$TinOFFscl[100:1],min(subset(new.dat.clim, Species_eggs=="HOWA", select=TinOFFscl))),
        y=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToFF)), 
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=uprToFF)),   
            subset(new.dat.clim, Species_eggs=="HOWA",select=uprToFF, drop=TRUE),     
            max(subset(new.dat.clim, Species_eggs=="HOWA",select=uprToFF)),         
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToFF)),
            rev(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToFF, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToFF))), 
        col=rgb(0.5,0,0,.2), border=FALSE)
lines(predictionToFF~TinOFFscl, data=subset(new.dat.clim, Species_eggs=="BTBW"), col="blue")
polygon(x=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=TinOFFscl)),
            min(subset(new.dat.clim, Species_eggs=="BTBW", select=TinOFFscl)),
            new.dat.clim$TinOFFscl[101:200],max(subset(new.dat.clim, Species_eggs=="BTBW", select=TinOFFscl)), 
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=TinOFFscl)),
            new.dat.clim$TinOFFscl[200:101],
            min(subset(new.dat.clim, Species_eggs=="BTBW", select=TinOFFscl))),
        y=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=lwrToFF)), 
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=uprToFF)),   
            subset(new.dat.clim,Species_eggs=="BTBW",select=uprToFF, drop=TRUE),     
            max(subset(new.dat.clim,Species_eggs=="BTBW",select=uprToFF)),         
            min(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrToFF)),
            rev(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrToFF, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="BTBW",select=lwrToFF))), 
        col=rgb(0,0,0.5,.2), border=FALSE)
text(-1.8, .1, "A", font=2, cex=1.2)
## mtext(side=3, "Foraging", font=2, cex=.9)
## mtext(side=1, "Inner Temperature (\u00B0C)", cex=.7, adj=.5, padj=2.7)
mtext("Probability of hatching", side=2, line=0, outer=TRUE)

plot(predictionToN~TinONscl, data=subset(new.dat.clim, Species_eggs=="HOWA"), col="red",type="l", ylim=c(0,1), ##xlim=c(-2.0, 1.8), 
     xaxt='n', xlab="Inner Temperature (\u00B0C)", ylab="", main="Incubating", mgp=c(2,1,0)) ##"Probablity of Hatching")
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
lines(predictionToN~TinONscl, data=subset(new.dat.clim, Species_eggs=="BTBW"), col="blue")
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
legend(-0.8, .2, c("Hooded", "Black-throated blue"), col=c("red", "blue"), title="Warbler Species", lty=1, cex=.8)
text(-1.9, .1, "B", font=2, cex=1.2)
##mtext(side=3, "Incubating", font=2, cex=.9)
##mtext(side=1, "Inner Humidity", cex=.7, adj=.5, padj=2.7)

dev.off()
system("gopen hatch_temp.png")












ls()





















#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#### female behavior ####


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#bring in the data. 
load("femalebehavior.gzip")

#black-throated blue on-nest models  -- log transformed -- random effects include day within nest

library(nlme)
library(AICcmodavg)

lme.onB1<-lme(log(time)~T.out.mn+H.out.mn, random=~1|nestID/day, data=D3_9_on.B)
summary(lme.onB1)
lme.onB2<-lme(log(time)~T.out.mn, random=~1|nestID/day, data=D3_9_on.B)
summary(lme.onB2)
lme.onB3<-lme(log(time)~H.out.mn, random=~1|nestID/day, data=D3_9_on.B)
summary(lme.onB3)
lme.onB4<-lme(log(time)~H.out.mn*T.out.mn, random=~1|nestID/day, data=D3_9_on.B)
summary(lme.onB4)
class(lme.onB1)

#a few predictions for length of on bouts
new.dat.on.B.T<-data.frame(T.out.mn=seq(min(D3_9_on.B$T.out.mn),max(D3_9_on.B$T.out.mn),length=100),
                           H.out.mn=rep(mean(D3_9_on.B$H.out.mn),100))
new.dat.on.B.H<-data.frame(H.out.mn=seq(min(D3_9_on.B$H.out.mn),max(D3_9_on.B$H.out.mn),length=100),
                           T.out.mn=rep(mean(D3_9_on.B$T.out.mn),100))

pred.on.B.T<-predictSE.lme(lme.onB2, new.dat.on.B.T, level=0, se=T, interval="confidence",type="link", asList=T)
new.dat.on.B.T$fit1<-exp(pred.on.B.T$fit)
new.dat.on.B.T$low<-exp(pred.on.B.T$fit+1.96*pred.on.B.T$se)
new.dat.on.B.T$up<-exp(pred.on.B.T$fit-1.96*pred.on.B.T$se)

plot(fit1~T.out.mn, data=new.dat.on.B.T, type="l", ylim=c(14,20))
lines(low~T.out.mn, data=new.dat.on.B.T)
lines(up~T.out.mn, data=new.dat.on.B.T)

pred.on.B.H<-predictSE.lme(lme.onB3, new.dat.on.B.H, level=0, se=TRUE, interval="confidence", type="link")
new.dat.on.B.H$fit2<-exp(pred.on.B.H$fit)
new.dat.on.B.H$low<-exp(pred.on.B.H$fit+1.96*pred.on.B.H$se)
new.dat.on.B.H$up<-exp(pred.on.B.H$fit+-1.96*pred.on.B.H$se)

plot(fit2~H.out.mn, data=new.dat.on.B.H, type="l", ylim=c(14,20))
lines(low~H.out.mn, data=new.dat.on.B.H)
lines(up~H.out.mn, data=new.dat.on.B.H)

#hooded warbler on-nest models -- log transformed -- random effects include day within nest

lme.onH1<-lme(log(time)~T.out.mn+H.out.mn, random=~1|nestID/day, data=D3_9_on.H)
summary(lme.onH1)

lme.onH2<-lme(log(time)~T.out.mn, random=~1|nestID/day, data=D3_9_on.H)
summary(lme.onH2)

lme.onH3<-lme(log(time)~H.out.mn, random=~1|nestID/day, data=D3_9_on.H)
summary(lme.onH3)

lme.onH4<-lme(log(time)~H.out.mn*T.out.mn, random=~1|nestID/day, data=D3_9_on.H)
summary(lme.onH4)

#predictions
new.dat.on.H.T<-data.frame(T.out.mn=seq(min(D3_9_on.H$T.out.mn),max(D3_9_on.H$T.out.mn),length=100),
                           H.out.mn=rep(mean(D3_9_on.H$H.out.mn),100))
new.dat.on.H.H<-data.frame(H.out.mn=seq(min(D3_9_on.H$H.out.mn),max(D3_9_on.H$H.out.mn),length=100),
                           T.out.mn=rep(mean(D3_9_on.H$T.out.mn),100))


pred.on.H.T<-predictSE.lme(lme.onH2, new.dat.on.H.T, level=0, se=TRUE, interval="confidence", type="link")
new.dat.on.H.T$fit1<-exp(pred.on.H.T$fit)
new.dat.on.H.T$low<-exp(pred.on.H.T$fit-1.96*pred.on.H.T$se)
new.dat.on.H.T$up<-exp(pred.on.H.T$fit+1.96*pred.on.H.T$se)
plot(fit1~T.out.mn, data=new.dat.on.H.T, type="l",  ylim=c(15,29))
lines(low~T.out.mn, data=new.dat.on.H.T)
lines(up~T.out.mn, data=new.dat.on.H.T)

pred.on.H.H<-predictSE.lme(lme.onH3, new.dat.on.H.H, level=0, se=TRUE, interval="confidence", type="link")
new.dat.on.H.H$fit2<-exp(pred.on.H.H$fit)
new.dat.on.H.H$low<-exp(pred.on.H.H$fit-1.96*pred.on.H.H$se)
new.dat.on.H.H$up<-exp(pred.on.H.H$fit+1.96*pred.on.H.H$se)
plot(fit2~H.out.mn, data=new.dat.on.H.H, type="l",  ylim=c(15,29))
lines(low~H.out.mn, data=new.dat.on.H.H)
lines(up~H.out.mn, data=new.dat.on.H.H)




plot(fit1~T.out.mn, data=new.dat.on.B.T, type="l", col="blue", ylim=c(0,40), ylab="Time on nest")
lines(fit1~T.out.mn, data=new.dat.on.H.T, lty=1, col="red")

plot(fit2~H.out.mn, data=new.dat.on.B.H, type="l", col="blue", ylim=c(10,30), ylab="Time on nest")
lines(fit2~H.out.mn, data=new.dat.on.H.H, lty=1, col="red")



#black-throated blue off-nest models -- log transformed -- random effects include day within nest
lme.offB1<-lme(log(time)~T.out.mn+H.out.mn, random=~1|nestID/day, data=D3_9_off.B)
summary(lme.offB1)

lme.offB2<-lme(log(time)~T.out.mn, random=~1|nestID/day, data=D3_9_off.B)
summary(lme.offB2)
lme.offB3<-lme(log(time)~H.out.mn, random=~1|nestID/day, data=D3_9_off.B)
summary(lme.offB3)
lme.offB4<-lme(log(time)~H.out.mn*T.out.mn, random=~1|nestID/day, data=D3_9_off.B)
summary(lme.offB4)


#a few predictions for length of off bouts 
new.dat.off.B.T<-data.frame(T.out.mn=seq(min(D3_9_off.B$T.out.mn),max(D3_9_off.B$T.out.mn),length=100),
                            H.out.mn=rep(mean(D3_9_off.B$H.out.mn),100))
new.dat.off.B.H<-data.frame(H.out.mn=seq(min(D3_9_off.B$H.out.mn),max(D3_9_off.B$H.out.mn),length=100),
                            T.out.mn=rep(mean(D3_9_off.B$T.out.mn),100))


pred.off.B.T<-predictSE.lme(lme.offB2, new.dat.off.B.T, level=0, se=TRUE, interval="confidence", type="link")
new.dat.off.B.T$fit1<-exp(pred.off.B.T$fit)
new.dat.off.B.T$low<-exp(pred.off.B.T$fit-1.96*pred.off.B.T$se)
new.dat.off.B.T$up<-exp(pred.off.B.T$fit+1.96*pred.off.B.T$se)
plot(fit1~T.out.mn, data=new.dat.off.B.T, type="l",  ylim=c(4,8))
lines(low~T.out.mn, data=new.dat.off.B.T)
lines(up~T.out.mn, data=new.dat.off.B.T)



pred.off.B.H<-predictSE.lme(lme.offB3, new.dat.off.B.H, level=0, se=TRUE, interval="confidence", type="link")
new.dat.off.B.H$fit2<-exp(pred.off.B.H$fit)
new.dat.off.B.H$low<-exp(pred.off.B.H$fit-1.96*pred.off.B.H$se)
new.dat.off.B.H$up<-exp(pred.off.B.H$fit+1.96*pred.off.B.H$se)
plot(fit2~H.out.mn, data=new.dat.off.B.H, type="l",  ylim=c(4,8))
lines(low~H.out.mn, data=new.dat.off.B.H)
lines(up~H.out.mn, data=new.dat.off.B.H)



#hooded warbler off-nest models -- log transformed -- random effects include day within nest
lme.offH1<-lme(log(time)~T.out.mn+H.out.mn, random=~1|nestID/day, data=D3_9_off.H)
summary(lme.offH1)
lme.offH2<-lme(log(time)~T.out.mn, random=~1|nestID/day, data=D3_9_off.H)
summary(lme.offH2)
lme.offH3<-lme(log(time)~H.out.mn, random=~1|nestID/day, data=D3_9_off.H)
summary(lme.offH3)
lme.offH4<-lme(log(time)~H.out.mn*T.out.mn, random=~1|nestID/day, data=D3_9_off.H)
summary(lme.offH4)


new.dat.off.H.T<-data.frame(T.out.mn=seq(min(D3_9_off.H$T.out.mn),max(D3_9_off.H$T.out.mn),length=100),
                            H.out.mn=rep(mean(D3_9_off.H$H.out.mn),100))
new.dat.off.H.H<-data.frame(H.out.mn=seq(min(D3_9_off.H$H.out.mn),max(D3_9_off.H$H.out.mn),length=100),
                            T.out.mn=rep(mean(D3_9_off.H$T.out.mn),100))


pred.off.H.T<-predictSE.lme(lme.offH2, new.dat.off.H.T, level=0, se=TRUE, interval="confidence", type="link")
new.dat.off.H.T$fit1<-exp(pred.off.H.T$fit)
new.dat.off.H.T$low<-exp(pred.off.H.T$fit-1.96*pred.off.H.T$se)
new.dat.off.H.T$up<-exp(pred.off.H.T$fit+1.96*pred.off.H.T$se)
plot(fit1~T.out.mn, data=new.dat.off.H.T, type="l",  ylim=c(6,9))
lines(low~T.out.mn, data=new.dat.off.H.T)
lines(up~T.out.mn, data=new.dat.off.H.T)



pred.off.H.H<-predictSE.lme(lme.offH3, new.dat.off.H.H, level=0, se=TRUE, interval="confidence", type="link")
new.dat.off.H.H$fit2<-exp(pred.off.H.H$fit)
new.dat.off.H.H$low<-exp(pred.off.H.H$fit-1.96*pred.off.H.H$se)
new.dat.off.H.H$up<-exp(pred.off.H.H$fit+1.96*pred.off.H.H$se)
plot(fit2~H.out.mn, data=new.dat.off.H.H, type="l",  ylim=c(6,9))
lines(low~H.out.mn, data=new.dat.off.H.H)
lines(up~H.out.mn, data=new.dat.off.H.H)








#




#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#### 6 panel plot ####


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!














png("On_Off_Bouts_6panel.png", res=800, units="in", width=6, height=6)
par(mfrow=c(3,2), mar=c(2,4,1,1), oma=c(1,1,1,1))


plot(predictionToFF~TinOFFscl, data=subset(new.dat.clim, Species_eggs=="HOWA"), col="red",type="l", ylim=c(0,1), xlim=c(-2.1, 1.9), 
     xaxt='n',xlab=NA,ylab="Probablity of Hatching")

axis(1, at=round(seq(min(new.dat.clim$TinOFFscl), max(new.dat.clim$TinOFFscl),length=5),1),
     labels=round(seq(min(insums$TinOFF), max(insums$TinOFF),length=5),0), tick=TRUE, padj=-.75)

polygon(x=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=TinOFFscl)),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=TinOFFscl)), 
            new.dat.clim$TinOFFscl[1:100],max(subset(new.dat.clim, Species_eggs=="HOWA", select=TinOFFscl)),
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=TinOFFscl)), 
            new.dat.clim$TinOFFscl[100:1],min(subset(new.dat.clim, Species_eggs=="HOWA", select=TinOFFscl))),
        y=c(min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToFF)), 
            max(subset(new.dat.clim, Species_eggs=="HOWA", select=uprToFF)),   
            subset(new.dat.clim, Species_eggs=="HOWA",select=uprToFF, drop=TRUE),     
            max(subset(new.dat.clim, Species_eggs=="HOWA",select=uprToFF)),         
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToFF)),
            rev(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToFF, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="HOWA", select=lwrToFF))), 
        col=rgb(0.5,0,0,.2), border=FALSE)


lines(predictionToFF~TinOFFscl, data=subset(new.dat.clim, Species_eggs=="BTBW"), col="blue")
polygon(x=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=TinOFFscl)),
            min(subset(new.dat.clim, Species_eggs=="BTBW", select=TinOFFscl)),
            new.dat.clim$TinOFFscl[101:200],max(subset(new.dat.clim, Species_eggs=="BTBW", select=TinOFFscl)), 
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=TinOFFscl)),
            new.dat.clim$TinOFFscl[200:101],
            min(subset(new.dat.clim, Species_eggs=="BTBW", select=TinOFFscl))),
        y=c(min(subset(new.dat.clim, Species_eggs=="BTBW", select=lwrToFF)), 
            max(subset(new.dat.clim, Species_eggs=="BTBW", select=uprToFF)),   
            subset(new.dat.clim,Species_eggs=="BTBW",select=uprToFF, drop=TRUE),     
            max(subset(new.dat.clim,Species_eggs=="BTBW",select=uprToFF)),         
            min(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrToFF)),
            rev(subset(new.dat.clim,Species_eggs=="BTBW", select=lwrToFF, drop=TRUE)),
            min(subset(new.dat.clim, Species_eggs=="BTBW",select=lwrToFF))), 
        col=rgb(0,0,0.5,.2), border=FALSE)
text(-1.9, .1, "A", font=2, cex=1.2)
mtext(side=3, "Foraging", font=2, cex=.9)
mtext(side=1, "Inner Temperature (\u00B0C)", cex=.7, adj=.5, padj=2.7)

plot(predictionToN~TinONscl, data=subset(new.dat.clim, Species_eggs=="HOWA"), col="red",type="l", ylim=c(0,1), xlim=c(-2.0, 1.8), 
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

lines(predictionToN~TinONscl, data=subset(new.dat.clim, Species_eggs=="BTBW"), col="blue")
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
legend(-0.55, .35, c("Hooded", "Black-throated blue"), col=c("red", "blue"), title="Warbler Species", lty=1, cex=.8)
text(-1.9, .1, "B", font=2, cex=1.2)
mtext(side=3, "Incubating", font=2, cex=.9)
mtext(side=1, "Inner Temperature (\u00B0C)", cex=.7, adj=.5, padj=2.7)





plot(fit1~T.out.mn, data=new.dat.on.B.T, type="l", col="blue", xlim=c(10,29),ylim=c(12,28), xlab=NA,ylab="Time Incubating", lwd=2, xaxt='n')

axis(1, at=seq(10,29,length=6),
     labels=seq(10,30,length=6), tick=TRUE, padj=-.75)


polygon(x=c(9.1,9.1, new.dat.on.B.T$T.out.mn[1:100],29.3, 29.3, new.dat.on.B.T$T.out.mn[100:1],9.1),
        y=c(min(subset(new.dat.on.B.T, select=low)), 
            max(subset(new.dat.on.B.T, select=up)),   
            subset(new.dat.on.B.T,select=up, drop=TRUE),     
            max(subset(new.dat.on.B.T,select=up)),         
            min(subset(new.dat.on.B.T, select=low)),
            rev(subset(new.dat.on.B.T, select=low, drop=TRUE)),
            min(subset(new.dat.on.B.T, select=low))), 
        col=rgb(0,0,0.5,.2), border=FALSE)


lines(fit1~T.out.mn, data=new.dat.on.H.T, lty=1, col="red", lwd=2)
polygon(x=c(7.07,7.07, new.dat.on.H.T$T.out.mn[1:100], 30.73, 30.73, new.dat.on.H.T$T.out.mn[100:1],7.07),
        y=c(min(subset(new.dat.on.H.T, select=low)), 
            max(subset(new.dat.on.H.T, select=up)),   
            subset(new.dat.on.H.T,select=up, drop=TRUE),     
            max(subset(new.dat.on.H.T,select=up)),         
            min(subset(new.dat.on.H.T, select=low)),
            rev(subset(new.dat.on.H.T, select=low, drop=TRUE)),
            min(subset(new.dat.on.H.T, select=low))), 
        col=rgb(0.5,0,0,.2), border=FALSE)

text(12, 13.3, "C", font=2, cex=1.2)
mtext(side=1, "Ambient Temperature (\u00B0C)", cex=.7, adj=.5, padj=2.7)



#off
#temperature
plot(fit1~T.out.mn, data=new.dat.off.B.T, type="l", col="blue", xlim=c(10,29),ylim=c(5,9), xlab=NA, ylab="Time Foraging", lwd=2, xaxt='n')

axis(1, at=seq(10,29,length=6),
     labels=seq(10,30,length=6), tick=TRUE, padj=-.75)

polygon(x=c(9.1,9.1, new.dat.off.B.T$T.out.mn[1:100], 31.26, 31.26, new.dat.off.B.T$T.out.mn[100:1],9.1),
        y=c(min(subset(new.dat.off.B.T, select=low)), 
            max(subset(new.dat.off.B.T, select=up)),   
            subset(new.dat.off.B.T,select=up, drop=TRUE),     
            max(subset(new.dat.off.B.T,select=up)),         
            min(subset(new.dat.off.B.T, select=low)),
            rev(subset(new.dat.off.B.T, select=low, drop=TRUE)),
            min(subset(new.dat.off.B.T, select=low))), 
        col=rgb(0,0,0.5,.2), border=FALSE)


# lines(low~T.out.mn, data=new.dat.off.B.T, lty=2, col="blue")
# lines(up~T.out.mn, data=new.dat.off.B.T, lty=2, col="blue")


lines(fit1~T.out.mn, data=new.dat.off.H.T, lty=1, col="red", lwd=2)
polygon(x=c(7.1,7.1, new.dat.off.H.T$T.out.mn[1:100], 29.17, 29.17, new.dat.off.H.T$T.out.mn[100:1],7.1),
        y=c(min(subset(new.dat.off.H.T, select=low)), 
            max(subset(new.dat.off.H.T, select=up)),   
            subset(new.dat.off.H.T,select=up, drop=TRUE),     
            max(subset(new.dat.off.H.T,select=up)),         
            min(subset(new.dat.off.H.T, select=low)),
            rev(subset(new.dat.off.H.T, select=low, drop=TRUE)),
            min(subset(new.dat.off.H.T, select=low))), 
        col=rgb(0.5,0,0,.2), border=FALSE)

text(11, 5.3, "D", font=2, cex=1.2)
mtext(side=1, "Ambient Temperature (\u00B0C)", cex=.7, adj=.5, padj=2.7)




#on
#humidity
plot(fit2~H.out.mn, data=new.dat.on.B.H, type="l", col="blue", xlim=c(41,99),ylim=c(12,30), xlab=NA, xaxt='n', ylab="Time Incubating", lwd=2)



axis(1, at=seq(40,100,length=7),
     labels=seq(40,100,length=7), tick=TRUE, padj=-.75)

polygon(x=c(37.1,37.1, new.dat.on.B.H$H.out.mn[1:100], 100, 100, new.dat.on.B.H$H.out.mn[100:1],37.1),
        y=c(min(subset(new.dat.on.B.H, select=low)), 
            max(subset(new.dat.on.B.H, select=up)),   
            subset(new.dat.on.B.H,select=up, drop=TRUE),     
            max(subset(new.dat.on.B.H,select=up)),         
            min(subset(new.dat.on.B.H, select=low)),
            rev(subset(new.dat.on.B.H, select=low, drop=TRUE)),
            min(subset(new.dat.on.B.H, select=low))), 
        col=rgb(0,0,0.5,.2), border=FALSE)


lines(fit2~H.out.mn, data=new.dat.on.H.H, lty=1, col="red", lwd=2)
polygon(x=c(42.39,42.39, new.dat.on.H.H$H.out.mn[1:100], 100, 100, new.dat.on.H.H$H.out.mn[100:1],42.39),
        y=c(min(subset(new.dat.on.H.H, select=low)), 
            max(subset(new.dat.on.H.H, select=up)),   
            subset(new.dat.on.H.H,select=up, drop=TRUE),     
            max(subset(new.dat.on.H.H,select=up)),         
            min(subset(new.dat.on.H.H, select=low)),
            rev(subset(new.dat.on.H.H, select=low, drop=TRUE)),
            min(subset(new.dat.on.H.H, select=low))), 
        col=rgb(0.5,0,0,.2), border=FALSE)

text(42, 13.4, "E", font=2, cex=1.2)

mtext(side=1, "Ambient Humidity", cex=.7, adj=.5, padj=2.7)

#off
#humidity
plot(fit2~H.out.mn, data=new.dat.off.B.H, type="l", col="blue",xlim=c(40,100), ylim=c(5,9), xlab=NA, xaxt='n', ylab="Time Foraging", lwd=2)


axis(1, at=seq(40,100,length=7),
     labels=seq(40,100,length=7), tick=TRUE, padj=-.75)

polygon(x=c(37.7,37.7, new.dat.off.B.H$H.out.mn[1:100], 100, 100, new.dat.off.B.H$H.out.mn[100:1],37.7),
        y=c(min(subset(new.dat.off.B.H, select=low)), 
            max(subset(new.dat.off.B.H, select=up)),   
            subset(new.dat.off.B.H,select=up, drop=TRUE),     
            max(subset(new.dat.off.B.H,select=up)),         
            min(subset(new.dat.off.B.H, select=low)),
            rev(subset(new.dat.off.B.H, select=low, drop=TRUE)),
            min(subset(new.dat.off.B.H, select=low))), 
        col=rgb(0,0,0.5,.2), border=FALSE)

lines(fit2~H.out.mn, data=new.dat.off.H.H, lty=1, col="red", lwd=2)
polygon(x=c(42.5,42.5, new.dat.off.H.H$H.out.mn[1:100], 100, 100, new.dat.off.H.H$H.out.mn[100:1],42.5),
        y=c(min(subset(new.dat.off.H.H, select=low)), 
            max(subset(new.dat.off.H.H, select=up)),   
            subset(new.dat.off.H.H,select=up, drop=TRUE),     
            max(subset(new.dat.off.H.H,select=up)),         
            min(subset(new.dat.off.H.H, select=low)),
            rev(subset(new.dat.off.H.H, select=low, drop=TRUE)),
            min(subset(new.dat.off.H.H, select=low))), 
        col=rgb(0.5,0,0,.2), border=FALSE)

text(47, 5.3, "F", font=2, cex=1.2)
mtext(side=1, "Ambient Humidity", cex=.7, adj=.5, padj=2.7)

dev.off()
system("open On_Off_Bouts_6panel.png")


