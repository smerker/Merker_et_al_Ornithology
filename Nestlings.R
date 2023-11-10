####Nestling Growth Rates####
#Looking at nestling growth rates in swapped, control and unmanipulated nests.

#only run if switching between projects
#getwd()

#read in a .csv of nestling measurements and associated data

load("Nestlings.gzip")

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


####  mass gain  ####


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#just species
fm.mg_sp<-lm(massGain~species, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.mg_sp)

#just brood size
fm.mg_brd<-lm(massGain~broodsize, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.mg_brd)

#treatment
fm.mg_trt<-lm(massGain~trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.mg_trt)

#species &  treatment
fm.mg_sp.trt<-lm(massGain~species+trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.mg_sp.trt)

#species x  treatment
fm.mg_spxtrt<-lm(massGain~species*trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.mg_spxtrt)

#species & brood size
fm.mg_sp.brd<-lm(massGain~species+broodsize, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.mg_sp.brd)

#species x brood size
fm.mg_spxbrd<-lm(massGain~species*broodsize, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.mg_spxbrd)

#species & brood size & treatment
fm.mg_sp.brd.trt<-lm(massGain~species+broodsize+trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.mg_sp.brd.trt)

#species & brood size x treament
fm.mg_sp.brdxtrt<-lm(massGain~species+broodsize*trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.mg_sp.brdxtrt)

#species & broodsize x treatment
fm.mg_spxbrd.trt<-lm(massGain~species*broodsize+trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.mg_spxbrd.trt)

# species & brood & size x treatment
fm.mg_spxbrdxtrt<-lm(massGain~species*broodsize*trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.mg_spxbrdxtrt)


massAIC<-AIC(fm.mg_sp,
             fm.mg_brd,
             fm.mg_trt,
             fm.mg_sp.trt,
             fm.mg_spxtrt,
             fm.mg_sp.brd,
             fm.mg_spxbrd,
             fm.mg_sp.brd.trt,
             fm.mg_spxbrd.trt,
             fm.mg_spxbrdxtrt)

massAIC$deltaAIC<-massAIC$AIC-min(massAIC$AIC)

massAIC$w <- exp(-0.5*massAIC$deltaAIC)/sum(exp(-0.5*massAIC$deltaAIC))

massAIC_order <- round(massAIC[order(massAIC$AIC),],3)
massAIC_order
massAIC

#data for predictions
massgain.new.dat<-data.frame(species=rep(as.factor(c("HOWA","BTBW")),each=6), 
                             trt=rep(as.factor(c("swap", "unmanipulated")), each=3), 
                             broodsize=c(2,3,4))
massgain.new.dat
#str(massgain.new.dat)
massgain.pred1<-predict(fm.mg_sp, massgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
massgain.pred2<-predict(fm.mg_brd, massgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
massgain.pred3<-predict(fm.mg_trt, massgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
massgain.pred4<-predict(fm.mg_sp.trt, massgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
massgain.pred5<-predict(fm.mg_spxtrt, massgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
massgain.pred6<-predict(fm.mg_sp.brd, massgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
massgain.pred7<-predict(fm.mg_spxbrd, massgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
massgain.pred8<-predict(fm.mg_sp.brd.trt, massgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
massgain.pred9<-predict(fm.mg_spxbrd.trt, massgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
massgain.pred10<-predict(fm.mg_spxbrdxtrt, massgain.new.dat, type="response", se.fit = TRUE, interval="confidence")


#combine predictions
pred.fit.mg<-cbind(massgain.pred1$fit[,1],
                   massgain.pred2$fit[,1],
                   massgain.pred3$fit[,1],
                   massgain.pred4$fit[,1],
                   massgain.pred5$fit[,1],
                   massgain.pred6$fit[,1],
                   massgain.pred7$fit[,1],
                   massgain.pred8$fit[,1],
                   massgain.pred9$fit[,1],
                   massgain.pred10$fit[,1])

#apply weights
massgain.new.dat$pred.fit<-pred.fit.mg%*%massAIC$w

# get some CIs
pred.CI.lwr.mg<-cbind(massgain.pred1$fit[,2],
                      massgain.pred2$fit[,2],
                      massgain.pred3$fit[,2],
                      massgain.pred4$fit[,2],
                      massgain.pred5$fit[,2],
                      massgain.pred6$fit[,2],
                      massgain.pred7$fit[,2],
                      massgain.pred8$fit[,2],
                      massgain.pred9$fit[,2],
                      massgain.pred10$fit[,2])
massgain.new.dat$pred.CI.lwr<-pred.CI.lwr.mg%*%massAIC$w

pred.CI.upr.mg<-cbind(massgain.pred1$fit[,3],
                      massgain.pred2$fit[,3],
                      massgain.pred3$fit[,3],
                      massgain.pred4$fit[,3],
                      massgain.pred5$fit[,3],
                      massgain.pred6$fit[,3],
                      massgain.pred7$fit[,3],
                      massgain.pred8$fit[,3],
                      massgain.pred9$fit[,3],
                      massgain.pred10$fit[,3])
massgain.new.dat$pred.CI.upr<-pred.CI.upr.mg%*%massAIC$w

massgain.new.dat


par(mar=c(4,4,1,2))
plot(pred.fit~broodsize, data=subset(massgain.new.dat, species=="BTBW"& trt=="swap"),col=c("blue"), pch=16,type='b',ylim=c(4.7,5.67) ,ylab="Nestling mass gain(g)")
points(pred.fit~broodsize, data=subset(massgain.new.dat, species=="HOWA" & trt=="swap"),col=c("red"), pch=16,type='b')

points(pred.fit~broodsize, data=subset(massgain.new.dat, species=="HOWA" & trt=="unmanipulated"),col=c("red"), pch=15,type='b', lty=3)
points(pred.fit~broodsize, data=subset(massgain.new.dat, species=="BTBW"&  trt=="unmanipulated"),col=c("blue"), pch=15,type='b', lty=3)
legend(3.1,5.65, c("HOWA-swap", "HOWA-unmanipulated"), pch=c(16,15),lty=c(1,3), col=c("red","red"),cex=.8)

legend(2,5.05, c("BTBW-swap","BTBW-umanipulated"), pch=c(16,15),lty=c(1,3), col=c("blue","blue"),cex=.8)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#### tarsus gain ####


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#just species
fm.ts_sp<-lm(tarsGain~species, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ts_sp)

#broodsize
fm.ts_brd<-lm(tarsGain~broodsize, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ts_brd)

#treatment
fm.ts_trt<-lm(tarsGain~trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ts_trt)

#species & treatment
fm.ts_sp.trt<-lm(tarsGain~species+trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ts_sp.trt)

#species x treatment
fm.ts_spxtrt<-lm(tarsGain~species*trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ts_spxtrt)

#species & brood size
fm.ts_sp.brd<-lm(tarsGain~species+broodsize, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ts_sp.brd)

#species x brood size
fm.ts_spxbrd<-lm(tarsGain~species*broodsize, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ts_spxbrd)

#species & brood size & treatment
fm.ts_sp.brd.trt<-lm(tarsGain~species+broodsize+trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ts_sp.brd.trt)

#species &  brood size x treatment
fm.ts_sp.brdxtrt<-lm(tarsGain~species+broodsize*trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ts_sp.brdxtrt)

#species x broodsize & treatment
fm.ts_spxbrd.trt<-lm(tarsGain~species*broodsize+trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ts_spxbrd.trt)

# species x brood & size x trt
fm.ts_spxbrdxtrt<-lm(tarsGain~species*broodsize*trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ts_spxbrdxtrt)

#AIC
tarsAIC<-AIC(fm.ts_sp,
             fm.ts_brd,
             fm.ts_trt,
             fm.ts_sp.trt,
             fm.ts_spxtrt,
             fm.ts_sp.brd,
             fm.ts_spxbrd,
             fm.ts_sp.brd.trt,
             fm.ts_spxbrd.trt,
             fm.ts_spxbrdxtrt)

tarsAIC$deltaAIC<-tarsAIC$AIC-min(tarsAIC$AIC)

tarsAIC$w <- exp(-0.5*tarsAIC$deltaAIC)/sum(exp(-0.5*tarsAIC$deltaAIC))

tarsAIC_order <- round(tarsAIC[order(tarsAIC$AIC),],3)
tarsAIC_order
tarsAIC

#prediction data
tarsgain.new.dat<-data.frame(species=rep(as.factor(c("HOWA","BTBW")),each=6), 
                             trt=rep(as.factor(c("swap", "unmanipulated")), each=3), 
                             broodsize=c(2,3,4))
tarsgain.new.dat
#str(tarsgain.new.dat)
tarsgain.pred1<-predict(fm.ts_sp, tarsgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
tarsgain.pred2<-predict(fm.ts_brd, tarsgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
tarsgain.pred3<-predict(fm.ts_trt, tarsgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
tarsgain.pred4<-predict(fm.ts_sp.trt, tarsgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
tarsgain.pred5<-predict(fm.ts_spxtrt, tarsgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
tarsgain.pred6<-predict(fm.ts_sp.brd, tarsgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
tarsgain.pred7<-predict(fm.ts_spxbrd, tarsgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
tarsgain.pred8<-predict(fm.ts_sp.brd.trt, tarsgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
tarsgain.pred9<-predict(fm.ts_spxbrd.trt, tarsgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
tarsgain.pred10<-predict(fm.ts_spxbrdxtrt, tarsgain.new.dat, type="response", se.fit = TRUE, interval="confidence")


#combine predictions
pred.fit.ts<-cbind(tarsgain.pred1$fit[,1],
                   tarsgain.pred2$fit[,1],
                   tarsgain.pred3$fit[,1],
                   tarsgain.pred4$fit[,1],
                   tarsgain.pred5$fit[,1],
                   tarsgain.pred6$fit[,1],
                   tarsgain.pred7$fit[,1],
                   tarsgain.pred8$fit[,1],
                   tarsgain.pred9$fit[,1],
                   tarsgain.pred10$fit[,1])

#apply weights
tarsgain.new.dat$pred.fit<-pred.fit.ts%*%tarsAIC$w

#get CIs
pred.CI.lwr.ts<-cbind(tarsgain.pred1$fit[,2],
                      tarsgain.pred2$fit[,2],
                      tarsgain.pred3$fit[,2],
                      tarsgain.pred4$fit[,2],
                      tarsgain.pred5$fit[,2],
                      tarsgain.pred6$fit[,2],
                      tarsgain.pred7$fit[,2],
                      tarsgain.pred8$fit[,2],
                      tarsgain.pred9$fit[,2],
                      tarsgain.pred10$fit[,2])
tarsgain.new.dat$pred.CI.lwr<-pred.CI.lwr.ts%*%tarsAIC$w

pred.CI.upr.ts<-cbind(tarsgain.pred1$fit[,3],
                      tarsgain.pred2$fit[,3],
                      tarsgain.pred3$fit[,3],
                      tarsgain.pred4$fit[,3],
                      tarsgain.pred5$fit[,3],
                      tarsgain.pred6$fit[,3],
                      tarsgain.pred7$fit[,3],
                      tarsgain.pred8$fit[,3],
                      tarsgain.pred9$fit[,3],
                      tarsgain.pred10$fit[,3])
tarsgain.new.dat$pred.CI.upr<-pred.CI.upr.ts%*%tarsAIC$w

tarsgain.new.dat


par(mar=c(4,4,1,2))
plot(pred.fit~broodsize, data=subset(tarsgain.new.dat, species=="BTBW"& trt=="swap"),col=c("blue"), pch=16,type='b',ylim=c(6.9,11.6) ,ylab="Nestling tarsus gain(g)")
points(pred.fit~broodsize, data=subset(tarsgain.new.dat, species=="HOWA" & trt=="swap"),col=c("red"), pch=16,type='b')

points(pred.fit~broodsize, data=subset(tarsgain.new.dat, species=="HOWA" & trt=="unmanipulated"),col=c("red"), pch=15,type='b', lty=3)
points(pred.fit~broodsize, data=subset(tarsgain.new.dat, species=="BTBW"&  trt=="unmanipulated"),col=c("blue"), pch=15,type='b', lty=3)
legend(3.1,10.65, c("HOWA-swap", "HOWA-unmanipulated"), pch=c(16,15),lty=c(1,3), col=c("red","red"),cex=.8)

legend(3.1,7.55, c("BTBW-swap","BTBW-umanipulated"), pch=c(16,15),lty=c(1,3), col=c("blue","blue"),cex=.8)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


####    nares    ####


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#just species
fm.ns_sp<-lm(naresGain~species, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ns_sp)

#broodsize 
fm.ns_brd<-lm(naresGain~broodsize, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ns_brd)

#treatment
fm.ns_trt<-lm(naresGain~trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ns_trt)

#species &  treatment
fm.ns_sp.trt<-lm(naresGain~species+trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ns_sp.trt)

#species x treatment
fm.ns_spxtrt<-lm(naresGain~species*trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ns_spxtrt)

#species & brood size
fm.ns_sp.brd<-lm(naresGain~species+broodsize, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ns_sp.brd)

#species * brood size
fm.ns_spxbrd<-lm(naresGain~species*broodsize, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ns_spxbrd)

#species & brood size & treatment
fm.ns_sp.brd.trt<-lm(naresGain~species+broodsize+trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ns_sp.brd.trt)

#species & brood size x treatment
fm.ns_sp.brdxtrt<-lm(naresGain~species+broodsize*trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ns_sp.brdxtrt)

#species x broodsize + treatment
fm.ns_spxbrd.trt<-lm(naresGain~species*broodsize+trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ns_spxbrd.trt)

# species x broodsize x trt
fm.ns_spxbrdxtrt<-lm(naresGain~species*broodsize*trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.ns_spxbrdxtrt)

#AIC
naresAIC<-AIC(fm.ns_sp,
              fm.ns_brd,
              fm.ns_trt,
              fm.ns_sp.trt,
              fm.ns_spxtrt,
              fm.ns_sp.brd,
              fm.ns_spxbrd,
              fm.ns_sp.brd.trt,
              fm.ns_spxbrd.trt,
              fm.ns_spxbrdxtrt)

naresAIC$deltaAIC<-naresAIC$AIC-min(naresAIC$AIC)

naresAIC$w <- exp(-0.5*naresAIC$deltaAIC)/sum(exp(-0.5*naresAIC$deltaAIC))

naresAIC_order <- round(naresAIC[order(naresAIC$AIC),],3)
naresAIC_order
naresAIC

#prediction data
naresgain.new.dat<-data.frame(species=rep(as.factor(c("HOWA","BTBW")),each=6), 
                              trt=rep(as.factor(c("swap", "unmanipulated")), each=3), 
                              broodsize=c(2,3,4))
naresgain.new.dat
#str(naresgain.new.dat)
naresgain.pred1<-predict(fm.ns_sp, naresgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
naresgain.pred2<-predict(fm.ns_brd, naresgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
naresgain.pred3<-predict(fm.ns_trt, naresgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
naresgain.pred4<-predict(fm.ns_sp.trt, naresgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
naresgain.pred5<-predict(fm.ns_spxtrt, naresgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
naresgain.pred6<-predict(fm.ns_sp.brd, naresgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
naresgain.pred7<-predict(fm.ns_spxbrd, naresgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
naresgain.pred8<-predict(fm.ns_sp.brd.trt, naresgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
naresgain.pred9<-predict(fm.ns_spxbrd.trt, naresgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
naresgain.pred10<-predict(fm.ns_spxbrdxtrt, naresgain.new.dat, type="response", se.fit = TRUE, interval="confidence")


#combine predictions
pred.fit.ns<-cbind(naresgain.pred1$fit[,1],
                   naresgain.pred2$fit[,1],
                   naresgain.pred3$fit[,1],
                   naresgain.pred4$fit[,1],
                   naresgain.pred5$fit[,1],
                   naresgain.pred6$fit[,1],
                   naresgain.pred7$fit[,1],
                   naresgain.pred8$fit[,1],
                   naresgain.pred9$fit[,1],
                   naresgain.pred10$fit[,1])

naresgain.new.dat$pred.fit<-pred.fit.ns%*%naresAIC$w

# Get CIs
pred.CI.lwr.ns<-cbind(naresgain.pred1$fit[,2],
                      naresgain.pred2$fit[,2],
                      naresgain.pred3$fit[,2],
                      naresgain.pred4$fit[,2],
                      naresgain.pred5$fit[,2],
                      naresgain.pred6$fit[,2],
                      naresgain.pred7$fit[,2],
                      naresgain.pred8$fit[,2],
                      naresgain.pred9$fit[,2],
                      naresgain.pred10$fit[,2])
naresgain.new.dat$pred.CI.lwr<-pred.CI.lwr.ns%*%naresAIC$w

pred.CI.upr.ns<-cbind(naresgain.pred1$fit[,3],
                      naresgain.pred2$fit[,3],
                      naresgain.pred3$fit[,3],
                      naresgain.pred4$fit[,3],
                      naresgain.pred5$fit[,3],
                      naresgain.pred6$fit[,3],
                      naresgain.pred7$fit[,3],
                      naresgain.pred8$fit[,3],
                      naresgain.pred9$fit[,3],
                      naresgain.pred10$fit[,3])
naresgain.new.dat$pred.CI.upr<-pred.CI.upr.ns%*%naresAIC$w

naresgain.new.dat



par(mar=c(4,4,1,2))
plot(pred.fit~broodsize, data=subset(naresgain.new.dat, species=="BTBW"& trt=="swap"),col=c("blue"), pch=16,type='b',ylim=c(0,2) ,ylab="Nestling nares gain(g)")
points(pred.fit~broodsize, data=subset(naresgain.new.dat, species=="HOWA" & trt=="swap"),col=c("red"), pch=16,type='b')

points(pred.fit~broodsize, data=subset(naresgain.new.dat, species=="HOWA" & trt=="unmanipulated"),col=c("red"), pch=15,type='b', lty=3)
points(pred.fit~broodsize, data=subset(naresgain.new.dat, species=="BTBW"&  trt=="unmanipulated"),col=c("blue"), pch=15,type='b', lty=3)
legend(3.1,1.65, c("HOWA-swap", "HOWA-unmanipulated"), pch=c(16,15),lty=c(1,3), col=c("red","red"),cex=.8)

legend(3.1,.55, c("BTBW-swap","BTBW-umanipulated"), pch=c(16,15),lty=c(1,3), col=c("blue","blue"),cex=.8)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


##### bill width  ####


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#just species
fm.wd_sp<-lm(widthGain~species, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wd_sp)

#broodsize
fm.wd_brd<-lm(widthGain~broodsize, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wd_brd)

#treatment
fm.wd_trt<-lm(widthGain~trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wd_trt)

#species &  treatment
fm.wd_sp.trt<-lm(widthGain~species+trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wd_sp.trt)

#species x treatment
fm.wd_spxtrt<-lm(widthGain~species*trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wd_spxtrt)

#species & brood size
fm.wd_sp.brd<-lm(widthGain~species+broodsize, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wd_sp.brd)

#species x brood size
fm.wd_spxbrd<-lm(widthGain~species*broodsize, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wd_spxbrd)

#species & broodsize & treatment
fm.wd_sp.brd.trt<-lm(widthGain~species+broodsize+trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wd_sp.brd.trt)

#species & brood size * treatment
fm.wd_sp.brdxtrt<-lm(widthGain~species+broodsize*trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wd_sp.brdxtrt)

#species x broodsize + treatment
fm.wd_spxbrd.trt<-lm(widthGain~species*broodsize+trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wd_spxbrd.trt)

# species x broodsize x trt
fm.wd_spxbrdxtrt<-lm(widthGain~species*broodsize*trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wd_spxbrdxtrt)

#AIC
widthAIC<-AIC(fm.wd_sp,
              fm.wd_brd,
              fm.wd_trt,
              fm.wd_sp.trt,
              fm.wd_spxtrt,
              fm.wd_sp.brd,
              fm.wd_spxbrd,
              fm.wd_sp.brd.trt,
              fm.wd_spxbrd.trt,
              fm.wd_spxbrdxtrt)

widthAIC$deltaAIC<-widthAIC$AIC-min(widthAIC$AIC)

widthAIC$w <- exp(-0.5*widthAIC$deltaAIC)/sum(exp(-0.5*widthAIC$deltaAIC))

widthAIC_order <- round(widthAIC[order(widthAIC$AIC),],3)
widthAIC_order
widthAIC

#prediction data
widthgain.new.dat<-data.frame(species=rep(as.factor(c("HOWA","BTBW")),each=6), 
                              trt=rep(as.factor(c("swap", "unmanipulated")), each=3), 
                              broodsize=c(2,3,4))
widthgain.new.dat
#str(widthgain.new.dat)
widthgain.pred1<-predict(fm.wd_sp, widthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
widthgain.pred2<-predict(fm.wd_brd, widthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
widthgain.pred3<-predict(fm.wd_trt, widthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
widthgain.pred4<-predict(fm.wd_sp.trt, widthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
widthgain.pred5<-predict(fm.wd_spxtrt, widthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
widthgain.pred6<-predict(fm.wd_sp.brd, widthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
widthgain.pred7<-predict(fm.wd_spxbrd, widthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
widthgain.pred8<-predict(fm.wd_sp.brd.trt, widthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
widthgain.pred9<-predict(fm.wd_spxbrd.trt, widthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
widthgain.pred10<-predict(fm.wd_spxbrdxtrt, widthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")


#cmobine predictions
pred.fit.wd<-cbind(widthgain.pred1$fit[,1],
                   widthgain.pred2$fit[,1],
                   widthgain.pred3$fit[,1],
                   widthgain.pred4$fit[,1],
                   widthgain.pred5$fit[,1],
                   widthgain.pred6$fit[,1],
                   widthgain.pred7$fit[,1],
                   widthgain.pred8$fit[,1],
                   widthgain.pred9$fit[,1],
                   widthgain.pred10$fit[,1])

widthgain.new.dat$pred.fit<-pred.fit.wd%*%widthAIC$w

#get CIs
pred.CI.lwr.wd<-cbind(widthgain.pred1$fit[,2],
                      widthgain.pred2$fit[,2],
                      widthgain.pred3$fit[,2],
                      widthgain.pred4$fit[,2],
                      widthgain.pred5$fit[,2],
                      widthgain.pred6$fit[,2],
                      widthgain.pred7$fit[,2],
                      widthgain.pred8$fit[,2],
                      widthgain.pred9$fit[,2],
                      widthgain.pred10$fit[,2])
widthgain.new.dat$pred.CI.lwr<-pred.CI.lwr.wd%*%widthAIC$w

pred.CI.upr.wd<-cbind(widthgain.pred1$fit[,3],
                      widthgain.pred2$fit[,3],
                      widthgain.pred3$fit[,3],
                      widthgain.pred4$fit[,3],
                      widthgain.pred5$fit[,3],
                      widthgain.pred6$fit[,3],
                      widthgain.pred7$fit[,3],
                      widthgain.pred8$fit[,3],
                      widthgain.pred9$fit[,3],
                      widthgain.pred10$fit[,3])
widthgain.new.dat$pred.CI.upr<-pred.CI.upr.wd%*%widthAIC$w

widthgain.new.dat



par(mar=c(4,4,1,2))
plot(pred.fit~broodsize, data=subset(widthgain.new.dat, species=="BTBW"& trt=="swap"),col=c("blue"), pch=16,type='b',ylim=c(0,1) ,ylab="Nestling bill width gain(g)")
points(pred.fit~broodsize, data=subset(widthgain.new.dat, species=="HOWA" & trt=="swap"),col=c("red"), pch=16,type='b')

points(pred.fit~broodsize, data=subset(widthgain.new.dat, species=="HOWA" & trt=="unmanipulated"),col=c("red"), pch=15,type='b', lty=3)
points(pred.fit~broodsize, data=subset(widthgain.new.dat, species=="BTBW"&  trt=="unmanipulated"),col=c("blue"), pch=15,type='b', lty=3)
legend(3.1,0.75, c("HOWA-swap", "HOWA-unmanipulated"), pch=c(16,15),lty=c(1,3), col=c("red","red"),cex=.8)

legend(2.1,.45, c("BTBW-swap","BTBW-umanipulated"), pch=c(16,15),lty=c(1,3), col=c("blue","blue"),cex=.8)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#### bill depth ####


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

##just species
fm.bd_sp<-lm(depthGain~species, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.bd_sp)

#broodsize
fm.bd_brd<-lm(depthGain~broodsize, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.bd_brd)

#treatment
fm.bd_trt<-lm(depthGain~trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.bd_trt)

#species &  treatment
fm.bd_sp.trt<-lm(depthGain~species+trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.bd_sp.trt)

#species x treatment
fm.bd_spxtrt<-lm(depthGain~species*trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.bd_spxtrt)

#species & brood size
fm.bd_sp.brd<-lm(depthGain~species+broodsize, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.bd_sp.brd)

#species x brood size
fm.bd_spxbrd<-lm(depthGain~species*broodsize, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.bd_spxbrd)

#species & broodsize & treatment
fm.bd_sp.brd.trt<-lm(depthGain~species+broodsize+trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.bd_sp.brd.trt)

#species & brood size x treatment
fm.bd_sp.brdxtrt<-lm(depthGain~species+broodsize*trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.bd_sp.brdxtrt)

#species x broodsize + treatment
fm.bd_spxbrd.trt<-lm(depthGain~species*broodsize+trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.bd_spxbrd.trt)

# species x broodsize x treatment
fm.bd_spxbrdxtrt<-lm(depthGain~species*broodsize*trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.bd_spxbrdxtrt)

#AIC
depthAIC<-AIC(fm.bd_sp,
              fm.bd_brd,
              fm.bd_trt,
              fm.bd_sp.trt,
              fm.bd_spxtrt,
              fm.bd_sp.brd,
              fm.bd_spxbrd,
              fm.bd_sp.brd.trt,
              fm.bd_spxbrd.trt,
              fm.bd_spxbrdxtrt)

depthAIC$deltaAIC<-depthAIC$AIC-min(depthAIC$AIC)

depthAIC$w <- exp(-0.5*depthAIC$deltaAIC)/sum(exp(-0.5*depthAIC$deltaAIC))

depthAIC_order <- round(depthAIC[order(depthAIC$AIC),],3)
depthAIC_order
depthAIC

#prediction data
depthgain.new.dat<-data.frame(species=rep(as.factor(c("HOWA","BTBW")),each=6), 
                              trt=rep(as.factor(c("swap", "unmanipulated")), each=3), 
                              broodsize=c(2,3,4))
depthgain.new.dat
#str(depthgain.new.dat)
depthgain.pred1<-predict(fm.bd_sp, depthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
depthgain.pred2<-predict(fm.bd_brd, depthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
depthgain.pred3<-predict(fm.bd_trt, depthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
depthgain.pred4<-predict(fm.bd_sp.trt, depthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
depthgain.pred5<-predict(fm.bd_spxtrt, depthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
depthgain.pred6<-predict(fm.bd_sp.brd, depthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
depthgain.pred7<-predict(fm.bd_spxbrd, depthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
depthgain.pred8<-predict(fm.bd_sp.brd.trt, depthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
depthgain.pred9<-predict(fm.bd_spxbrd.trt, depthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")
depthgain.pred10<-predict(fm.bd_spxbrdxtrt, depthgain.new.dat, type="response", se.fit = TRUE, interval="confidence")


#combine predictions
pred.fit.bd<-cbind(depthgain.pred1$fit[,1],
                   depthgain.pred2$fit[,1],
                   depthgain.pred3$fit[,1],
                   depthgain.pred4$fit[,1],
                   depthgain.pred5$fit[,1],
                   depthgain.pred6$fit[,1],
                   depthgain.pred7$fit[,1],
                   depthgain.pred8$fit[,1],
                   depthgain.pred9$fit[,1],
                   depthgain.pred10$fit[,1])
#apply weights
depthgain.new.dat$pred.fit<-pred.fit.bd%*%depthAIC$w

#get CIs
pred.CI.lwr.bd<-cbind(depthgain.pred1$fit[,2],
                      depthgain.pred2$fit[,2],
                      depthgain.pred3$fit[,2],
                      depthgain.pred4$fit[,2],
                      depthgain.pred5$fit[,2],
                      depthgain.pred6$fit[,2],
                      depthgain.pred7$fit[,2],
                      depthgain.pred8$fit[,2],
                      depthgain.pred9$fit[,2],
                      depthgain.pred10$fit[,2])
depthgain.new.dat$pred.CI.lwr<-pred.CI.lwr.bd%*%depthAIC$w

pred.CI.upr.bd<-cbind(depthgain.pred1$fit[,3],
                      depthgain.pred2$fit[,3],
                      depthgain.pred3$fit[,3],
                      depthgain.pred4$fit[,3],
                      depthgain.pred5$fit[,3],
                      depthgain.pred6$fit[,3],
                      depthgain.pred7$fit[,3],
                      depthgain.pred8$fit[,3],
                      depthgain.pred9$fit[,3],
                      depthgain.pred10$fit[,3])
depthgain.new.dat$pred.CI.upr<-pred.CI.upr.bd%*%depthAIC$w

depthgain.new.dat



par(mar=c(4,4,1,2))
plot(pred.fit~broodsize, data=subset(depthgain.new.dat, species=="BTBW"& trt=="swap"),col=c("blue"), pch=16,type='b',ylim=c(0,.7) ,ylab="Nestling bill depth gain(g)")
points(pred.fit~broodsize, data=subset(depthgain.new.dat, species=="HOWA" & trt=="swap"),col=c("red"), pch=16,type='b')

points(pred.fit~broodsize, data=subset(depthgain.new.dat, species=="HOWA" & trt=="unmanipulated"),col=c("red"), pch=15,type='b', lty=3)
points(pred.fit~broodsize, data=subset(depthgain.new.dat, species=="BTBW"&  trt=="unmanipulated"),col=c("blue"), pch=15,type='b', lty=3)
legend(3.1,0.61, c("HOWA-swap", "HOWA-unmanipulated"), pch=c(16,15),lty=c(1,3), col=c("red","red"),cex=.8)

legend(2.1,.21, c("BTBW-swap","BTBW-umanipulated"), pch=c(16,15),lty=c(1,3), col=c("blue","blue"),cex=.8)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#### wing chord ####


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#just species
fm.wg_sp<-lm(wingGain~species, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wg_sp)

#broodsize
fm.wg_brd<-lm(wingGain~broodsize, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wg_brd)

#treatment
fm.wg_trt<-lm(wingGain~trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wg_trt)

#treatment
fm.wg_sp.trt<-lm(wingGain~species+trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wg_sp.trt)

#species x treatment
fm.wg_spxtrt<-lm(wingGain~species*trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wg_spxtrt)

#species & brood size
fm.wg_sp.brd<-lm(wingGain~species+broodsize, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wg_sp.brd)

#species x brood size
fm.wg_spxbrd<-lm(wingGain~species*broodsize, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wg_spxbrd)

#species & broodsize & treatment
fm.wg_sp.brd.trt<-lm(wingGain~species+broodsize+trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wg_sp.brd.trt)

#species &  brood size x treatment
fm.wg_sp.brdxtrt<-lm(wingGain~species+broodsize*trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wg_sp.brdxtrt)

#species x broodsize + treatment
fm.wg_spxbrd.trt<-lm(wingGain~species*broodsize+trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wg_spxbrd.trt)

# species x broodsize x treatment
fm.wg_spxbrdxtrt<-lm(wingGain~species*broodsize*trt, data=nestlingGains_no_ctrl, na.action=na.omit)
summary(fm.wg_spxbrdxtrt)

# AIC
wingAIC<-AIC(fm.wg_sp,
             fm.wg_brd,
             fm.wg_trt,
             fm.wg_sp.trt,
             fm.wg_spxtrt,
             fm.wg_sp.brd,
             fm.wg_spxbrd,
             fm.wg_sp.brd.trt,
             fm.wg_spxbrd.trt,
             fm.wg_spxbrdxtrt)

wingAIC$deltaAIC<-wingAIC$AIC-min(wingAIC$AIC)

wingAIC$w <- exp(-0.5*wingAIC$deltaAIC)/sum(exp(-0.5*wingAIC$deltaAIC))

wingAIC_order <- round(wingAIC[order(wingAIC$AIC),],3)
wingAIC_order
wingAIC

#prediction data
winggain.new.dat<-data.frame(species=rep(as.factor(c("HOWA","BTBW")),each=6), 
                             trt=rep(as.factor(c("swap", "unmanipulated")), each=3), 
                             broodsize=c(2,3,4))
winggain.new.dat
#str(winggain.new.dat)
winggain.pred1<-predict(fm.wg_sp, winggain.new.dat, type="response", se.fit = TRUE, interval="confidence")
winggain.pred2<-predict(fm.wg_brd, winggain.new.dat, type="response", se.fit = TRUE, interval="confidence")
winggain.pred3<-predict(fm.wg_trt, winggain.new.dat, type="response", se.fit = TRUE, interval="confidence")
winggain.pred4<-predict(fm.wg_sp.trt, winggain.new.dat, type="response", se.fit = TRUE, interval="confidence")
winggain.pred5<-predict(fm.wg_spxtrt, winggain.new.dat, type="response", se.fit = TRUE, interval="confidence")
winggain.pred6<-predict(fm.wg_sp.brd, winggain.new.dat, type="response", se.fit = TRUE, interval="confidence")
winggain.pred7<-predict(fm.wg_spxbrd, winggain.new.dat, type="response", se.fit = TRUE, interval="confidence")
winggain.pred8<-predict(fm.wg_sp.brd.trt, winggain.new.dat, type="response", se.fit = TRUE, interval="confidence")
winggain.pred9<-predict(fm.wg_spxbrd.trt, winggain.new.dat, type="response", se.fit = TRUE, interval="confidence")
winggain.pred10<-predict(fm.wg_spxbrdxtrt, winggain.new.dat, type="response", se.fit = TRUE, interval="confidence")


#combine predictions
pred.fit.wg<-cbind(winggain.pred1$fit[,1],
                   winggain.pred2$fit[,1],
                   winggain.pred3$fit[,1],
                   winggain.pred4$fit[,1],
                   winggain.pred5$fit[,1],
                   winggain.pred6$fit[,1],
                   winggain.pred7$fit[,1],
                   winggain.pred8$fit[,1],
                   winggain.pred9$fit[,1],
                   winggain.pred10$fit[,1])

winggain.new.dat$pred.fit<-pred.fit.wg%*%wingAIC$w
#get CIs
pred.CI.lwr.wg<-cbind(winggain.pred1$fit[,2],
                      winggain.pred2$fit[,2],
                      winggain.pred3$fit[,2],
                      winggain.pred4$fit[,2],
                      winggain.pred5$fit[,2],
                      winggain.pred6$fit[,2],
                      winggain.pred7$fit[,2],
                      winggain.pred8$fit[,2],
                      winggain.pred9$fit[,2],
                      winggain.pred10$fit[,2])
winggain.new.dat$pred.CI.lwr<-pred.CI.lwr.wg%*%wingAIC$w

pred.CI.upr.wg<-cbind(winggain.pred1$fit[,3],
                      winggain.pred2$fit[,3],
                      winggain.pred3$fit[,3],
                      winggain.pred4$fit[,3],
                      winggain.pred5$fit[,3],
                      winggain.pred6$fit[,3],
                      winggain.pred7$fit[,3],
                      winggain.pred8$fit[,3],
                      winggain.pred9$fit[,3],
                      winggain.pred10$fit[,3])
winggain.new.dat$pred.CI.upr<-pred.CI.upr.wg%*%wingAIC$w

winggain.new.dat



par(mar=c(4,4,1,2))
plot(pred.fit~broodsize, data=subset(winggain.new.dat, species=="BTBW"& trt=="swap"),col=c("blue"), pch=16,type='b',ylim=c(13,24) ,ylab="Nestling wing gain(g)")
points(pred.fit~broodsize, data=subset(winggain.new.dat, species=="HOWA" & trt=="swap"),col=c("red"), pch=16,type='b')

points(pred.fit~broodsize, data=subset(winggain.new.dat, species=="HOWA" & trt=="unmanipulated"),col=c("red"), pch=15,type='b', lty=3)
points(pred.fit~broodsize, data=subset(winggain.new.dat, species=="BTBW"&  trt=="unmanipulated"),col=c("blue"), pch=15,type='b', lty=3)
legend(3.1,22.5, c("HOWA-swap", "HOWA-unmanipulated"), pch=c(16,15),lty=c(1,3), col=c("red","red"),cex=.8)

legend(3.1,14.5, c("BTBW-swap","BTBW-umanipulated"), pch=c(16,15),lty=c(1,3), col=c("blue","blue"),cex=.8)




#### BY species 6-panels ####


#### BTBW

#### 6-panel plot ####

xtick<-seq(2, 4, by=1)
axis(side=1, at=xtick, labels = FALSE)
#mass gain
png("BTBW_NESTLINGS.png", width=8, height=10, units="in", res=800)
par(mfrow=c(3,2),mar=c(4,4,1,1),oma=c(1,1,1,1))
plot(pred.fit~broodsize, data=subset(massgain.new.dat, species=="BTBW"& trt=="swap"),xaxt='n',col=c("blue"), pch=16,type='b',xlim=c(2,4.1),ylim=c(4,6.0) ,ylab="Gain(g)", xlab="Brood size", lwd=2)
arrows(c(2,3,4), massgain.new.dat$pred.CI.lwr[massgain.new.dat$species=="BTBW"& massgain.new.dat$trt=="swap"], c(2,3,4), massgain.new.dat$pred.CI.upr[massgain.new.dat$species=="BTBW"& massgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="blue", lwd=2)


points(pred.fit~I(broodsize+.09), data=subset(massgain.new.dat, species=="BTBW"&  trt=="unmanipulated"),col=c("blue"), pch=15,type='b', lty=3, lwd=2)
arrows(c(2.09,3.09,4.09), massgain.new.dat$pred.CI.lwr[massgain.new.dat$species=="BTBW"& massgain.new.dat$trt=="unmanipulated"], c(2.09,3.09,4.09), massgain.new.dat$pred.CI.upr[massgain.new.dat$species=="BTBW"& massgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="blue", lty=2, lwd=2)

# add means
#range(nestlingGains_no_ctrl$massGain, na.rm=T)
points(massGain~I(broodsize), data=subset(nestlingGains_no_ctrl, species=="BTBW"&  trt=="swap"),col=c("black"), pch=16)#,type='b', lty=3, lwd=2)
points(massGain~I(broodsize+.09), data=subset(nestlingGains_no_ctrl, species=="BTBW"&  trt=="unmanipulated"),col=c("black"), pch=15)#,type='b', lty=3, lwd=2)


text(2.5,6, "A) Mass", font=2, cex=1.2)
legend(2.1,4.3, c("BTBW-translocated","BTBW-umanipulated"), pch=c(16,15),lty=c(1,3), col=c("blue","blue"),cex=.85)
axis(side=1, at=xtick, labels = TRUE)

#tarsus gain


plot(pred.fit~I(broodsize+.02), data=subset(tarsgain.new.dat, species=="BTBW"& trt=="swap"),xaxt='n',col=c("blue"), pch=16,type='b',xlim=c(2,4.1),ylim=c(4.2,10.1) ,ylab="Gain(mm)", xlab="Brood size", lwd=2)
arrows(c(2.02,3.02,4.02), tarsgain.new.dat$pred.CI.lwr[tarsgain.new.dat$species=="BTBW"& tarsgain.new.dat$trt=="swap"], c(2.02,3.02,4.02), tarsgain.new.dat$pred.CI.upr[tarsgain.new.dat$species=="BTBW"& tarsgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="blue", lwd=2)


points(pred.fit~I(broodsize+.09), data=subset(tarsgain.new.dat, species=="BTBW"&  trt=="unmanipulated"),col=c("blue"), pch=15,type='b', lty=3, lwd=2)
arrows(c(2.09,3.09,4.09), tarsgain.new.dat$pred.CI.lwr[tarsgain.new.dat$species=="BTBW"& tarsgain.new.dat$trt=="unmanipulated"], c(2.09,3.09,4.09), tarsgain.new.dat$pred.CI.upr[tarsgain.new.dat$species=="BTBW"& tarsgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="blue", lty=2, lwd=2)

#range(nestlingGains_no_ctrl$tarsGain, na.rm=T)
points(tarsGain~I(broodsize), data=subset(nestlingGains_no_ctrl, species=="BTBW"&  trt=="swap"),col=c("black"), pch=16)#,type='b', lty=3, lwd=2)
points(tarsGain~I(broodsize+.09), data=subset(nestlingGains_no_ctrl, species=="BTBW"&  trt=="unmanipulated"),col=c("black"), pch=15)#,type='b', lty=3, lwd=2)


text(2.5,10, "B) Tarsus", font=2, cex=1.2)
legend(2.15,5.1, c("BTBW-translocated","BTBW-umanipulated"), pch=c(16,15),lty=c(1,3), col=c("blue","blue"),cex=.85)
axis(side=1, at=xtick, labels = TRUE)

#wing

plot(pred.fit~I(broodsize+.02), data=subset(winggain.new.dat, species=="BTBW"& trt=="swap"),xaxt='n',col=c("blue"), pch=16,type='b',xlim=c(2,4.1),ylim=c(10,20) ,ylab="Gain(mm)", xlab="Brood size", lwd=2)
arrows(c(2.02,3.02,4.02), winggain.new.dat$pred.CI.lwr[winggain.new.dat$species=="BTBW"& winggain.new.dat$trt=="swap"], c(2.02,3.02,4.02), winggain.new.dat$pred.CI.upr[winggain.new.dat$species=="BTBW"& winggain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="blue", lwd=2)

points(pred.fit~I(broodsize+.09), data=subset(winggain.new.dat, species=="BTBW"&  trt=="unmanipulated"),col=c("blue"), pch=15,type='b', lty=3, lwd=2)
arrows(c(2.09,3.09,4.09), winggain.new.dat$pred.CI.lwr[winggain.new.dat$species=="BTBW"& winggain.new.dat$trt=="unmanipulated"], c(2.09,3.09,4.09), winggain.new.dat$pred.CI.upr[winggain.new.dat$species=="BTBW"& winggain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="blue", lty=2, lwd=2)


range(nestlingGains_no_ctrl$wingGain, na.rm=T)
points(wingGain~I(broodsize), data=subset(nestlingGains_no_ctrl, species=="BTBW"&  trt=="swap"),col=c("black"), pch=16)#,type='b', lty=3, lwd=2)
points(wingGain~I(broodsize+.09), data=subset(nestlingGains_no_ctrl, species=="BTBW"&  trt=="unmanipulated"),col=c("black"), pch=15)#,type='b', lty=3, lwd=2)


text(2.5,20, "C) Wing", font=2, cex=1.2)
legend(3.2,11.5, c("BTBW-translocated","BTBW-umanipulated"), pch=c(16,15),lty=c(1,3), col=c("blue","blue"),cex=.85)
axis(side=1, at=xtick, labels = TRUE)


#nares

plot(pred.fit~I(broodsize+.02), data=subset(naresgain.new.dat, species=="BTBW"& trt=="swap"),xaxt='n',col=c("blue"), pch=16,type='b',xlim=c(2,4.1),ylim=c(0.29,1.5) ,ylab="Gain(mm)", xlab="Brood size", lwd=2)
arrows(c(2.02,3.02,4.02), naresgain.new.dat$pred.CI.lwr[naresgain.new.dat$species=="BTBW"& naresgain.new.dat$trt=="swap"], c(2.02,3.02,4.02), naresgain.new.dat$pred.CI.upr[naresgain.new.dat$species=="BTBW"& naresgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="blue", lwd=2)

points(pred.fit~I(broodsize+.09), data=subset(naresgain.new.dat, species=="BTBW"&  trt=="unmanipulated"),col=c("blue"), pch=15,type='b', lty=3, lwd=2)
arrows(c(2.09,3.09,4.09), naresgain.new.dat$pred.CI.lwr[naresgain.new.dat$species=="BTBW"& naresgain.new.dat$trt=="unmanipulated"], c(2.09,3.09,4.09), naresgain.new.dat$pred.CI.upr[naresgain.new.dat$species=="BTBW"& naresgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="blue", lty=2, lwd=2)

range(nestlingGains_no_ctrl$naresGain, na.rm=T)
points(naresGain~I(broodsize), data=subset(nestlingGains_no_ctrl, species=="BTBW"&  trt=="swap"),col=c("black"), pch=16)#,type='b', lty=3, lwd=2)
points(naresGain~I(broodsize+.09), data=subset(nestlingGains_no_ctrl, species=="BTBW"&  trt=="unmanipulated"),col=c("black"), pch=15)#,type='b', lty=3, lwd=2)


text(2.5,1.3, "D) Nares", font=2, cex=1.2)
legend(2,0.43, c("BTBW-translocated","BTBW-umanipulated"), pch=c(16,15),lty=c(1,3), col=c("blue","blue"),cex=.85)
axis(side=1, at=xtick, labels = TRUE)


#bill width


plot(pred.fit~I(broodsize+.02), data=subset(widthgain.new.dat, species=="BTBW"& trt=="swap"),xaxt='n',col=c("blue"), pch=16,type='b',xlim=c(2,4.1),ylim=c(-0.15,1.1) ,ylab="Gain(mm)", xlab="Brood size", lwd=2)
arrows(c(2.02,3.02,4.02), widthgain.new.dat$pred.CI.lwr[widthgain.new.dat$species=="BTBW"& widthgain.new.dat$trt=="swap"], c(2.02,3.02,4.02), widthgain.new.dat$pred.CI.upr[widthgain.new.dat$species=="BTBW"& widthgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="blue", lwd=2)

points(pred.fit~I(broodsize+.09), data=subset(widthgain.new.dat, species=="BTBW"&  trt=="unmanipulated"),col=c("blue"), pch=15,type='b', lty=3, lwd=2)
arrows(c(2.09,3.09,4.09), widthgain.new.dat$pred.CI.lwr[widthgain.new.dat$species=="BTBW"& widthgain.new.dat$trt=="unmanipulated"], c(2.09,3.09,4.09), widthgain.new.dat$pred.CI.upr[widthgain.new.dat$species=="BTBW"& widthgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="blue", lty=2, lwd=2)

range(nestlingGains_no_ctrl$widthGain, na.rm=T)
points(widthGain~I(broodsize), data=subset(nestlingGains_no_ctrl, species=="BTBW"&  trt=="swap"),col=c("black"), pch=16)#,type='b', lty=3, lwd=2)
points(widthGain~I(broodsize+.09), data=subset(nestlingGains_no_ctrl, species=="BTBW"&  trt=="unmanipulated"),col=c("black"), pch=15)#,type='b', lty=3, lwd=2)


text(2.5,1.1, "E) Bill Width", font=2, cex=1.2)
legend(2.0,0.0, c("BTBW-translocated","BTBW-umanipulated"), pch=c(16,15),lty=c(1,3), col=c("blue","blue"),cex=.85)
axis(side=1, at=xtick, labels = TRUE)

#Depth

plot(pred.fit~I(broodsize+.02), data=subset(depthgain.new.dat, species=="BTBW"& trt=="swap"),xaxt='n',col=c("blue"), lwd=2, pch=16,type='b',xlim=c(2,4.1),ylim=c(-0.1,0.6), ylab="Gain(mm)", xlab="Brood size")
arrows(c(2.02,3.02,4.02), depthgain.new.dat$pred.CI.lwr[depthgain.new.dat$species=="BTBW"& depthgain.new.dat$trt=="swap"], c(2.02,3.02,4.02), depthgain.new.dat$pred.CI.upr[depthgain.new.dat$species=="BTBW"& depthgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="blue", lwd=2)

points(pred.fit~I(broodsize+.09), data=subset(depthgain.new.dat, species=="BTBW"&  trt=="unmanipulated"),col=c("blue"), pch=15,type='b', lty=3, lwd=2)
arrows(c(2.09,3.09,4.09), depthgain.new.dat$pred.CI.lwr[depthgain.new.dat$species=="BTBW"& depthgain.new.dat$trt=="unmanipulated"], c(2.09,3.09,4.09), depthgain.new.dat$pred.CI.upr[depthgain.new.dat$species=="BTBW"& depthgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="blue", lty=2, lwd=2)
text(2.5,0.6, "F) Bill Depth", font=2, cex=1.2)

#range(nestlingGains_no_ctrl$depthGain, na.rm=T)
points(depthGain~I(broodsize), data=subset(nestlingGains_no_ctrl, species=="BTBW"&  trt=="swap"),col=c("black"), pch=16)#,type='b', lty=3, lwd=2)
points(depthGain~I(broodsize+.09), data=subset(nestlingGains_no_ctrl, species=="BTBW"&  trt=="unmanipulated"),col=c("black"), pch=15)#,type='b', lty=3, lwd=2)


legend(2,-.02, c("BTBW-translocated","BTBW-umanipulated"), pch=c(16,15),lty=c(1,3), col=c("blue","blue"),cex=.85)
axis(side=1, at=xtick, labels = TRUE)

dev.off()
system("open BTBW_NESTLINGS.png")


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
#





#### HOWA

#### 6-panel plot ####
png("HOWA_NESTLINGS.png", width=8, height=10, units="in", res=800)
par(mfrow=c(3,2),mar=c(4,4,1,1),oma=c(1,1,1,1))
plot(pred.fit~broodsize, data=subset(massgain.new.dat, species=="HOWA"& trt=="swap"),xaxt='n',col=c("red"), pch=16,type='b',xlim=c(2,4.1),ylim=c(4,6.4) ,ylab="Gain(g)", xlab="Brood size", lwd=2)
arrows(c(2,3,4), massgain.new.dat$pred.CI.lwr[massgain.new.dat$species=="HOWA"& massgain.new.dat$trt=="swap"], c(2,3,4), massgain.new.dat$pred.CI.upr[massgain.new.dat$species=="HOWA"& massgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="red", lwd=2)


points(pred.fit~I(broodsize+.09), data=subset(massgain.new.dat, species=="HOWA"&  trt=="unmanipulated"),col=c("red"), pch=15,type='b', lty=3, lwd=2)
arrows(c(2.09,3.09,4.09), massgain.new.dat$pred.CI.lwr[massgain.new.dat$species=="HOWA"& massgain.new.dat$trt=="unmanipulated"], c(2.09,3.09,4.09), massgain.new.dat$pred.CI.upr[massgain.new.dat$species=="HOWA"& massgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="red", lty=2, lwd=2)

# add means
range(nestlingGains_no_ctrl$massGain, na.rm=T)
points(massGain~I(broodsize-.05), data=subset(nestlingGains_no_ctrl, species=="HOWA"&  trt=="swap"),col=c("black"), pch=16)#,type='b', lty=3, lwd=2)
points(massGain~I(broodsize+.05), data=subset(nestlingGains_no_ctrl, species=="HOWA"&  trt=="unmanipulated"),col=c("black"), pch=15)#,type='b', lty=3, lwd=2)


text(2.5,6.4, "A) Mass", font=2, cex=1.2)
legend(2.1,4.3, c("HOWA-translocated","HOWA-umanipulated"), pch=c(16,15),lty=c(1,3), col=c("red","red"),cex=.85)
axis(side=1, at=xtick, labels = TRUE)

#tarsus gain


plot(pred.fit~I(broodsize+.02), data=subset(tarsgain.new.dat, species=="HOWA"& trt=="swap"),xaxt='n',col=c("red"), pch=16,type='b',xlim=c(2,4.1),ylim=c(5.1,13.1) ,ylab="Gain(mm)", xlab="Brood size", lwd=2)
arrows(c(2.02,3.02,4.02), tarsgain.new.dat$pred.CI.lwr[tarsgain.new.dat$species=="HOWA"& tarsgain.new.dat$trt=="swap"], c(2.02,3.02,4.02), tarsgain.new.dat$pred.CI.upr[tarsgain.new.dat$species=="HOWA"& tarsgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="red", lwd=2)


points(pred.fit~I(broodsize+.09), data=subset(tarsgain.new.dat, species=="HOWA"&  trt=="unmanipulated"),col=c("red"), pch=15,type='b', lty=3, lwd=2)
arrows(c(2.09,3.09,4.09), tarsgain.new.dat$pred.CI.lwr[tarsgain.new.dat$species=="HOWA"& tarsgain.new.dat$trt=="unmanipulated"], c(2.09,3.09,4.09), tarsgain.new.dat$pred.CI.upr[tarsgain.new.dat$species=="HOWA"& tarsgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="red", lty=2, lwd=2)

#range(nestlingGains_no_ctrl$tarsGain, na.rm=T)
points(tarsGain~I(broodsize-.05), data=subset(nestlingGains_no_ctrl, species=="HOWA"&  trt=="swap"),col=c("black"), pch=16)#,type='b', lty=3, lwd=2)
points(tarsGain~I(broodsize+.05), data=subset(nestlingGains_no_ctrl, species=="HOWA"&  trt=="unmanipulated"),col=c("black"), pch=15)#,type='b', lty=3, lwd=2)


text(2.5,13, "B) Tarsus", font=2, cex=1.2)
legend(2.15,6.1, c("HOWA-translocated","HOWA-umanipulated"), pch=c(16,15),lty=c(1,3), col=c("red","red"),cex=.85)
axis(side=1, at=xtick, labels = TRUE)


#wing
plot(pred.fit~I(broodsize+.02), data=subset(winggain.new.dat, species=="HOWA"& trt=="swap"),xaxt='n',col=c("red"), pch=16,type='b',xlim=c(2,4.1),ylim=c(10,25) ,ylab="Gain(mm)", xlab="Brood size", lwd=2)
arrows(c(2.02,3.02,4.02), winggain.new.dat$pred.CI.lwr[winggain.new.dat$species=="HOWA"& winggain.new.dat$trt=="swap"], c(2.02,3.02,4.02), winggain.new.dat$pred.CI.upr[winggain.new.dat$species=="HOWA"& winggain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="red", lwd=2)

points(pred.fit~I(broodsize+.09), data=subset(winggain.new.dat, species=="HOWA"&  trt=="unmanipulated"),col=c("red"), pch=15,type='b', lty=3, lwd=2)
arrows(c(2.09,3.09,4.09), winggain.new.dat$pred.CI.lwr[winggain.new.dat$species=="HOWA"& winggain.new.dat$trt=="unmanipulated"], c(2.09,3.09,4.09), winggain.new.dat$pred.CI.upr[winggain.new.dat$species=="HOWA"& winggain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="red", lty=2, lwd=2)


range(nestlingGains_no_ctrl$wingGain, na.rm=T)
points(wingGain~I(broodsize-.05), data=subset(nestlingGains_no_ctrl, species=="HOWA"&  trt=="swap"),col=c("black"), pch=16)#,type='b', lty=3, lwd=2)
points(wingGain~I(broodsize+.05), data=subset(nestlingGains_no_ctrl, species=="HOWA"&  trt=="unmanipulated"),col=c("black"), pch=15)#,type='b', lty=3, lwd=2)


text(2.5,25, "C) Wing", font=2, cex=1.2)
legend(3.2,11.8, c("HOWA-translocated","HOWA-umanipulated"), pch=c(16,15),lty=c(1,3), col=c("red","red"),cex=.85)
axis(side=1, at=xtick, labels = TRUE)


#nares

plot(pred.fit~I(broodsize+.02), data=subset(naresgain.new.dat, species=="HOWA"& trt=="swap"),xaxt='n',col=c("red"), pch=16,type='b',xlim=c(2,4.1),ylim=c(0.9,1.81) ,ylab="Gain(mm)", xlab="Brood size", lwd=2)
arrows(c(2.02,3.02,4.02), naresgain.new.dat$pred.CI.lwr[naresgain.new.dat$species=="HOWA"& naresgain.new.dat$trt=="swap"], c(2.02,3.02,4.02), naresgain.new.dat$pred.CI.upr[naresgain.new.dat$species=="HOWA"& naresgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="red", lwd=2)

points(pred.fit~I(broodsize+.09), data=subset(naresgain.new.dat, species=="HOWA"&  trt=="unmanipulated"),col=c("red"), pch=15,type='b', lty=3, lwd=2)
arrows(c(2.09,3.09,4.09), naresgain.new.dat$pred.CI.lwr[naresgain.new.dat$species=="HOWA"& naresgain.new.dat$trt=="unmanipulated"], c(2.09,3.09,4.09), naresgain.new.dat$pred.CI.upr[naresgain.new.dat$species=="HOWA"& naresgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="red", lty=2, lwd=2)

range(nestlingGains_no_ctrl$naresGain, na.rm=T)

points(naresGain~I(broodsize-.05), data=subset(nestlingGains_no_ctrl, species=="HOWA"&  trt=="swap"),col=c("black"), pch=16)#,type='b', lty=3, lwd=2)
points(naresGain~I(broodsize+.05), data=subset(nestlingGains_no_ctrl, species=="HOWA"&  trt=="unmanipulated"),col=c("black"), pch=15)#,type='b', lty=3, lwd=2)


text(2.5,1.81, "D) Nares", font=2, cex=1.2)
legend(3.15,1.8, c("HOWA-translocated","HOWA-umanipulated"), pch=c(16,15),lty=c(1,3), col=c("red","red"),cex=.85)
axis(side=1, at=xtick, labels = TRUE)


#bill width


plot(pred.fit~I(broodsize+.02), data=subset(widthgain.new.dat, species=="HOWA"& trt=="swap"),xaxt='n',col=c("red"), pch=16,type='b',xlim=c(2,4.1),ylim=c(-.1,1.1) ,ylab="Gain(mm)", xlab="Brood size", lwd=2)
arrows(c(2.02,3.02,4.02), widthgain.new.dat$pred.CI.lwr[widthgain.new.dat$species=="HOWA"& widthgain.new.dat$trt=="swap"], c(2.02,3.02,4.02), widthgain.new.dat$pred.CI.upr[widthgain.new.dat$species=="HOWA"& widthgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="red", lwd=2)

points(pred.fit~I(broodsize+.09), data=subset(widthgain.new.dat, species=="HOWA"&  trt=="unmanipulated"),col=c("red"), pch=15,type='b', lty=3, lwd=2)
arrows(c(2.09,3.09,4.09), widthgain.new.dat$pred.CI.lwr[widthgain.new.dat$species=="HOWA"& widthgain.new.dat$trt=="unmanipulated"], c(2.09,3.09,4.09), widthgain.new.dat$pred.CI.upr[widthgain.new.dat$species=="HOWA"& widthgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="red", lty=2, lwd=2)

range(nestlingGains_no_ctrl$widthGain, na.rm=T)
points(widthGain~I(broodsize), data=subset(nestlingGains_no_ctrl, species=="HOWA"&  trt=="swap"),col=c("black"), pch=16)#,type='b', lty=3, lwd=2)
points(widthGain~I(broodsize+.09), data=subset(nestlingGains_no_ctrl, species=="HOWA"&  trt=="unmanipulated"),col=c("black"), pch=15)#,type='b', lty=3, lwd=2)


text(2.5,1.1, "E) Bill Width", font=2, cex=1.2)
legend(2.0,0.05, c("HOWA-translocated","HOWA-umanipulated"), pch=c(16,15),lty=c(1,3), col=c("red","red"),cex=.85)
axis(side=1, at=xtick, labels = TRUE)

#Depth

plot(pred.fit~I(broodsize+.02), data=subset(depthgain.new.dat, species=="HOWA"& trt=="swap"),xaxt='n',col=c("red"), lwd=2, pch=16,type='b',xlim=c(2,4.1),ylim=c(-0.1,.81), ylab="Gain(mm)", xlab="Brood size")
arrows(c(2.02,3.02,4.02), depthgain.new.dat$pred.CI.lwr[depthgain.new.dat$species=="HOWA"& depthgain.new.dat$trt=="swap"], c(2.02,3.02,4.02), depthgain.new.dat$pred.CI.upr[depthgain.new.dat$species=="HOWA"& depthgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="red", lwd=2)

points(pred.fit~I(broodsize+.09), data=subset(depthgain.new.dat, species=="HOWA"&  trt=="unmanipulated"),col=c("red"), pch=15,type='b', lty=3, lwd=2)
arrows(c(2.09,3.09,4.09), depthgain.new.dat$pred.CI.lwr[depthgain.new.dat$species=="HOWA"& depthgain.new.dat$trt=="unmanipulated"], c(2.09,3.09,4.09), depthgain.new.dat$pred.CI.upr[depthgain.new.dat$species=="HOWA"& depthgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="red", lty=2, lwd=2)


range(nestlingGains_no_ctrl$depthGain, na.rm=T)
points(depthGain~I(broodsize), data=subset(nestlingGains_no_ctrl, species=="HOWA"&  trt=="swap"),col=c("black"), pch=16)#,type='b', lty=3, lwd=2)
points(depthGain~I(broodsize+.09), data=subset(nestlingGains_no_ctrl, species=="HOWA"&  trt=="unmanipulated"),col=c("black"), pch=15)#,type='b', lty=3, lwd=2)

text(2.5,0.81, "F) Bill Depth", font=2, cex=1.2)
legend(2,0.02, c("HOWA-translocated","HOWA-umanipulated"), pch=c(16,15),lty=c(1,3), col=c("red","red"),cex=.85)
axis(side=1, at=xtick, labels = TRUE)

dev.off()
system("open HOWA_NESTLINGS.png")







# uncomment to see nesting comparisions in the same plots.

# 
# #### 6-panel plot ####
# 
# xtick<-seq(2, 4, by=1)
# axis(side=1, at=xtick, labels = FALSE)
# #mass gain
# png("NESTLINGS.png", width=8, height=10, units="in", res=800)
# par(mfrow=c(3,2),mar=c(4,4,1,1),oma=c(1,1,1,1))
# plot(pred.fit~broodsize, data=subset(massgain.new.dat, species=="BTBW"& trt=="swap"),xaxt='n',col=c("blue"), pch=16,type='b',xlim=c(2,4.1),ylim=c(4.3,5.9) ,ylab="Gain(g)", xlab="Brood size", lwd=2)
# points(pred.fit~I(broodsize+.07), data=subset(massgain.new.dat, species=="HOWA" & trt=="swap"),col=c("red"), pch=16,type='b', lwd=2)
# arrows(c(2,3,4), massgain.new.dat$pred.CI.lwr[massgain.new.dat$species=="BTBW"& massgain.new.dat$trt=="swap"], c(2,3,4), massgain.new.dat$pred.CI.upr[massgain.new.dat$species=="BTBW"& massgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="blue", lwd=2)
# arrows(c(2.07,3.07,4.07), massgain.new.dat$pred.CI.lwr[massgain.new.dat$species=="HOWA"& massgain.new.dat$trt=="swap"], c(2.07,3.07,4.07), massgain.new.dat$pred.CI.upr[massgain.new.dat$species=="HOWA"& massgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="red", lwd=2)
# 
# points(pred.fit~I(broodsize), data=subset(massgain.new.dat, species=="HOWA" & trt=="unmanipulated"),col=c("red"), pch=15,type='b', lty=3, lwd=2)
# points(pred.fit~I(broodsize+.09), data=subset(massgain.new.dat, species=="BTBW"&  trt=="unmanipulated"),col=c("blue"), pch=15,type='b', lty=3, lwd=2)
# arrows(c(2.09,3.09,4.09), massgain.new.dat$pred.CI.lwr[massgain.new.dat$species=="BTBW"& massgain.new.dat$trt=="unmanipulated"], c(2.09,3.09,4.09), massgain.new.dat$pred.CI.upr[massgain.new.dat$species=="BTBW"& massgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="blue", lty=2, lwd=2)
# arrows(c(2,3,4), massgain.new.dat$pred.CI.lwr[massgain.new.dat$species=="HOWA"& massgain.new.dat$trt=="unmanipulated"], c(2,3,4), massgain.new.dat$pred.CI.upr[massgain.new.dat$species=="HOWA"& massgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="red", lty=2, lwd=2)
# 
# text(3,5.8, "A) Mass", font=2, cex=1.2)
# legend(2.15,4.62, c("HOWA-translocated", "HOWA-unmanipulated","BTBW-translocated","BTBW-umanipulated"), pch=c(16,15,16,15),lty=c(1,3,1,3), col=c("red","red","blue","blue"),cex=.85)
# axis(side=1, at=xtick, labels = TRUE)
# #text(3.8, 5.82, "A", font=2, cex=1.5)
# 
# #tarsus gain
# 
# #par(oma=c(1,1,1,1))
# plot(pred.fit~I(broodsize+.02), data=subset(tarsgain.new.dat, species=="BTBW"& trt=="swap"),xaxt='n',col=c("blue"), pch=16,type='b',xlim=c(2,4.1),ylim=c(4.7,13.1) ,ylab="Gain(mm)", xlab="Brood size", lwd=2)
# points(pred.fit~I(broodsize+.07), data=subset(tarsgain.new.dat, species=="HOWA" & trt=="swap"),col=c("red"), pch=16,type='b', lwd=2)
# arrows(c(2.02,3.02,4.02), tarsgain.new.dat$pred.CI.lwr[tarsgain.new.dat$species=="BTBW"& tarsgain.new.dat$trt=="swap"], c(2.02,3.02,4.02), tarsgain.new.dat$pred.CI.upr[tarsgain.new.dat$species=="BTBW"& tarsgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="blue", lwd=2)
# arrows(c(2.07,3.07,4.07), tarsgain.new.dat$pred.CI.lwr[tarsgain.new.dat$species=="HOWA"& tarsgain.new.dat$trt=="swap"], c(2.07,3.07,4.07), tarsgain.new.dat$pred.CI.upr[tarsgain.new.dat$species=="HOWA"& tarsgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="red", lwd=2)
# 
# 
# points(pred.fit~broodsize, data=subset(tarsgain.new.dat, species=="HOWA" & trt=="unmanipulated"),col=c("red"), pch=15,type='b', lty=3, lwd=2)
# points(pred.fit~I(broodsize+.09), data=subset(tarsgain.new.dat, species=="BTBW"&  trt=="unmanipulated"),col=c("blue"), pch=15,type='b', lty=3, lwd=2)
# arrows(c(2.09,3.09,4.09), tarsgain.new.dat$pred.CI.lwr[tarsgain.new.dat$species=="BTBW"& tarsgain.new.dat$trt=="unmanipulated"], c(2.09,3.09,4.09), tarsgain.new.dat$pred.CI.upr[tarsgain.new.dat$species=="BTBW"& tarsgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="blue", lty=2, lwd=2)
# arrows(c(2,3,4), tarsgain.new.dat$pred.CI.lwr[tarsgain.new.dat$species=="HOWA"& tarsgain.new.dat$trt=="unmanipulated"], c(2,3,4), tarsgain.new.dat$pred.CI.upr[tarsgain.new.dat$species=="HOWA"& tarsgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="red", lty=2, lwd=2)
# 
# text(3,12.5, "B) Tarsus", font=2, cex=1.2)
# legend(2.15,6.37, c("HOWA-translocated", "HOWA-unmanipulated","BTBW-translocated","BTBW-umanipulated"), pch=c(16,15,16,15),lty=c(1,3,1,3), col=c("red","red","blue","blue"),cex=.85)
# axis(side=1, at=xtick, labels = TRUE)
# #text(3.8, 12.8, "B", font=2, cex=1.5)
# 
# #box(which="plot",lty = "solid", col="black")
# 
# #wing
# #par(oma=c(1,1,1,1))
# plot(pred.fit~I(broodsize+.02), data=subset(winggain.new.dat, species=="BTBW"& trt=="swap"),xaxt='n',col=c("blue"), pch=16,type='b',xlim=c(2,4.1),ylim=c(10,26) ,ylab="Gain(mm)", xlab="Brood size", lwd=2)
# points(pred.fit~I(broodsize+.07), data=subset(winggain.new.dat, species=="HOWA" & trt=="swap"),col=c("red"), pch=16,type='b', lwd=2)
# arrows(c(2.02,3.02,4.02), winggain.new.dat$pred.CI.lwr[winggain.new.dat$species=="BTBW"& winggain.new.dat$trt=="swap"], c(2.02,3.02,4.02), winggain.new.dat$pred.CI.upr[winggain.new.dat$species=="BTBW"& winggain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="blue", lwd=2)
# arrows(c(2.07,3.07,4.07), winggain.new.dat$pred.CI.lwr[winggain.new.dat$species=="HOWA"& winggain.new.dat$trt=="swap"], c(2.07,3.07,4.07), winggain.new.dat$pred.CI.upr[winggain.new.dat$species=="HOWA"& winggain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="red", lwd=2)
# 
# points(pred.fit~broodsize, data=subset(winggain.new.dat, species=="HOWA" & trt=="unmanipulated"),col=c("red"), pch=15,type='b', lty=3, lwd=2)
# points(pred.fit~I(broodsize+.09), data=subset(winggain.new.dat, species=="BTBW"&  trt=="unmanipulated"),col=c("blue"), pch=15,type='b', lty=3, lwd=2)
# arrows(c(2.09,3.09,4.09), winggain.new.dat$pred.CI.lwr[winggain.new.dat$species=="BTBW"& winggain.new.dat$trt=="unmanipulated"], c(2.09,3.09,4.09), winggain.new.dat$pred.CI.upr[winggain.new.dat$species=="BTBW"& winggain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="blue", lty=2, lwd=2)
# arrows(c(2,3,4), winggain.new.dat$pred.CI.lwr[winggain.new.dat$species=="HOWA"& winggain.new.dat$trt=="unmanipulated"], c(2,3,4), winggain.new.dat$pred.CI.upr[winggain.new.dat$species=="HOWA"& winggain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="red", lty=2, lwd=2)
# #box()
# 
# text(3,25, "C) Wing", font=2, cex=1.2)
# legend(3.15,13.5, c("HOWA-translocated", "HOWA-unmanipulated","BTBW-translocated","BTBW-umanipulated"), pch=c(16,15,16,15),lty=c(1,3,1,3), col=c("red","red","blue","blue"),cex=.85)
# axis(side=1, at=xtick, labels = TRUE)
# #text(3.8, 25.5, "C", font=2, cex=1.5)
# #box(which="plot",lty = "solid", col="black")
# 
# 
# #bill measurement plot
# 
# #nares
# #par(oma=c(1,1,1,1))
# plot(pred.fit~I(broodsize+.02), data=subset(naresgain.new.dat, species=="BTBW"& trt=="swap"),xaxt='n',col=c("blue"), pch=16,type='b',xlim=c(2,4.1),ylim=c(0.5,1.5) ,ylab="Gain(mm)", xlab="Brood size", lwd=2)
# points(pred.fit~I(broodsize+.07), data=subset(naresgain.new.dat, species=="HOWA" & trt=="swap"),col=c("red"), pch=16,type='b', lwd=2)
# arrows(c(2.02,3.02,4.02), naresgain.new.dat$pred.CI.lwr[naresgain.new.dat$species=="BTBW"& naresgain.new.dat$trt=="swap"], c(2.02,3.02,4.02), naresgain.new.dat$pred.CI.upr[naresgain.new.dat$species=="BTBW"& naresgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="blue", lwd=2)
# arrows(c(2.07,3.07,4.07), naresgain.new.dat$pred.CI.lwr[naresgain.new.dat$species=="HOWA"& naresgain.new.dat$trt=="swap"], c(2.07,3.07,4.07), naresgain.new.dat$pred.CI.upr[naresgain.new.dat$species=="HOWA"& naresgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="red", lwd=2)
# 
# points(pred.fit~broodsize, data=subset(naresgain.new.dat, species=="HOWA" & trt=="unmanipulated"),col=c("red"), pch=15,type='b', lty=3, lwd=2)
# points(pred.fit~I(broodsize+.09), data=subset(naresgain.new.dat, species=="BTBW"&  trt=="unmanipulated"),col=c("blue"), pch=15,type='b', lty=3, lwd=2)
# arrows(c(2.09,3.09,4.09), naresgain.new.dat$pred.CI.lwr[naresgain.new.dat$species=="BTBW"& naresgain.new.dat$trt=="unmanipulated"], c(2.09,3.09,4.09), naresgain.new.dat$pred.CI.upr[naresgain.new.dat$species=="BTBW"& naresgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="blue", lty=2, lwd=2)
# arrows(c(2,3,4), naresgain.new.dat$pred.CI.lwr[naresgain.new.dat$species=="HOWA"& naresgain.new.dat$trt=="unmanipulated"], c(2,3,4), naresgain.new.dat$pred.CI.upr[naresgain.new.dat$species=="HOWA"& naresgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="red", lty=2, lwd=2)
# 
# text(3,1.45, "D) Nares", font=2, cex=1.2)
# legend(3.08,0.72, c("HOWA-translocated", "HOWA-unmanipulated","BTBW-translocated","BTBW-umanipulated"), pch=c(16,15,16,15),lty=c(1,3,1,3), col=c("red","red","blue","blue"),cex=.85)
# axis(side=1, at=xtick, labels = TRUE)
# #text(3.8, 0.57, "A", font=2, cex=1.5)
# #box(which="plot",lty = "solid", col="black")
# 
# #bill width
# 
# #par(oma=c(1,1,1,1))
# plot(pred.fit~I(broodsize+.02), data=subset(widthgain.new.dat, species=="BTBW"& trt=="swap"),xaxt='n',col=c("blue"), pch=16,type='b',xlim=c(2,4.1),ylim=c(0,1.1) ,ylab="Gain(mm)", xlab="Brood size", lwd=2)
# points(pred.fit~I(broodsize+.07), data=subset(widthgain.new.dat, species=="HOWA" & trt=="swap"),col=c("red"), pch=16,type='b', lwd=2)
# arrows(c(2.02,3.02,4.02), widthgain.new.dat$pred.CI.lwr[widthgain.new.dat$species=="BTBW"& widthgain.new.dat$trt=="swap"], c(2.02,3.02,4.02), widthgain.new.dat$pred.CI.upr[widthgain.new.dat$species=="BTBW"& widthgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="blue", lwd=2)
# arrows(c(2.07,3.07,4.07), widthgain.new.dat$pred.CI.lwr[widthgain.new.dat$species=="HOWA"& widthgain.new.dat$trt=="swap"], c(2.07,3.07,4.07), widthgain.new.dat$pred.CI.upr[widthgain.new.dat$species=="HOWA"& widthgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="red", lwd=2)
# 
# points(pred.fit~broodsize, data=subset(widthgain.new.dat, species=="HOWA" & trt=="unmanipulated"),col=c("red"), pch=15,type='b', lty=3, lwd=2)
# points(pred.fit~I(broodsize+.09), data=subset(widthgain.new.dat, species=="BTBW"&  trt=="unmanipulated"),col=c("blue"), pch=15,type='b', lty=3, lwd=2)
# arrows(c(2.09,3.09,4.09), widthgain.new.dat$pred.CI.lwr[widthgain.new.dat$species=="BTBW"& widthgain.new.dat$trt=="unmanipulated"], c(2.09,3.09,4.09), widthgain.new.dat$pred.CI.upr[widthgain.new.dat$species=="BTBW"& widthgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="blue", lty=2, lwd=2)
# arrows(c(2,3,4), widthgain.new.dat$pred.CI.lwr[widthgain.new.dat$species=="HOWA"& widthgain.new.dat$trt=="unmanipulated"], c(2,3,4), widthgain.new.dat$pred.CI.upr[widthgain.new.dat$species=="HOWA"& widthgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="red", lty=2, lwd=2)
# 
# text(3,1.04, "E) Bill Width", font=2, cex=1.2)
# legend(2.0,0.25, c("HOWA-translocated", "HOWA-unmanipulated","BTBW-translocated","BTBW-umanipulated"), pch=c(16,15,16,15),lty=c(1,3,1,3), col=c("red","red","blue","blue"),cex=.85)
# axis(side=1, at=xtick, labels = TRUE)
# 
# #text(3.8, 0.07, "B", font=2, cex=1.5)
# 
# #box(which="plot",lty = "solid", col="black")
# 
# 
# #depth
# 
# 
# #par(oma=c(1,1,1,1))
# plot(pred.fit~I(broodsize+.02), data=subset(depthgain.new.dat, species=="BTBW"& trt=="swap"),xaxt='n',col=c("blue"), lwd=2, pch=16,type='b',xlim=c(2,4.1),ylim=c(0,0.7), ylab="Gain(mm)", xlab="Brood size")
# points(pred.fit~I(broodsize+.07), data=subset(depthgain.new.dat, species=="HOWA" & trt=="swap"),col=c("red"), pch=16,type='b', lwd=2)
# arrows(c(2.02,3.02,4.02), depthgain.new.dat$pred.CI.lwr[depthgain.new.dat$species=="BTBW"& depthgain.new.dat$trt=="swap"], c(2.02,3.02,4.02), depthgain.new.dat$pred.CI.upr[depthgain.new.dat$species=="BTBW"& depthgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="blue", lwd=2)
# arrows(c(2.07,3.07,4.07), depthgain.new.dat$pred.CI.lwr[depthgain.new.dat$species=="HOWA"& depthgain.new.dat$trt=="swap"], c(2.07,3.07,4.07), depthgain.new.dat$pred.CI.upr[depthgain.new.dat$species=="HOWA"& depthgain.new.dat$trt=="swap"], code=3, angle=90, length=.05, col="red", lwd=2)
# 
# points(pred.fit~broodsize, data=subset(depthgain.new.dat, species=="HOWA" & trt=="unmanipulated"),col=c("red"), pch=15,type='b', lty=3, lwd=2)
# points(pred.fit~I(broodsize+.09), data=subset(depthgain.new.dat, species=="BTBW"&  trt=="unmanipulated"),col=c("blue"), pch=15,type='b', lty=3, lwd=2)
# arrows(c(2.09,3.09,4.09), depthgain.new.dat$pred.CI.lwr[depthgain.new.dat$species=="BTBW"& depthgain.new.dat$trt=="unmanipulated"], c(2.09,3.09,4.09), depthgain.new.dat$pred.CI.upr[depthgain.new.dat$species=="BTBW"& depthgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="blue", lty=2, lwd=2)
# arrows(c(2,3,4), depthgain.new.dat$pred.CI.lwr[depthgain.new.dat$species=="HOWA"& depthgain.new.dat$trt=="unmanipulated"], c(2,3,4), depthgain.new.dat$pred.CI.upr[depthgain.new.dat$species=="HOWA"& depthgain.new.dat$trt=="unmanipulated"], code=3, angle=90, length=.05, col="red", lty=2, lwd=2)
# text(3,0.65, "F) Bill Depth", font=2, cex=1.2)
# 
# legend(2.0,0.15, c("HOWA-translocated", "HOWA-unmanipulated","BTBW-translocated","BTBW-umanipulated"), pch=c(16,15,16,15),lty=c(1,3,1,3), col=c("red","red","blue","blue"),cex=.85)
# axis(side=1, at=xtick, labels = TRUE)
# #text(3.8, 0.046, "C", font=2, cex=1.5)
# 
# dev.off()
# system("open NESTLINGS.png")

