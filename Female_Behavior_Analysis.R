

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#### female behavior ####


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#bring in the data. 
load("female_behavior.gzip")

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

BTBW.on.aic<-AIC(lme.onB1,lme.onB2,lme.onB3,lme.onB4)
BTBW.on.aic<-BTBW.on.aic[order(BTBW.on.aic$AIC),]
BTBW.on.aic$deltaAIC<-BTBW.on.aic$AIC-min(BTBW.on.aic$AIC)
BTBW.on.aic$weight<-round(exp(-0.5*BTBW.on.aic$deltaAIC)/sum(exp(-0.5*BTBW.on.aic$deltaAIC)),2)
BTBW.on.aic


#a few predictions for length of on bouts
new.dat.on.B.T<-data.frame(T.out.mn=seq(min(D3_9_on.B$T.out.mn),max(D3_9_on.B$T.out.mn),length=100),
                           H.out.mn=rep(mean(D3_9_on.B$H.out.mn),100))
new.dat.on.B.H<-data.frame(H.out.mn=seq(min(D3_9_on.B$H.out.mn),max(D3_9_on.B$H.out.mn),length=100),
                           T.out.mn=rep(mean(D3_9_on.B$T.out.mn),100))

pred.on.B.T<-predictSE.lme(lme.onB2, new.dat.on.B.T, level=0, se=T, interval="confidence",type="link", asList=T)
new.dat.on.B.T$fit1<-exp(pred.on.B.T$fit)
new.dat.on.B.T$low<-exp(pred.on.B.T$fit+1.96*pred.on.B.T$se)
new.dat.on.B.T$up<-exp(pred.on.B.T$fit-1.96*pred.on.B.T$se)


pred.on.B.H<-predictSE.lme(lme.onB3, new.dat.on.B.H, level=0, se=TRUE, interval="confidence", type="link")
new.dat.on.B.H$fit2<-exp(pred.on.B.H$fit)
new.dat.on.B.H$low<-exp(pred.on.B.H$fit+1.96*pred.on.B.H$se)
new.dat.on.B.H$up<-exp(pred.on.B.H$fit+-1.96*pred.on.B.H$se)

#hooded warbler on-nest models -- log transformed -- random effects include day within nest

lme.onH1<-lme(log(time)~T.out.mn+H.out.mn, random=~1|nestID/day, data=D3_9_on.H)
summary(lme.onH1)

lme.onH2<-lme(log(time)~T.out.mn, random=~1|nestID/day, data=D3_9_on.H)
summary(lme.onH2)

lme.onH3<-lme(log(time)~H.out.mn, random=~1|nestID/day, data=D3_9_on.H)
summary(lme.onH3)

lme.onH4<-lme(log(time)~H.out.mn*T.out.mn, random=~1|nestID/day, data=D3_9_on.H)
summary(lme.onH4)


HOWA.on.aic<-AIC(lme.onH1,lme.onH2,lme.onH3,lme.onH4)
HOWA.on.aic<-HOWA.on.aic[order(HOWA.on.aic$AIC),]
HOWA.on.aic$deltaAIC<-HOWA.on.aic$AIC-min(HOWA.on.aic$AIC)
HOWA.on.aic$weight<-round(exp(-0.5*HOWA.on.aic$deltaAIC)/sum(exp(-0.5*HOWA.on.aic$deltaAIC)),2)
HOWA.on.aic

#predictions
new.dat.on.H.T<-data.frame(T.out.mn=seq(min(D3_9_on.H$T.out.mn),max(D3_9_on.H$T.out.mn),length=100),
                           H.out.mn=rep(mean(D3_9_on.H$H.out.mn),100))
new.dat.on.H.H<-data.frame(H.out.mn=seq(min(D3_9_on.H$H.out.mn),max(D3_9_on.H$H.out.mn),length=100),
                           T.out.mn=rep(mean(D3_9_on.H$T.out.mn),100))


pred.on.H.T<-predictSE.lme(lme.onH2, new.dat.on.H.T, level=0, se=TRUE, interval="confidence", type="link")
new.dat.on.H.T$fit1<-exp(pred.on.H.T$fit)
new.dat.on.H.T$low<-exp(pred.on.H.T$fit-1.96*pred.on.H.T$se)
new.dat.on.H.T$up<-exp(pred.on.H.T$fit+1.96*pred.on.H.T$se)


pred.on.H.H<-predictSE.lme(lme.onH3, new.dat.on.H.H, level=0, se=TRUE, interval="confidence", type="link")
new.dat.on.H.H$fit2<-exp(pred.on.H.H$fit)
new.dat.on.H.H$low<-exp(pred.on.H.H$fit-1.96*pred.on.H.H$se)
new.dat.on.H.H$up<-exp(pred.on.H.H$fit+1.96*pred.on.H.H$se)


#black-throated blue off-nest models -- log transformed -- random effects include day within nest
lme.offB1<-lme(log(time)~T.out.mn+H.out.mn, random=~1|nestID/day, data=D3_9_off.B)
summary(lme.offB1)

lme.offB2<-lme(log(time)~T.out.mn, random=~1|nestID/day, data=D3_9_off.B)
summary(lme.offB2)
lme.offB3<-lme(log(time)~H.out.mn, random=~1|nestID/day, data=D3_9_off.B)
summary(lme.offB3)
lme.offB4<-lme(log(time)~H.out.mn*T.out.mn, random=~1|nestID/day, data=D3_9_off.B)
summary(lme.offB4)


BTBW.off.aic<-AIC(lme.offB1,lme.offB2,lme.offB3,lme.offB4)
BTBW.off.aic<-BTBW.off.aic[order(BTBW.off.aic$AIC),]
BTBW.off.aic$deltaAIC<-BTBW.off.aic$AIC-min(BTBW.off.aic$AIC)
BTBW.off.aic$weight<-round(exp(-0.5*BTBW.off.aic$deltaAIC)/sum(exp(-0.5*BTBW.off.aic$deltaAIC)),2)
BTBW.off.aic

#a few predictions for length of off bouts 
new.dat.off.B.T<-data.frame(T.out.mn=seq(min(D3_9_off.B$T.out.mn),max(D3_9_off.B$T.out.mn),length=100),
                            H.out.mn=rep(mean(D3_9_off.B$H.out.mn),100))
new.dat.off.B.H<-data.frame(H.out.mn=seq(min(D3_9_off.B$H.out.mn),max(D3_9_off.B$H.out.mn),length=100),
                            T.out.mn=rep(mean(D3_9_off.B$T.out.mn),100))


pred.off.B.T<-predictSE.lme(lme.offB2, new.dat.off.B.T, level=0, se=TRUE, interval="confidence", type="link")
new.dat.off.B.T$fit1<-exp(pred.off.B.T$fit)
new.dat.off.B.T$low<-exp(pred.off.B.T$fit-1.96*pred.off.B.T$se)
new.dat.off.B.T$up<-exp(pred.off.B.T$fit+1.96*pred.off.B.T$se)


pred.off.B.H<-predictSE.lme(lme.offB3, new.dat.off.B.H, level=0, se=TRUE, interval="confidence", type="link")
new.dat.off.B.H$fit2<-exp(pred.off.B.H$fit)
new.dat.off.B.H$low<-exp(pred.off.B.H$fit-1.96*pred.off.B.H$se)
new.dat.off.B.H$up<-exp(pred.off.B.H$fit+1.96*pred.off.B.H$se)




#hooded warbler off-nest models -- log transformed -- random effects include day within nest
lme.offH1<-lme(log(time)~T.out.mn+H.out.mn, random=~1|nestID/day, data=D3_9_off.H)
summary(lme.offH1)
lme.offH2<-lme(log(time)~T.out.mn, random=~1|nestID/day, data=D3_9_off.H)
summary(lme.offH2)
lme.offH3<-lme(log(time)~H.out.mn, random=~1|nestID/day, data=D3_9_off.H)
summary(lme.offH3)
lme.offH4<-lme(log(time)~H.out.mn*T.out.mn, random=~1|nestID/day, data=D3_9_off.H)
summary(lme.offH4)


HOWA.off.aic<-AIC(lme.offH1,lme.offH2,lme.offH3,lme.offH4)
HOWA.off.aic<-HOWA.off.aic[order(HOWA.off.aic$AIC),]
HOWA.off.aic$deltaAIC<-HOWA.off.aic$AIC-min(HOWA.off.aic$AIC)
HOWA.off.aic$weight<-round(exp(-0.5*HOWA.off.aic$deltaAIC)/sum(exp(-0.5*HOWA.off.aic$deltaAIC)),2)
HOWA.off.aic

new.dat.off.H.T<-data.frame(T.out.mn=seq(min(D3_9_off.H$T.out.mn),max(D3_9_off.H$T.out.mn),length=100),
                            H.out.mn=rep(mean(D3_9_off.H$H.out.mn),100))
new.dat.off.H.H<-data.frame(H.out.mn=seq(min(D3_9_off.H$H.out.mn),max(D3_9_off.H$H.out.mn),length=100),
                            T.out.mn=rep(mean(D3_9_off.H$T.out.mn),100))


pred.off.H.T<-predictSE.lme(lme.offH2, new.dat.off.H.T, level=0, se=TRUE, interval="confidence", type="link")
new.dat.off.H.T$fit1<-exp(pred.off.H.T$fit)
new.dat.off.H.T$low<-exp(pred.off.H.T$fit-1.96*pred.off.H.T$se)
new.dat.off.H.T$up<-exp(pred.off.H.T$fit+1.96*pred.off.H.T$se)



pred.off.H.H<-predictSE.lme(lme.offH3, new.dat.off.H.H, level=0, se=TRUE, interval="confidence", type="link")
new.dat.off.H.H$fit2<-exp(pred.off.H.H$fit)
new.dat.off.H.H$low<-exp(pred.off.H.H$fit-1.96*pred.off.H.H$se)
new.dat.off.H.H$up<-exp(pred.off.H.H$fit+1.96*pred.off.H.H$se)


## 4 panel plot



png("Figure_6.png", res=600, units="in", width=7, height=7)
par(mfcol=c(2,2), mar=c(4,4,2,1), oma=c(0,0,0,0))
## text(-1.9, .1, "B", font=2, cex=1.2)
plot(fit1~T.out.mn, data=new.dat.on.B.T, type="l", col="blue", xlim=c(7,31),lty=2,ylim=c(13,29),
     xlab="Ambient Temperature (\u00B0C)", ylab="Incubation Bout Duration", lwd=2, xaxt='s', mgp=c(2.5,1,0))
## axis(1, at=seq(10,29,length=6),
##      labels=seq(10,30,length=6), tick=TRUE, padj=-.75)
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
text(8, 13.4, "A", font=2, cex=1.2)
##mtext(side=1, "Ambient Temperature (\u00B0C)", cex=.7, adj=.5, padj=2.7)
legend(8, 29, c("Hooded", "Black-throated blue"), col=c("red", "blue"), title="Incubating Warbler Species", lty=c(1,2), cex=.8)
## off, temperature
plot(fit1~T.out.mn, data=new.dat.off.B.T, type="l", col="blue", xlim=c(7,31),ylim=c(5,9),
     xlab="Ambient Temperature (\u00B0C)", ylab="Off Bout Duration", lwd=2, lty=2,xaxt='s', mgp=c(2.5,1,0))
## axis(1, at=seq(10,29,length=6),
##      labels=seq(10,30,length=6), tick=TRUE, padj=-.75)
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
text(8, 5.1, "B", font=2, cex=1.2)
## mtext(side=1, "Ambient Temperature (\u00B0C)", cex=.7, adj=.5, padj=2.7)
## On, humidity
plot(fit2~H.out.mn, data=new.dat.on.B.H, type="l", col="blue", xlim=c(38,99),ylim=c(13,30),
     xlab="Ambient Humidity", xaxt='s', ylab="Incubation Bout Duration", lwd=2, lty=2,mgp=c(2.5,1,0))
## axis(1, at=seq(40,100,length=7),
##      labels=seq(40,100,length=7), tick=TRUE, padj=-.75)
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
text(40, 13.4, "C", font=2, cex=1.2)
##mtext(side=1, "Ambient Humidity", cex=.7, adj=.5, padj=2.7)
## off, humidity
plot(fit2~H.out.mn, data=new.dat.off.B.H, type="l", col="blue",xlim=c(38,100), ylim=c(5,9),
     xlab="Ambient Humidity", xaxt='s', ylab="Off Bout Duration", lwd=2,lty=2, mgp=c(2.5,1,0))
## axis(1, at=seq(40,100,length=7),
##      labels=seq(40,100,length=7), tick=TRUE, padj=-.75)
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
text(40, 5.1, "D", font=2, cex=1.2)
##mtext(side=1, "Ambient Humidity", cex=.7, adj=.5, padj=2.7)
dev.off()

system("open Figure_6.png")

