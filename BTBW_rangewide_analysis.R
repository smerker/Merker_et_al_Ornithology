# loads the object "BTBW_Range_Hatch_Data"
load("rangedata.gzip")

#performs glms.

egg.glm.T<-glm(cbind(hatched,unhatched)~range*year*meanT, binomial, data=BTBW_Range_Hatch_Data)
summary(egg.glm.T)

egg.glm2.T<-glm(cbind(hatched,unhatched)~meanT, binomial, data=BTBW_Range_Hatch_Data)
summary(egg.glm2.T)

egg.glm3.T<-glm(cbind(hatched,unhatched)~year*meanT, binomial, data=BTBW_Range_Hatch_Data) 
summary(egg.glm3.T)

egg.glm4.T<-glm(cbind(hatched,unhatched)~range*meanT, binomial, data=BTBW_Range_Hatch_Data)
summary(egg.glm4.T)

#creates AIC table

range.AIC.T<-AIC(egg.glm.T, egg.glm2.T,egg.glm3.T,egg.glm4.T)
range.AIC.T$deltaAIC.T<-range.AIC.T$AIC-min(range.AIC.T$AIC)
range.AIC.T$w <- exp(-0.5*range.AIC.T$deltaAIC)/sum(exp(-0.5*range.AIC.T$deltaAIC))

range.AIC_order.T <- round(range.AIC.T[order(range.AIC.T$AIC),],3)
range.AIC_order.T



#making predictions
#new data frame for predicting

BTBW_Range_Hatch_Data_agg<-aggregate(meanT~range+year, data=BTBW_Range_Hatch_Data, FUN=mean)

# make prediction
pred1.T_mean<-predict(egg.glm.T, BTBW_Range_Hatch_Data_agg, se.fit=TRUE, type="response", interval="confidence")
pred1.T_mean

# add to data frame
BTBW_Range_Hatch_Data_agg$pred<-pred1.T_mean$fit #
BTBW_Range_Hatch_Data_agg$se1<-pred1.T_mean$se.fit
BTBW_Range_Hatch_Data_agg$prd1.CI.lwr<-pred1.T_mean$fit - 1.96*pred1.T_mean$se.fit
BTBW_Range_Hatch_Data_agg$prd1.CI.upr<-pred1.T_mean$fit + 1.96*pred1.T_mean$se.fit


# create figure (without bird images)

library(plotrix)

#for y axis
ytick<-c(0.0,0.85,0.9,0.95,1.0)


png(file="3-way_meanT_year.png", res=800, units = 'in', height=5, width=6.5)

plot(pred~year, data=subset(BTBW_Range_Hatch_Data_agg, range=="core"), type='l', col="blue", ylim=c(0.8,1), yaxt='n', lwd=2, ylab="Hatch Rate", xlab="Year")
axis(side=2, at=c(0.8,0.85,0.9,0.95,1.0), labels = FALSE)
axis.break(2, 0.82, style = "slash", brw=.07)
text(par("usr")[1], c(0.8,0.85,0.9,0.95,1.0),
     labels = ytick, pos = 2, xpd=NA, offset=1)

polygon(x=c(min(BTBW_Range_Hatch_Data_agg$year[BTBW_Range_Hatch_Data_agg$range=="core"]), min(BTBW_Range_Hatch_Data_agg$year[BTBW_Range_Hatch_Data_agg$range=="core"]), 
            BTBW_Range_Hatch_Data_agg$year[BTBW_Range_Hatch_Data_agg$range=="core"], 
            max(BTBW_Range_Hatch_Data_agg$year[BTBW_Range_Hatch_Data_agg$range=="core"]), 
            max(BTBW_Range_Hatch_Data_agg$year[BTBW_Range_Hatch_Data_agg$range=="core"]), 
            rev(BTBW_Range_Hatch_Data_agg$year[BTBW_Range_Hatch_Data_agg$range=="core"]),min(BTBW_Range_Hatch_Data_agg$year[BTBW_Range_Hatch_Data_agg$range=="core"])),
        y=c(min(subset(BTBW_Range_Hatch_Data_agg, range=="core", select=prd1.CI.lwr)), 
            max(subset(BTBW_Range_Hatch_Data_agg, range=="core", select=prd1.CI.upr)),   
            subset(BTBW_Range_Hatch_Data_agg, range=="core", select=prd1.CI.upr, drop=TRUE),     
            max(subset(BTBW_Range_Hatch_Data_agg, range=="core", select=prd1.CI.upr)),         
            min(subset(BTBW_Range_Hatch_Data_agg, range=="core", select=prd1.CI.lwr)),
            rev(subset(BTBW_Range_Hatch_Data_agg, range=="core", select=prd1.CI.lwr, drop=TRUE)),
            min(subset(BTBW_Range_Hatch_Data_agg, range=="core", select=prd1.CI.lwr))), 
        col=rgb(0,0,0.5,.3), border=FALSE)

lines(pred~year, data=subset(BTBW_Range_Hatch_Data_agg, range=="trailing"), col="red", lwd=2)
polygon(x=c(min(BTBW_Range_Hatch_Data_agg$year[BTBW_Range_Hatch_Data_agg$range=="trailing"]), min(BTBW_Range_Hatch_Data_agg$year[BTBW_Range_Hatch_Data_agg$range=="trailing"]), 
            BTBW_Range_Hatch_Data_agg$year[BTBW_Range_Hatch_Data_agg$range=="trailing"], 
            max(BTBW_Range_Hatch_Data_agg$year[BTBW_Range_Hatch_Data_agg$range=="trailing"]), 
            max(BTBW_Range_Hatch_Data_agg$year[BTBW_Range_Hatch_Data_agg$range=="trailing"]), 
            rev(BTBW_Range_Hatch_Data_agg$year[BTBW_Range_Hatch_Data_agg$range=="trailing"]),min(BTBW_Range_Hatch_Data_agg$year[BTBW_Range_Hatch_Data_agg$range=="trailing"])),
        y=c(min(subset(BTBW_Range_Hatch_Data_agg, range=="trailing", select=prd1.CI.lwr)), 
            max(subset(BTBW_Range_Hatch_Data_agg, range=="trailing", select=prd1.CI.upr)),   
            subset(BTBW_Range_Hatch_Data_agg, range=="trailing", select=prd1.CI.upr, drop=TRUE),     
            max(subset(BTBW_Range_Hatch_Data_agg, range=="trailing", select=prd1.CI.upr)),         
            min(subset(BTBW_Range_Hatch_Data_agg, range=="trailing", select=prd1.CI.lwr)),
            rev(subset(BTBW_Range_Hatch_Data_agg, range=="trailing", select=prd1.CI.lwr, drop=TRUE)),
            min(subset(BTBW_Range_Hatch_Data_agg, range=="trailing", select=prd1.CI.lwr))), 
        col=rgb(.50,0,0,.3), border=FALSE)
legend(2008,.83, c("Core", "Trailing-edge"), col=c("blue","red"), lwd=3, title="Black-throated blue warbler \nRange Position", 
       bty="n", horiz=T, cex=0.9)


dev.off()
system("open 3-way_meanT_year.png")







