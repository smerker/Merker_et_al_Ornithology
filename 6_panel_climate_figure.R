#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#### six panels! ####

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


load("climatedata.gzip")

png("6_panel_microclimate.png", res=800, width=6, height=8, units="in")

par(mfrow=c(3,2), mar=c(0,4,4,1), oma=c(0,0,1,1))

# nighttime temperature INSIDE nests. 
plot(pred.fit~elevation, subset(new.dat.mod3, species=="BTBW", ), type="l",lwd=2, 
     xlab="Elevation",col="blue", ylab="Mean Temperature (\u00B0C)", ylim=c(15,40), xlim = c(755, 1505), xaxt='n')

polygon(x=c(970, 970, new.dat.mod3$elevation[1:55], 1510, 1510, new.dat.in.temp$elevation[55:1],970),
        y=c(min(subset(new.dat.mod3, species=="BTBW", select=pred.CI.lwr)), 
            max(subset(new.dat.mod3, species=="BTBW", select=pred.CI.upr)),   
            subset(new.dat.mod3, species=="BTBW", select=pred.CI.upr, drop=TRUE),     
            max(subset(new.dat.mod3, species=="BTBW", select=pred.CI.upr)),         
            min(subset(new.dat.mod3, species=="BTBW", select=pred.CI.lwr)),
            rev(subset(new.dat.mod3, species=="BTBW", select=pred.CI.lwr, drop=TRUE)),
            min(subset(new.dat.mod3, species=="BTBW", select=pred.CI.lwr))), 
        col=rgb(0,0,.50,.2), border=FALSE)


points(meanT~elevation, subset(night.sum.stats, species=="BTBW"), pch=16,col="blue", cex=1)


lines(pred.fit~elevation, subset(new.dat.mod3, species=="HOWA"), type="l", col="red")
polygon(x=c(735, 735, new.dat.mod3$elevation[56:102], 1195, 1195, new.dat.in.temp$elevation[102:56],735),
        y=c(min(subset(new.dat.mod3, species=="HOWA", select=pred.CI.lwr)), 
            max(subset(new.dat.mod3, species=="HOWA", select=pred.CI.upr)),   
            subset(new.dat.mod3, species=="HOWA", select=pred.CI.upr, drop=TRUE),     
            max(subset(new.dat.mod3, species=="HOWA", select=pred.CI.upr)),         
            min(subset(new.dat.mod3, species=="HOWA", select=pred.CI.lwr)),
            rev(subset(new.dat.mod3, species=="HOWA", select=pred.CI.lwr, drop=TRUE)),
            min(subset(new.dat.mod3, species=="HOWA", select=pred.CI.lwr))), 
        col=rgb(0.5,0,0,.2), border=FALSE)

mtext(side=3, "Nighttime (in)", font=2)
text(800,17, "A", font=2, cex=1.5)
points(meanT~elevation, subset(night.sum.stats, species=="HOWA"), pch=16,col="red", cex=1)


#humidity inside nests.

plot(pred.fit~elevation, subset(new.dat.loghumid, species=="BTBW"), 
     type="l", col="blue",ylim=c(0,1), ylab="Relative Humidity", 
     xlab="Elevation",xlim = c(750, 1510), lwd=2, xaxt='n')

polygon(x=c(970, 970, new.dat.loghumid$elevation[1:55], 1510, 1510, new.dat.loghumid$elevation[55:1],970),
        y=c(min(subset(new.dat.loghumid, species=="BTBW", select=pred.CI.lwr)), 
            max(subset(new.dat.loghumid, species=="BTBW", select=pred.CI.upr)),   
            subset(new.dat.loghumid, species=="BTBW", select=pred.CI.upr, drop=TRUE),     
            max(subset(new.dat.loghumid, species=="BTBW", select=pred.CI.upr)),         
            min(subset(new.dat.loghumid, species=="BTBW", select=pred.CI.lwr)),
            rev(subset(new.dat.loghumid, species=="BTBW", select=pred.CI.lwr, drop=TRUE)),
            min(subset(new.dat.loghumid, species=="BTBW", select=pred.CI.lwr))), 
        col=rgb(0,0,0.5,.2), border=FALSE)


points(meanH/100~elevation, subset(night.sum.stats, species=="BTBW"), pch=16,col="blue", cex=1)

lines(pred.fit~elevation, subset(new.dat.loghumid, species=="HOWA"), type="l", col="red")#, ylim=c(0,1), ylab="",yaxt='n',lwd=2, xlim = c(750, 1200))
polygon(x=c(735, 735, new.dat.loghumid$elevation[56:102], 1195, 1195, new.dat.loghumid$elevation[102:56],735),
        y=c(min(subset(new.dat.loghumid, species=="HOWA", select=pred.CI.lwr)), 
            max(subset(new.dat.loghumid, species=="HOWA", select=pred.CI.upr)),   
            subset(new.dat.loghumid, species=="HOWA", select=pred.CI.upr, drop=TRUE),     
            max(subset(new.dat.loghumid, species=="HOWA", select=pred.CI.upr)),         
            min(subset(new.dat.loghumid, species=="HOWA", select=pred.CI.lwr)),
            rev(subset(new.dat.loghumid, species=="HOWA", select=pred.CI.lwr, drop=TRUE)),
            min(subset(new.dat.loghumid, species=="HOWA", select=pred.CI.lwr))), 
        col=rgb(0.5,0,0,.2), border=FALSE)


mtext(side=3, "Nighttime (in)", font=2)
text(800,0.09, "B", font=2, cex=1.5)
points(meanH/100~elevation, subset(night.sum.stats, species=="HOWA"), pch=16,col="red", cex=1)
legend(1090, .26, pch=16, cex=1, col=c("red", "blue"), 
       c("Hooded", "Black-throated blue"), title="Warbler Species")


par(mar=c(2,4,2,1))

plot(pred.fit~elevation, new.dat.out.temp, type="l", col="black", ylab="Mean Temperature (\u00B0C)", xlab=NA,ylim=c(11,19.5), lwd=2, xaxt='n')

polygon(x=c(739, 739, new.dat.out.temp$elevation[1:79], 1519, 1519, new.dat.out.temp$elevation[79:1],739),
        y=c(min(subset(new.dat.out.temp, select=pred.CI.lwr)), 
            max(subset(new.dat.out.temp, select=pred.CI.upr)),   
            subset(new.dat.out.temp,select=pred.CI.upr, drop=TRUE),     
            max(subset(new.dat.out.temp,select=pred.CI.upr)),         
            min(subset(new.dat.out.temp, select=pred.CI.lwr)),
            rev(subset(new.dat.out.temp, select=pred.CI.lwr, drop=TRUE)),
            min(subset(new.dat.out.temp, select=pred.CI.lwr))), 
        col=rgb(0.4,0.4,.40,.2), border=FALSE)
mtext(side=3, "Nighttime (out)", font=2)
points(meanT~elevation, subset(night.out.sum.stats, species=="HOWA"), pch=16,col="red", cex=1.1)
points(meanT~elevation, subset(night.out.sum.stats, species=="BTBW"), pch=16, col="blue", cex=1.1)
text(800,11.6, "C", font=2, cex=1.5)

####plotting night time

plot(pred.fit~elevation, new.dat.loghumid.out, type="l", col="black",ylim=c(0.8,1),xlab=NA ,ylab="Relative Humidity", xaxt='n', lwd=2)
polygon(x=c(739, 739, new.dat.loghumid.out$elevation[1:79], 1519, 1519, new.dat.loghumid.out$elevation[79:1],739),
        y=c(min(subset(new.dat.loghumid.out, select=pred.CI.lwr)), 
            max(subset(new.dat.loghumid.out, select=pred.CI.upr)),   
            subset(new.dat.loghumid.out,select=pred.CI.upr, drop=TRUE),     
            max(subset(new.dat.loghumid.out,select=pred.CI.upr)),         
            min(subset(new.dat.loghumid.out, select=pred.CI.lwr)),
            rev(subset(new.dat.loghumid.out, select=pred.CI.lwr, drop=TRUE)),
            min(subset(new.dat.loghumid.out, select=pred.CI.lwr))), 
        col=rgb(0.4,0.4,.40,.2), border=FALSE)
mtext(side=3, "Nighttime (out)", font=2)
points(meanH/100~elevation, subset( night.out.sum.stats,species=="BTBW"), pch=16, col="blue", cex=1.1)
points(meanH/100~elevation, subset( night.out.sum.stats,species=="HOWA"), pch=16, col="red", cex=1.1)
text(800,0.813, "D", font=2, cex=1.5)

#### daytime
par(mar=c(4,4,0,1))

plot(pred.fit.T~elevation, new.dat.out.daytemp, type="l", col="black", ylab="Mean Temperature (\u00B0C)", xlab="Elevation (m)",ylim=c(14.6,22.5), lwd=2)

polygon(x=c(739, 739, new.dat.out.daytemp$elevation[1:79], 1519, 1519, new.dat.out.daytemp$elevation[79:1],739),
        y=c(min(subset(new.dat.out.daytemp, select=pred.CI.T.lwr)), 
            max(subset(new.dat.out.daytemp, select=pred.CI.T.upr)),   
            subset(new.dat.out.daytemp,select=pred.CI.T.upr, drop=TRUE),     
            max(subset(new.dat.out.daytemp,select=pred.CI.T.upr)),         
            min(subset(new.dat.out.daytemp, select=pred.CI.T.lwr)),
            rev(subset(new.dat.out.daytemp, select=pred.CI.T.lwr, drop=TRUE)),
            min(subset(new.dat.out.daytemp, select=pred.CI.T.lwr))), 
        col=rgb(0.4,0.4,.40,.2), border=FALSE)

mtext(side=3, "Daytime (out)", font=2)
points(meanT~elevation, subset(day.out.sum.stats, species=="HOWA"), pch=16,col="red", cex=1.1)
points(meanT~elevation, subset(day.out.sum.stats, species=="BTBW"), pch=16, col="blue", cex=1.1)
text(800,15.3, "E", font=2, cex=1.5)


####plotting btbw in vs out
plot(pred.fit.H~elevation, new.dat.out.daytemp, type="l", col="black",ylim=c(0.67,1),xlab="Elevation (m)",ylab="Relative Humidity", xlim = c(750, 1510), lwd=2)
polygon(x=c(739, 739, new.dat.out.daytemp$elevation[1:79], 1519, 1519, new.dat.out.daytemp$elevation[79:1],739),
        y=c(min(subset(new.dat.out.daytemp, select=pred.CI.H.lwr)), 
            max(subset(new.dat.out.daytemp, select=pred.CI.H.upr)),   
            subset(new.dat.out.daytemp,select=pred.CI.H.upr, drop=TRUE),     
            max(subset(new.dat.out.daytemp,select=pred.CI.H.upr)),         
            min(subset(new.dat.out.daytemp, select=pred.CI.H.lwr)),
            rev(subset(new.dat.out.daytemp, select=pred.CI.H.lwr, drop=TRUE)),
            min(subset(new.dat.out.daytemp, select=pred.CI.H.lwr))), 
        col=rgb(0.4,0.4,.40,.2), border=FALSE)


mtext(side=3, "Daytime (out)", font=2)
points(meanH/100~elevation, subset(day.out.sum.stats,species=="BTBW"), pch=16, col="blue", cex=1.1)
points(meanH/100~elevation, subset(day.out.sum.stats,species=="HOWA"), pch=16, col="red", cex=1.1)
text(800,0.7, "F", font=2, cex=1.5)

dev.off()
system("open 6_panel_microclimate.png")
