load("time_data.gzip")

howa.t.test<-t.test(TimeEGGSout~Treatment, howa.timex, paired=FALSE, var.equal=TRUE)
howa.t.test

howa.mu.swap<-mean(howa.timex$TimeEGGSout[howa.timex$Treatment=="swap"])
howa.mu.ctrl<-mean(howa.timex$TimeEGGSout[howa.timex$Treatment=="ctrl"])

std.error(howa.timex$TimeEGGSout)

btbw.t.test<-t.test(TimeEGGSout~Treatment, btbw.timex, paired=FALSE, var.equal=TRUE)#, conf.level=0.95, conf.int=T)
btbw.t.test
std.error(btbw.timex$TimeEGGSout)

qt(.025, 20)
species.t.test<-t.test(btbw.timex$TimeEGGSout, howa.timex$TimeEGGSout, paired=FALSE, var.equal = TRUE)
var.howa.timex<-var(howa.timex$TimeEGGSout)
var.btbw.timex<-var(btbw.timex$TimeEGGSout)
n.howatimex<-nrow(howa.timex)
n.btbw.timex<-nrow(btbw.timex)

time.t.test.df<-data.frame(Species=c("BTBW", "HOWA", "BTBW vs HOWA"),
                           Test=c("swap vs control", "swap vs control", "BTBW vs HOWA"), 
                           df=c(btbw.t.test$parameter,howa.t.test$parameter,species.t.test$parameter),
                           meanControltime=round(c(btbw.t.test$estimate[1],howa.t.test$estimate[1],species.t.test$estimate[1]),2),
                           meanSwaptime=round(c(btbw.t.test$estimate[2],howa.t.test$estimate[2],species.t.test$estimate[2]),2),
                           tstat=round(c(btbw.t.test$statistic,howa.t.test$statistic,species.t.test$statistic),3),
                           pval=round(c(btbw.t.test$p.value,howa.t.test$p.value,species.t.test$p.value),3))


