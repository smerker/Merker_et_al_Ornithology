## Load translocation data
load("Translocation_Data.gzip")

## Fit the binomial GLM
glm1 <- glm(cbind(Eggs.Hatched,unhatched) ~ treatment_combined, family=binomial, data=Translocation_Data)
summary(glm1)
anova(glm1, test="LRT")  ## Likelihood ratio test
step(glm1)               ## Model with treatment is better than the null

## Multiple comparisons
library(multcomp)
glm1.sum.mc <- summary(glht(glm1, linfct=mcp(treatment_combined="Tukey")))
glm1.sum.mc

#
glm.df <- data.frame(estimate=glm1.sum.mc$test$coefficients[1:15],
                     std.error=glm1.sum.mc$test$sigma[1:15],
                     p.value=glm1.sum.mc$test$pvalues[1:15])
round(glm.df, 3)


## New data frame for predicting
levs <- c("BTBWswap", "BTBWctrl", "BTBWunman",
          "HOWAswap", "HOWActrl", "HOWAunman")
##new.dat <- data.frame(treatment_combined=unique(Translocation_Data$treatment_combined))
new.dat <- data.frame(treatment_combined=levs)
new.dat
## Predict on link scale
pred1 <- predict(glm1, new.dat, se.fit=TRUE, type="link")
pred1
## Predict on response
pred2 <- predict(glm1, new.dat, se.fit=TRUE, type="response")
pred2
#add predictions to new.dat 
new.dat$prediction<-plogis(pred1$fit)
new.dat$se <- pred2$se.fit ## SE on response scale
new.dat$lwr<-plogis(pred1$fit-(1.96*new.dat$se))
new.dat$upr<-plogis(pred1$fit+(1.96*new.dat$se))
new.dat


#create figure (without bird images)
library(plotrix)
#creating a label list
labs<-c("Translocated", "Control", "Unmanipulated","Translocated", "Control", "Unmanipulated")
labs

ytick<-c(0.0,0.6,0.8,1.0)

png("Figure_3.png", width=8, height=7, units="in", res=600)
par(mai=c(0.9,0.9,0.2,0.4))
bpx <- barplot(new.dat$prediction, ylim=c(0.4,1), space=c(0,0,0,.3,0,0), 
               col=c("lightblue","lightblue","lightblue", "orangered", "orangered", "orangered"),
               ylab="Hatch Rate", yaxt='n', xpd=FALSE, cex.lab=1.2)
axis(side=2, at=c(.4,.6,.8,1), labels = FALSE)
axis.break(2, 0.5, style = "slash", brw=0.06)
text(par("usr")[1],c(0.4,.6,.8,1),
     labels = ytick, pos = 2, xpd=NA, offset=1)
arrows(bpx, new.dat$prediction, 
       bpx, new.dat$prediction+new.dat$se, code=3, angle=90, length=.05)
text(bpx-0.1, par("usr")[3]-0.01, labels = labs, srt = 330, adj=0.0, xpd = TRUE,
     cex=1.1, font=1)
box(which="plot", bty=c("]"))
dev.off()

system("open Figure_3.png")




