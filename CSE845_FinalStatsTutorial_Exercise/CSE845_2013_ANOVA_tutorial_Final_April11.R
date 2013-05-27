### R code from vignette source '~/BEACON_COURSE_2011/CSE891_R_tutorials/CSE891_FinalStatsTutorial/CSE845_2013_ANOVA_tutorial_Final_April11.Rnw'

###################################################
### code chunk number 1: CSE845_2013_ANOVA_tutorial_Final_April11.Rnw:11-13
###################################################
options(show.signif.stars=F)
options(digits=3)


###################################################
### code chunk number 2: CSE845_2013_ANOVA_tutorial_Final_April11.Rnw:17-20
###################################################
require(car)
require(sciplot)
require(arm)


###################################################
### code chunk number 3: CSE845_2013_ANOVA_tutorial_Final_April11.Rnw:25-32
###################################################
parasite.data <- read.csv('http://beaconcourse.pbworks.com/f/sexAsex.csv', h=T)
parasite.data$Sex <- factor(parasite.data$Sex, 
  levels=c(0,1), labels=c("NoSex", "Sex"))
parasite.data$Parasites <- factor(parasite.data$Parasites, 
  levels=c(0,1), labels=c("NoParasites", "Parasites"))   
parasite.data$HostMutationRateC <- scale(parasite.data$HostMutationRate, 
  center=T, scale=F)


###################################################
### code chunk number 4: CSE845_2013_ANOVA_tutorial_Final_April11.Rnw:50-55
###################################################
lineplot.CI(x.factor=Parasites, response = FinalRichness,
  group=Sex, data = parasite.data, lwd=3,
  ylab = "Richness", xlab = "Parasites",
  x.leg = 1, y.leg = 35, 
  ci.fun= function(x) c(mean(x)- 2*se(x), mean(x)+ 2*se(x)))


###################################################
### code chunk number 5: CSE845_2013_ANOVA_tutorial_Final_April11.Rnw:64-91
###################################################
with(parasite.data, plot(FinalRichness~HostMutationRateC, 
  col=c("blue", "red")[Parasites], pch=c(16,17)[Sex]))
  
with(parasite.data[parasite.data$Parasites=="Parasites" & parasite.data$Sex=="Sex",], 
  lines(smooth.spline(x=HostMutationRateC, y=FinalRichness, cv=F),
    lwd=3, col="red", lty=1))
    
with(parasite.data[parasite.data$Parasites=="NoParasites" & parasite.data$Sex=="Sex",], lines(smooth.spline(x=HostMutationRateC, 
    y=FinalRichness, cv=F),lwd=3, col="blue", lty=1)) 
    
with(parasite.data[parasite.data$Parasites=="Parasites" 
  & parasite.data$Sex=="NoSex",], 
lines(smooth.spline(x=HostMutationRateC, 
    y=FinalRichness, cv=F),
    lwd=3, col="red", lty=3))
    
with(parasite.data[parasite.data$Parasites=="NoParasites" 
  & parasite.data$Sex=="NoSex",], 
lines(smooth.spline(x=HostMutationRateC, 
    y=FinalRichness, cv=F),
    lwd=3, col="blue", lty=3))
    
legend(x=-0.3, y=90, 
  legend=c("Parasites-Sex", "NoParasites-Sex", 
  "Parasites-NoSex", "NoParasites-NoSex"),
  col=c("red", "blue", "red", "blue"),
  lty=c(1,1,3,3), bty="n", lwd=3)        


###################################################
### code chunk number 6: CSE845_2013_ANOVA_tutorial_Final_April11.Rnw:102-107
###################################################
model_1 <- lm(FinalRichness ~ (HostMutationRateC + Parasites + Sex)^2 , 
  data=parasite.data)
summary(model_1)$coef[,1:3] # hide p-values

confint(model_1)


###################################################
### code chunk number 7: CSE845_2013_ANOVA_tutorial_Final_April11.Rnw:115-118
###################################################
vif(model_1)
#print(cov2cor(vcov(model_1)) , digits=2)
# Run but do not print in document, too big and messy


###################################################
### code chunk number 8: CSE845_2013_ANOVA_tutorial_Final_April11.Rnw:121-126
###################################################
mod.X <- model.matrix(model_1)
eigen.x <- eigen(t(mod.X) %*%mod.X)
eigen.x$val # eigenvalues from the design matrix
sqrt(max(eigen.x$val)/min(eigen.x$val))
  # condition numbers


###################################################
### code chunk number 9: CSE845_2013_ANOVA_tutorial_Final_April11.Rnw:133-138
###################################################
par(mfrow=c(2,2))
confidenceEllipse(model_1, which.coef=c(3,5))
confidenceEllipse(model_1, which.coef=c(3,7))
confidenceEllipse(model_1, which.coef=c(4,7))
confidenceEllipse(model_1, which.coef=c(4,6))


###################################################
### code chunk number 10: CSE845_2013_ANOVA_tutorial_Final_April11.Rnw:146-152
###################################################
plot(parasite.data$FinalRichness ~ fitted(model_1), 
  xlim=c(min(parasite.data$FinalRichness), max(parasite.data$FinalRichness)),
  ylim=c(min(parasite.data$FinalRichness), max(parasite.data$FinalRichness)),
  ylab= "observed FinalRichness",
  col=densCols(fitted(model_1)))
abline(a=0, b=1,lty=3)  


###################################################
### code chunk number 11: CSE845_2013_ANOVA_tutorial_Final_April11.Rnw:160-161
###################################################
avPlots(model_1)


###################################################
### code chunk number 12: CSE845_2013_ANOVA_tutorial_Final_April11.Rnw:169-171
###################################################
coefplot(model_1, int=T, h.axis=T, vertical=F, var.las=2, 
  mar= c(10,3,5.1,2), main= "estimated parameters")


###################################################
### code chunk number 13: CSE845_2013_ANOVA_tutorial_Final_April11.Rnw:177-183
###################################################
Richness_control <- subset(parasite.data, 
  subset=Sex=="NoSex" & 
  Parasites=="NoParasites" & HostMutationRate==0.1)
  
sd(Richness_control$FinalRichness)
coef(model_1)/sd(Richness_control$FinalRichness)


###################################################
### code chunk number 14: CSE845_2013_ANOVA_tutorial_Final_April11.Rnw:189-191
###################################################
par(mfrow=c(2,2))
plot(model_1)


###################################################
### code chunk number 15: CSE845_2013_ANOVA_tutorial_Final_April11.Rnw:197-199
###################################################
par(mfrow=c(1,1))
hist(resid(model_1))


###################################################
### code chunk number 16: CSE845_2013_ANOVA_tutorial_Final_April11.Rnw:208-223
###################################################
par(mfrow=c(2,2))

plot(model_1$resid ~ model_1$model$HostMutationRateC,
     xlab = "HostMutationRate (centered)",
     ylab = " residuals")
 
lines(smooth.spline(x=model_1$model$HostMutationRateC, y=model_1$resid, 
    cv=F), lwd=3, col="red", lty=1)

plot(model_1$resid ~ model_1$model$Sex,
    xlab = "Sex", ylab = "residuals")  

plot(model_1$resid ~ model_1$model$Parasites,
    xlab = "Parasites",  ylab = "residuals")
    


###################################################
### code chunk number 17: CSE845_2013_ANOVA_tutorial_Final_April11.Rnw:229-234
###################################################
sd(model_1$resid[model_1$model$Sex=="Sex"])
sd(model_1$resid[model_1$model$Sex=="NoSex"])

sd(model_1$resid[model_1$model$Parasites=="Parasites"])
sd(model_1$resid[model_1$model$Parasites=="NoParasites"])


