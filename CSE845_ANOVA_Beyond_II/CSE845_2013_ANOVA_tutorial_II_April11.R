### R code from vignette source '~/BEACON_COURSE_2011/CSE891_R_tutorials/CSE891_ANOVA_Beyond_II/CSE845_2013_ANOVA_tutorial_II_April11.Rnw'

###################################################
### code chunk number 1: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:8-11
###################################################
#changing some options to make printing prettier for the document
options(show.signif.stars=F)
options(digits=3)


###################################################
### code chunk number 2: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:21-24
###################################################
require(car)
require(sciplot)
require(arm)


###################################################
### code chunk number 3: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:29-34
###################################################
parasite.data <- read.csv('http://beaconcourse.pbworks.com/f/sexAsex.csv', h=T)
parasite.data$Sex <- factor(parasite.data$Sex, 
  levels=c(0,1), labels=c("NoSex", "Sex"))
parasite.data$Parasites <- factor(parasite.data$Parasites, 
  levels=c(0,1), labels=c("NoParasites", "Parasites"))   


###################################################
### code chunk number 4: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:41-45
###################################################
model_1 <- lm(FinalDiversityIndex ~ Parasites, 
  data=parasite.data)
summary(model_1)
confint(model_1)


###################################################
### code chunk number 5: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:56-59
###################################################
model_coef <- as.numeric(coef(model_1)) 
  # just for convenience
sum(model_coef)/model_coef[1]


###################################################
### code chunk number 6: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:64-66
###################################################
(confint(model_1)[2,1] + model_coef[1])/model_coef[1]
(confint(model_1)[2,2] + model_coef[1])/model_coef[1]


###################################################
### code chunk number 7: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:72-75
###################################################
model_coef[2]/model_coef[1]
confint(model_1)[2,1]/model_coef[1]
confint(model_1)[2,2]/model_coef[1]


###################################################
### code chunk number 8: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:87-91
###################################################
diversityNoPar <- subset(parasite.data, Parasites=="NoParasites")
model_coef[2]/sd(diversityNoPar$FinalDiversityIndex)
confint(model_1)[2,1]/sd(diversityNoPar$FinalDiversityIndex)
confint(model_1)[2,2]/sd(diversityNoPar$FinalDiversityIndex)


###################################################
### code chunk number 9: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:101-117
###################################################
diversityPar <- subset(parasite.data, Parasites=="Parasites")
var_diversity_Par <- var(diversityPar$FinalDiversityIndex)
  # variance for one subset
var_diversity_NoPar <- var(diversityNoPar$FinalDiversityIndex)
  # variance for the other
n_Par <- nrow(diversityPar)
  # number of observations for one subset
n_NoPar <- nrow(diversityNoPar)
  # number of observations for the other subset
sd_pooled <- sqrt( (((n_Par - 1)*var_diversity_Par) + 
                   ((n_NoPar - 1)*var_diversity_NoPar))
                     /(n_Par + n_NoPar - 2))

model_coef[2]/sd_pooled                    
confint(model_1)[2,1]/sd_pooled
confint(model_1)[2,2]/sd_pooled


###################################################
### code chunk number 10: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:123-124
###################################################
anova(model_1)


###################################################
### code chunk number 11: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:133-137
###################################################
model_2 <- lm(FinalDiversityIndex ~ Sex, 
  data=parasite.data)
summary(model_2)
confint(model_2)


###################################################
### code chunk number 12: InteractionPlot
###################################################
lineplot.CI(x.factor=Parasites, response = FinalDiversityIndex,
  group=Sex, data = parasite.data, lwd=3,
  ylab = "task diversity", xlab = "Parasites",
  x.leg = 1, y.leg = 3.25, 
  ci.fun= function(x) c(mean(x)- 2*se(x), mean(x)+ 2*se(x)))
  # note: excluding the ci.fun line will default to one SE, instead of 2


###################################################
### code chunk number 13: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:160-164
###################################################
model_3 <- lm(FinalDiversityIndex ~ Sex + Parasites, 
  data=parasite.data)
summary(model_3)
confint(model_3)


###################################################
### code chunk number 14: FittedVsObserved
###################################################
plot(parasite.data$FinalDiversityIndex ~ fitted(model_3), 
  xlim=c(0,5), ylim=c(0,5), pch=16, cex=1.3,
  xlab = expression(paste("fitted values, ", hat(y))),
  ylab = "observed values of y")
abline(a=0, b=1, lwd = 2, col="grey")  


###################################################
### code chunk number 15: CoefPlots
###################################################
par(mfrow=c(1,1))
coefplot(model_3, int=T, var.las=0,
  h.axis=T, cex.pts=2, vertical=F, 
  main= " Comparing estimates for different models", lwd=3,
  ylim=c(0.5,2.5))
coefplot(model_2, int=T, var.las=0, add=T,
  h.axis=T, cex.pts=2, vertical=F, col="red")


###################################################
### code chunk number 16: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:211-215
###################################################
model_4 <- lm(FinalDiversityIndex ~ Sex*Parasites, 
  data=parasite.data)
summary(model_4)
confint(model_4)


###################################################
### code chunk number 17: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:220-221
###################################################
anova(model_3, model_4)


###################################################
### code chunk number 18: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:227-228
###################################################
print(AIC(model_1, model_2, model_3, model_4), digits=4)


###################################################
### code chunk number 19: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:235-236
###################################################
print(BIC(model_1, model_2, model_3, model_4), digits=4)


###################################################
### code chunk number 20: DiversityMutation
###################################################
plot(FinalDiversityIndex~HostMutationRate, data=parasite.data,
  col=densCols(c(FinalDiversityIndex,HostMutationRate)))
with(parasite.data, lines(smooth.spline(x=HostMutationRate, y=FinalDiversityIndex, cv=F),
  lwd=3))


###################################################
### code chunk number 21: DiversityMutation2
###################################################
with(parasite.data, plot(FinalDiversityIndex~HostMutationRate, 
  col=c("blue", "red")[Parasites], pch=c(16,17)[Sex]))
with(parasite.data[parasite.data$Parasites=="Parasites" & parasite.data$Sex=="Sex",], 
  lines(smooth.spline(x=HostMutationRate, y=FinalDiversityIndex, cv=F),
    lwd=3, col="red", lty=1))
with(parasite.data[parasite.data$Parasites=="NoParasites" & parasite.data$Sex=="Sex",], 
  lines(smooth.spline(x=HostMutationRate, y=FinalDiversityIndex, cv=F),
    lwd=3, col="blue", lty=1)) 
with(parasite.data[parasite.data$Parasites=="Parasites" & parasite.data$Sex=="NoSex",], 
  lines(smooth.spline(x=HostMutationRate, y=FinalDiversityIndex, cv=F),
    lwd=3, col="red", lty=3))
with(parasite.data[parasite.data$Parasites=="NoParasites" & parasite.data$Sex=="NoSex",], 
  lines(smooth.spline(x=HostMutationRate, y=FinalDiversityIndex, cv=F),
    lwd=3, col="blue", lty=3))
legend(x=0.15, y=5.45, 
  legend=c("Parasites-Sex", "NoParasites-Sex", "Parasites-NoSex", "NoParasites-NoSex"),
  col=c("red", "blue", "red", "blue"),
  lty=c(1,1,3,3), bty="n", lwd=3)        


###################################################
### code chunk number 22: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:288-295
###################################################
parasite.data$HostMutationRate_centered <- scale(parasite.data$HostMutationRate, 
  center=T, scale=F)
model_5 <- lm(FinalDiversityIndex ~ Parasites + 
  Sex*HostMutationRate_centered, data=parasite.data)

summary(model_5)
confint(model_5)


###################################################
### code chunk number 23: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:314-325
###################################################
model_6 <- lm(FinalDiversityIndex ~ 
  (Parasites + Sex + HostMutationRate_centered)^2 - Parasites:Sex, 
  data=parasite.data)   
  
anova(model_3, model_5, model_6)

print(AIC(model_1, model_2, model_3, model_4, model_5, model_6), 
  digits=4)
  
print(BIC(model_1, model_2, model_3, model_4, model_5, model_6), 
  digits=4)


###################################################
### code chunk number 24: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:336-337
###################################################
avPlots(model_5)


###################################################
### code chunk number 25: CSE845_2013_ANOVA_tutorial_II_April11.Rnw:346-351
###################################################
plot(parasite.data$FinalDiversityIndex ~ fitted(model_5), 
  xlim=c(0,5), ylim=c(0,5), pch=16, col=densCols(fitted(model_5)),
  xlab = "fitted values", ylab="observed values",
  main = "fitted VS. observed model_5")
 abline(a=0, b=1, lwd=2) 


