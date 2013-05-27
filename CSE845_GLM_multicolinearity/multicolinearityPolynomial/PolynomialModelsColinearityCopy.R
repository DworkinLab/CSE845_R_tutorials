### R code from vignette source '~/BEACON_COURSE_2011/CSE891_R_tutorials/CSE891_GLM_multicolinearity/multicolinearityPolynomial/PolynomialModelsColinearityCopy.Rnw'

###################################################
### code chunk number 1: PolynomialModelsColinearityCopy.Rnw:8-11
###################################################
#setting options for prettier printing
options(show.signif.stars=F)
options(digits=3)


###################################################
### code chunk number 2: PolynomialModelsColinearityCopy.Rnw:27-28
###################################################
require(car)


###################################################
### code chunk number 3: PolynomialModelsColinearityCopy.Rnw:34-36
###################################################
parasite.data <- read.csv('http://beaconcourse.pbworks.com/f/sexAsex.csv', h=T)
noParasitesNoSex <- subset(parasite.data, Parasites==0 & Sex==0)


###################################################
### code chunk number 4: PolynomialModelsColinearityCopy.Rnw:41-59
###################################################
par(mfrow=c(2,1))
plot(FinalDiversityIndex ~ HostMutationRate, data=noParasitesNoSex, 
  main="Relationship between Diversity and mutation rate")
lines(smooth.spline(y=noParasitesNoSex$FinalDiversityIndex ,
  x=noParasitesNoSex$HostMutationRate), col="red")
lines(lowess(y=noParasitesNoSex$FinalDiversityIndex ,
  x= noParasitesNoSex$HostMutationRate), col="blue")
   
model_2 <- lm(FinalDiversityIndex ~ HostMutationRate + I(HostMutationRate^2), 
  data=noParasitesNoSex)
new.dat.1 <- data.frame(HostMutationRate= seq(from=0, to=1 , by=0.01))
pred.data.1 <- predict(model_2, new.dat.1, interval="confidence")

plot(FinalDiversityIndex ~ HostMutationRate, 
  data=noParasitesNoSex, pch=16)
lines(x=new.dat.1[,1], y=pred.data.1[,1], col="purple", lwd=4)
lines(x=new.dat.1[,1], y=pred.data.1[,2], col="purple", lwd=4, lty=2)
lines(x=new.dat.1[,1], y=pred.data.1[,3], col="purple", lwd=4, lty=2)


###################################################
### code chunk number 5: PolynomialModelsColinearityCopy.Rnw:67-72
###################################################
par(mfrow=c(1,1))
plot(jitter(noParasitesNoSex$HostMutationRate), 
 jitter(I(noParasitesNoSex$HostMutationRate^2)),
 xlab= "Host Mutation rate", ylab = "(Host Mutation rate)^2",
 main = "Houston... we have a problem.." ) 


###################################################
### code chunk number 6: PolynomialModelsColinearityCopy.Rnw:81-83
###################################################
par(mfrow=c(1,1))
confidenceEllipse(model_2) 


###################################################
### code chunk number 7: PolynomialModelsColinearityCopy.Rnw:89-90
###################################################
vcov(model_2)


###################################################
### code chunk number 8: PolynomialModelsColinearityCopy.Rnw:95-97
###################################################
sqrt(vcov(model_2)[1,1])
summary(model_2)$coef[1,2]


###################################################
### code chunk number 9: PolynomialModelsColinearityCopy.Rnw:101-103
###################################################
sqrt(vcov(model_2)[2,2])
summary(model_2)$coef[2,2]


###################################################
### code chunk number 10: PolynomialModelsColinearityCopy.Rnw:108-110
###################################################
sqrt(vcov(model_2)[3,3])
summary(model_2)$coef[3,2]


###################################################
### code chunk number 11: PolynomialModelsColinearityCopy.Rnw:115-116
###################################################
cov2cor(vcov(model_2))


###################################################
### code chunk number 12: PolynomialModelsColinearityCopy.Rnw:125-127
###################################################
model_1 <- lm(FinalDiversityIndex ~ HostMutationRate, 
  data=noParasitesNoSex)


###################################################
### code chunk number 13: PolynomialModelsColinearityCopy.Rnw:132-133
###################################################
vcov(model_1)


###################################################
### code chunk number 14: PolynomialModelsColinearityCopy.Rnw:138-141
###################################################
sqrt(vcov(model_1)[1,1]) # se for intercept
sqrt(vcov(model_1)[2,2]) # se for slope
summary(model_1)$coef[ ,1:2]


###################################################
### code chunk number 15: PolynomialModelsColinearityCopy.Rnw:146-147
###################################################
cov2cor(vcov(model_1))


###################################################
### code chunk number 16: PolynomialModelsColinearityCopy.Rnw:153-155
###################################################
par(mfrow=c(1,1))
confidenceEllipse(model_1) 


###################################################
### code chunk number 17: PolynomialModelsColinearityCopy.Rnw:169-183
###################################################
a=5  # intercept
b=0.7  # slope
x <- seq(5,25)
y_fixed <- a + b*x
y.sim.0 <- rnorm(length(x), mean=y_fixed, sd=4)
lm.sim <- lm(y.sim.0 ~ x)

par(mfrow=c(2,1))
plot(y.sim.0 ~ x, xlim=c(0, 25), ylim=c(0, 25),
 main = "relationship between y and x in observational space")
abline(lm.sim, lwd=2, col="red")

confidenceEllipse(lm.sim, 
  main = "confidence ellipse in parameter space")


###################################################
### code chunk number 18: PolynomialModelsColinearityCopy.Rnw:190-199
###################################################
simmie.1 <- function() {
	y.sim.1 <- rnorm(length(x), mean=y_fixed, sd=4) 
	 # generate a sample from the distribution
	y.sim.1.lm <- lm(y.sim.1 ~ x) # using the new sample, re-run the model
    coef(y.sim.1.lm)  # extract the coefficients from this new model
  }

coef.sim <- t(replicate(n=1000, simmie.1())) 
  # repeatedly call the function n times.


###################################################
### code chunk number 19: PolynomialModelsColinearityCopy.Rnw:204-212
###################################################
par(mfrow=c(2,1))
confidenceEllipse(lm.sim, 
  main = "confidence ellipse in parameter space")

plot(coef.sim[,2] ~ coef.sim[,1], pch=16, col="red",
  xlab= " estimated intercepts from sim",
  ylab = "estimated slopes from sim",
  main = " simulated slopes and intercepts")


###################################################
### code chunk number 20: PolynomialModelsColinearityCopy.Rnw:221-224
###################################################
x.centered <- x - mean(x)
lm.sim.cent <- lm(y.sim.0 ~ x.centered)
summary(lm.sim.cent)$coef


###################################################
### code chunk number 21: PolynomialModelsColinearityCopy.Rnw:227-228
###################################################
summary(lm.sim)$coef


###################################################
### code chunk number 22: PolynomialModelsColinearityCopy.Rnw:233-237
###################################################
par(mfrow=c(1,2))
plot(y.sim.0 ~ x.centered)
abline(lm.sim.cent, col ="red", lwd=2)
confidenceEllipse(lm.sim.cent)


###################################################
### code chunk number 23: PolynomialModelsColinearityCopy.Rnw:243-244
###################################################
cov2cor(vcov(lm.sim.cent))


###################################################
### code chunk number 24: PolynomialModelsColinearityCopy.Rnw:253-259
###################################################

host_mutation_rate_centered <- with(noParasitesNoSex, HostMutationRate - mean(HostMutationRate))

mean(host_mutation_rate_centered)

plot(jitter(host_mutation_rate_centered), jitter(I(host_mutation_rate_centered^2)))


###################################################
### code chunk number 25: PolynomialModelsColinearityCopy.Rnw:265-269
###################################################
model_2_centered <- lm(FinalDiversityIndex ~ host_mutation_rate_centered + 
  I(host_mutation_rate_centered^2), data=noParasitesNoSex)
summary(model_2)$coef
summary(model_2_centered)$coef


###################################################
### code chunk number 26: PolynomialModelsColinearityCopy.Rnw:274-277
###################################################
par(mfrow=c(1,2))
confidenceEllipse(model_2)
confidenceEllipse(model_2_centered)


###################################################
### code chunk number 27: PolynomialModelsColinearityCopy.Rnw:284-285
###################################################
cov2cor(vcov(model_2))


###################################################
### code chunk number 28: PolynomialModelsColinearityCopy.Rnw:289-290
###################################################
cov2cor(vcov(model_2_centered))


