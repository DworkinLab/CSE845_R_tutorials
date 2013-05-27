### R code from vignette source '~/BEACON_COURSE_2011/CSE891_R_tutorials/CSE891_IntroRegression/CSE845_2013_LM_tutorial1_inClass.Rnw'

###################################################
### code chunk number 1: CSE845_2013_LM_tutorial1_inClass.Rnw:8-11
###################################################
options(show.signif.stars=F)
options(digits=3)
source('http://beaconcourse.pbworks.com/w/file/fetch/36678466/Source_useful_R_function_ID_Feb24_2011.R')


###################################################
### code chunk number 2: CSE845_2013_LM_tutorial1_inClass.Rnw:23-24
###################################################
parasite_data <- read.csv("http://beaconcourse.pbworks.com/w/file/fetch/37890621/sexAsex.csv", h=T)


###################################################
### code chunk number 3: CSE845_2013_LM_tutorial1_inClass.Rnw:31-32
###################################################
str(parasite_data)


###################################################
### code chunk number 4: CSE845_2013_LM_tutorial1_inClass.Rnw:37-42
###################################################
no_parasites_no_sex <- subset(parasite_data, Parasites==0 & Sex==0)

str(no_parasites_no_sex)

summary(no_parasites_no_sex)


###################################################
### code chunk number 5: CSE845_2013_LM_tutorial1_inClass.Rnw:50-66
###################################################

par(mfrow=c(2,1))
plot(FinalDiversityIndex ~ factor(HostMutationRate), 
  data=no_parasites_no_sex, 
  main="Relationship between diversity and mutation rate",
  xlab = "HostMutationRate" )

plot(FinalDiversityIndex ~ HostMutationRate, 
  data=no_parasites_no_sex, 
  main="Relationship between diversity and mutation rate")
  
lines(smooth.spline(y=no_parasites_no_sex$FinalDiversityIndex,
  x=no_parasites_no_sex$HostMutationRate), col="red", lwd=2)

lines(lowess(y=no_parasites_no_sex$FinalDiversityIndex,
  x= no_parasites_no_sex$HostMutationRate), col="blue", lwd=2)


###################################################
### code chunk number 6: CSE845_2013_LM_tutorial1_inClass.Rnw:96-100
###################################################
model_1 <- lm(FinalDiversityIndex ~ HostMutationRate, 
  data=no_parasites_no_sex)

summary(model_1)


###################################################
### code chunk number 7: CSE845_2013_LM_tutorial1_inClass.Rnw:110-111
###################################################
confint(model_1)


###################################################
### code chunk number 8: CSE845_2013_LM_tutorial1_inClass.Rnw:119-123
###################################################
par(mfrow=c(1,1))
plot(FinalDiversityIndex ~ HostMutationRate, 
  data=no_parasites_no_sex, pch=16)
abline(model_1, lwd=3, col="purple")


###################################################
### code chunk number 9: CSE845_2013_LM_tutorial1_inClass.Rnw:133-135
###################################################
new.dat <- data.frame(HostMutationRate= seq(from=0, to=1 , by=0.01))
pred.data <- predict(model_1, new.dat, interval="confidence")


###################################################
### code chunk number 10: CSE845_2013_LM_tutorial1_inClass.Rnw:141-147
###################################################
par(mfrow=c(1,1))
plot(FinalDiversityIndex ~ HostMutationRate, 
  data=no_parasites_no_sex, pch=16)
lines(x=new.dat[,1], y=pred.data[,1], col="purple", lwd=4)
lines(x=new.dat[,1], y=pred.data[,2], col="purple", lwd=4, lty=2)
lines(x=new.dat[,1], y=pred.data[,3], col="purple", lwd=4, lty=2)


###################################################
### code chunk number 11: CSE845_2013_LM_tutorial1_inClass.Rnw:157-162
###################################################
plot(y =no_parasites_no_sex$FinalDiversityIndex, 
  x= fitted(model_1), xlim=c(0,5), ylim=c(0,5),
  ylab = "observed values of response",
  xlab = expression(paste("fitted values of response, ", hat(y))))
abline(a=0, b=1)


###################################################
### code chunk number 12: CSE845_2013_LM_tutorial1_inClass.Rnw:173-174
###################################################
hist(resid(model_1))


###################################################
### code chunk number 13: CSE845_2013_LM_tutorial1_inClass.Rnw:183-186
###################################################
par(mfrow=c(2,2))
plot(model_1)
par(mfrow=c(1,1))


###################################################
### code chunk number 14: CSE845_2013_LM_tutorial1_inClass.Rnw:206-209
###################################################
model_2 <- lm(FinalDiversityIndex ~ 1 + HostMutationRate + I(HostMutationRate^2), 
  data=no_parasites_no_sex)
summary(model_2)  


###################################################
### code chunk number 15: CSE845_2013_LM_tutorial1_inClass.Rnw:214-215
###################################################
anova(model_1, model_2)


###################################################
### code chunk number 16: CSE845_2013_LM_tutorial1_inClass.Rnw:220-221
###################################################
anova(model_2)


###################################################
### code chunk number 17: CSE845_2013_LM_tutorial1_inClass.Rnw:226-228
###################################################
AIC(model_1)
AIC(model_2)


###################################################
### code chunk number 18: CSE845_2013_LM_tutorial1_inClass.Rnw:235-237
###################################################
new.dat.1 <- data.frame(HostMutationRate= seq(from=0, to=1 , by=0.01))
pred.data.1 <- predict(model_2, new.dat, interval="confidence")


###################################################
### code chunk number 19: CSE845_2013_LM_tutorial1_inClass.Rnw:244-250
###################################################
par(mfrow=c(1,1))
plot(FinalDiversityIndex ~ HostMutationRate, 
  data=no_parasites_no_sex, pch=16)
lines(x=new.dat.1[,1], y=pred.data.1[,1], col="purple", lwd=4)
lines(x=new.dat.1[,1], y=pred.data.1[,2], col="purple", lwd=4, lty=2)
lines(x=new.dat.1[,1], y=pred.data.1[,3], col="purple", lwd=4, lty=2)


###################################################
### code chunk number 20: CSE845_2013_LM_tutorial1_inClass.Rnw:258-264
###################################################
plot(y =no_parasites_no_sex$FinalDiversityIndex, 
  x= fitted(model_2), xlim=c(0,5), ylim=c(0,5),
  main = "Model with linear and quadratic terms",
  ylab = "observed values of response",
  xlab = expression(paste("fitted values of response, ", hat(y))))
abline(a=0, b=1, lwd=2)


