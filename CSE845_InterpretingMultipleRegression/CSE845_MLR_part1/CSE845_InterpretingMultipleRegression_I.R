### R code from vignette source '~/BEACON_COURSE_2011/CSE891_R_tutorials/CSE845_InterpretingMultipleRegression/CSE845_MLR_part1/CSE845_InterpretingMultipleRegression_I.Rnw'

###################################################
### code chunk number 1: CSE845_InterpretingMultipleRegression_I.Rnw:11-13
###################################################
options(show.signif.stars=F)
options(digits=3)


###################################################
### code chunk number 2: CSE845_InterpretingMultipleRegression_I.Rnw:16-21
###################################################
require(car)
require(arm)
require(effects)
require(asbio)
require(MASS)


###################################################
### code chunk number 3: CSE845_InterpretingMultipleRegression_I.Rnw:24-31
###################################################
# condition number for multi-colinearity
ConditionNumber <- function(X){
    mod.X <- model.matrix(X)
    eigen.x <- eigen(t(mod.X) %*%mod.X)
    eigen.x$val # eigenvalues from the design matrix
    sqrt(max(eigen.x$val)/min(eigen.x$val))
}


###################################################
### code chunk number 4: CSE845_InterpretingMultipleRegression_I.Rnw:98-102
###################################################
sigma = matrix( c(1, 0.6, 0.6, 1), byrow=T, nrow=2, ncol=2)
X <- mvrnorm(100, mu=c(0,0), Sigma=sigma)
colnames(X) <- c("x1", "x2")
X <- data.frame(X)


###################################################
### code chunk number 5: CSE845_InterpretingMultipleRegression_I.Rnw:108-110
###################################################
cor(X)
cov(X) 


###################################################
### code chunk number 6: CSE845_InterpretingMultipleRegression_I.Rnw:115-116
###################################################
plot(X, pch=16)


###################################################
### code chunk number 7: CSE845_InterpretingMultipleRegression_I.Rnw:124-125
###################################################
y <- rnorm(100, 3 +2*X$x1 + 2.5*X$x2, 5)


###################################################
### code chunk number 8: CSE845_InterpretingMultipleRegression_I.Rnw:130-134
###################################################
par(mfrow=c(1,2))
plot(y ~ X$x1, pch=16)
plot( y ~ X$x2, pch=16)
par(mfrow=c(1,1))


###################################################
### code chunk number 9: CSE845_InterpretingMultipleRegression_I.Rnw:140-143
###################################################
model_x1x2  <- lm(y ~ x1 + x2, data=X)
model_x1     <- lm(y ~  x1, data=X)
model_x2     <- lm(y ~ x2, data=X)


###################################################
### code chunk number 10: CSE845_InterpretingMultipleRegression_I.Rnw:146-149
###################################################
coef(model_x1x2)
coef(model_x1)
coef(model_x2)


###################################################
### code chunk number 11: CSE845_InterpretingMultipleRegression_I.Rnw:153-158
###################################################
model_x1_adjusted  <- lm(x1~x2, data=X) 
# accounts for x1|x2
model_adjusted <- lm(model_x2$resid ~ model_x1_adjusted$resid)
coef(model_adjusted)
coef(model_x1x2)


###################################################
### code chunk number 12: CSE845_InterpretingMultipleRegression_I.Rnw:162-163
###################################################
cbind(summary(model_x1x2)$coef[,1:2], confint(model_x1x2))


###################################################
### code chunk number 13: CSE845_InterpretingMultipleRegression_I.Rnw:167-169
###################################################
cbind(summary(model_x1)$coef[,1:2], confint(model_x1))
cbind(summary(model_adjusted)$coef[,1:2], confint(model_adjusted))


###################################################
### code chunk number 14: CSE845_InterpretingMultipleRegression_I.Rnw:178-180
###################################################
vcov(model_x1x2)
cov2cor(vcov(model_x1x2))


###################################################
### code chunk number 15: CSE845_InterpretingMultipleRegression_I.Rnw:184-186
###################################################
cor(X)[1,2]
cov2cor(vcov(model_x1x2))[2,3]


###################################################
### code chunk number 16: CSE845_InterpretingMultipleRegression_I.Rnw:191-193
###################################################
par(mfrow=c(1,1))
confidenceEllipse(model_x1x2, which.coef=c(2,3))


###################################################
### code chunk number 17: CSE845_InterpretingMultipleRegression_I.Rnw:199-201
###################################################
ConditionNumber(model_x1x2)
vif(model_x1x2)


