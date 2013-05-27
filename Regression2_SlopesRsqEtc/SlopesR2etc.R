### R code from vignette source '/Users/ian/BEACON_COURSE_2011/CSE891_R_tutorials/Regression2_SlopesRsqEtc/SlopesR2etc.Rnw'

###################################################
### code chunk number 1: SlopesR2etc.Rnw:9-12
###################################################
#setting options for prettier printing
options(show.signif.stars=F)
options(digits=3)


###################################################
### code chunk number 2: SlopesR2etc.Rnw:21-27
###################################################
x <- 1:50
y1 <- rnorm(length(x), 2 + 0.5*x, sd=4)
y2 <- rnorm(length(x), 2 + 2*x, sd=4)
y3 <- rnorm(length(x), 2 + 0.5*x, sd=8 )
y4 <- rnorm(length(x), 2 + 2*x, sd=8)
y5 <- rnorm(length(x), 2 + 0.5*x, sd =12)


###################################################
### code chunk number 3: SlopesR2etc.Rnw:35-37
###################################################
model_1 <- lm(y1 ~ x)
summary(model_1)


###################################################
### code chunk number 4: SlopesR2etc.Rnw:43-57
###################################################
new.dat.1 <- data.frame(x=0:50)
pred.data.1 <- predict(model_1, new.dat.1, interval="confidence")
par(mfrow=c(2,1))
plot(y1~x, pch=16, main = "y1 ~ N(2 + 0.5x, sd = 4)")
lines(x=new.dat.1[,1], y=pred.data.1[,1], col="purple", lwd=3)
lines(x=new.dat.1[,1], y=pred.data.1[,2], col="purple", lwd=3, lty=2)
lines(x=new.dat.1[,1], y=pred.data.1[,3], col="purple", lwd=3, lty=2)

plot(y1 ~ fitted(model_1),
  ylab = "observed values of y",
  xlab = expression(paste("fitted values (", hat(y), ")")),
  ylim=c(0, max(y1)), xlim=c(0, max(y1)), pch=16
  )
abline(a=0, b=1, lwd=2, col="red")


###################################################
### code chunk number 5: SlopesR2etc.Rnw:64-66
###################################################
model_2 <- lm(y2 ~ x)
summary(model_2)


###################################################
### code chunk number 6: SlopesR2etc.Rnw:71-85
###################################################
new.dat.2 <- data.frame(x=0:50)
pred.data.2 <- predict(model_2, new.dat.1, interval="confidence")
par(mfrow=c(2,1))
plot(y2~x, pch=16, main = "y1 ~ N(2 + 2x, sd = 4)")
lines(x=new.dat.2[,1], y=pred.data.2[,1], col="purple", lwd=3)
lines(x=new.dat.2[,1], y=pred.data.2[,2], col="purple", lwd=3, lty=2)
lines(x=new.dat.2[,1], y=pred.data.2[,3], col="purple", lwd=3, lty=2)

plot(y2 ~ fitted(model_2),
  ylab = "observed values of y",
  xlab = expression(paste("fitted values (", hat(y), ")")),
  ylim=c(0, max(y2)), xlim=c(0, max(y2)), pch=16
  )
abline(a=0, b=1, lwd=2, col="red")


###################################################
### code chunk number 7: SlopesR2etc.Rnw:92-94
###################################################
model_3 <- lm(y3 ~ x)
summary(model_3)


###################################################
### code chunk number 8: SlopesR2etc.Rnw:99-113
###################################################
new.dat.3 <- data.frame(x=0:50)
pred.data.3 <- predict(model_3, new.dat.1, interval="confidence")
par(mfrow=c(2,1))
plot(y3~x, pch=16, main = "y1 ~ N(2 + 0.5x, sd = 8)")
lines(x=new.dat.3[,1], y=pred.data.3[,1], col="purple", lwd=3)
lines(x=new.dat.3[,1], y=pred.data.3[,2], col="purple", lwd=3, lty=2)
lines(x=new.dat.3[,1], y=pred.data.3[,3], col="purple", lwd=3, lty=2)

plot(y3 ~ fitted(model_3),
  ylab = "observed values of y",
  xlab = expression(paste("fitted values (", hat(y), ")")),
  ylim=c(0, max(y3)), xlim=c(0, max(y3)), pch=16
  )
abline(a=0, b=1, lwd=2, col="red")  


###################################################
### code chunk number 9: SlopesR2etc.Rnw:120-122
###################################################
model_4 <- lm(y4 ~ x)
summary(model_4)


###################################################
### code chunk number 10: SlopesR2etc.Rnw:127-141
###################################################
new.dat.4 <- data.frame(x=0:50)
pred.data.4 <- predict(model_4, new.dat.4, interval="confidence")
par(mfrow=c(2,1))
plot(y4~x, pch=16, main = "y1 ~ N(2 + 2x, sd = 8)")
lines(x=new.dat.4[,1], y=pred.data.4[,1], col="purple", lwd=3)
lines(x=new.dat.4[,1], y=pred.data.4[,2], col="purple", lwd=3, lty=2)
lines(x=new.dat.4[,1], y=pred.data.4[,3], col="purple", lwd=3, lty=2)

plot(y4 ~ fitted(model_4),
  ylab = "observed values of y",
  xlab = expression(paste("fitted values (", hat(y), ")")),
  ylim=c(0, max(y4)), xlim=c(0, max(y4)), pch=16
  )
abline(a=0, b=1, lwd=2, col="red") 


###################################################
### code chunk number 11: SlopesR2etc.Rnw:148-150
###################################################
model_5 <- lm(y5 ~ x)
summary(model_5)


###################################################
### code chunk number 12: SlopesR2etc.Rnw:155-169
###################################################
new.dat.5 <- data.frame(x=0:50)
pred.data.5 <- predict(model_5, new.dat.5, interval="confidence")
par(mfrow=c(2,1))
plot(y5~x, pch=16, main = "y1 ~ N(2 + 0.5x, sd = 12)")
lines(x=new.dat.5[,1], y=pred.data.5[,1], col="purple", lwd=3)
lines(x=new.dat.5[,1], y=pred.data.5[,2], col="purple", lwd=3, lty=2)
lines(x=new.dat.5[,1], y=pred.data.5[,3], col="purple", lwd=3, lty=2)

plot(y5 ~ fitted(model_5),
  ylab = "observed values of y",
  xlab = expression(paste("fitted values (", hat(y), ")")),
  ylim=c(0, max(y5)), xlim=c(0, max(y5)), pch=16
  )
abline(a=0, b=1, lwd=2, col="red") 


