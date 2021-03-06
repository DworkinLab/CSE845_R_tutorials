
require(car)
require(MASS)
sigma = matrix( c(1, 0.6, 0.6, 1), byrow=T, nrow=2, ncol=2)
X <- MASS::mvrnorm(100, mu=c(0,0), Sigma=sigma)
colnames(X) <- c("x1", "x2")
X <- data.frame(X)

y <- rnorm(100, 3 +2*X$x1 + 2.5*X$x2, 5)
crap <- lm(y ~ x1 + x2, data=X)

# we predict new values for x1, keeping x2 constant (at its mean))

newdata.x1 <- data.frame(x1=seq(round(min(X$x1)), round(max(X$x1)) , by=0.25), x2 = mean(X$x2))

# We predict values of x1, 
predict.crap.x1 <- predict.lm(crap, newdata.x1, interval="confidence")

#pdf("trial.pdf")
par(mfrow=c(2,2))
plot(y~X$x1, main="response ~ predicted for x1 | x2")
lines(predict.crap.x1[,1]~ newdata.x1$x1, col="red")
lines(predict.crap.x1[,2]~ newdata.x1$x1, col="grey", lwd=3)
lines(predict.crap.x1[,3]~ newdata.x1$x1, col="grey", lwd=3)

# The fitted line if we only modeled x1..
crap.lm.x1 <- lm(y~X$x1)
abline(crap.lm.x1, col="black")

plot(y ~ fitted(crap), main="observed ~ fitted")

#dev.off()


newdata.x2 <- data.frame(x2=seq(round(min(X$x2)), round(max(X$x2)) , by=0.25), x1 = mean(X$x1))
# We predict values of x2 
predict.crap.x2 <- predict.lm(crap, newdata.x2, interval="confidence")

plot(y~X$x2, main="response ~ predicted for x2 | x1")
lines(predict.crap.x2[,1]~ newdata.x2$x2, col="red")
lines(predict.crap.x2[,2]~ newdata.x2$x2, col="grey", lwd=3)
lines(predict.crap.x2[,3]~ newdata.x2$x2, col="grey", lwd=3)

# The fitted line if we only modeled x1..
crap.lm.x2 <- lm(y~X$x2)
abline(crap.lm.x2, col="black")

b <- coef(crap)
plot( y ~ I(b[1] + b[2]*X$x1+ b[3]*X$x2))  # Observed VS fitted

### So now let's do it for a perspective plot for both x1 and x2
#...

cov2cor(vcov(crap))

# Some plots in the car library that are also useful.
avPlots(crap)
leveragePlots(crap)
mmps(crap)
crPlots(crap)
residualPlots(crap)
ceresPlots(crap)
rm(list=ls())
