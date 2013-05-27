x <- seq(1,10,by =0.1)
y <- rnorm(length(x), mean= 6 + 2*x - 1.5*(x^2) + 0.3*(x^3),  sd=1.5*x)
plot(y~x,
    ylab="response", xlab="covariate", main="What is the underlying process generating this data?")

# A few examples of non-parametric curve fitting (splines)

lines(spline(x,y),  lwd=3, col="grey")
lines(lowess(y~x), lwd=2)
lines(smooth.spline(x,y),lwd=2, lty=2)
text(x=8,y=10, "non-parametric models")

# But we are interested in fitting a data model.
model_1 <- lm(y ~ poly(x,3))
param <- coef(model_1)
lines(x=x, y = fitted(model_1), col="purple", lwd=2)

model_2 <- lm( y ~ x)
lines(x=x, y =fitted(model_2), col="purple", lwd =2, lty=2)
text(x=8,y=0, "Parametric models", col="purple")

model_3 <- lm( y ~ poly(x,2))
lines(x=x, y = fitted(model_3), col="blue", lwd=2)

model_4 <- lm(y~ poly(x, 3))
lines(x=x, y = fitted(model_4), col="blue", lwd=2, lty=2)

model_26 <- lm(y~ poly(x, 26))
lines(x=x, y = fitted(model_26), col="red", lwd=2)

text(x=3.25, y=(max(y) -5), 
    expression(paste("~N( ", mu, " = ", 6 + 2*x -1.5*x^2 +0.3*x^3, ", ",sigma, " = 1.5x)")) )
    
# But what happens with a new set of values generated from the same process

y2 <- rnorm(length(x), mean= 6 + 2*x - 1.5*(x^2) + 0.3*(x^3),  sd=1.5*x)

plot(y2~x, ylab="response", xlab="covariate", main="What happens to the model fit with new data (but same process)")
lines(spline(x,y),  lwd=3, col="grey")
lines(lowess(y~x), lwd=2)
lines(smooth.spline(x,y),lwd=2, lty=2)

lines(x=x, y = fitted(model_1), col="purple", lwd=2, lty=2)
lines(x=x, y = fitted(model_2), col="purple", lwd =2, lty=1)
lines(x=x, y = fitted(model_3), col="blue", lwd=2)
lines(x=x, y = fitted(model_4), col="blue", lwd=2, lty=2)
lines(x=x, y = fitted(model_26), col="red", lwd=2)
    