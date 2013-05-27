### CSE845 - Ian Dworkin Feb 14th 2013
## Law of Large numbers example - For regression model.

 ## In the last class I asked you to generate (using simulated data) a plot examining how the estimate and standard error for the slope of a regression changes with increasing sample size.
 
 
 # Here is what I was getting at.....
 
sample.size <- 5:300  # These will be the values of the sample size
length(sample.size)
a <- 5 # true intercept
b <- 0.7 # true slope

# This function (simmie.1)  will input the different "sample sizes" and then run the simulation of the regression given our "true" values for slope and intercept.
simmie.1 <- function(N=sample.size) {
	x <- seq(from=2, to=20, length.out=N) # Generating our x values
	y_fixed <- a + b*x  # The "true" deterministic component of the model generating the data
	y.sim.1 <- rnorm(length(x), mean=y_fixed, sd=5) # generate a sample from the distribution
	y.sim.1.lm <- lm(y.sim.1 ~ x) # using the new sample, re-run the model
  slope <- coef(y.sim.1.lm)[2]  # extracts the slope
  SE.slope <- summary(y.sim.1.lm)$coef[1,2] # extracts the standard error of the slope
  p.slope <- summary(y.sim.1.lm)$coef[1,4] # extracts the pValue for the slope
  CoefDet <- summary(y.sim.1.lm)$r.squared # extracts R^2
  sigma <- summary(y.sim.1.lm)$sigma # extracts Residual error
  return(c(slope = slope, StdErr  = SE.slope, 
    pValueSlope = p.slope, CoefDet = CoefDet, sigma = sigma )) 
  } 
  
# Now all we need to do is run this for different sample sizes!
# The easiest way to do this is with one of the apply functions, here I use sapply
output.estimates <- sapply(X=sample.size, FUN=simmie.1)  

# We transpose our output so that it is easier to use
output.estimates <- t(output.estimates)


# Now we can take a look.
par(mfrow=c(2,1))
plot(output.estimates[,1] ~ sample.size, 
 ylab=expression(paste("estimated slope, ", hat(beta))), 
 xlab="sample size", type="p", cex=0.75, 
 ylim=range(output.estimates[,1]),
 main=expression(paste("How does the estimated slope, ", hat(beta) ," change with sample size?") ))
abline(h=0.7, col="red", lwd=2, lty=3) # The true value of the slope drawn onto the plot

plot(output.estimates[,2] ~ sample.size, 
 ylab=expression(paste("standard error for ", hat(beta))), 
 xlab="sample size", type="p", cex=1, 
 ylim=c(0.0, max(output.estimates[,2])), 
 main=expression(paste("How does the standard error for ",hat(beta) ," change with sample size?")))

#### This is all to illustrate the so-called "law of large numbers"

# "Specifically, the law says that, as the sample grows large, the sample mean converges to the population mean"
# Blume and Royall (2003)



# Let's also look at some other important (and surprising) effects related to sample size.
par(mfrow=c(3,1))
plot(log10(1/output.estimates[,3]) ~ sample.size, 
 ylab="estimated log(1/p-value)", xlab="sample size", type="p", cex=1,  
 main="How does the p-value change with sample size?")

plot(output.estimates[,4] ~ sample.size, ylab=expression(R^2), 
  xlab="sample size", type="p", cex=1, 
  main=expression(paste("How does ", R^2, " change with sample size?")))

lines(smooth.spline(x = sample.size, y = output.estimates[,4]), lwd=3, col="red")

plot(output.estimates[,5] ~ sample.size, ylab=expression(hat(sigma)), 
  xlab="sample size", type="p", cex=1, cex.lab=1.5, 
  main=expression(paste("How does residual standard deviation, ", hat(sigma), " change with sample size?")))
  
lines(smooth.spline(x = sample.size, y = output.estimates[,5]), lwd=3, col="red")