

# Tutorial to introduce some of the concepts for the t distribution...
# Ian Dworkin, last updated February 6th 2012

# Let's start with sampling observations from a distribution under the null hypothesis of no difference.

# That is, the true population mean is no different than mu.

# This tutorial is designed to explain what the t distribution is about, and how it works.

StandardError <- function(x) {sd(x)/sqrt(length(x))} # Just computing the standard error


# In this function we are going to generate a sample from a normal distribution with known mean and sd. We will then compare it to our "hypothesis" value

# Our default values is that there is no difference between observed and expected means.
SamplingFunction_t <- function(n=100, mean=172, sd=5, hypothesis=169.7) {
 	one.sample.from.population <- rnorm(n,mean=mean, sd=sd)
 	diff <- mean(one.sample.from.population) - hypothesis
 	t    <- diff/StandardError(one.sample.from.population)
 	return(c(mean=mean(one.sample.from.population), SE = StandardError(one.sample.from.population),diff=diff, t=t))
 }


# let's use a simulation to examine this
# Replicating the sampler for t, I have transposed the output, and turned the matrix into a data frame (for ease of plotting)

samples_for_t <- data.frame(t(replicate(1000,expr=SamplingFunction_t())))  # Replicating the sampler for t 


plot(density(samples_for_t$t), lwd=2, xlab="t",xlim=c(-5,10), main="simulated and theoretical values of t") # The distribution of t-values from repeated sampling

# Let's compare this to our theoretical expectation of the t-distribution
curve(dt(x, df= 9),-4,4, add=T, col="red", lwd=2, lty=2) # Note the degrees of freedom (df)!!!!!

# It is important to note this is not quite a standard normal (Z) distribution.
curve(dnorm(x),-4,4, add=T, col="grey", lwd=4, lty=3)

legend(x=6,y=0.3, legend=c("simulated t", "theoretical t", "Z"), col=c("black","red","grey"),lwd=2, lty=c(1,2,3))
abline(v=-2, lty=6) # line at -2 "standard deviations"
abline(v=2, lty=6)

#However as the df increases this will get closer (asymptotically approach) a standard normal distribution.

# Individually or in groups I want you to play with
# A) Sample size and examine the effects of the simulated vs theoretical t values
# B) Change what happens when your observations do not come from the same "distribution" as your "hypothesis".