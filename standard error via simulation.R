# The concept behind the standard error (using the standard error of the mean as an example)
# Revised Feb3rd 2013


# making it clear what the standard error is, and where it comes from!

# The standard error of a statistic is really about providing information about what would happen if you repeatedly sampled from a population and calculated your sample statistic or estimator. It helps relate the quantities you compute from your sample and how they can be used as estimates of the population parameters of interest. 

x <- rnorm(n = 5, mean = 10,sd = 1) # produces a random sample of 500 observations from a normally distributed variable with mean=10 and sd=1


par(mfrow=c(2,1))
plot(density(x), xlim=c(min(x),max(x)), main="distribution of a single sample of 500 observations") # Quick look at the distribution of the data
hist(x)
# we can look at some basic moments of the distribution (mean and standard deviation = sqrt(variance))
mean(x)
sd(x)

# We can also calculate the standard error of the mean.


sd(x)/sqrt(5) # estimate of Standard error of the mean. But what does this mean.


# x is just a single sample (which has 500 observations in it). We can estimate the mean (and sd), but we may not know how good an estimate it is. How might we determine this?
# An alternative way of stating the same question might be " If we were to repeat the sampling process, and generate another sample of 500 observations from the same population, how similar would the estimated mean for the second sample be to the first sample? If we repeated this sampling process over and over again, how variable would the estimated means be?"

# THis is exactly what the standard error is all about, a way to quantify the uncertainty in our estimates. 
# It is fundamentally a statement about what we would expect to see if we repeated the sampling process a very large number of times. If we were able to do so (like we can do for these simulations), then we can examine this (sampling) distribution, and ask how variable our estimates are (by calculating the standard deviation from the sampling distribution )




# Below is a little function that calculates the mean random sample of 500 observations from a normally distributed variable with mean=10 and sd=1
Dist1 <- function(n=5, mean=10, sd=1){
  x <- rnorm(n=n, mean=mean, sd=sd)
  y <- mean(x) # Computes the mean of our randomly generated values.
  y}
 

sample.means <- replicate(1000, Dist1()) # This replicates the function 1000 times. i.e. it repeatedly samples 500 individuals from a population and estimates the mean for 1000 samples.

plot(density(sample.means), xlim=c(min(x),max(x)), 
    main="sampling distribution for the means") 
    # histogram of means from the repeated samples

# What do you notice about the differences in the variation between these distributions? What is the same?


# Now let's compare the standard deviation from this
sd(sample.means) # similar to the SEM, via repeated sampling.
sd(x)/sqrt(500)

 
# Thus the Standard error of the mean is an estimate of the precision of the mean based on what would hypothetically happen if we sampled a large number of times from the same population, and repeatedely estimated the mean. IF we did this and estimated the standard deviation among the sample means, this would be our standard error of the mean (SEM)

t.test(x, x2)