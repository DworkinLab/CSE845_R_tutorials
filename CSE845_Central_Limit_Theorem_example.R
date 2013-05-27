# Illustrating the central limit theorem

# Let's look at the quote from Vasihsth and Broe (2010) again:

#"Provided the sample size is large enough, the sampling distribution 
#  of the sample mean will be close to normal irrespective of what the 
#  population’s distribution looks like."


# What is this telling us?

# We are of course used to seeing a normal distribution of data...
par(mfrow=c(2,1))
curve(dnorm(x,mean=0, sd=1), -5,5, lwd=2, 
   ylab='~density', main=" ye olde Normal/Gaussian Distribution")

# So it is of no great surprise that if repeatedly generate samples from this distribution, calculate the sample means, and then draw a histogram of the sample means, we get another approximately normal distribution for the sample means.

random_normal_sample_means <- replicate(1000, expr=mean(rnorm(n=50, mean=0, sd=1)))

plot(density(random_normal_sample_means), xlim=c(-5,5), main="hey why is this distribution so much narrower?", lwd=2, xlab="x")
hist(random_normal_sample_means, add=T, col="grey", freq=F)


# Because it is the distribution of the sampling means!!!!




####### But what if our data does not look particularly normal? Here we try a poisson distribution (discrete, bounded by zero.)
par(mfrow=c(2,1))
random_poisson_data <- rpois(n=10000, lambda=1)
hist(random_poisson_data ,freq=F)


# What do we expect to see if we look at the sampling distribution of the means for this distribution?
random_poisson_sample_means <- replicate(10000, expr=mean(rpois(n=50, lambda=1)))

plot(density(random_poisson_sample_means),  main="This looks pretty normal....", lwd=2, xlab="x")
hist(random_poisson_sample_means, add=T, col="grey", freq=F)

# INdividually or in groups, play with this, you can try some other distributions (gamma, beta, binomial...), play with them and see what you get....

# We can also try exponential

par(mfrow=c(2,1))
random_exponential_sample <- rexp(1000, rate = 1)

plot(density(random_exponential_sample), ylim=c(0,1))
hist(random_exponential_sample, add=T, col="grey", freq=F)


random_exponential_sample_means <- replicate(10000, expr=mean(rexp(n=1000, rate =1)))

plot(density(random_exponential_sample_means ),  main="This looks pretty normal....", lwd=2, xlab="x")
hist(random_exponential_sample_means , add=T, col="grey", freq=F)