# Ian Dworkin 

# Helping to explain confidence intervals from a simulation point of view...

# Let's start with the view from a sample from a population. 
# Say we sampled 150 individuals from a population
new_sample <- rnorm(n=150, mean=72, sd=4)

plot(density(new_sample, bw=1.5), ylim=c(0,0.15), xlim=c(55,90), lwd=2, xlab="new sample")

curve(dnorm(x,mean=72,sd=4),55,90, add=T, col="red", lty=3, lwd=2) # The "true" population distribution

# In general ~ 67% of observations fall within +/- SD of the mean for a Normal distribution....

# below
abline(v=mean(new_sample) - sd(new_sample), col="grey", lwd=2, lty=5)

# above the mean
abline(v=mean(new_sample) + sd(new_sample), col="grey", lwd=2, lty=5)


# And in general ~96% of observations fall within +/- 2 SD of the mean

abline(v=mean(new_sample) - 2*sd(new_sample), col="grey", lwd=2, lty=6)

# above the mean
abline(v=mean(new_sample) + 2*sd(new_sample), col="grey", lwd=2, lty=6)

legend(x=82,y=0.1, col=c("black", "red", "grey","grey"), lwd=2, lty=c(1,3,5,6), legend=c("data", "theoretical", "1 SD", "2 SD"))


# How is this useful to us?


 # Remember that the sampling distribution of the means (or of other estimates) is approximately normal!!!!
 
 # So if we generate a distribution of sampling means
sample_means <- replicate(1000, expr=mean(rnorm(50,20,5)))
plot(density(sample_means))
 
abline(v=mean(sample_means) - 2*sd(sample_means), col="grey", lwd=2, lty=6)

# above the mean
abline(v=mean(sample_means) + 2*sd(sample_means), col="grey", lwd=2, lty=6)

# How do we interpret this figure? Understanding this is extremely important for understanding inferential statistics....


# Question 1A - What do you expect to happen to the standard deviation for the distribution of sample_means as the sample size decreases from 50 to 20? 
sd(sample_means)
sample_means2 <- replicate(1000, expr=mean(rnorm(20,20,5)))
sd(sample_means2)

# Question 1B - What does this do to the confidence intervals, Why?  

# Question 1C - Explain why this (changes in sample size) is useful for inferential statistics.



################## Understanding the classic definition of confidence intervals based on sampling theory

### Let us again repeatedly sample from a distribution with mean 20, and sd=5

StandardError <- function(x) {sd(x)/sqrt(length(x))} # Just computing the standard error


# In this function we are going to generate a sample from a normal distribution with known mean and sd. We will then return the mean and standard error from that sample.

SamplingFunction <- function(n=20, mean=20, sd=5) {
 	one.sample.from.population <- rnorm(n,mean=mean, sd=sd)
 	return(c(mean=mean(one.sample.from.population), SE = StandardError(one.sample.from.population)))
 }

# the t() is for transpose, which just transposes the matrix generated.
samples_for_CI <- t(replicate(100,expr=SamplingFunction()))  # Replicating the sampler

#Now we generate the approximate 95% confidence intervals.
lower <- (samples_for_CI[,1]) - (1.96*samples_for_CI[,2] )
# The mean minus 1.96*the standard error of the mean.
upper <- (samples_for_CI[,1]) + (1.96*samples_for_CI[,2] )
# The mean plus 1.96*the standard error of the mean.


replicates <- 1:length(samples_for_CI[,1]) # just creating a variable for the number of replicates.

plot(samples_for_CI[,1] ~ replicates, ylab="sample values", 
    ylim=c(min(lower), max(upper)), # Just setting the lower and upper values of the Y axis
    main=" Demonstrating the meaning of (frequentist) confidence intervals")


# generate Confidence Intervals
for ( i in 1:length(samples_for_CI[,2])) {
  lines(x=c(replicates[i],replicates[i] ), y=c(lower[i],upper[i]), lwd=2.75, col="grey")  
}

# Given that we know the true value for the mean (20 in this case, unless you changed it.) We can plot this on
abline(h=20, col="red", lwd=3, lty=4) 


# Please answer the following questions.

# Question 2A - Change the number of observations per sample (currently set at n=50) to 20. What happens to the interval size as you decrease or increase this? (Generate some nice plots for my simple mind). Explain why.

# Question 2B - Is the interval size for our estimates confidence intervals the same for each simulated sample? Explain why or why not.


# Question 3A - Approximately what proportion of the time do the confidence intervals for the sample (with n=50) overlap with the "true" population paramter (in this example, 20)? 
# Question 3B - What would you expect this proportion to converge upon if you did 100,000 simulated samples (each sample with 50 observations)? 

# Question 4 - Instead of using the approximate confidence intervals (based on 1.96*Standard error), use the generalized confidence interval function (written below) to do the plot of the confidence intervals that you used from question 2. This may take a bit of re-coding, so you may want to write it in a fresh script.

ConfidenceIntervalMean <- function(data, alpha=0.05){
	data <- na.omit(data) # removing missing data!!!
	mean_data <- mean(data)
	std_err <- sd(data)/sqrt(length(data))
	lower_CI <- mean(data) + std_err*(qt(p=alpha/2,df=length(data)-1))
	upper_CI <- mean(data) + std_err*(qt(p=(1- alpha/2),df=length(data)-1))
	return(c(mean=mean_data, StdErr=std_err, UpperCI=upper_CI, Lower.CI=lower_CI))
	}
