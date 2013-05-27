# Here is the code that I (Ian) wrote to to answer this. 



# Write a function to compute the Coefficient of variation
CoefVar <- function(x) {
	sd(x)/mean(x)
}


# Write a function to compute the Standard error
StdErr <- function(x) {
	sd(x)/sqrt(length(x))
}



# Here we write a function to generate the random numbers given the sample and then compute the mean, sd, etc.... 

EstimatesLawLargeNumbers <- function(x){
	# generate random numbers from the distribution
	sample_vals <- rnorm(n=x, mean = 20, sd = 6)
	
	# Return the values we want to compute 
	return(c(sample_size = x, 
	  sample_mean = mean(sample_vals), 
	  sample_sd = sd(sample_vals), 
	  sample_cv = CoefVar(sample_vals), 
	  sample_se = StdErr(sample_vals) ))
}

# Create the Vector for the different sample sizes
sample_size <- 5:1000


# Use the sapply function to run the EstimatesLaw... function for each value in the sample_size vector.  the t() is to transpose the matrix (just a convenience for me)
samples <- t(sapply(sample_size, EstimatesLawLargeNumbers, simplify=T))


# Setting the plot up
par(mfrow=c(2,2))

plot(samples[,2] ~ samples[,1], main= "Sample Mean VS sample size", 
    ylab= "sample mean", xlab=" sample size", pch=1)
abline(h=20, lwd=3, col="red") # generates a horizontal line

plot(samples[,3] ~ samples[,1], main= "Sample sd VS sample size", 
    ylab= "sample sd", xlab=" sample size", pch=1 )
abline(h=6, lwd= 3, col="red")

plot(samples[,4] ~ samples[,1], main= "Sample cv VS sample size", 
    ylab= "sample cv", xlab=" sample size", pch=1 )
abline(h=0.3, lwd= 3, col="red")

plot(samples[,5] ~ samples[,1], main= "standard error of the mean VS sample size",
    ylab= "sample SE", xlab=" sample size", pch=1 )
