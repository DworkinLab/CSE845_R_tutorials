# Source file for CSE891 of simple useful functions. I will add more as the class goes on.
# Ian Dworkin - Last updated March 01 2011

# co-efficient of variation
cv.fun <- function(x) {
	       x <- na.omit(x)
	       sd(x)/abs(mean(x))}
comment(cv.fun) <- "cv, calculates co-efficient of variation, given an input vector of numeric observations"
	
	
# Standard error of the mean  
se.fun <- function(x) {
	        x <- na.omit(x)
	        sd(x)/sqrt(length(x))}
comment(se.fun) <- "se, calculates the simple standard error for a numeric vector of observations"     


# calculates bandwith to use (page 164 in the R book) for the kernel density estimator.
# a function scaling the difference between the max and min values by a function of number of observations
bandwidth.fun <- function(vector) 
{
	vector <- na.omit(vector)
	(max(vector) - min (vector)) / (2*(1+log(length(vector),2))) 
}
comment(bandwidth.fun) <- c("This calculates the 'rule of thumb' bandwidth advocated by Michael Crawley and Venables and Ripley", " page 164, Chapter 5 in The R Book")

	   
# Approximate mode of a continuous distribution for a kernel density estimator. 
approx_mode.fun <- function(x)
{
   x <- na.omit(x)
   den <- density(x) # approximate density of posterior
   den.1 <- den[[2]] # extract heights of approximate density, double brackets [[]] extracts from list 
   max.den <- as.numeric(which.max(den.1)) # find the index for the maximum height of the density
   den.2 <- den[[1]] # extract data values corresponding to density
   den.2[max.den] # Approximate mode 
}
comment(approx_mode.fun) <-"This is very approximate, and will provide different estimates based on bandwidths used in the density"


#### Just a quick way of comparing some basic raw data plots.
data_plotter.fun <- function(x) 
{
    par(mfrow=c(1,3)) # Setting up a 2x2 graphical window
   
    x <- na.omit(x) # removes missing data
       
    hist(x) # histogram of the data
    abline(v=mean(x), col="red") # plots vertical line for the mean
    abline(v=median(x), col="blue", lty=2) # median
    abline(v=approx_mode.fun(x), col="black") # approximate mode
    
    plot(density(x)) # Approximate density of the data
    abline(v=mean(x), col="red") # plots vertical line for the mean
    abline(v=median(x), col="blue", lty=2) # median
    abline(v=approx_mode.fun(x), col="black") # approximate mode
    
    quant.data <- quantile(x) ### Data quantiles, for the boxplot

    boxplot(x, ylab="The data", main= "An annotated box plot of the data")
    arrows(1.34, median(x), 1.22, median(x), lwd = 1, angle=15, col="blue") # Draws an arrow, pointing to median
    text(1.40 ,median(x),"median", col="blue") # Adds text to the graph
    arrows(1.34, mean(x), 1.22, mean(x), lwd = 1, angle=15, col="red")
    text(1.4 ,mean(x),"mean", col="red")
    arrows(1.34, approx_mode.fun(x), 1.22, approx_mode.fun(x), lwd = 1, angle=15, col="black")
    text(1.4 ,approx_mode.fun(x),"mode", col="black")
    
    ### For the upper and lower quantile
    arrows(1.34, quant.data[2], 1.22, quant.data[2], lwd = 1, angle=15, col="grey")
    text(1.44 ,quant.data[2],"lower quantile")

    arrows(1.34, quant.data[4], 1.22, quant.data[4], lwd = 1, angle=15, col="grey")
    text(1.44 ,quant.data[4],"upper quantile")


	}
comment(data_plotter.fun) <- "Provide a vector of numeric data, and it outputs few useful summary graphs"



confidence.interval.mean <- function(data, alpha=0.05){
	data <- na.omit(data)
	mean.data <- mean(data)
	StdErr <- sd(data)/sqrt(length(data))
	Lower.CI <- mean(data) + StdErr*(qt(p=alpha/2,df=length(data)-1))
	Upper.CI <- mean(data) + StdErr*(qt(p=(1- alpha/2),df=length(data)-1))
	return(c(mean=mean.data,StdErr=StdErr, Upper.CI=Upper.CI, Lower.CI=Lower.CI))
	}
comment(confidence.interval.mean) <- "THis usually the generalized t approach for the confidence interval"	