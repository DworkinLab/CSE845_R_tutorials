# Updated Jan 10th 2012, syntax updated for style guide.

ConfidenceIntervalMean <- function(data, alpha=0.05){
	data <- na.omit(data) # removing missing data!!!
	mean_data <- mean(data)
	std_err <- sd(data)/sqrt(length(data))
	lower_CI <- mean(data) + std_err*(qt(p=alpha/2,df=length(data)-1))
	upper_CI <- mean(data) + std_err*(qt(p=(1- alpha/2),df=length(data)-1))
	return(c(mean=mean_data, 
	    std_err=std_err, 
	    upperCI=upper_CI, 
	    LowerCI=lower_CI))
	}
	
	
# This can be useful for something like this!
some_samples <- replicate(10, rnorm(20, 15, 2)) # Generating "10" samples each with 20 observations
ci_applish <- apply(X=some_samples, MARGIN=2, FUN=ConfidenceIntervalMean) # apply the CI function for all of these.

plot(ci_applish[1,], ylab="value", 
    ylim=range(ci_applish[3:4,]), cex=1.75, lwd=3, pch=16)
segments(x0=1:10,x1=1:10, y0=ci_applish[3,],
    y1=ci_applish[4,], lwd=3)	
