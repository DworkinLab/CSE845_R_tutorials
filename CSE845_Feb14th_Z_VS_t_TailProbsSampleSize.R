
# This little example shows the relationship between Z and t tail probabilities for varying sample sizes.

# Different Sample Sizes
sample.size <- 2:100 # We will allow sample size to vary

# Now we find the "critical values" for out confidence intervals. For alpha=0.05, we want to consider the 1-alpha (0.95) region, so as a specific case, we can look at the 0.025 regions at the upper and lower ends of the distribution.
#
t.values.upper <- qt(0.975, df=sample.size-1)
t.values.lower <- qt(0.025, df=sample.size-1)

par(mfrow=c(2,1))
plot(t.values.upper ~ sample.size, type="p", main="0.975 t-values")
abline(h=1.96, col="red", lwd=2)
plot(t.values.lower ~ sample.size, type="p", main="0.025 t-values")
abline(h=-1.96, col="red", lwd=2)


# Another way to think about this is how well the t-distribution approximates the Z distribution (standard normal distribution)
par(mfrow=c(1,1))
curve(qnorm(x,mean=0, sd=1),0,1,
    ylim=c(-12, 14), 
    lwd=3, lty=1, ylab="value of t/Z", 
    xlab="probability", 
    col="black", 
    main="relationship between t and Z, for varying degrees of freedom")
curve(qt(x,df=1),0,1, lwd=3, lty=6, col="red", add=T)
curve(qt(x,df=2),0,1, lwd=3, lty=5, col="purple", add=T)
curve(qt(x,df=5),0,1, lwd=3, lty=4, col="blue", add=T)
curve(qt(x,df=10),0,1, lwd=3, lty=2, col="orange", add=T)
legend(0, y=14, legend=c("Z","1 df","2 df","5 df", "10df"), 
    col=c("black","red", "purple", "blue", "orange"),
    lwd=3, lty=c(1,6,5,4, 2))


curve(qnorm(x,mean=0, sd=1),0.9,1, lwd=4, lty=1,ylab="value of t/Z", xlab="upper tail probability", col="red", main="relationship between t and Z at tail probabilities, for various df")
curve(qt(x,df=5),0.9,1, lwd=4, lty=6, col="black", add=T)
curve(qt(x,df=50),0.9,1, lwd=4, lty=5, col="purple", add=T)
curve(qt(x,df=500),0.9,1, lwd=4, lty=4, col="blue", add=T)
legend(0.9,y=3,, legend=c("Z",5,50,500), col=c("red","black", "purple", "blue"), lwd=4, lty=c(1,6,5,4))
abline(v=0.975, col="grey", lwd=2, lty=4)
text(x=0.978,y=1.5, labels="0.975", col="black")
