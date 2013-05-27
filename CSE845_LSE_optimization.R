# Optimize for LSE problem - Written by Ian Dworkin - last modified feb 23rd 2013 (syntax updated for style guide)

# This tutorial is to help aid you in understanding what least squares estimation (LSE) is all about, and how it works in practice.


###### First simple example######

# let us generate some "fake" data:
x <- 1:20 # Our covariate for the model, we are assuming for the time being that these are known, or measured WITHOUT error.
y <- rnorm(n=length(x), mean = x, sd = 1 ) # Our response variable
# We are assuming an intercept of 0 for this very simple case

plot(y~x)

# Now, let us perform the regression, and estimate the slope. We will use the pre-built linear model function (for which linear regression is a special case)

summary(lm(y ~ 0 + x)) # we force the intercept to be zero for this problem.
# This is NOT generally advisable ( a model with no intercept), and I am doing this here for the sake of simplicity, and to correspond to the model above. 

# It is also worth taking a look at the residual sum of squares (RSS)
anova(lm(y ~ 0 + x)) # The whole ANOVA table. Do not worry about all of this yet....
anova(lm(y ~ 0 + x))[2,2] # Just extracting the Residual Sum of squares via indexing.

# This will be the same as the sum of the squared residuals for the model
sum(resid(lm(y ~ 0 + x))^2)
###############



###############
# How should we think about this with respect to Least Squares estimation? Let us remember what we are trying to do? See the powerpoint presentation.

# For this regression (with an intercept fixed at 0):
# We are trying to find b such that we minimize this quantity
MinSumSq <- function(slope) { sum((y -slope*x)^2) } 
# Let us remind ourselves of what we are looking at: 
# It is also worth reminding ourselves that Least Squares Estimation is "Non-parametric" as it does not assume any for of distribution for the data.

# We can approximate this using "brute force"
slope <- seq(-2, 2, by=0.01) # Here we are generating a set of plausible values for the slope.

ss_est <- sapply(slope, MinSumSq ) #For each value in the vector "slope" calculate the min.ss

together <- cbind(slope, ss_est) # just joining the data together.
plot(ss_est ~ slope, type="b")

# The approximate minimum occurs at
together[which.min(together[,2]),] # We can see that the estimate for b, and RSS is close (but not exactly the same) as we found for the linear model. Why?

# This is an ad-hoc brute force "grid" approach. 
###############



###############
#Let us use something a bit smarter. For instance some approaches based on calculus.
# Here we use an optimization function that is designed for simple one dimensional problems like this one.
optim_min <- optimize(f = MinSumSq , interval=c(-10000, 10000), maximum=F)
# The inputs for this function are the function we have written, the minimum and maximum values for examining, and whether we are searching for a maximum or minimum value (in this case maximum=F)
optim_min # These values match what we see for the lm function.
# The optimize() function is only useful for one dimensional problems (where we are estimating just a single parameter).
# How might you double check that these values make sense?
###############


##########
# For more general purpose optimization, we use the optim() function
optim_min_2 <- optim(par=3, MinSumSq, method="BFGS") # We will learn about different optimization methods in ZOL851... Do not worry about it now.

# Let's try this again for a simple model with real data.
#Read data into R
dll_data <- read.csv("http://beaconcourse.pbworks.com/w/file/fetch/35183279/dll.csv", header=TRUE)   #data frame input
dll_data <-na.omit(dll_data) # removing missing values
dll_lm_1 <- lm(SCT ~ 1 + tarsus, data=dll_data)
coef(dll_lm_1)
anova(dll_lm_1)[2,2] # RSS
plot(SCT ~ tarsus, data=dll_data)
abline(dll_lm_1, lwd=3)
#################




############
# So how would we do this if we wanted to write a function for which we are going to optimize over the parameters (intercept and slope)
y <- dll_data$SCT    # Extracting our response variable (SCT = Sex Comb Teeth)
x <- dll_data$tarsus # Extracting our covariate/explanatory variable (length of tarsus)

MinSumSq2 <- function(b) { 
	intercept <- b[1] # First value in b, corresponding to the intercept
	slope     <- b[2] # second value in b, corresponding to the slope of the model 
	sum((y -intercept -slope*x)^2) } 
	
# We have just added the intercept (b[1]) and slope is now b[2]. This strange notation is courtesy of the optim() function, and is very annoying, as it wants all parameters estimated to be in a single vector (which we called b). Actually once you start thinking in vectors, it is kind of nice.

optim_min_2 <- optim(par=c(0, 0), MinSumSq2, method="BFGS")
# par=c(0,0) is setting the initial parameter values, such that we are saying that
# intercept <- b[1] should be initially set to 0, and the same for slope.
optim_min_2$par # Parameters
optim_min_2$value # What has been minimized... In this case the sum of squares.

# We can double check this by putting the parameter values back into the function we optimized, and make sure we get the same residual sum of squares.
MinSumSq2(b = optim_min_2$par) # And indeed we do!
########



########
# So this is all there is to LSE. Having said this, this is not how R (or any program) generally estimates the parameters. Instead most use analytical expressions for the parameters such as the intercept and slope that have been derived from some nice calculus (See the powerpoint for some examples, and the next tutorial on linear models). 

 # The basic analytical expressions for the slope (as you see in a textbook)
slope <- cov(y,x)/var(x) # covariance of x and y divided by variance in x
slope
intercept <- mean(y) - slope*mean(x)
intercept
#########



##########
# R uses matrix expressions that have been derived that are FAR faster than anything I have written above. Plus they generalize to arbitrarily complex linear models.

# The matrix way (based on analytical derivation). FYI. YOU DO NOT NEED TO REMEMBER THIS!!!!
x_matrix = cbind(1, x) 
head(x_matrix)  # Let's look at this.. to make sense of it.... Chalkboard....
# This is a simple example of a design matrix.

sol <- (solve (t(x_matrix) %*% x_matrix) ) %*% t(x_matrix) %*% y  #The solution for the estimates of the parameters. This is the solution to Ordinary Least Squares (OLS)
sol