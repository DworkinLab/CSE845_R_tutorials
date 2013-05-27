# Modified April 3rd 2012

# Multiple regression when predictors are uncorrelated versus correlated.
# This is a toy example of two predictor/co-variates that are correlated, and when both included in a model as co-variates produce some instability in the estimates.

require(MASS)  # for mvrnorn  the random number generator for a multivariate normal
require(car)  # to look at regression diagnostics, in particular the VIF

Sigma <- matrix(c(5,3,3,2.5),2,2) # covariance matrix
Sigma
x <- mvrnorm(n=100, mu=c(5,10),Sigma=Sigma)
plot(x)

#  relationship between covariance and correlation
mean(x[,1]); mean(x[,2])
cov(x[,1],x[,2]) # ~ should be approximatey 3
cor(x[,1],x[,2])

# We can standardize the variabes by mean centering and dividing by the standard deviation. What is the name for this?
x.1.std <- (x[,1]-mean(x[,1]))/sd(x[,1])
x.2.std <- (x[,2]-mean(x[,2]))/sd(x[,2])
mean(x.1.std); mean(x.2.std)
sd(x.1.std); sd(x.2.std)
cor(x.1.std,x.2.std)
#BUT
cov(x.1.std,x.2.std) # now equals the correlation. Think about why this has happened

# FYI.. there is a function scale() that does the scaling we did above. It has arguments about whether you want to center the data and scale it by the sd.

#### Onto the regression models.

 # We will first do two seperate models, one for each predictor, and compare the estimates with the actual values

# for predictor x.1.std
RSE <- rnorm(length(x.1.std),0,1) # residual standard error=1
y1 <- 3 + 2.0*x.1.std + RSE  # intercept of 3, slope = 2
plot(y1~x.1.std)

lm.1<- lm(y1 ~x.1.std)
summary(lm.1)


# for predictor x.2.std
y2 <- 3 + 1.0*x.2.std + RSE # intercept of 3, slope = 1, residual standard error=1
plot(y2~x.2.std)

lm.2<- lm(y2 ~x.2.std)
summary(lm.2)


# For both of these models, the parameter estimates are good. How about for a multiple regression given their correlation?

y3 <- 3 + 2.0*x.1.std + 1.0*x.2.std + RSE
lm.3<- lm(y3 ~x.1.std + x.2.std)
summary(lm.3) # estimates are much less precise, and the estimates are pretty far off
vcov(lm.3)  # covarying parameters.
cov2cor(vcov(lm.3))

vif(lm.3) # not small.... However, the VIF tends to (in my experience) underestimate colinearity.

#Eigenvalues and condition number
mod.X <- model.matrix(lm.3)
eigen.x <- eigen(t(mod.X) %*%mod.X)
eigen.x$val # eigenvalues from the design matrix
sqrt(eigen.x$val[1]/eigen.x$val) # condition numbers


## Let's try again, but this time with two variables that are uncorrelated. What do you think is going to happen?
Sigma <- matrix(c(5,0,0,2.5),2,2) # covariance matrix. The variables have no relationship
x <- mvrnorm(n=100, mu=c(5,10),Sigma=Sigma)
plot(x)

x.1.std <- (x[,1]-mean(x[,1]))/sd(x[,1])
x.2.std <- (x[,2]-mean(x[,2]))/sd(x[,2])
cor(x.1.std,x.2.std)

RSE <- rnorm(length(x.1.std),0,1) # residual standard error=1
y1 <- 3 + 2.0*x.1.std + RSE
y2 <- 3 + 1.0*x.2.std + RSE
y3 <- 3 + 2.0*x.1.std + 1.0*x.2.std + RSE

lm.1<- lm(y1 ~x.1.std)
summary(lm.1)

lm.2<- lm(y2 ~x.2.std)
summary(lm.2)

lm.3<- lm(y3 ~x.1.std + x.2.std)
summary(lm.3) 
vcov(lm.3) 
vif(lm.3) # much lower...estimates are pretty close.

# this suggests two things. 

#First highly correlated variables  (multi-collinearity) really can influence your estimates. VIF can often underestimate the effect. So if two predictor variables are highly correlated proceed with caution (consider alternatives like using only one variable or performing a PCA)

# Second. When the variables are completely uncorrelated, then it does not really matter if you perform two seperate regression models (one for each predictor), or a single multiple regression, the results will be similar.

# This suggests that the best use of the multiple regression framework is when predictors are somewhat correlated, but not TOO correlated..  fun stuff.
