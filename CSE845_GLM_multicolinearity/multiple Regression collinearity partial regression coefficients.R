# CSE891
# Multiple regression, partial regression co-efficients and co-linearity
# Last updated April 3rd 2012.


 # We are trying to account for variation in the number of sex comb teeth in Drosophila given the length of different segments of the leg, femur, tibia and tarsus. The Sex comb teeth are a secondary sexual structure on male Drosophila used for clasping females during copulation. See the movie.
 
# The problem is the predictors (leg lengths) are all correlated. 
# So how do we get good estimates. 
 
require(car)
require(lattice)

dll.data.complete = read.csv("http://beaconcourse.pbworks.com/f/dll.csv", header=TRUE)   
summary(dll.data.complete)

# Today we only want to work with a subset of the data, namely for mutant flies (genotype=="Dll") raised at 25C (temp==25)

dll.data <- subset(dll.data.complete, genotype=="Dll" & temp==25)

dll.data <- na.omit(dll.data)
#Some quick plots
plot(dll.data[,5:8])

# Trellis style plots from the lattice library
xyplot(SCT ~  femur + tibia + tarsus,  data = dll.data)
xyplot(femur ~  tibia + tarsus,  data = dll.data, ylim=c(0.4,0.8))


# How correlated are these?
legs <- with(dll.data, cbind(femur,tibia, tarsus))
cor(legs, use="pairwise.complete.obs")

# How about log transformed?
cor(log(legs),use="pairwise.complete.obs") # does not make much of a difference

# Before we go ahead and proceed with fitting the model, is there anything we should do to our variables to make things easier to fit and interpret?

dll.data$femur_c <- scale(dll.data$femur, center=T, scale=F)
dll.data$tibia_c <- scale(dll.data$tibia, center=T, scale=F)
dll.data$tarsus_c <- scale(dll.data$tarsus, center=T, scale=F)

# Remember that we have only centered the data, so this will not change their variance, sd or co-variances between the variables.


# First let us look at just tarsus as an explanatory variable
lm.tarsus <- lm(SCT ~  tarsus_c, data=dll.data)
summary(lm.tarsus)

#Before we go on, let us think about this summary table. It looks like tarsus is having a huge effect, but let us think about what this parameter means.

# Always keep in mind the scale of your predictors.

### Other models (We will use them later) ####

lm.femur <- lm(SCT ~  femur_c, data=dll.data)
lm.tibia <- lm(SCT ~  tibia_c, data=dll.data)

#############

# So what happens if we just fit the model with all three leg segments as explanatory variables?
lm.full <- lm(SCT ~ femur_c + tibia_c + tarsus_c, data=dll.data)
summary(lm.full)
anova(lm.full)
car::Anova(lm.full, type=2) # Note this is a slightly different type of Anova called a type II sum of squares. Do not worry about it for the moment, just know that it simultaneously adjusts for all main effect terms.

# let us look at the correlations among parameter estimates
cov2cor(vcov(lm.full))

par(mfrow=c(2,2))
car::confidenceEllipse(lm.full, which.coef=c(2,3))
car::confidenceEllipse(lm.full, which.coef=c(2,4))
car::confidenceEllipse(lm.full, which.coef=c(3,4))
car::confidenceEllipse(lm.full, which.coef=c(1,4))

# These correlations are actually not too bad, but they still may have some influence on each other.


# Multiple regression and partial regression co-efficients

# Since our predictors are correlated with each other, they will not suprisingly influence each others parameter estimates. This is both good and bad. It is good because we want to estimates that reflect all of the information we have in the data, including how our observed values for the predictors JOINTLY influence the model. It is bad because if they are too highly correlated with each other, it adds a lot of instability and uncertainty to our estimates.

# IMPORTANT NOTE: If the predictors are completely uncorrelated with each other, then they DO NOT influence the estimated parameters for each other in the model. This is because there is no more information jointly than there are individually. While the parameter estimates do not change in this case, the overall explanatory power of the model may increase (if each predictor is independently accounting for variation in the observed response).

############Partial Regression co-efficients###########
# So what do the "partial" regression co-efficients tell us? These marginal estimates of the predictor tell us the estimates if all other co-variation between predictors is already accounted for. 


# Another, far more intuitive way of thinking about it is as follows:

# for model  y ~ x1 + x2 + x3

# the co-efficient for x1 for the model above will be the same as if we:
#model1  y ~ x2 + x3
#model2  x1 ~ x2 + x3
#model3  resid(model1)~ resid(model2)



lm.tarsus <- lm(SCT ~  tarsus_c, data=dll.data)

summary(lm.full)$coef
summary(lm.tarsus)$coef # Similar, but not the same
summary(lm.full)$r.squared
summary(lm.tarsus)$r.squared

#demonstrating partial regression co-efficients
par(mfrow=c(2,1))
plot(SCT ~ tarsus_c, data=na.omit(dll.data), col=densCols(SCT))
lines(lowess(na.omit(dll.data)$SCT~na.omit(dll.data)$tarsus_c))

# Now we want to get the models to account for variation in # SCT by femur and tibia
lm.D1 <- lm(SCT ~ femur_c + tibia_c, data=na.omit(dll.data))

# And similarly variation in tarsus (as a "response") accounting for femur and tibia as predictors
lm.D2 <- lm(tarsus_c ~ femur_c + tibia_c , data=na.omit(dll.data))

# Now let's look at the variation left over in both SCT # and tarsus length, having accounted for femur & tibia

plot(resid(lm.D1) ~ resid(lm.D2), col=densCols(resid(lm.D1)), ylab="residuals for SCT | femur + tibia", xlab="residuals for tarsus | femur + tibia" )
lines(lowess( resid(lm.D1) ~ resid(lm.D2) ))


# Now we fit the models of residuals for SCT ~ residuals tarsus
lm.partial <- lm(resid(lm.D1) ~ resid(lm.D2))

summary(lm.partial)$coef[,1:3] # voila!!!
summary(lm.full)$coef[, 1:3]



####### Partial co-efficient of determination

# In addition to the partial regression co-efficients (the estimates parameters), we can also ask a similar question for variance in the observed response accounted for each term in the model. Specifically:
# " How much variation in the observed response is accounted for by predictor[k], after having accounted for all other predictors in the model?"

# This is called the partial co-efficient of determination.

# We will use a function partial.R2 in the asbio library (which you will need to install). FYI, my post-doc and I have written some more general and useful functions for this, and if you want them email me (they will be included in an upcoming R library we are writing)

# Let us think about what we expect

summary(lm.full)$r.squared
summary(lm.tarsus)$r.squared
summary(lm.femur)$r.squared
summary(lm.tibia)$r.squared



# First we fit the model without tarsus
modelNoTarsus <- lm(SCT ~ tibia_c + femur_c, data=dll.data)

# Then 
asbio::partial.R2(modelNoTarsus, lm.full)



########### Co-linearity
# We have some tools to examine how bad a problem co-linearity may be. One is called the condition number (kappa), the other is the Variance Inflation Factor or VIF.

# vif aims to tell us how much the standard errors (well SE^2 actually) are inflated due to co-linearity between variables. It computes the "tolerance" of a predictor[k], which is the 1 - R^2 of the model where we fit the following model
# predictor[k] ~ all other predictors
# The vif[k] for predictor[k] is just 1/tolerance[k]

# So the sqrt(vif[k]) tells us how much our uncertainty is inflated (multiply vif*SE) relative to a model with out the co-linearity.

# The general "rules of thumb" is that vif > 8 are considered problematic, and average(vif) > 6 is also a sign of issues.

### While vif is widely used, in my experience vif (or at least the rule of thumbs) tends to underestimate the degree problems associated with colinearity between variables.

# vif function from the car package.
vif(lm.full)

# Let us show the calculation more explicitly
# examine VIF
lm.tarsusVIF <- lm(tarsus_c ~ femur_c + tibia_c, data=dll.data)
summary(lm.tarsusVIF)$coef
tol.tarsus  <- 1 - summary(lm.tarsusVIF)$r.squared
VIF.tarsus <-  1/tol.tarsus
VIF.tarsus

lm.tibiaVIF <- lm(tibia_c ~ femur_c + tarsus_c, data=dll.data)
summary(lm.tibiaVIF)$coef
tol.tibia  <- 1 - summary(lm.tibiaVIF)$r.squared
VIF.tibia <-  1/tol.tibia
VIF.tibia

lm.femurVIF <- lm(femur_c ~ tibia_c + tarsus_c, data=dll.data)
summary(lm.femurVIF)$coef
tol.femur  <- 1 - summary(lm.femurVIF)$r.squared
VIF.femur <-  1/tol.femur
VIF.femur

VIF.tibia; VIF.tarsus; VIF.femur
vif(lm.full)




#### Condition number (kappa)

# Condition number is an independent approach to assessing co-linearity. It is based on the eigenvalues of the design matrix (both terms you may be unfamiliar with at this point). At it's worse we can have an issue where the design matrix X, or more specifically X[t]*X is singular (rank deficient), likely because two columns or rows in X are exactly equal to one another (or a row or column is an exact linear combination of one or more columns or rows).

# More generally the issue is a matter of degree, and we use the relative size of eigenvalues of X[t]*X as a measure. In particular we want to examine the relative sizes of the eigenvalues (we will have p eigenvalues, where p is the number of columns in our design matrix). When the eigenvalues are really unequal we have a problem. We usually use a single summary called kappa (condition number) which is calculated as follows.

# The general rule of thumb is that condition numbers (Kappa) > 30 suggests enough co-linearity to be a problem.


# examine condition number (here is a function to do it)

ConditionNumber <- function(YourModel=lm.full) {
  x <- model.matrix(YourModel)
  eigen.x <- eigen(t(x) %*%x)
  sqrt(max(eigen.x$values)/min(eigen.x$values))
  }
  
ConditionNumber(lm.full) # ~ 70, so our "rule of thumb" suggests we should take our results with some caution.


# Let's discuss what we can do about the co-linearity.


##### You can also use this approach for looking for unbalanced designs
model.X <- lm(SCT ~ genotype + temp, data=dll.data)
tapply(dll.data$SCT, INDEX=list(dll.data$genotype,dll.data$temp), length) # Make a little table to see the lack of balance in the design
design.X <- model.matrix(model.X)
head(design.X)
vif(model.X)
eigen.x <- eigen(t(design.X) %*%design.X)
sqrt(max(eigen.x$values)/min(eigen.x$values))



# How about with interactions
ModelPlay <- lm(SCT ~ genotype*temp, data=dll.data)  # genotype + temp + genotype:temp
summary(ModelPlay)

# How much variation does the interaction effect account for.

ModelPlayNoINteraction <- lm(SCT ~ genotype + temp, data=dll.data)
summary(ModelPlayNoINteraction)

partial.R2(ModelPlayNoINteraction, ModelPlay)
