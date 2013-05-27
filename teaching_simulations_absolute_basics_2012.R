# Very basic introduction into performing simulations.
# Updated Jan 26 2012

# For this course we are going to use so called "monte carlo" simulation as a tool to learn basic concepts in statistics & probability (given a particular definition of probability)

#  What do I mean by simulate?

# Why would simulation be a useful tool in statistics?


#FYI: When used for statistical inference, many Monte Carlo approaches fall under the rubric of Parametric Bootstrapping (which is different than the non-parametric bootstrap which uses resampling of the original data to generate empirical sampling distributions)
 
# We are not going to discuss the mathematical or computational details (the latter I know little about) with respect to generating random numbers. We will for the moment just use some tools.

# R has many useful facilities for generating random numbers. Let's start with generating a single random number from a uniform distribution on [0,1]

runif(n=1, min=0, max=1) # the rNameOfDist functions generate random numbers. 
    # min sets lowest, max sets highest possible value, n is the number of draws from this distribution. 

# If we wanted 1000 such numbers we could use a for loop, or the replicate() in R. However the easier way would be to ask for 1000 numbers with the n=1000 argument.
runif(n=1000, min=0, max=1)
runif(n=1000)

# If I plotted a histogram of this data, what should it look like (at least theoretically)
random.uniform.1000 <- runif(n=100000, min=0, max=1) 
hist(random.uniform.1000) # calls the histogram function


# If we wanted to look at 100 draws from a normal distribution with mean =5 and sd=2
# i.e.  ~N(5,2)
random.normal.100 <- rnorm(n=100, mean=5,sd=2) 
par(mfrow=c(3,1))  # This (par) allows us to modify low-level plotting functions. In this case changing the graphical device (the palette) to have 3 rows and 1 column, so we can put three plots on it.
plot(random.normal.100)
boxplot(random.normal.100)
hist(random.normal.100)

#Let's look at some summary statistics from this simulation
mean(random.normal.100) 
sd(random.normal.100)
# While the mean and sd are close to the values we inputted, they are not exactly 5 & 2.

# let's repeat this experiment a few times
random.normal.100.1 <- rnorm(100, 5,2)
random.normal.100.2 <- rnorm(100, 5,2)
random.normal.100.3 <- rnorm(100, 5,2)
random.normal.100.4 <- rnorm(100, 5,2)
par(mfrow=c(2,2))
hist(random.normal.100.1)
hist(random.normal.100.2)
hist(random.normal.100.3)
hist(random.normal.100.4)
mean(random.normal.100.1); mean(random.normal.100.2); mean(random.normal.100.3); mean(random.normal.100.4)

# All close to, but not exactly equal to 5.


#### more efficient performing the simulation 
random.normal.100.rep <- replicate(n=4, rnorm(100,5,2)) # And if we want to call from the rnorm function more than 4 times, we just change the n= in the replicate function. 

random.normal.100.rep # We can see we have created 4 sets of random numbers from this distribution.
par(mfrow=c(2,2))
apply(X=random.normal.100.rep, MARGIN=2, FUN=hist) # using the basic apply() to repeat the call to hist, by columns (MARGIN=2). MARGIN=1 would be along rows.

apply(X=random.normal.100.rep, MARGIN=2, FUN=mean)

apply(random.normal.100.rep, 2, sd) # If you use the order correctly you do not need to specificy the names of the arguments.

# the advantage of this approach is you can also just do
summary(random.normal.100.rep)

# We can ask about the spread of our experimental trials
sd(apply(random.normal.100.rep,2,mean)) # Let's talk about what we have done here.

# Let's repeat this experiment with lower sample size (say 25). What happens to the spread?
norm.sim.all.3 <- replicate(n=4, rnorm(25,5,2))
summary(norm.sim.all.3)
apply(norm.sim.all.3,2,sd)
sd(apply(norm.sim.all.3,2,mean))

# How about if we have much larger sample size (say 1000)
norm.sim.all.4 <- replicate(n=4, rnorm(1000,5,2))
summary(norm.sim.all.4)
apply(norm.sim.all.4,2,sd)
sd(apply(norm.sim.all.4,2,mean))

# Do you see a pattern for the precision of the mean?
sd(apply(random.normal.100.rep,2,mean)) # n=100
sd(apply(norm.sim.all.3,2,mean)) # n=25
sd(apply(norm.sim.all.4,2,mean)) # n=1000

# how about the sd
sd(apply(random.normal.100.rep,2,sd)) # n=100
sd(apply(norm.sim.all.3,2,sd)) # n=25
sd(apply(norm.sim.all.4,2,sd)) # n= 1000


# We have just learned about monte carlo methods. When we perform simulations from that distributions, as our n increase we will get closer to the expected value (which in this case we provided)

# Instead of simulating just a mean and sd, let's specify something more interesting.. 
# How about a regression!!!!!

# let's have a regression  Y ~ N(a+b*x, sd)
par(mfrow=c(2,2))
a=5
b=0.7
x <- seq(2,20)
y_fixed <- a + b*x # we are expressing the relationship between y and x as a linear model. In this case we are generating the data using such a model.
plot(y_fixed~x, main= "Deterministic Component of the model") # A linear model 
abline(a=5, b=0.7)
# But data is generally generated by a noiser (and usually unknown process), so we add some variation. In this case let's say sd=2. Let's think about what this means?

y.sim.1 <- rnorm(length(x), mean=y_fixed, sd=2) # Explain length(x)
plot(y.sim.1~x)
abline(a=5, b=0.7) # Expected relationship based on the parameters we used.

# But what is the actual parameter estimates for the regression?
y.sim.1.lm <- lm(y.sim.1 ~ x)
summary(y.sim.1.lm)  # notice parameter estimates and RSE!
confint(y.sim.1.lm) # does it include our expected values
abline(reg=y.sim.1.lm,lty=2) # estimated values based on simulated data.
# The point is, we have now done a little simulation of our regression model. 



# One more little point. Say we wanted to simulate a roll on a 6-sided die. How might we accomplish this?
# There are a number of ways but the easiest does not use a rNameOfDist

# Instead we create a sequence of the integers we want (1:6) and then sample from this distribution
one.to.six <- 1:6

sample(x=one.to.six, size=1, replace=T) # we want one roll of the die, 

# Say we wanted 100 dice rolls
die.rolls <- sample(1:6, size=100, replace=T) # replace =T means sampling WITH replacement.