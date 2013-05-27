
# BEACON CSE845 Feb 28 2013
# First let us bring in the course source file


source('http://beaconcourse.pbworks.com/w/file/fetch/36678466/Source_useful_R_function_ID_Feb24_2011.R')# to see what functions this has....


# The data we are going to be using for this tutorial were provided by Luis (who else!).
# Luis set up some experiments to look at the joint influences of recombination, and mutation rate with and without parasitism on species(?) diversity and richness, and whether or not parasites were lost (for runs that had them). F

parasite_data <- read.csv("http://beaconcourse.pbworks.com/w/file/fetch/37890621/sexAsex.csv", h=T)

# remember to always check your data coming in
str(parasite_data)
summary(parasite_data)
head(parasite_data)
dim(parasite_data)  # check against wc -l sexAsex.csv


# First we need to change some variable types
parasite_data$Sex <- factor(parasite_data$Sex) # was sex turned on
parasite_data$Parasites <- factor(parasite_data$Parasites) # did we have parasites initially in the experiment
levels(parasite_data$Sex) <- c("NoSex", "Sex") # This is the easiest way of recoding the levels of the factor.. below is another approach.
levels(parasite_data$Parasites) <- c("NoParasites","Parasites")

# parasite_data$Sex <- ifelse(parasite_data$Sex=="0", "NoSex","Sex") # remember ifelse is vectorized!
# parasite_data$Parasites <- ifelse(parasite_data$Parasites=="0", "NoParasites","Parasites")
# parasite_data$Sex <- factor(parasite_data$Sex) # was sex turned on
# parasite_data$Parasites <- factor(parasite_data$Parasites) # did we have parasites initially in the experiment

# Quick reminder of the structure of the experiment.
with(parasite_data, table(factor(HostMutationRate), Sex, Parasites))




	
# A plot to remind us of what is going on
require(lattice)
bwplot(FinalDiversityIndex ~ factor(HostMutationRate)|Sex*Parasites, data=parasite_data)
	

# So We are asking the basic question (to start with) on how FinalDiversity is influenced by several underlying covariates including mutation rate, whether there is sexual recombination and whether parasites occur. The latter two variables are categorical, so we will treat these as factors (although as you will soon realize it does not really matter for linear models, when there are only two levels.)

# So we have one continuous covariate (mutation rate)
# and two treatment factors with two levels (on or off).


# Let's start by just looking at a subset of the data, with the parasites non present and no sex.

parasite_data_0_0 <- parasite_data[parasite_data$Sex =="NoSex" & parasite_data$Parasites =="NoParasites",]



str(parasite_data_0_0) # Do you notice anything weird about this?

edit(parasite_data_0_0)
levels(parasite_data_0_0$Sex)


# It turns out R has a "feature" which most people consider annoying, that is, it reports levels of a factor from the original data set, even if you subset on levels of that factor.

# It turns out this will not influence most analyses, but it is safest to just "fix" this
#parasite_data_0_0$Sex <- factor(parasite_data_0_0$Sex) # one method to drop levels.....
parasite_data_0_0$Sex <- droplevels(parasite_data_0_0$Sex) # this does the same as the line above
levels(parasite_data_0_0$Sex)

# Note this can also be done using the droplevels()
parasite_data_0_0$Parasites <- droplevels(parasite_data_0_0$Parasites)



plot(FinalDiversityIndex ~ jitter(HostMutationRate, factor=0.5), data=parasite_data_0_0, pch=16)

# HEre is the classic regression of FinalDiversity on mutation rate
parasites_lm_1 <- lm(FinalDiversityIndex ~ 1 + HostMutationRate, data=parasite_data_0_0)

# Before we do anything else, let's think about the design matrix 
#  y[i] ~ a + bx[i] + e[i] 
#  Y = BetaX + E
edit(model.matrix(~1+ parasite_data_0_0$HostMutationRate))


#or
model.matrix(parasites_lm_1)


# Now let's take account of what this all means....
summary(parasites_lm_1)
# Let's step through this..


#####Output from summary
# Call:
# lm(formula = FinalDiversityIndex ~ HostMutationRate, data = parasite_data_0_0)

# Residuals:
     # Min       1Q   Median       3Q      Max 
# -1.44135 -0.64813 -0.03321  0.47003  2.29853 

# Coefficients:
                 # Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        0.9857     0.1616   6.101 2.11e-08 ***
# HostMutationRate   0.6452     0.2813   2.294   0.0239 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

# Residual standard error: 0.7955 on 98 degrees of freedom
# Multiple R-squared: 0.05096,	Adjusted R-squared: 0.04127  # 1 - (residual SS/total SS) OR  #regression SS/total SS
# F-statistic: 5.262 on 1 and 98 DF,  p-value: 0.02393 


# The fitted line
abline(parasites_lm_1, lwd=3) 
confint(parasites_lm_1)

# As you have read about, and we have discussed briefly, there are a number of assumptions that are made in a linear model. There are a number of graphical tools we can use to evaluate them. Some of them are even automated in R.

par(mfrow=c(2,2))
plot(parasites_lm_1)
# Let's talk about these. By far the first two are the most useful...

#There are other ways of looking at these.
plot(resid(parasites_lm_1) ~ parasite_data_0_0$HostMutationRate)
boxplot(resid(parasites_lm_1) ~ as.factor(parasite_data_0_0$HostMutationRate), ylab="residuals", xlab="HostMutationRate")
#Sometimes I find a histogram or density plot is most useful for the residuals as opposed to a q-q plot
data_plotter.fun(resid(parasites_lm_1))


# So we have clearly violated one assumption (homogeneity of variance). The residuals look more or less normally distributed, but clearly increase with mutation rate. It is sometimes difficult to know the exact impact of this. There are a number of things we can do to address this if we are worried though (GLS, non-parametric models, inference using resampling.)

# One of the most useful (but not so frequently used) plots is fitted values ~ observed.
plot(fitted(parasites_lm_1) ~ parasite_data_0_0$FinalDiversityIndex, xlim=c(0,4), ylim=c(0,4))
abline(a=0,b=1)
# In many ways this provides a graphical interpretation of R^2. As the numbers suggested, we have not really done a good job of fitting the data with such a simple model.

anova(parasites_lm_1)
# To understand this, we need to think about a couple of things.
# F distributions...
# So we need to think about the sum of squares of two models.


parasites.lm.0 <- lm(FinalDiversityIndex ~ 1, data=parasite_data_0_0) # What does this model mean?
anova(parasites.lm.0, parasites_lm_1) # Compare the two models, We are asking how much of the variance have we accounted for in the model/ residual variance (unaccounted for by the model.)



# Now let us take a look at a different subset of the data, where we are including Parasites
parasite_data_p_0 <- parasite_data[parasite_data$Sex =="NoSex" & parasite_data$HostMutationRate==0.1,]
dim(parasite_data_P_0)

plot(FinalDiversityIndex ~ Parasites, data=parasite_data_p_0)

# So since the only factor (parasites present or not) has only two levels we could try a t-test...
# But we could instead ask the same question we asked above, how much variation can we account for by fitting a model

lm_parasites_1 <- lm(FinalDiversityIndex ~ 1 + Parasites, data=parasite_data_p_0)
lm_parasites_0 <- lm(FinalDiversityIndex ~ 1, data=parasite_data_p_0)
# In this linear model framework, it is important to think about what the design matrix might look like..
# This is something we will come back to alot, and is probably the most important thing 

anova(lm.parasites_1)
anova(lm.parasites_0, lm.parasites_1) # comparing the models

# Let's think about what we are doing here. We are computing the ratio of the variation between groups (Parasites VS NoParasites) to the variation within the population (or within groups.)

# We know SS.total = SS.model + SS.E

# we can think about this is a slightly different manner for the ANOVA.

# in particular the relative ratio of SS.model and SS.E (although we will need to adjust these for degrees of freedom.)

# We can calculate SS.model and error as before, but there is a different "traditional" approach for anova.

# variation between groups  VS. variation within group.s

# variation between groups. Often called SS.treatment

# first we calculate each treatment mean (in this case, with and without parasite).

treatment_means <- with(parasite_data_p_0, tapply(X=FinalDiversityIndex,INDEX=Parasites, FUN=mean))

# Then for each we have to compute the sum of squared differences between the treatment mean, and the sample (grand mean for sample)
TreatmentSquaredDifferences <- (treatment_means- mean(parasite_data_p_0$FinalDiversityIndex))^2
# It is important to think about what we have just done (we will draw it on the board)...
# The treatment means are the fitted values for ALL of the observations..

# So the difference we have computed has to multipied by the number of individuals we have for each group.

TreatmentSampleSize <- with(parasite_data_p_0, tapply(X=FinalDiversityIndex,INDEX=Parasites, FUN=length))

TreatmentSumSquares <- sum( TreatmentSquaredDifferences*TreatmentSampleSize )


# The same logic is essentially applied for the error, but computing the sum of squares within each group, and summing across those.


summary(lm_parasites_1)
# Let's think about what these parameters mean, as it depends  ALOT on how we specified the design matrix.
# SO what does this mean?

lm_parasites_1_cell <- lm(FinalDiversityIndex ~ 0 + Parasites, data=parasite_data_p_0)


##### The whole shebang #!
LmParasiteDataComplete <- lm(FinalDiversityIndex ~ (Parasites*HostMutationRate*Sex) - Parasites:HostMutationRate:Sex , data=parasite_data)

summary(LmParasiteDataComplete)
par(mfrow=c(1,1))
plot(parasite_data$FinalDiversityIndex ~fitted(LmParasiteDataComplete), xlim=c(0,5), ylim=c(0,5))
abline(a=0, b=1)

par(mfrow=c(2,2))
plot(LmParasiteDataComplete)
hist(resid(LmParasiteDataComplete))