# BEACON CSE845 Feb 14th 2013

# First let us bring in the course source file


source('http://beaconcourse.pbworks.com/w/file/fetch/36678466/Source_useful_R_function_ID_Feb24_2011.R')# to see what functions this has....
ls()

# Visualizations using R.

# This set of tutorials is in two parts. The first is about HONEST data exploration (which is part of exploratory data analysis or EDA). 
# The second part is about summaring your important findings using graphics, usually done after modeling.



# The data we are going to be using for this tutorial were provided by Luis (who else!).
# Luis set up some experiments to look at the joint influences of recombination, and mutation rate with and without parasitism on species(?) diversity and richness, and whether or not parasites were lost (for runs that had them). F

# setwd("/Users/ian/BEACON_COURSE_2011/CSE891_R_tutorials/CSE891_2011_EDA_DataVis")

parasite.data <- read.csv("http://beaconcourse.pbworks.com/w/file/fetch/37890621/sexAsex.csv", h=T)

# remember to always check your data coming in
str(parasite.data)
summary(parasite.data)
head(parasite.data)
dim(parasite.data)  # check against wc -l sexAsex.csv


# First we need to change some variable types
parasite.data$Sex <- factor(parasite.data$Sex) # was sex turned on
parasite.data$Parasites <- factor(parasite.data$Parasites) # did we have parasites initially in the experiment


# Quick look to remind us of the data structure.
with(parasite.data, 
    table(factor(HostMutationRate), Sex, Parasites))

# So an obvious place to start is to explore the data graphically (without any fitting, be very careful..). This is to hone the specifics of the modeling, and great care must be taken about using this for hypothesis generation.


# So let's plot

# There are potentially a number of ways to look at this data... Simple examples such as histograms or density plots can be useful as a starting point.

# I wrote a helper function data_plotter.fun()
data_plotter.fun(parasite.data$FinalFitness)
data_plotter.fun(parasite.data$FinalDiversityIndex)

# These may not appear "normal" but that is ok, remember that this includes all of the treatment effects (sex, mutation rate, etc..) so these can be multimodal.
par(mfrow=c(1,1))
plot(FinalFitness ~ HostMutationRate, data=parasite.data) # Not so useful.
plot(FinalFitness ~ Sex, data=parasite.data)
plot(FinalFitness ~ jitter(as.numeric(Sex), factor=0.25), data=parasite.data,xlab="Sex") # just adds a bit of noise along the X axis to make it easier to see

# Of course we probably want to look at a boxplot
boxplot(FinalFitness ~ Sex, data=parasite.data, xlab="SEX", ylab="FinalFitness")

par(mfrow=c(1,2))
with(parasite.data, plot(FinalFitness ~ factor(HostMutationRate),
   subset=Sex==1, main="Fitness ~ mutation rate with sex "))
                      
with(parasite.data, plot(FinalFitness ~ factor(HostMutationRate),
    subset=Sex==0, main="Fitness ~ mutation rate without sex "))

# The above plot can be misleading since they are on fairly different scales.

# Let's do this but with both axes scaled the same.

# One approach (for clarity) is to define a variable for the min, max of Fitness
FitMinMax <- c(min(parasite.data$FinalFitness), 
    max(parasite.data$FinalFitness))

par(mfrow=c(1,2))
with(parasite.data, plot(FinalFitness ~ factor(HostMutationRate),
    subset=Sex==1, main="Fitness ~ mutation rate with sex ",
    ylim=FitMinMax) )
                     
with(parasite.data, plot(FinalFitness ~ factor(HostMutationRate),
    subset=Sex==0,main="Fitness ~ mutation rate without sex",
    ylim=FitMinMax))



# All combinations of Sex and Parasites
par(mfrow=c(2,2))

for (i in levels(factor(parasite.data$Parasites))) {
	for (j in levels(factor(parasite.data$Sex))){
		with(parasite.data, plot(FinalFitness ~ factor(HostMutationRate), 
		                     subset=(Sex==j & Parasites==i), main="Fitness ~ mutation rate", ylim=FitMinMax) )
		}
	}
	
# You can also use lattice graphics
require(lattice)
bwplot(FinalFitness ~ factor(HostMutationRate)|Sex*Parasites, data=parasite.data)
	

# Another way to look at some of the data is to focus on HostMutationRate Numerically
plot(FinalFitness ~ jitter(HostMutationRate, factor=0.25), 
     data=parasite.data, pch=c(0,1)[factor(parasite.data$Sex)], 
     col=c("red","blue")[factor(parasite.data$Parasites)])

# Or if you do not really care about specifying the colours per se.
#plot(FinalFitness ~ jitter(HostMutationRate, factor=0.25), data=parasite.data, col=as.integer(Sex:Parasites))


legend(0.1,0.187, legend=c("NoSex NoParasite", "Sex NoParasite"," NoSex Parasite", "Sex Parasite" ), 
  pch=c(0,1,0,1), col=c("red", "red", "blue", "blue"))


plot(FinalFitness ~ FinalDiversityIndex, data=parasite.data )

# How about for time series data (i.e. avida runs....)
gens <- 1000
generations <- 1:gens
fitness.1 <-rep(NA,gens)
fitness.1[1] <- 0.5
for (i in 2:gens) {
	fitness.1[i] <-  rnorm(1, 1.015*fitness.1[i-1], 0.07*fitness.1[i-1]) }

plot(log(fitness.1) ~ generations)  # Can you think about a better way of visualizing this?
# We need to think about the autocorrelations...
acf(cbind(fitness.1,generations))