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

data_subset <- parasite.data[parasite.data$Parasites=="0",]

with(data_subset, 
    plot(FinalDiversityIndex ~ HostMutationRate,
    col=c(1,2)[Sex]))
    
with(data_subset, 
    lines(lowess(x=HostMutationRate[Sex=="0"], 
        y=FinalDiversityIndex[Sex=="0"]), col="black"))  

with(data_subset, 
    lines(lowess(x=HostMutationRate[Sex=="1"], 
        y=FinalDiversityIndex[Sex=="1"]), col="red"))            