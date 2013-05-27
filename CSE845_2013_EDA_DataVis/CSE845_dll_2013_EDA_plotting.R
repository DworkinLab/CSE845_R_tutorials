# Graphical examination and exploration of the data
# Written by Ian Dworkin, last modified on Sept 11/2012


# The Anscombe data (Anscombe 1973).... Or why you must plot!!!!
data(anscombe)
anscombe
summary(anscombe)
apply(anscombe, 2, sd)
colMeans(anscombe)
coef(lm(y1~x1, data = anscombe))
coef(lm(y2~x2, data = anscombe))
coef(lm(y3~x3, data = anscombe))
coef(lm(y4~x4, data = anscombe))

par(mfrow=c(2,2)) # Here we are setting up some basic parameters for our graphical device.
# In this case we are asking R to divide it up into 2 row by 2 columns. This will produce 4 graphs on a single graphical device, which is really useful

plot(y1 ~ x1, data=anscombe, xlim=c(4, 19), ylim=c(3,14), main= "ANSCOMBE DATA" ) 
# Scatterplot of y1 on x1 
# we are also setting the limits on the axes of the graph
# xlim=c(4,19) tells R to make the x axis to go from 4 to 9.
# main just makes a "main" label.
# by default the labels for the axes will be the variable names. This can be changed using the xlab="" and ylab="" options.

abline(lm(y1~x1, data=anscombe)) # the abline() plots a line with intercept=a and slope = b. When combined with the lm() it automatically inputs these values. abline can also be used for generating horizontal (option h=) or verticial (option v=) lines instead.

plot(y2 ~ x2, data = anscombe, xlim=c(4, 19), ylim=c(3,14) )
abline(lm(y2~x2, data = anscombe))

plot(y3 ~ x3, data = anscombe, xlim=c(4, 19), ylim=c(3,14))
abline(lm(y3~x3, data = anscombe))

plot(y4 ~ x4, data = anscombe, xlim=c(4, 19), ylim=c(3,14))
abline(lm(y4~x4, data = anscombe))

# We would also detect this with residual plots, but this after we have modeled the data. We would have liked to have known about these issues before we utilized our a priori models, no?

# Similarly we can have an example from a correlation

#### Making up data... don't worry about it...
# vcv <- matrix(c(2,-1.7,-1.7,2),2,2)
 # a <- MASS::mvrnorm(20, mu =c(5,5), Sigma =vcv)
 # b <- MASS::mvrnorm(20, mu =c(10,10), Sigma =vcv)
 # c <- MASS::mvrnorm(20, mu =c(15,15), Sigma =vcv)
 # d <- MASS::mvrnorm(20, mu =c(20,20), Sigma =vcv)
 
 # fac_levels <- gl(4,20, labels = c("a", "b", "c", "d"))
 # X <- data.frame(rbind(a,b,c,d),fac_levels)
# X.test <- X[,1:2]
#######

cor(X.test)
plot(X.test)

########Different approaches to graphing in R
# basic approach using base graphics implementation
# trellis style using the lattice package

#ggplot and ggplot2 are both implementations of the grammar of graphics
# see Hadley Wickham's website   http://had.co.nz/
# (lots of other useful packages like reshape, classify, clusterfly...)

# gplots (also see gtools) some basic tools



#
#### Histogram example from the powerpoint slides.
the.data <- rnorm(40, 10,1)
par(mfrow=c(3,1))  # Setting a graphical parameter so that it will plot 3 graphics.
hist(x=the.data, breaks=2,  main="histogram with 2 breaks")
hist(the.data, breaks=5, main="histogram with 5 breaks")
hist(the.data, breaks=8,  main="histogram with 8 breaks")

# as an alternative to a histogram, using a kernel density estimator
plot(density(the.data, adjust=0.5))

####plot####
par(mfrow=c(2,1))
plot(the.data, type="p", main = "the.data")
plot(sort(the.data), type="p", main= "SAME DATA AS ABOVE JUST SORTED")

par(mfrow=c(2,1))
plot(the.data, type="s", main = "the.data")
plot(sort(the.data), type="s", main= "SAME DATA AS ABOVE JUST SORTED")


###### boxplot#####
med <- median(the.data)
mean.data <- mean(the.data)
quant.data <- quantile(the.data) ### Data quantiles
par(mfrow=c(1,1))
boxplot(the.data, ylab="The data", main= "An annotated box plot of our fake data")

### Adding extra features
#### For the median
arrows(x0=1.34, y0=med, x1=1.22, y1=med, 
    lwd = 1, angle=15)
text(1.40 ,med,"median")

# For the mean
arrows(1.34, mean.data, 1.22, mean.data, lwd = 3, angle=15)
text(1.4 ,mean.data,"mean")

### For the upper and lower quantile
arrows(x0 = 1.34, y0 = quant.data[2], x1 = 1.22, y1 = quant.data[2], lwd = 1, angle=15)
text(1.44 ,quant.data[2],"lower quantile")

arrows(1.34, quant.data[4], 1.22, quant.data[4], lwd = 1, angle=15)
text(1.44 ,quant.data[4],"upper quantile")




#### OK, let's work with some real data
# make sure you start in the correct directory (working directory), with the input data

# setwd("/Users/ian/R/R scripts/Dll data/") 
# GUI method - under the R gui go to misc -> change working directory.
 
dll.data = read.csv("http://beaconcourse.pbworks.com/w/file/fetch/35183279/dll.csv", header=TRUE)   #data frame input

# Just a reminder
class(dll.data)
mode(dll.data)
attributes(dll.data)


# Sometimes it is useful to have a set of comments associated with a data. For ANY object we can add a comment easily. (This is formally setting an attribute to the object... but you probably do not care)

 x <- 1
 comment(x) <- c("x gets 1", " Thus we are assigning the value one to the symbol x")
comment(x)

# More useful for data frames, where you can place information
comment(dll.data) <- c("Data from Dworkin, I. 2005. ... Evolution & Development", "Collected in Toronto", "1999")


attributes(dll.data) # You can see it in the attributes of the object

# Comment can also be useful for functions that you write.



# take a look at the data
fix(dll.data) # provides a spreadsheet to examine data. Useful to see if missing
# values are encoded correctly.
# you need to close the spreadsheet before proceeding.


# Quality Control of the data frame - making sure that everything makes sense
str(dll.data) # displays the structure of the object (in this case the data frame)
## i.e. lists variables and their types (numeric, integer or factor)


summary(dll.data) # basic summary information for variables in the object

head(dll.data) # outputs the first 6 observations
tail(dll.data)
class(dll.data) # outputs the class of the object

names(dll.data) # names of variables


# graphical summary of ALL data
plot(dll.data) # all two-way scatterplots. Not so useful but try it once :)


### What if we wanted to look at a particular column of data

dll.data$genotype  #   data.set$column
summary(dll.data$genotype)
#################################



# Fixing the data as need be
#   transform numeric (integer) to "character" variables
#   both temperature and replicate were coded numerically originally
str(dll.data) # lists variables and their types to show this
dll.data$temp <- factor(dll.data$temp) # Factor is another class of object....
dll.data$replicate <- factor(dll.data$replicate)
str(dll.data) # check that this fixed the problem

 # After making these changes is when I would consider using attach(dll.data)



##### Attaching the data

# I may occassionally use attach during class, but you should not use  it!!! See my Intro lecture for why this is the case.

# instead you can use the with() function.



 # removing  missing data (NA's)
# one approach is to "clean-up" the data set from the beginning.
# normally you would not want to do that, but for the moment it will help
dll.data = na.omit(dll.data)


###############PLOTTING#############

##########################################
#manipulating and extracting data from the data frame object (the data!!).


### 
  # Subsetting the data often you want a subset of the data
  par(mfrow=c(1,1)) # Resetting the graphical device to have a single graphing window
  SCT_Dll = subset(dll.data, genotype == "Dll" )
  SCT_wt= subset(dll.data, genotype == "wt" )
  
hist(na.omit(SCT_Dll$SCT))  # Pretty basic... Let's jump right in and make this much more useful



# First we will draw the histogram  
hist(na.omit(SCT_Dll$SCT), col= "grey",
  # Sometimes the default breaks are not so good, so here we use a sequence from lowest observed SCT to highest
  breaks = seq(min(dll.data$SCT), max(dll.data$SCT)), # sets up breaks for histograms
  
  # Since we are planning on displaying two data sets, we need to make sure our y axis goes high enough
  # below we compute the tables for # of SCT for each subset
  ylim = c(0, max( c(table(SCT_wt$SCT), table(SCT_Dll$SCT)))),  # sets up the maximum
  
  # Ditto for  the x axis
  xlim = c(min(dll.data$SCT), max(dll.data$SCT)),
  
  main = "histogram for Sex Comb Teeth in Drosophila",  # Main title
  
  # Ignore the next line for the moment!!!! 
  #main = expression(paste("histogram for Sex Comb Teeth in ", italic(Drosophila))),  # Main title
  
  xlab = " # Sex Comb Teeth ",  # label for X axis
  border = "black" ) # this is where the call to hist ends!
   
# Now we add on a second histogram  
hist(na.omit(SCT_wt$SCT), col= "red",
 density = 4,  # number of "crossing" lines
 border = "red",  # The border colour for the histogram
 breaks=seq(min(dll.data$SCT),
 max(dll.data$SCT)), 
 add = TRUE)
  
#Perhaps a legend would also be useful here
legend(x = 15, # The beginning x coordinate for the legend
       y = 250,
       
       # Sometimes for our text we want to use italics or generate a mathematical expression. we use expression()
       # for more details ?text or ?plotmath
       # legend = c("mutant", "wildtype"),
       legend = c(expression(italic("Distal-less/+")), 
           expression(italic("wild-type"))),
       
       col = c("grey", "red"),  # Setting up the colours for the legend box
       lwd=4)   # lwd sets the line width


# While this may look good, sometimes histograms can be misleading, especially if the number of breaks is left to vary according to the whims of R.


#### Perhaps a better way of examining the data is to use a "kernel density estimator".
par(mfrow=c(2,1))

hist(na.omit(SCT_Dll$SCT), col= "blue", xlim=c(5,30))
  
plot(density(dll.data$SCT[dll.data$genotype=="Dll"],  
    na.rm=T, bw = 1),xlim=c(5,30), main="density")

  # of arguable validity or utility, we could also add the theoretical normal distribution
curve(dnorm(x, mean(na.omit(SCT_Dll$SCT)), 
      sd(na.omit(SCT_Dll$SCT))), from=5, to=30, col="red", add=T)
   # Just adding back the theoretical normal distribution based on the method of moments (MOM) estimates for the mean and sd.

  
# If you wanted to have both the histogram, and the curve for the density kernel estimator on the same plot, we need to use a little trick, since plot does not have an option for plotting multiple things on the same time.  There are actually a few tricks!!

# The reason I am showing you each of these ways, is that they will each be useful for a variety of applications. I want to make sure you have access to these approaches, so that you will be able to adapt them to your own needs.

####### Trick 1: First plot the density, then use hist() to add the density. 

 # Remember to Reset the option for how many plotting windows we want in the graphics device.
par(mfrow=c(1,1))

# Now we set up the initial density plot
plot(density(dll.data$SCT[dll.data$genotype=="Dll"],  na.rm=T, bw=1), 
       ylim=c(0, 0.25), xlim=c(5,30), 
       main= "Approximate density for # of SCT", xlab = "# of SCT", lwd=2 ) 

hist(na.omit(SCT_Dll$SCT), col= "blue", xlim=c(5,30), breaks=15, freq=F, add=T) # We stated freq=F, so that instead of frequencies, it is plotting ~ probability densities, like the density function does. Also noticed that we used add=T, to let R know that we want to add the histogram to the existing graphical device.

curve(dnorm(x, mean(na.omit(SCT_Dll$SCT)), sd(na.omit(SCT_Dll$SCT))), from=5, to=30, col="red", add=T, lwd=2) 


  
###### Trick 2: Plot the histogram, and then add the density using lines()  
hist(na.omit(SCT_Dll$SCT),
 col= "blue", xlim=c(5,30), ylim=c(0, 0.25), freq=F, 
 main= "Approximate density for # of SCT")   
 
# Now we use the line function (instead of plot) to add the density on. 
lines(density(dll.data$SCT[dll.data$genotype=="Dll"],  na.rm=T, bw=1), xlim=c(5,30) , lwd=2)

# Finally, if we want to put the theoretical normal distribution
curve(dnorm(x, mean(na.omit(SCT_Dll$SCT)),sd(na.omit(SCT_Dll$SCT))), 
  from=5, to=30, col="red", add=T, lwd = 2) 
  
####### Trick 3: using primitive graphical functions (par) to override the default behaviour of generating a new grapical device everytime plot() is called

hist(na.omit(SCT_Dll$SCT), col= "blue", xlim=c(5,30), breaks=15, freq=F, xlab="", ylab="", main="")

par(new=T)  # Here we are telling it not to use the default behaviour of replacing the graphics window.

plot(density(dll.data$SCT[dll.data$genotype=="Dll"],  na.rm=T, bw=1.00),
 ylim=c(0, 0.25), xlim=c(5,30), lwd=2, xlab= "", ylab="" ) 

# Clearly, one issue with this last approach is that it is plotting some words on top of others! With tinkering, you can get rid of this. THis approach is particularly useful for generating plots with multiple "y" axes.


### One more thing, before we move on. Just like examining histograms can lead you astray, so can using density.
par(mfrow=c(1,1)) 

plot(density(dll.data$SCT[dll.data$genotype=="Dll"],  na.rm=T, bw=1.0),
 ylim=c(0, 0.25), xlim=c(5,30), 
 main= "Approximate density for # of SCT", xlab = "# of SCT", lwd=2, col= "black" ) 
 
# Change the bandwidith (bw) to 0.75 
lines(density(dll.data$SCT[dll.data$genotype=="Dll"],  na.rm=T, bw=0.75),
 ylim=c(0, 0.25), xlim=c(5,30), 
 main= "Approximate density for # of SCT", xlab = "# of SCT", lwd=2, col= "grey" ) 

# Change bw=0.5 
lines(density(dll.data$SCT[dll.data$genotype=="Dll"],  na.rm=T, bw=0.5), lwd=2, col="red") # bw = 0.5

lines(density(dll.data$SCT[dll.data$genotype=="Dll"],  na.rm=T, bw=0.25), lwd=2, col="blue") # bw = 0.25

### What do you think is going on?




# It is important to investigate the effects of bandwith (bw=) on your empirical density estimate.
# One rule of thumb (for quantitative traits) for choosing a bandwith is to use (page 164 in the R book) a formula scaling the difference between the max and min values by a function of number of observations

bandwidth <- function(vector) {
	(max(vector) - min (vector)) / (2*(1+log(length(vector),2))) }

comment(bandwidth) <- c("This calculates the 'rule of thumb' bandwidth advocated by Michael Crawley and Venables and Ripley", " page 164, Chapter 5 in The R Book")	

# Utilize the bandwidth function
bw.1 <- bandwidth(SCT_Dll$SCT)	

plot(density(dll.data$SCT[dll.data$genotype=="Dll"],  na.rm=T, bw=bw.1), 
       ylim=c(0, 0.25), xlim=c(5,30), 
       main= "Approximate density for # of SCT", xlab = "# of SCT", lwd=2, col= "black" ) 


#####################

# Some other things we may want to consider
cor(dll.data$SCT,dll.data$tarsus, use="pairwise.complete.obs")

# cor.test(dll.data$SCT,dll.data$tarsus, use="pairwise.complete.obs") # We really should not be using this... I just wanted you to have the code...

# A basic scatterplot of two variables, the number of Sex Comb teeh (SCT and the length of the tarsus)
plot(dll.data$SCT ~ dll.data$tarsus, ylim=c(5,25)) # This is pretty basic , and it is hard to see what it going on, with all the points overlapping each other.

# A few options
#1 we could make the points smaller and see if that helps
plot(dll.data$SCT ~ dll.data$tarsus, ylim=c(5,25), cex=0.75, pch=16) # cex is character expand, we are multipying by 0.75

#2 This helped a bit, but we can also add some "jitter" around the points along the SCT (which are discrete)
plot(jitter(dll.data$SCT) ~ dll.data$tarsus, 
       ylim=c(5,25), cex=0.75) # cex is character expand, we are multipying by 0.75
 
 # This clearly helps make it clearer

#3 we can use col = densCols(x) argument in the plot function.
plot(dll.data$SCT ~ dll.data$tarsus, 
      ylim=c(5,25), 
      col = densCols(dll.data$SCT, dll.data$tarsus), # here the colour represents the local density of points.
      pch=16, cex=1.5) 

# A similar idea is to use smoothScatter
smoothScatter(dll.data$SCT ~ dll.data$tarsus, ylim=c(5,25), nrpoints = Inf, cex=2) # nrpoints=inf is just so we plot all of the points of data


###### We could instead think about the different treatment levels, and color those... 
# Fundamentally we are trying to convey a different set of information.

# we could also color spots that are useful to us

# First we re-set so that our graphics panel only has one plot on it.
par(mfrow=c(2,1))


# Now we are going to produce the scatterplot, but with color and symbols specific to our grouping variables
plot(jitter(dll.data$SCT) ~ dll.data$tarsus, 
       col=c("red", "blue")[dll.data$genotype], # Here we are saying pick colours based on the levels of genotype
       pch=c(16,17)[dll.data$temp],  # And here we are choosing the page characters (filled dots and triangles) based on temperature
       ylim=c(5,25), 
       cex=1.15)                       # expanding the size of the page characters
par(mfrow=c(1,1))
# This above chart is pretty basic, let's add some information about the axis, etc...
plot(jitter(dll.data$SCT) ~ dll.data$tarsus, 
       col=c("red", "blue")[dll.data$genotype], 
       pch=c(16,17)[dll.data$temp], 
       ylim=c(5,25), cex=1.15, 
       xlab="# of Sex Comb Teeth", ylab="Tarsus length", 
       main="scatterplot of Sex comb teeth and basi-tarsus length in Drosophia")


####### end of new bit
#lowess -locally-weighted polynomial regression
lines(lowess(dll.data$SCT ~ dll.data$tarsus), lwd=2, col="black") # using lowess 

# The problem is, that this non-linear spline is for both of the genotypes, we want a better sense across the two genotypes.

#using old version of lowess
lines(lowess(dll.data$SCT[dll.data$genotype=="Dll"] ~ dll.data$tarsus[dll.data$genotype=="Dll"]), lwd=2, col="red") 

lines(lowess(dll.data$SCT[dll.data$genotype=="wt"] ~ dll.data$tarsus[dll.data$genotype=="wt"]), lwd=2, col="blue")  

# You can argue about whether the fit out at the tails (where there is little data), is useful.
# Try a different approach, using a cubic smoothing spline

lines(smooth.spline(dll.data$SCT[dll.data$genotype=="Dll"]~ dll.data$tarsus[dll.data$genotype=="Dll"]), lwd=2, col="red", lty=2) # fitting a non-linear spline using lowess 

lines(smooth.spline(dll.data$SCT[dll.data$genotype=="wt"] ~ dll.data$tarsus[dll.data$genotype=="wt"]), lwd=2, col="blue", lty=2)  



# Some information for the figure for the lecture

arrows(0.25,21.5,0.25, 15, lwd=1, angle=15)
text(0.25, 22.2 ,"LOWESS")

legend(0.11, 22.5, 
  legend=c(expression(italic("Distal-less/+")), expression(italic("wild-type"))) , 
  col=c("red", "blue"), pch=c(16,17), cex=1.5) # How to add a basic legend for basic plots.
# We did not fully specify the legend here, but it would be easy!

  
  
# It is sometimes useful to identify individual points on a plot (for outlier detection)
identify(dll.data$SCT, dll.data$tarsus)

# should we try a log scale?
plot(log(dll.data$SCT), log(dll.data$tarsus))
lines(lowess(log(dll.data$SCT), log(dll.data$tarsus)), lwd=2, col="red")


######Boxplots####
par(mfrow=c(1,4), cex = 1)
plot(dll.data$SCT ~ dll.data$genotype, col="grey", main= "SCT" ) # notice how we did not call boxplot
plot(dll.data$femur ~ dll.data$genotype, col="red", main= "femur" ) # notice how we did not call boxplot
plot(dll.data$tibia ~ dll.data$genotype, col = "blue", main= "tibia" )
plot(dll.data$tarsus ~ dll.data$genotype, col = "purple", main= "tarsus")

# For more complicated boxplots, we need to use the boxplot()
boxplot(tarsus ~ genotype + temp, data=dll.data, col = "purple", ylab="tarsus length")
# see ?boxplot for some nice examples as well.



#### From the lattice graphic system###
library(lattice)
bwplot(SCT ~ temp|genotype,data=dll.data)
bwplot(SCT ~ genotype|temp,data=dll.data) # Sometimes it is useful to switch this around
bwplot(SCT ~ line|genotype*temp,data=dll.data)

# The lattice library, which implements the trellis style of graphics is incredibly powerful, but it can take a while to get used to using it. Here is one example of the kind of power it has.

xyplot(SCT ~ tarsus | line, data = na.omit(dll.data), 
  xlab = " tarsus length", ylab = "# SCT", lwd=3, pch=21, fill="grey",
  type = c("r", "p"),  # Here we have asked for points (p), and a regression line ("r")
  groups=genotype:temp,  # adds colours for particular levels of the grouping variables
  auto.key = list(points = TRUE, lines = TRUE, columns = 2)) # generating the legend


# This gives an alternative view of the data
xyplot(SCT ~ tarsus | genotype + temp, data = na.omit(dll.data), 
  xlab = " tarsus length", ylab = "# SCT", lwd=3, pch=21, fill="grey",
  type = c("r", "p"),  
  groups=line)


# In this case, we do not really have enough data to estimate each of the regression lines, but it gives us some ideas

# In any case if you like the lattice graphics, check out the book on lattice/trellis... links to it on angel.

# The other graphic system is ggplot2.. I really have not used it before, but here goes...
require(ggplot2)
ggplot(dll.data, aes(y = SCT, x = tarsus)) + geom_point() # geom_point makes this a scatterplot

#Add a smooth line and standard error spaces.#ggplot(dll.data, aes(y = SCT, x = tarsus)) + geom_point() +	stat_smooth(fill = alpha("blue", 0.2), 
#  colour = "darkblue", size = 2)



############# Interaction plots #################
dll.data$temp <- factor(dll.data$temp)
dll.data = na.omit(dll.data) # it has problems with NA's
par(mfrow=c(1,2), cex = 1)

interaction.plot(dll.data$temp, dll.data$line, dll.data$SCT, fixed=TRUE, legend=F)
interaction.plot(dll.data$temp, dll.data$genotype, dll.data$SCT, legend=F)

#the above approach produces ugly plots - instead use the sciplot package
par(mfrow=c(1,1), cex = 1)

install.packages("sciplot") # only need to do this once per computer

require(sciplot)

lineplot.CI(x.factor = temp, response =SCT, group = genotype, data = dll.data, 
  cex = 2, xlab = "temp", ylab = "SCT", cex.lab = 1.5, x.leg = 1.75,
  col = c("blue","red"), pch = c(16,16))


lineplot.CI(line, SCT, group = temp:genotype, data = na.omit(dll.data), cex = 2,
  xlab = "temp", ylab = "SCT", cex.lab = 1.5, x.leg = 1.75,
  col = c("blue","red"), pch = c(16,16))


 # OK... in groups... play with the data. Make informative and attractive plots that allow you to explore this data.  What do these graphs tell you? Does it give you ideas of how best to address the hypotheses I posed?