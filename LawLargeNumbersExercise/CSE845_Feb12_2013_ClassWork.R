# CSE891 Tuesday Feb 12th 2013.

# Please email ian the plots with answers to the questions as a PDF. email your code for the simulations as a .R text file.

# Today in class you will work in groups to perform some simple simulations with three goals in mind
# Goal 1 - Be able to write your own simple simulations in R.
# Goal 2 - To develop a sense (from the simulations) about an important "law" in statistics.
	# This law tells us about what we expect to observe about estimated values (such as an sample mean), relative to the "true" (population) parameter as sample size increases. 
  	#( I expect you to tell me the name of the law).
# Goal 3 - To get a sense of how different estimates of the "moments" of a distribution (in this case the sample mean and the sample std. dev.) approach the "true" values with increasing sample size.


# First. Let us say we get to play the role of a deity, and can "choose" the form and parameters for a distribution.  This distribution is ~N(mean = 20, sd = 6).
# You now collect samples from this population. You start by collecting 5 samples and use this to estimate the mean and sd. You then incremenetly increase your sample size by 1, until you reach a sample size of 1000.

# Before you start the simulation EACH person in your group should answer the following questions.
# 1) Are you estimating the sample or population mean/sd? Why?
# 2) What (if any) relationship do you expect to observe between your estimated mean and the true value for the mean as sample size increases?
# 3) What (if any) relationship do you expect to observe between your estimated std dev. and the true value std. dev. as sample size increases? In what way should this differ from the mean?


# Now I want you as a group to write a simulator to perform the above. 
# Compute the estimated mean, standard deviation, the standard error ( which is the standard deviation/sqrt(sample size)) and the coefficient of variation (standard deviation/mean)

# For each of these produce a plot relating the estimated value to the true value as sample size increases.


# Since we have not gone over plotting in much detail (though you should have read something about it in the text), here would be an example of one such way of doing the plot
y100 <- rnorm(100)
x100 <- 1:100

par(mfrow=c(2,2))  #  This creates a "palette" for 4 plots in 2 rows X 2 columns
plot(y100 ~ x100, main="plot title", xlab="x axis", ylab = "y axis")
plot(y100 ~ x100, main="2nd plot title", xlab="x axis", ylab = "y axis")
plot(y100 ~ x100, main="3rd plot title", xlab="x axis", ylab = "y axis")
plot(y100 ~ x100, main="4rd plot title", xlab="x axis", ylab = "y axis")

# 4) Now that you have run the simulations, what have you observed about the relationship between  sample size and your estimates? Does this change your answers to the questions above?
# 5) What do you observe for the CV and SE? How does this relate to the patterns for the mean and sd? Are the patterns the same? Is there any important differences between mean and sd with increasing sample size?
# 6) What is the name for this relationship?

# 7) Redo this simulation once more. Do you get the exact same result? Explain why or why not. Provide plots as well.
# 8) Redo the simulation once more but change the sd to 15. How does this change your results. Provide plots as well. Explain what the differences are why!

