### R code from vignette source 'CSE891_March15th_LinearModelsParametersIntro.Rnw'

###################################################
### code chunk number 1: CSE891_March15th_LinearModelsParametersIntro.Rnw:8-10
###################################################
options(show.signif.stars=F)
options(digits=3)


###################################################
### code chunk number 2: CSE891_March15th_LinearModelsParametersIntro.Rnw:18-26
###################################################
x <- 1:20 # Our "x" values.  

beta_0 <- 2 # Our "real" intecept. We know it is real, because we decide it apriori!!!!

beta_1 <- 2 # this is our "real" slope

unexplained_sd <- 2 # This is the unaccounted for variation. 
 # Compare it to the residual standard error for a regression model.


###################################################
### code chunk number 3: CSE891_March15th_LinearModelsParametersIntro.Rnw:31-32
###################################################
y <- rnorm(x, mean= beta_0 + beta_1*x, sd=unexplained_sd) 


###################################################
### code chunk number 4: CSE891_March15th_LinearModelsParametersIntro.Rnw:40-47
###################################################
VarFunction <- function(x) {
	S.S <- sum((x-mean(x))^2)  # Sum of Squares
	variance <- (1/(length(x)-1))*S.S
	return(variance)
}

VarFunction(y)


###################################################
### code chunk number 5: CSE891_March15th_LinearModelsParametersIntro.Rnw:52-53
###################################################
var(y)


###################################################
### code chunk number 6: CSE891_March15th_LinearModelsParametersIntro.Rnw:59-66
###################################################
CovarianceFunction <- function(x,y) {
	sum.cross.products <- sum( (x-mean(x)) * (y-mean(y)) )  # Sum of cross products
	covariance <- (1/(length(x)-1))*sum.cross.products 
	return(covariance)
}

CovarianceFunction(x,y) 


###################################################
### code chunk number 7: CSE891_March15th_LinearModelsParametersIntro.Rnw:70-71
###################################################
cov(y,x)


###################################################
### code chunk number 8: CSE891_March15th_LinearModelsParametersIntro.Rnw:79-87
###################################################
CorrelationFunction <- function(x,y){
	sum.cross.products <- sum( (x-mean(x)) * (y-mean(y)) )  # Sum of cross products
	covariance <- (1/(length(x)-1))*sum.cross.products
	correlation <- covariance/(sd(x)*sd(y)) # scaled by the products of the standard deviations for x and y
	return(correlation)
}

CorrelationFunction(y,x)


###################################################
### code chunk number 9: CSE891_March15th_LinearModelsParametersIntro.Rnw:92-93
###################################################
cor(y,x)


###################################################
### code chunk number 10: CSE891_March15th_LinearModelsParametersIntro.Rnw:105-117
###################################################
plot(y~x, xlim=c(0, max(x)), ylim=c(0, max(y)) )
abline(lm(y~x), lty=2) # The best fit regression line

# add the mean of x and y as a point (red dot)
points(mean(x), mean(y), pch=16, col="red", cex=2)

# and some text to remind us of a very significant issue
text(x = 15.5, y = 20, col="red",
 expression(paste("The fitted regression line ", bold(y) == hat(beta)[0] + hat(beta)[1]))) 

text(x = 15, y = 18, col="red",
  expression(paste("must pass through " , "(",  bar(x), ", ", bar(y), ")")))


###################################################
### code chunk number 11: CSE891_March15th_LinearModelsParametersIntro.Rnw:125-126
###################################################
model.1 <- lm(y~x)


###################################################
### code chunk number 12: CSE891_March15th_LinearModelsParametersIntro.Rnw:131-132
###################################################
summary(model.1)


###################################################
### code chunk number 13: CSE891_March15th_LinearModelsParametersIntro.Rnw:144-146
###################################################
slope <- cov(y,x)/var(x)
slope


###################################################
### code chunk number 14: CSE891_March15th_LinearModelsParametersIntro.Rnw:151-154
###################################################
cross.product.xy <- sum( (x - mean(x))*(y - mean(y))) 
SS.x             <- sum( (x- mean(x))^2 ) 
(slope.alt       <- cross.product.xy / SS.x )


###################################################
### code chunk number 15: CSE891_March15th_LinearModelsParametersIntro.Rnw:159-161
###################################################
intercept <- mean(y) - slope*mean(x)
intercept 


###################################################
### code chunk number 16: CSE891_March15th_LinearModelsParametersIntro.Rnw:167-187
###################################################
plot(y~x, xlim=c(0, max(x)), ylim=c(0, max(y)) )
abline(lm(y~x), lty=2) # The best fit regression line

# add the mean of x and y as a point (red dot)
points(mean(x), mean(y), pch=16, col="red", cex=2)

abline(v=mean(x), lty=4, col="grey") # mean of x
abline(h=mean(y), lty=4, col="grey") # mean of y

points(x=0, y=intercept, col="blue", pch=16, cex=2)

text(x = 4.25, y =1, col="blue",
  expression(paste("The intercept ", hat(beta)[0], " when ",  x == 0)))

# and some text to remind us of a very significant issue
text(x = 15.5, y = 20, col="red",
 expression(paste("The fitted regression line ", bold(y) == hat(beta)[0] + hat(beta)[1]))) 

text(x = 15, y = 18, col="red",
  expression(paste("must pass through " , "(",  bar(x), ", ", bar(y), ")")))


###################################################
### code chunk number 17: CSE891_March15th_LinearModelsParametersIntro.Rnw:197-201
###################################################
mean(resid(model.1)) # ~ 0, explain why.

RSE <- sqrt(sum(resid(model.1)^2)/(length(y) - 2))
RSE


###################################################
### code chunk number 18: CSE891_March15th_LinearModelsParametersIntro.Rnw:210-213
###################################################
cor(y,x)^2
# Compare to
summary(model.1)$r.squared


###################################################
### code chunk number 19: CSE891_March15th_LinearModelsParametersIntro.Rnw:219-223
###################################################
anova(model.1)
model.SS <- anova(model.1)[1,2]
total.SS <- anova(model.1)[1,2] + anova(model.1)[2,2]
model.SS/total.SS


###################################################
### code chunk number 20: CSE891_March15th_LinearModelsParametersIntro.Rnw:228-230
###################################################
total.SS.alt <- sum( (y - mean(y))^2 )  # The total amount of variation (measured in SS) for y!
total.SS.alt


###################################################
### code chunk number 21: CSE891_March15th_LinearModelsParametersIntro.Rnw:238-240
###################################################
variance.explained <- var(y) - RSE^2 # Total variance - unexplained variance.
(R.adj <- variance.explained/var(y))


###################################################
### code chunk number 22: CSE891_March15th_LinearModelsParametersIntro.Rnw:256-264
###################################################
plot(y ~ fitted(model.1), pch = 16, 
  ylab  = expression(paste("observed values of response, ", bold(y))),
  xlab  = expression(paste(" model fitted values of the response, ", hat(bold(y)))),
  main  = expression(paste("fitted VS. observed values for the response, ", 
  bold(y), " VS. ", hat(bold(y)))))

# Now we can fit a  line of 1 to 1 correspondence
abline(a = 0, b =1, col ="grey", lwd=2)  


###################################################
### code chunk number 23: CSE891_March15th_LinearModelsParametersIntro.Rnw:273-274
###################################################
anova(model.1)


###################################################
### code chunk number 24: CSE891_March15th_LinearModelsParametersIntro.Rnw:280-281
###################################################
RSE^2


###################################################
### code chunk number 25: CSE891_March15th_LinearModelsParametersIntro.Rnw:288-293
###################################################
null.model <- lm( y ~ 1) 
summary(null.model) # This fits only one parameter, the sample mean of y
mean(y)

anova(null.model) 


###################################################
### code chunk number 26: CSE891_March15th_LinearModelsParametersIntro.Rnw:297-299
###################################################
null.model.SS <- anova(null.model)[1,2]
total.SS


###################################################
### code chunk number 27: CSE891_March15th_LinearModelsParametersIntro.Rnw:304-305
###################################################
(null.model.SS - model.SS) 


###################################################
### code chunk number 28: CSE891_March15th_LinearModelsParametersIntro.Rnw:312-313
###################################################
anova(null.model, model.1)


###################################################
### code chunk number 29: CSE891_March15th_LinearModelsParametersIntro.Rnw:317-318
###################################################
anova(model.1)


###################################################
### code chunk number 30: CSE891_March15th_LinearModelsParametersIntro.Rnw:324-325
###################################################
summary(model.1)


###################################################
### code chunk number 31: CSE891_March15th_LinearModelsParametersIntro.Rnw:334-336
###################################################
SE.slope <- sqrt(RSE^2/SS.x)
SE.slope


###################################################
### code chunk number 32: CSE891_March15th_LinearModelsParametersIntro.Rnw:340-341
###################################################
slope/SE.slope


###################################################
### code chunk number 33: CSE891_March15th_LinearModelsParametersIntro.Rnw:348-349
###################################################
confint(model.1)


###################################################
### code chunk number 34: CSE891_March15th_LinearModelsParametersIntro.Rnw:352-353
###################################################
model.1$coef


