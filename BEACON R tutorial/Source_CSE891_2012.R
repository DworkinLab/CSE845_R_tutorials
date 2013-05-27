# Source file for CSE891 of simple useful functions. I will add more as the class goes on.
# Ian Dworkin - Last updated Jan 12th 2012
# co-efficient of variation
CoefVar   <- function(x) {
	          x <- na.omit(x)
	          sd(x)/abs(mean(x))}
comment(CoefVar) <- "cv, calculates co-efficient of variation, given an input vector of numeric observations"
	
	
# Standard error of the mean  
StdErr    <- function(x) {
	           x <- na.omit(x)
	           sd(x)/sqrt(length(x))}
comment(StdErr) <- "se, calculates the simple standard error for a numeric vector of observations"     
