SimmieFunction <- function(){
	y <-rnorm(50); x <- rnorm(50) # What is important here is that y, x and are random wrt each other
	lm.1 <- lm(y~x)
	return(summary(lm.1)$coef[2,4])
	}


p <- replicate(10000, SimmieFunction())

p_05 <- length(p[p<=0.05])/length(p)

hist(p)

p_05