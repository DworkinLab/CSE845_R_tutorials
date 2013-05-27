x <- rnorm(100, 20, 4)

par(mfrow=c(2,2))
hist(x, main="default breaks")
hist(x, breaks=4, main = "4 breaks")
hist(x, breaks=8, main = "8 breaks")
hist(x, breaks=20, main = "20 breaks")

par(mfrow=c(2,2))
plot(density(x), main="default bw")
plot(density(x, bw=0.5), main="bw = 0.5")
plot(density(x, bw=1), main="bw = 1")
plot(density(x, bw=2), main="bw = 2")


# repeat with sample size = 1000

x <- rnorm(1000, 20, 4)

par(mfrow=c(2,2))
hist(x, main="default breaks")
hist(x, breaks=4, main = "4 breaks")
hist(x, breaks=8, main = "8 breaks")
hist(x, breaks=20, main = "20 breaks")

par(mfrow=c(2,2))
plot(density(x), main="default bw")
plot(density(x, bw=0.5), main="bw = 0.5")
plot(density(x, bw=1), main="bw = 1")
plot(density(x, bw=2), main="bw = 2")