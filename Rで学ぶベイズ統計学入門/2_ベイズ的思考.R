library(LearnBayes)

p <- seq(0.05, 0.95, by=0.1)
prior <- c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior <- prior/sum(prior)
plot(p, prior, type="h", ylab="Prior Probability")


data <- c(11, 16)
post <- pdisc(p, prior, data)
round(cbind(p, prior, post),2)

library(lattice)
PRIOR <- data.frame("Prior", p, prior)
POST <- data.frame("Posterior", p, post)
names(PRIOR) <- c("Type", "p", "Probability")
names(POST) <- c("Type", "p", "Probability")
data <- rbind(PRIOR, POST)
xyplot(Probability ~ p|Type, data=data, layour=c(1,2), type="h", lwd=3, col="black")



a <- 3.26
b <- 7.18
s <- 11
f <- 16
qbeta(c(0.05, 0.95), a+s, b+f)


