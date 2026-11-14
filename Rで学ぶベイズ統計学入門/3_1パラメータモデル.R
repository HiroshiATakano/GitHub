alpha <- 16; beta <- 15174
yobs <- 1; ex <- 66
y <- 0:10

lam <- alpha/beta

py <- dpois(y, lam*ex) * dgamma(lam, shape = alpha, rate = beta)/
  + dgamma(lam, shape = alpha + y, rate = beta + ex)
cbind(y, round(py,3))

mu <- 100
tau <- 12.16
sigma <- 16
n <- 4

se <- sigma/sqrt(n)
ybar <- c(110, 125, 140)
tau1 <- 1/sqrt(1/se^2 + 1/tau^2)
mu1 <- (ybar/se^2 + mu/tau^2)*tau1^2
summ1 <- cbind(ybar, mu1, tau1)
summ1



tscale <- 20/qt(0.95, 2)

norm.t.compute <- function(ybar) {
  theta <- seq(60, 180, length=500)
  like <- dnorm(theta, mean=ybar, sd=sigma/sqrt(n))
  prior <- dt((theta - mu)/tscale, 2)
  post <- prior*like
  post <- post/sum(post)
  m <- sum(theta*post)
  s <- sqrt(sum(theta^2*post)-m^2)
  c(ybar, m, s)
}

summ2 <- t(sapply(c(110, 120, 140), norm.t.compute))

summ2


theta <- seq(60, 180, length=500)

normpost <- dnorm(theta, mu1[3], tau1)
normpost <- normpost/sum(normpost)
plot(theta, normpost, type="l", lwd=3, )


mu1[3]
