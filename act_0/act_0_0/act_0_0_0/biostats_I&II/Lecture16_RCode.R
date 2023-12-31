#t-test power using power.t.test function


power.t.test(n = 16, delta = 2/4, type = "one.sample", alt = "one.sided")
power.t.test(n = 16, delta = 2, sd=4, type = "one.sample", alt = "one.sided")


#t-test power using Monte Carlo simulation

nosim <- 100000
n <- 16
sigma <- 4
mu0 <- 30
mua <- 32
z <- rnorm(nosim)
xsq <- rchisq(nosim, df = 15)
t <- qt(.95, n-1)
mean(z+sqrt(n)*(mua-mu0)/sigma > t/sqrt(n-1)*sqrt(xsq))

#power calculation for question 9 from HW5
1-pnorm(qnorm(0.95)-0.01/(0.04/10))
