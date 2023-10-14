######################################################################
## Delta Method Example 
######################################################################

######################################################################
## We begin by simulating the mean of 1000 bernoulli trials 10,000 
## times
######################################################################

n <- 1000
p <- .5 
x <- rbinom(10000, n, p )/n

##histogram of the data 

hist(x)

## mean of the means 
mean.p <- mean(x)
print(mean.p)

## variance of the means 
var(x)

## calculated the true variance of the mean (using p) for all 10000 simulations 
p * ( 1 - p) / n

## calculated the estimated variance of the mean for all 10000 simulations   
est.var <- x * ( 1 - x) / n
hist(est.var)

## 95% confidence interval for all 10000 estimates of the mean 
upper.ci <- x + qnorm(.975) * sqrt( est.var)
lower.ci <- x - qnorm(.975) * sqrt( est.var)

##calculate coverage probability

sum(upper.ci >= .5 &  lower.ci <= .5) / 10000

######################################################################
## We now perform the same calculations for the sin of x 
######################################################################

hist(sin(x))

mean(sin(x))

var(sin(x))

## calculated the true variance of the mean (using p)  
cos(p)^2 * p * ( 1 - p) / n

## calculated the estimated variance of the mean (using estimate of  p)  
est.sin.var <- cos(x)^2 * x * ( 1 - x) / n
hist(est.sin.var)

## 95% confidence interval for our estimate of the sin of p
upper.ci <- sin(x) + qnorm(.975) * sqrt(est.sin.var)
lower.ci <- sin(x) - qnorm(.975) * sqrt(est.sin.var)

##calculate coverage probability

sum(upper.ci >= sin(.5) &  lower.ci <= sin(.5)) / 10000


######################################################################
## Permutation Test 
######################################################################

set.seed(33)

## Here we wish to test if the difference in the means of the two groups is 0 
group.1 <- rnorm(100, mean = 0, sd = 1)
group.2 <- rnorm(100, mean = .5, sd = 1)

##Visulaize the data 
hist(group.2, col = 'red', ylim = c(0,60))
hist(group.1, col = 'blue', add = TRUE)

##since we know the distribution of the two groups, we can just do a 
##t-test

t.test(group.1, group.2)

##what if we did not know that the two groups followed a normal distribution
##and were not willing to make this assumption? 

mean.diff <- mean(group.2) - mean(group.1)
all.data <- c(group.1, group.2)

##permutaiton test

permutation.means <- rep(0, 100000)

for(i in 1:100000){
index <- sample(1:length(all.data), length(all.data)/2, replace = FALSE)
permutation.means[i] <- mean(all.data[index] - all.data[-index])
}

##plot results 

hist(permutation.means)
abline(v = mean.diff, col = 'red')

