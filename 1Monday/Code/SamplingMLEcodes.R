## Simulation: counting orchids once

set.seed(12345) # For reproducibility
p.orchid = 0.4 # The true proportion of orchids
n.picks = 10 # The number of picks in the field
n.times = 1
# Collect data
y <- rbinom(n.times, size = n.picks, prob = p.orchid) 
y/n.picks # Proportion of orchids

## Simulation: counting orchids once

set.seed(12345) # For reproducibility
n.times = 1e5 # The number of picks in the field
n.picks <- 1
 # Collect data
y <- rbinom(n.times, size = n.picks, prob = p.orchid)
mean(y/n.picks) # Proportion of orchids

## Simulation: counting orchids 50x10 times

set.seed(12345) # For reproducibility
n.times <- 50
n.picks = 10 # The number of picks in the field
# Collect data
y <- rbinom(n.times, size = n.picks, prob = p.orchid) 
hist(y, xlab = "Proportion of orchids", 
     ylab = "Number of samplings")

## Simulation: counting orchids 50x10 times

plot(table(y/n.picks), xlab = "Proportion of orchids", ylab = "Number of samplings")
abline(v= 0.4, col = "red", lty = "dashed")

  
## Q: Are more than half of the flowers in this field orchids?

quantile(y/n.picks,c(0.025,.975))

## Q: Are more than half of the flowers in this field orchids?
set.seed(12345)
n.picks = 100
n.times <- 50

# Collecting data
y <- rbinom(n.times, size = n.picks, prob = p.orchid) 
quantile(y/n.picks,c(0.025,.975))

## On average, we will get it right.

set.seed(12345)
n.picks = 1;n.times <- 100
y <- rbinom(n.times, size = n.picks, prob = p.orchid)
quantile(y/n.picks,c(0.025,.975))
mean(y/n.picks)

## The likelihood (3)

plot(dnorm, from = -1, to = 1, xlab = "Some parameter", ylab = "Likelihood")

## The log-likelihood
ll <- function(x)dnorm(x, log = TRUE)
plot(ll, from = -1, to = 1, xlab = "Some parameter", ylab = "log-Likelihood")

## Finding the proportion of orchids
set.seed(12345)
n.picks = 100
n.times <- 50
y <- rbinom(n.times, size = n.picks, prob = p.orchid) # Collecting data

ll <- function(p, n.picks, y)prod(dbinom(y, n.picks,p))
phat <- seq(0.35,0.45,length.out=3)
plot(sapply(phat, ll, n.picks = n.picks, y = y), 
     x = phat, type = "l", xlab=expression(hat(pi)), ylab="Likelihood")

## Finding the proportion of orchids (2)
set.seed(12345)
n.picks = 100
n.times <- 50
y <- rbinom(n.times, size = n.picks, prob = p.orchid) # Collecting data

ll <- function(p, n.picks, y)prod(dbinom(y, n.picks,p))
phat <- seq(0.35,0.45,length.out=1000)
plot(sapply(phat, ll, n.picks = n.picks, y = y),
     x = phat, type = "l", xlab=expression(hat(pi)), ylab="Likelihood")

## Maximising the likelhood

Trying many values (grid) is very inefficient

We can:

- analytically: do mathematics
- numerically: use an algorithm (as in GLMs)
- simulation: try many values

## Finding the maximum
par(mfrow=c(2,2))
ll <- function(p, n.picks, y)sum(dbinom(y, n.picks,p, log = TRUE))
phat <- seq(0.35,0.45,length.out=1000)
lls <- sapply(phat, ll, n.picks = n.picks, y = y)
grad.dbinom=function(y, n.picks, prob) {
    sum(y/prob - (n.picks - y)/(1-prob))
}

p = c(0.36,0.38,0.40, 0.42)
for(i in p){
b<-grad.dbinom(y,n.picks,i)
a<--b*i+ll(i,n.picks,y)
pphat <- phat[order(abs(phat-i),decreasing=FALSE)[1:200]]
plot(lls, x = phat, type = "l", xlab=expression(hat(pi)), ylab="log-Likelihood", lty = "dashed",main = i)
segments(x0=i,x1 = i, y0=ll(0.1,n.picks,y), y1= ll(i,n.picks,y), col="red", lty = "dotted")
lines(x=pphat,y=a+b*pphat, col = "red")
}

## Finding the maximum: gradient
phat =  seq(0.05,0.95,length.out=1000)
plot(sapply(phat, grad.dbinom, y=y, n.picks = n.picks),
     x = phat, type = "l", xlab=expression(hat(pi)), 
     ylab="Gradient", lty = "dashed")
abline(h=0, col = "red")

## Letting \texttt{R} do the work
optimize(ll, n.picks = n.picks, y=y,
         lower = 0, upper = 1, maximum = TRUE)

# Uncertainty
ll <- function(p, n.picks, y)prod(dbinom(y, n.picks,p))
phat <- seq(0.35,0.45,length.out=1000)
plot(sapply(phat, ll, n.picks = n.picks, y = y), x = phat, type = "l", xlab=expression(hat(pi)), ylab="Likelihood", lty = "dashed")
p = optimize(ll, n.picks = n.picks, y=y,
         lower = 0, upper = 1, maximum = TRUE)
se = sqrt(1/(sum(y/p$maximum^2+(n.picks-y)/(1-p$maximum)^2)))

points(x=p$maximum, y=p$objective,col="red")
abline(v=p$maximum+1.96*se, col="red")
abline(v=p$maximum-1.96*se, col="red")

## Why not?
set.seed(12345)
p.orchid = 0.01
n.times <- 10
n.picks =10
y <- rbinom(n.times, size = n.picks, prob = p.orchid) 

ll <- function(p, n.picks, y)prod(dbinom(y, n.picks,p))
phat <- seq(1e-10,0.2,length.out=1000)
plot(sapply(phat, ll, n.picks = n.picks, y = y), x = phat, type = "l", xlab=expression(hat(pi)), ylab="Likelihood", lty = "dashed", xlim = c(-0.1,0.1))
p = optimize(ll, n.picks = n.picks, y=y,
         lower = 0, upper = 1, maximum = TRUE)
se = sqrt(1/(sum(y/p$maximum^2+(n.picks-y)/(1-p$maximum)^2)))

points(x=p$maximum, y=p$objective,col="red")
abline(v=p$maximum+1.96*se, col="red")
abline(v=p$maximum-1.96*se, col="red")

## Why not? (2)
set.seed(12345)
p.orchid = 0.99
n.times <- 10
n.picks =10
y <- rbinom(n.times, size = n.picks, prob = p.orchid) 

ll <- function(p, n.picks, y)prod(dbinom(y, n.picks,p))
phat <- seq(0.95,1-1e-12,length.out=1000)
plot(sapply(phat, ll, n.picks = n.picks, y = y), x = phat, type = "l", xlab=expression(hat(pi)), ylab="Likelihood", lty = "dashed", xlim = c(0.95,1.02))
p = optimize(ll, n.picks = n.picks, y=y,
         lower = 0, upper = 1, maximum = TRUE)
se = sqrt(1/(sum(y/p$maximum^2+(n.picks-y)/(1-p$maximum)^2)))

points(x=p$maximum, y=p$objective,col="red")
abline(v=p$maximum+1.96*se, col="red")
abline(v=p$maximum-1.96*se, col="red")

## Repetition
p.orchid = 0.4
n.times <- 10
n.picks = 10
CI <- NULL
for(i in 1:20){
y <- rbinom(n.times, size = n.picks, prob = p.orchid) 
CI <- rbind(CI,quantile(y/n.picks,c(0.025,.975)))
}

plot(1,1, type="n", xlim=range(CI), ylim=range(1:20), xlab=expression(hat(pi)), ylab="Replicate")
arrows(x0=CI[,1], y0=1:20, x1=CI[,2], y1=1:20, lwd=1.2, angle = 90, code=3, length=0.05)
abline(v=0.4, col="red")
