## The orchids example
set.seed(12345) # For reproducibility
n.times <- 50;n.picks=10;p.orchid <- 0.4
y <- rbinom(n.times, size = n.picks, prob = p.orchid) 

hist(y, xlab = "Proportion of orchids", 
     ylab = "Number of samplings", freq = FALSE, col = "white")
lines(density(y), col = "red", lty = "dashed")

## The orchids example
set.seed(12345) # For reproducibility
n.times <- 100;n.picks=1;p.orchid <- 0.4
y <- rbinom(n.times, size = n.picks, prob = p.orchid) 

hist(y, xlab = "Proportion of orchids", 
     ylab = "Number of samplings", freq = FALSE, col = "white")
lines(density(y), col = "red", lty = "dashed")

## The t-distribution
par(mfrow=c(2,2))
x <- sort(rnorm(1000))
plot(dt(x, .1), x = x, type="l", main = "df = .1")
plot(dt(x, .5), x = x, type="l", main = "df = .5")
plot(dt(x, 2), x = x, type="l", main = "df = 2")
plot(dt(x, 4), x = x, type="l", main = "df = 4")

## The t-test
set.seed(12345)
y <- rnorm(10)
x <- rnorm(10, mean = 2)
t.test(x, y)

## t-test visualized
set.seed(12345)
y <- rnorm(10)
x <- rnorm(10, mean = 2)
plot(c(1,2), y = range(c(x,y)), type="n", xaxt="n", ylab="Some variable", xlim = c(0,3), xlab="Groups")
axis(1, at = c(1,2), labels = c("x","y"))
points(rep(1:2,each=10),c(x,y))
points(c(1,2), c(mean(x),mean(y)), col="red", pch=16)
lines(c(1,2), c(mean(x),mean(y)), col="red")

## t-test as linear regression
data <- data.frame(y = c(x, y), 
                   var = c(rep(c("x","y"),each=10)))
lm(y~0+var, data = data)

## Examples of linear models: categorical $x_i$
set.seed(12345)
x <- rbinom(50, 0.5, size = 1)
y = 20 + 5*x + rnorm(50)
Means <- aggregate(y,list(x),mean)
par(mfrow=c(1,1), mar=c(4.1,4,1,1), oma=c(0,0,0,0))
plot(y, jitter(1*x), yaxt="n", ann=FALSE, 
     col="grey50") 
points(Means[,2], c(0,1), col=2, pch=3, cex=4)
text(Means[1,2], 0.2, expression(beta[0]), cex=3, adj=-0.2)
text(Means[2,2], 0.8, expression(beta[1]), cex=3, adj=-0.2)

axis(2, c("Something", "Something else"), at=c(0,1), las=1)

## Examples of linear models: categorical $x_i$
set.seed(12345)
x <- rbinom(50, 0.5, size = 1)
y = 20 + 5*x + rnorm(50)
Means <- aggregate(y,list(x),mean)
par(mfrow=c(1,1), mar=c(4.1,4,1,1), oma=c(0,0,0,0))
plot(y, jitter(1*x), yaxt="n", ann=FALSE, 
     col="grey50") 
points(Means[,2], c(0,1), col=2, pch=3, cex=4)
text(Means[1,2], 0.2, expression(alpha + 0*beta), cex=3, adj=-0.2)
text(Means[2,2], 0.8, expression(alpha + 1*beta), cex=3, adj=-0.2)

axis(2, c("Something", "Something else"), at=c(0,1), las=1)

## Examples of linear models: continuous $x_i$
x <- rnorm(30)+1;set.seed(2)
y <- rnorm(30,1+x*.4)
model <-  lm(y~x)
plot(y~x, ylab = "Response", xlab = "Explanatory", main=expression(alpha))
abline(model,col="red")
segments(y0=-3,y1=predict(model,data.frame(x=0)), x0 = 0, x1=0, col = "darkorange", lty = "dashed")
segments(y0=predict(model,data.frame(x=0)),y1=predict(model,data.frame(x=0)), x0 = 0, x1=-2, col = "darkorange", lty = "dashed")
text(y=predict(model,data.frame(x=-1)), -1, expression(alpha), adj = c(0.5,-1), cex=3, col = "darkorange")

## Examples of linear models: continuous $x_i$
x <- rnorm(30)+1;set.seed(2)
y <- rnorm(30,1+x*1)
model <-  lm(y~x)
plot(y~x, ylab = "Response", xlab = "Explanatory", main=expression(beta))
abline(model,col="red")
segments(y0=predict(model,data.frame(x=2)),y1=predict(model,data.frame(x=2)), x0 = 2, x1=3, col = "darkorange", lty = "dashed")
segments(y0=predict(model,data.frame(x=2)),y1=predict(model,data.frame(x=3)), x0 = 3, x1=3, col = "darkorange", lty = "dashed")
text(y=predict(model,data.frame(x=2.5)), 2.5, expression(beta), cex=3, adj=c(0,1.5), col = "darkorange")

## What is the best line?
x <- rnorm(30)+20;set.seed(2)
y <- rnorm(30,1+x*.4)
model <-  lm(y~x)
plot(y~x, ylab = expression(y), xlab = expression(x))
abline(model,col="red", lty = "dotted")
abline(h=9, col="red", lty = "dashed")

## How good is the line?
x <- rnorm(30)+20;set.seed(2)
y <- rnorm(30,1+x*.4)
model <-  lm(y~x)
plot(y~x, ylab = "Response", xlab = "Explanatory")
abline(model,col="red")

segments(x,y,x,predict(model), col = "blue")