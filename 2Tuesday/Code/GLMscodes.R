## The link function
x<-seq(binomial(link=logit)$linkinv(-5),binomial(link=logit)$linkinv(5),length.out=1000)
plot(x=binomial()$linkfun(x),x, cex.main = 5, cex.lab = 5, ylim = c(0,1), xlim = c(-5,5), type = "l", xlab=NA, ylab=NA, xaxt="n", yaxt="n")
abline(v=0,h=0,lty="dashed")
text(-3,.8,"Logit", cex=5)

x<-seq(poisson(link=log)$linkinv(-5),poisson(link=log)$linkinv(5),length.out=1000)
plot(x=poisson(link=log)$linkfun(x),x, cex.main = 5, xlim = c(-5,5), type = "l", xlab=NA, ylab=NA, xaxt="n", yaxt="n")
abline(v= 0, h = 0, lty="dashed")
text(-3,125,"Log", cex=5)

x<-seq(-5,5,length.out = 1000)
plot(x=x, 1/x, cex.main = 5, xlim =  c(-5,5),type = "l", xlab=NA, ylab=NA, xaxt="n", yaxt="n")
abline(v= 0, h = 0, lty="dashed")
text(-3,125,"Inverse", cex=5)

## Finding the maximum (from day 1)
set.seed(12345)
p.orchid = 0.4
n.picks = 100
n.times <- 50
y <- rbinom(n.times, size = n.picks, prob = p.orchid) # Collecting data

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

## Output
set.seed(12345)
beta <- rnorm(2)
X <- rnorm(20)
y <- rpois(20, exp(beta[1]+X*beta[2]))
summary(glm(y~X, family = "poisson"))