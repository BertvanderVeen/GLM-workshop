## The problem of model complexity
# Adapted from Bob Ohara
set.seed(12345)
N <- 100; P <- 90
x <- matrix(rnorm(N*P), nrow=N)
mu <- 0.1*x[,1] # true R^2 = 0.1^2/(0.1^2 + 1) = 1%
y <- rnorm(N, mu)
R2 <- sapply(2:P, function(pp, XX, Y) {
  mod <- lm(y ~ XX[,1:pp]) # fit the model
# return coefficient, conf. int. R^2
  c(coef(mod)["XX[, 1:pp]1"],
    confint(mod)["XX[, 1:pp]1",],
    summary(mod)$r.squared)
}, XX=x, Y=y)
plot(2:P, R2[4,], ylab="Explained variation", xlab = "Number of parameters", type="l",ylim=c(0,1))

## Model complexity
set.seed(12345)
x = rnorm(30)
y = 2+x*4 + x^2*-3 + rnorm(30, sd=3)
plot(y~x, xlab = NA, ylab = "Response variable", main = "Underfitting", cex.main = 3, xaxt="n")
abline(lm(y~x), col = "red", lty = "dashed")
plot(y~x, xlab = NA, ylab = "Response variable", main = "Just right", cex.main = 3, xaxt="n")
ynew <- predict(lm(y~x+I(x^2)), newdata=data.frame(x=seq(-2,2,length.out=1000)))
lines(y=ynew,seq(-2,2,length.out=1000),col="red",lty="dashed")
plot(y[order(x)]~sort(x), type="l", col = "red", xlab = "Covariate", ylab = "Response variable", main = "Overfitting", cex.main = 3)
points(x,y)

## Example: Lizards
data(lizards, package="aods3")
lizards <- lizards[-11,]
SiteTime = as.factor(paste(lizards$Site, lizards$Time, sep=" "))
plot(grahami~SiteTime, data = lizards, xlab= "Site:Time", ylab="Number of grahami lizards")

## Lizards: interaction
nmodel <- glm(cbind(grahami, opalinus)~Time+Site, 
             data = lizards, family="binomial")

amodel <- update(nmodel, formula = . ~ Time*Site)

## Lizards: interaction
summary(amodel)

## Lizards: LRT
(Lambda <- 2*(logLik(amodel)-logLik(nmodel)))
k <- attr(logLik(amodel),"df")-attr(logLik(nmodel),"df")
pchisq(Lambda, k,lower.tail=FALSE)

## LRT by simulation
Lambdas <- NULL
for(i in 1:1000){
ynew <- as.matrix(stats::simulate(nmodel))
nmodel2 <- glm(ynew~Time+Site, 
             data = lizards, family="binomial")
amodel2 <- update(nmodel2, formula = .~Time*Site)
# Store test statistic
Lambdas <- c(Lambdas, 
             2*(logLik(amodel2)-logLik(nmodel2)))
}
# if <0.05 our test statistic in the tail.
sum(Lambdas>Lambda)/1000

## LRT by simulation
hist(Lambdas, xlab= "Likelihood ratio", breaks = 100, probability = TRUE, cex.main = 1.5)
abline(v=Lambda,col="red", lwd = 3)
lines(y=dchisq(Lambdas, df = 2)[order(Lambdas)], x=sort(Lambdas), col = "blue", lwd = 3)

## Lizards: interaction selection
AIC(nmodel, amodel)
MuMIn::AICc(nmodel, amodel)
BIC(nmodel, amodel)

## Connection of AIC and LRT

## $\Delta$AIC by simulation under the better model
Lambdas <- NULL
for(i in 1:1000){
set.seed(i)
ynew <- as.matrix(stats::simulate(nmodel))
nmodel2 <- glm(ynew~Time+Site, 
             data = lizards, family="binomial")
amodel2 <- update(nmodel2, formula = .~Time*Site)
# Store test statistic
Lambdas <- c(Lambdas, 
             2*(logLik(nmodel2)-logLik(amodel2)) + 2*(attr(logLik(amodel2), "df") - attr(logLik(nmodel2), "df")))
}
# if <0.05 our test statistic in the tail.
dAIC <- 2*(logLik(nmodel)-logLik(amodel)) + 2*(attr(logLik(amodel), "df") - attr(logLik(nmodel), "df"))
hist(Lambdas, xlab = expression(AIC(M[1])-AIC(M[0])), main = "Positive: M0 is better. Negative: M1 is better.", breaks = 100, cex.main = 1.5)
abline(v=dAIC, col = "red", lty="dashed", lwd = 3)
abline(v=-2, col = "red", lty="solid", lwd = 3)

cat("Models with a more extreme test statistic:", sum(Lambdas<=dAIC)/1000)
cat("Models with more than 2 AIC difference:", sum(Lambdas>2)/1000)


## $\Delta$AICc by simulation under the better model
# if <0.05 our test statistic in the tail.
k1 <- attr(logLik(amodel), "df")
k0 <- attr(logLik(nmodel), "df")
n <- nrow(lizards)
dAICc = dAIC + (2*k1*(k1+1)/(n-k1-1)- 2*k0*(k0+1)/(n-k0-1))
Lambdas2 <- Lambdas + (2*k1*(k1+1)/(n-k1-1)- 2*k0*(k0+1)/(n-k0-1))

hist(Lambdas2, xlab = expression(AIC[c](M[1])-AIC[c](M[0])), main = "Positive: M0 is better. Negative: M1 is better.", cex.main = 1.5, breaks = 100)
abline(v=dAICc, col = "red", lty="dashed", lwd = 3)
abline(v=-2, col = "red", lty="solid", lwd = 3)

cat("Models with more than 2 AIC difference:", sum(Lambdas>2)/1000)
cat("Models with more than 2 AICc difference:", sum(Lambdas2>2)/1000)