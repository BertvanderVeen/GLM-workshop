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
plot(y~x, xlab = NA, ylab = "Response variable", main = "Underfitting", cex.main = 3)
abline(lm(y~x), col = "red", lty = "dashed")
plot(y~x, xlab = NA, ylab = "Response variable", main = "Just right", cex.main = 3)
ynew <- predict(lm(y~x+I(x^2)), newdata=data.frame(x=seq(-2,2,length.out=1000)))
lines(y=ynew,seq(-2,2,length.out=1000),col="red",lty="dashed")
plot(y[order(x)]~sort(x), type="l", col = "red", xlab = "Covariate", ylab = "Response variable", main = "Overfitting", cex.main = 3)
points(x,y)

## Explained variation
set.seed(12345)
x = rnorm(30)
y = 2+x*4 + x^2*-3 + rnorm(30, sd=3)
plot(y~x, xlab = NA, ylab = "Response variable", main = "Low R²", cex.main = 2, xaxt="n")
abline(lm(y~x), col = "red", lty = "dashed")
plot(y~x, xlab = NA, ylab = "Response variable", main = "Intermediate R²", cex.main = 2, xaxt="n")
ynew <- predict(lm(y~x+I(x^2)), newdata=data.frame(x=seq(-2,2,length.out=1000)))
lines(y=ynew,seq(-2,2,length.out=1000),col="red",lty="dashed")
plot(y[order(x)]~sort(x), type="l", col = "red", xlab = "Covariate", ylab = "Response variable", main = "High R²", cex.main = 2)
points(x,y)

## Adjusted $R^2$
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
    summary(mod)$r.squared,
    summary(mod)$adj.r.squared)
}, XX=x, Y=y)
plot(2:P, R2[4,], ylab="Explained variation", xlab = "Number of parameters", type="l",ylim=c(0,1), col = "red")
lines(2:P, R2[5,], col ="blue", lty = "dashed")
legend("topleft", col = c("red","blue"), lty = c("solid","dashed"), legend = c(expression(R^2),expression(R[adjusted]^2)), cex = 2)

## Example: Lizards interaction
data(lizards, package="aods3")
lizards <- lizards[-11,]
n = nrow(lizards)

model1 <- glm(cbind(grahami, opalinus)~Time+Site, 
             data = lizards, family="binomial")

model2 <- glm(cbind(grahami, opalinus)~Time*Site, 
             data = lizards, family="binomial")

## Lizards: $R^2$ under more complex model
nullmodel <- update(model1, formula = .~1)
(devianceR2 <- c(1-deviance(model1)/deviance(nullmodel),1-deviance(model2)/deviance(nullmodel)))
(adjdevianceR2 <- 1-(1-devianceR2)*(n-1)/(n-c(attr(logLik(model1), "df"),attr(logLik(model2), "df"))))
(mcfaddenR2 <- c(1-logLik(model1)/logLik(nullmodel), 1-logLik(model2)/logLik(nullmodel)))

## Lizards: deviance $R^2$
R2s <- NULL
for(i in 1:1000){
set.seed(i)
ynew <- as.matrix(stats::simulate(model2))
model2s <- update(model2, formula = ynew~.)
nullmodels <- update(model2s, formula = .~1)
R2s <- c(R2s, 1-deviance(model2s)/deviance(nullmodels))
}

hist(R2s, xlab = expression(R[deviance]^2), breaks = 100, cex.main = 1.5)
abline(v=devianceR2[2], col = "red", lty="dashed", lwd = 3,main="Red line = observed R2")

## Lizards: McKelvey-Zavoina $R^2$
R2s2 <- NULL
for(i in 1:1000){
set.seed(i)
ynew <- as.matrix(stats::simulate(model2))
model2s <- update(model2, formula = ynew~.)
nullmodels <- update(model2s, formula = .~1)
R2s2 <- c(R2s2, DescTools::PseudoR2(model2s, "McKelveyZavoina"))
}

hist(R2s2, xlab = expression(R[McKelveyZavoina]^2), breaks = 100, cex.main = 1.5, ,main="Red line = observed R2")
abline(v=DescTools::PseudoR2(model2, "McKelveyZavoina"), col = "red", lty="dashed", lwd = 3)