## Anscombe's quartet
par(mfrow=c(2, 2))
data("anscombe")
plot(y1~x1, data = anscombe)
model1 <- lm(y1~x1, data = anscombe)
abline(model1, col = "red")
plot(y2~x2, data = anscombe)
model2 <- lm(y2~x2, data = anscombe)
abline(model2, col = "red")
plot(y3~x3, data = anscombe)
model3 <- lm(y3~x3, data = anscombe)
abline(model3, col = "red")
plot(y4~x4, data = anscombe)
model4 <- lm(y4~x4, data = anscombe)
abline(model4, col = "red")

## Anscombe's quartet
par(mfrow=c(2, 2))
plot(y1~x1, data = anscombe)
model1 <- lm(y1~x1, data = anscombe)
abline(model1, col = "red")
segments(x0 = anscombe$x1, x1 = anscombe$x1, y0= predict(model1), y1=anscombe$y1, col = "blue")
plot(y2~x2, data = anscombe)
model2 <- lm(y2~x2, data = anscombe)
abline(model2, col = "red")
segments(x0 = anscombe$x2, x1 = anscombe$x2, y0= predict(model2), y1=anscombe$y2, col = "blue")
plot(y3~x3, data = anscombe)
model3 <- lm(y3~x3, data = anscombe)
abline(model3, col = "red")
segments(x0 = anscombe$x3, x1 = anscombe$x3, y0= predict(model3), y1=anscombe$y3, col = "blue")
plot(y4~x4, data = anscombe)
model4 <- lm(y4~x4, data = anscombe)
abline(model4, col = "red")
segments(x0 = anscombe$x4, x1 = anscombe$x4, y0= predict(model4), y1=anscombe$y4, col = "blue")

## Influence
plot(y3~x3, data = anscombe)
model3 <- lm(y3~x3, data = anscombe)
abline(model3, col = "red")

## Outliers
plot(y3~x3, data = anscombe)
model3 <- lm(y3~x3, data = anscombe)
abline(model3, col = "red")

## What is influential?
plot(model3, which = 4)

## Anscombe's quartet: residuals
par(mfrow=c(2, 2))
plot(residuals(model1)~fitted(model1))
plot(residuals(model2)~fitted(model2))
plot(residuals(model3)~fitted(model3))
plot(residuals(model4)~fitted(model4))

## Residual diagnostics: residuals vs. fitted
par(mar=c(5,5,4,2))
mod <- lm(y1~x1,data=anscombe)
plot(residuals.lm(mod)~fitted(mod), data = anscombe, col="red", cex.lab=2, cex=2, cex.main=2, main="linear", pch=16, xlab="Fitted", ylab="Residuals")
text(5.7,1.2,"Good", cex=6,col="green")

lines(loess.smooth(y=residuals.lm(mod),x=fitted(mod)),col="red", lty="dashed", lwd=4)

mod <- lm(y2~x2,data=anscombe)
plot(residuals.lm(mod)~fitted(mod), data = anscombe, col="red", cex.lab=2, cex=2, cex.main=2, main="non-linear", pch=16, xlab="Fitted", ylab="Residuals")
lines(loess.smooth(y=residuals.lm(mod),x=fitted(mod)),col="red", lty="dashed", lwd=4)
text(5.5,0.7,"Bad", cex=6,col="red")

mod <- lm(y3~x3,data=anscombe)
plot(residuals.lm(mod)~fitted(mod), data = anscombe, col="red", cex.lab=2, cex=2, cex.main=2, main="outlier", pch=16, xlab="Fitted", ylab="Residuals")
lines(loess.smooth(y=residuals.lm(mod),x=fitted(mod)),col="red", lty="dashed", lwd=4)
text(5.5,2.5,"Bad", cex=6,col="red")

mod <- lm(y4~x4,data=anscombe)
plot(residuals.lm(mod)~fitted(mod), data = anscombe, col="red", cex.lab=2, cex=2, cex.main=2, main="outlier", pch=16, xlab="Fitted", ylab="Residuals")
lines(loess.smooth(y=residuals.lm(mod),x=fitted(mod)),col="red", lty="dashed", lwd=4)
text(7.5,1.3,"Bad", cex=6,col="red")

## Residual diagnostics: normality with QQ-plot
par(mar=c(5,5,4,2))

n <- 200;alpha <- 0.5;beta <- -1
x <- rgamma(n = n, shape = 1)
mu <- alpha + beta*x
y <- rnorm(n = n, mean = mu)
mod <- lm(y~x)

plot(sort(residuals.lm(mod)),x=qnorm(ppoints(n)), cex.lab=2, cex.axis=2, cex.lab=2, cex=2, cex.main=2, ylab="", xlab="Theoretical quantiles", main="Normal Q-Q")
text(-1,2,"Good", cex=6,col="green")

abline(lm(quantile(residuals.lm(mod),c(0.25,0.75))~qnorm(c(0.25,0.75))), lwd=4,lty="dashed")

par(mar=c(5,5,4,2))
n <- 200;alpha <- 0.5;beta <- -1
x <- rgamma(n = n, shape = 1)
mu <- alpha + beta*x
y <- rlnorm(n = n, meanlog = mu, sdlog = 1)
mod <- lm(y~x)

plot(sort(residuals.lm(mod)),x=qnorm(ppoints(n)), cex.lab=2, cex.axis=2, cex.lab=2, cex=2, cex.main=2, ylab="", xlab="Theoretical quantiles", main="Normal Q-Q")
text(-1,8,"Bad", cex=6,col="red")

abline(lm(quantile(residuals.lm(mod),c(0.25,0.75))~qnorm(c(0.25,0.75))), lwd=4,lty="dashed")

## Residual diagnostics: constant variance
par(mar=c(5,5,4,2))

n <- 200;alpha <- 0.5;beta <- -1
x <- rnorm(n = n)
mu <- alpha + beta*x
y <- mu + rnorm(n)
mod <- lm(y~x)

plot(residuals.lm(mod)~fitted(mod), col="red", cex.lab=2, cex=2, cex.main=2, main="", pch=16, xlab="Fitted", ylab="Residuals")
text(-1,2,"Good", cex=6,col="green")

par(mar=c(5,5,4,2))
n <- 200;alpha <- 0.5;beta <- -1
x <- rnorm(n = n)
mu <- alpha + beta*x
y <- mu + rnorm(n)*x^2
mod <- lm(y~x)

plot(residuals.lm(mod)~fitted(mod), col="red", cex.lab=2, cex=2, cex.main=2, main="", pch=16, xlab="Fitted", ylab="Residuals")
text(-1,6,"Bad", cex=6,col="red")