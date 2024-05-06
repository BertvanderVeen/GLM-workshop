## Two explanatory variables
set.seed(12345)
X = data.frame(intercept = 1, x1 = rnorm(20), x2 = rnorm(20))
beta = rnorm(3, mean = 4)
y = as.matrix(X)%*%beta+rnorm(20)
model <- lm(y~x1+x2,data = X)
PlotData <- expand.grid(x1 = seq(-3,3,length.out = 20), x2 = seq(-3,3,length.out = 20))
ynew <- matrix(predict(model, newdata = PlotData), ncol = 20)

PlotData <- expand.grid(
  x1 = seq(-3,3,length.out=20),
  x2 = seq(-3,3,length.out=20)
)
ext <- fitted(model) + 5*resid(model)
fit <- fitted(model)
persp(x=seq(-3,3,length.out=20), y=seq(-3,3,length.out=20), z=ynew,
      xlab="Explanatory variable 1", ylab="Explanatory variable 2", theta=300, 
      zlab="Response variable") -> res
points(trans3d(x=X$x1, y=X$x2, z=ext, pmat = res), col = "red", pch = 16)

thing <- apply(cbind(X,ext,fit), 1, function(v) 
lines(trans3d(x=rep(v["x1"],2), y=rep(v["x2"],2), z=v[c("ext", "fit")], pmat = res), col = "blue", lwd=2))

## Plane
set.seed(12345)
X = data.frame(intercept = 1, x1 = rnorm(20), x2 = rnorm(20))
beta = rnorm(3, mean = 4)
y = as.matrix(X)%*%beta+rnorm(20)
model <- lm(y~x1+x2,data = X)
PlotData <- expand.grid(x1 = seq(-3,3,length.out = 20), x2 = seq(-3,3,length.out = 20))
ynew <- matrix(predict(model, newdata = PlotData), ncol = 20)

PlotData <- expand.grid(
  x1 = seq(-3,3,length.out=20),
  x2 = seq(-3,3,length.out=20)
)
ext <- fitted(model) + 5*resid(model)
fit <- fitted(model)
persp(x=seq(-3,3,length.out=20), y=seq(-3,3,length.out=20), z=ynew,
      xlab="Explanatory variable 1", ylab="Explanatory variable 2", theta=300, 
      zlab="Response variable") -> res
points(trans3d(x=X$x1, y=X$x2, z=ext, pmat = res), col = "red", pch = 16)

thing <- apply(cbind(X,ext,fit), 1, function(v) 
lines(trans3d(x=rep(v["x1"],2), y=rep(v["x2"],2), z=v[c("ext", "fit")], pmat = res), col = "blue", lwd=2))

## Examples of linear models: categorical $x_i$ (from yesterday)
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

## Writing the Model: continuous and categorical
set.seed(1234)
x <- data.frame(x1 = as.factor(sample(rep(1:2,15, replace=TRUE))), x2 = rnorm(30))
beta = rnorm(3)
y <- rnorm(30, model.matrix(~.,data.frame(x))%*%beta, 2)
model <- lm(y~x1+x2, data = x)

plot(x$x2, y, ylab="Response variable", xlab = "Covariate", type = "n", main = "Categorical and continuous covariate regression (ANCOVA)")
points(x$x2[x$x1==1], y[x$x1==1], col ="red")
points(x$x2[x$x1==2], y[x$x1==2], col ="purple")
abline(a = coef(model)[1], b = coef(model)[3], col = "red")
abline(a = coef(model)[1]+coef(model)[2], b = coef(model)[3], col = "purple", lty = "dashed")
legend("topright", lty = c("solid","dashed"),col=c("red","purple"), legend=c("Group 1", "Group 2"))

## Interactions: categorical-continuous
set.seed(1234)
x <- data.frame(x1 = as.factor(sample(rep(1:2,15, replace=TRUE))), x2 = rnorm(30))
beta = rnorm(4)
y <- rnorm(30, model.matrix(~x1+x2+x1:x2,data.frame(x))%*%beta, 2)
model <- lm(y~x1+x2+x1:x2, data = x)

plot(x$x2, y, ylab="Response variable", xlab = "Covariate", type = "n", main = "Categorical and continuous covariate regression interaction")
points(x$x2[x$x1==1], y[x$x1==1], col ="red")
points(x$x2[x$x1==2], y[x$x1==2], col ="purple")
abline(a = coef(model)[1], b = coef(model)[3], col = "red")
abline(a = coef(model)[1]+coef(model)[2], b = coef(model)[3]+coef(model)[4], col = "purple", lty = "dashed")
legend("topright", lty = c("solid","dashed"),col=c("red","purple"), legend=c("Group 1", "Group 2"))

## Surface: quadratic effects
set.seed(12345)
X = data.frame(intercept = 1, x1 = rnorm(20), x2 = rnorm(20))
beta = rnorm(3, mean = 4)
beta2 = c(-2,-2)
y = as.matrix(X)%*%beta+as.matrix(X[,c("x1","x2")]^2)%*%beta2+rnorm(20)
model <- lm(y~x1+I(x1^2)+x2+I(x2^2),data = X)
PlotData <- expand.grid(x1 = seq(-3,3,length.out = 20), x2 = seq(-3,3,length.out=20))
ynew <- matrix(predict(model, newdata = PlotData), ncol = 20)

ext <- fitted(model) + 5*resid(model)
fit <- fitted(model)
persp(x=seq(-3,3,length.out=20), y=seq(-3,3,length.out=20), z=ynew,
      xlab="Explanatory variable 1", ylab="Explanatory variable 2", theta=300, 
      zlab="Response variable") -> res
points(trans3d(x=X$x1, y=X$x2, z=ext, pmat = res), col = "red", pch = 16)

## Visualizing a multiple regression
set.seed(12345)
X = data.frame(intercept = 1, x1 = rnorm(20), x2 = rnorm(20))
beta = rnorm(3, mean = 4)
beta2 = c(-2,-2)
y = as.matrix(X)%*%beta+as.matrix(X[,c("x1","x2")]^2)%*%beta2+rnorm(20)
model <- lm(y~x1+I(x1^2)+x2+I(x2^2),data = X)
PlotData <- expand.grid(x1 = seq(-3,3,length.out = 20), x2 = seq(-3,3,length.out=20))
ynew <- matrix(predict(model, newdata = PlotData), ncol = 20)

ext <- fitted(model) + 5*resid(model)
fit <- fitted(model)
persp(x=seq(-3,3,length.out=20), y=seq(-3,3,length.out=20), z=ynew,
      xlab="Explanatory variable 1", ylab="Explanatory variable 2", theta=300, 
      zlab="Response variable", cex.lab = 2) -> res
points(trans3d(x=X$x1, y=X$x2, z=ext, pmat = res), col = "red", pch = 16)

set.seed(12345)
X = data.frame(intercept = 1, x1 = rnorm(20), x2 = rnorm(20))
beta = rnorm(3, mean = 4)
beta2 = c(-2,-2)
y = as.matrix(X)%*%beta+as.matrix(X[,c("x1","x2")]^2)%*%beta2+rnorm(20)
model <- lm(y~x1+I(x1^2)+x2+I(x2^2),data = X)
PlotData <- expand.grid(x1 = seq(-3,3,length.out = 20), x2 = seq(-3,3,length.out=20))
ynew <- matrix(predict(model, newdata = PlotData), ncol = 20)

ext <- fitted(model) + 5*resid(model)
fit <- fitted(model)
persp(x=seq(-3,3,length.out=20), y=seq(-3,3,length.out=20), z=ynew,
      xlab="Explanatory variable 1", ylab="Explanatory variable 2", theta=0, 
      zlab="Response variable", phi = 0, cex.lab = 2) -> res
points(trans3d(x=X$x1, y=X$x2, z=ext, pmat = res), col = "red", pch = 16)

## Visualizing a multiple regression
set.seed(12345)
X = data.frame(intercept = 1, x1 = rnorm(20), x2 = rnorm(20))
beta = rnorm(3, mean = 4)
beta2 = c(-2,-2)
y = as.matrix(X)%*%beta+as.matrix(X[,c("x1","x2")]^2)%*%beta2+rnorm(20)
model <- lm(y~x1+I(x1^2)+x2+I(x2^2),data = X)
PlotData <- expand.grid(x1 = seq(-3,3,length.out = 20), x2 = seq(-3,3,length.out=20))
ynew <- matrix(predict(model, newdata = PlotData), ncol = 20)

fit <- fitted(model)
persp(x=seq(-3,3,length.out=20), y=seq(-3,3,length.out=20), z=ynew,
      xlab="Explanatory variable 1", ylab="Explanatory variable 2", theta=0,phi=0, 
      zlab="Response variable", cex.lab = 2) -> res
points(trans3d(x=X$x1, y=X$x2, z=y, pmat = res), col = "red", pch = 16)

x1 <- seq(-3,3,length.out=100)
par(mar=c(5,5,4,2)+.1)
ypred <- predict(model, newdata=data.frame(x2=-3,x1=x1))
plot(NA, ylim = c(-50,9), xlim = c(-3,3), type = "n", cex.axis = 2, cex.lab = 2, ylab  ="Response variable", xlab="Explanatory variable 1")
points(X$x1, y, col = "red", pch = 16)

lines(x1, ypred, col = "black")
text(0, predict(model, newdata = data.frame(x2 = -3, x1 = 0)), labels = "x2=-3", cex = 2, adj = c(0,1))
ypred2 <- predict(model, newdata=data.frame(x2=-2,x1=x1))
lines(x1, ypred2, col = "red", lty = "dashed")
text(0, predict(model, newdata = data.frame(x2 = -2, x1 = 0)), labels = "x2=-2", cex = 2, adj = c(0,1))
ypred3 <- predict(model, newdata=data.frame(x2=-1,x1=x1))
lines(x1, ypred3, col = "orange", lty = "dotted")
text(0, predict(model, newdata = data.frame(x2 = -1, x1 = 0)), labels = "x2=-1", cex = 2, adj = c(0,1))
ypred4 <- predict(model, newdata=data.frame(x2=0,x1=x1))
lines(x1, ypred4, col = "brown", lty = "dotdash")
text(0, predict(model, newdata = data.frame(x2 = 0, x1 = 0)), labels = "x2=0", cex = 2, adj = c(0,1))