## Lizards: recap
par(mfrow=c(1,2))
data(lizards, package="aods3")
lizards <- lizards[-11,]
plot(lizards$grahami, x=lizards$Time, ylab = "Observed grahami lizards", xlab="Time")
plot(lizards$grahami, x=lizards$Site, ylab = "Observed grahami lizards", xlab="Site")

model1 <- glm(cbind(grahami, opalinus)~Time+Site, 
             data = lizards, family="binomial")

## Lizards: residuals
par(mfrow=c(1,3))
car::residualPlot(model1, type="response", cex.main = 2, cex = 2, cex.lab = 1.5, main  ="Response")
car::residualPlot(model1, type="deviance", cex.main = 2, cex = 2, cex.lab = 1.5, main  ="Deviance")
car::residualPlot(model1, type="pearson", cex.main = 2, cex = 2, cex.lab = 1.5, main  ="Pearson")

## Horseshoe crabs: recap
data(hcrabs, package="GLMsData")
hcrabs$Col <-factor(hcrabs$Col, levels= c("LM","M","DM","D"))
levels(hcrabs$Col) <- c("Light medium","Medium","Dark medium","Dark")
hcrabs$Spine <-factor(hcrabs$Spine, levels= c("BothOK","OneOK","NoneOK"))
colnames(hcrabs)<-c("Colour","Spine","Width","Sat","Weight")
par(mfrow=c(2,2))
plot(jitter(log1p(Sat)) ~Weight, data = hcrabs, ylab = "log(Number of satellites+1)", cex.lab=1.5, cex.axis = 2.5, cex = 2)
plot(jitter(log1p(Sat)) ~Colour, data = hcrabs, ylab = NA, yaxt="n", cex.lab=1.5, cex.axis=2.5, cex = 2)
plot(jitter(log1p(Sat)) ~Spine, data = hcrabs, ylab = NA, yaxt="n", cex.lab=1.5, cex.axis=2.5, cex = 2)
plot(jitter(log1p(Sat)) ~Width, data = hcrabs, ylab = NA, yaxt="n", cex.lab=1.5, cex.axis=2.5, cex = 2)

model2 <- glm(Sat ~ Spine + Colour + Width + Weight, 
             family = "poisson", data = hcrabs)

## Horseshoe crabs: Poisson model residuals
par(mfrow=c(1,3))
car::residualPlot(model2, type="response", cex.main = 2, cex = 2, cex.lab = 1.5, main  ="Response")
car::residualPlot(model2, type="deviance", cex.main = 2, cex = 2, cex.lab = 1.5, main  ="Deviance")
car::residualPlot(model2, type="pearson", cex.main = 2, cex = 2, cex.lab = 1.5, main  ="Pearson")

## Horseshoe crabs: NB model residuals
model3 <- MASS::glm.nb(Sat ~ Spine + Colour + Width + Weight,
                        data = hcrabs)
par(mfrow=c(1,3))
car::residualPlot(model3, type="response", cex.main = 2, cex = 2, cex.lab = 1.5, main  ="Response")
car::residualPlot(model3, type="deviance", cex.main = 2, cex = 2, cex.lab = 1.5, ylab = NA, main = "Deviance")
car::residualPlot(model3, type="pearson", cex.main = 2, cex = 2, cex.lab = 1.5, ylab = NA, main = "Pearson")

### Continuous
hist(qnorm(ppois(hcrabs$Sat, predict.glm(model2,type="response"))), main="", cex.main=2, cex.lab=1.5, col="white", xlab="Normal deviates", ylab="")
hist(ppois(hcrabs$Sat, predict.glm(model2,type="response")), main="", cex.main=2, cex.lab=1.5, col="white", xlab="Quantiles", ylab="")

## Lizards and Horseshoe crabs
par(mfrow=c(1,3))
# model 1
b <- pbinom(as.vector(lizards$grahami),size=lizards$grahami+lizards$opalinus, predict(model1, type = "response"))
a <- pmin(b, pbinom(as.vector(lizards$grahami)-1,size=lizards$grahami+lizards$opalinus, predict(model1, type = "response")))
u <- runif(length(a),min=a,max=b)
plot(predict(model1),qnorm(u), ylab = "Quantile residual", xlab  = "Fitted values", main = "Lizards", cex.main = 2, cex = 2, cex.lab = 1.5)
panel.smooth(y=qnorm(u),predict(model1), cex = 0)
# model 2
b <- ppois(as.vector(hcrabs$Sat), predict(model2, type = "response"))
a <- pmin(b, ppois(as.vector(hcrabs$Sat)-1, predict(model2, type = "response")))
u <- runif(length(a),min=a,max=b)
plot(predict(model2),qnorm(u), ylab = NA, xlab  = "Fitted values", main = "Horseshoe crabs Poisson", cex.main = 2, cex = 2, cex.lab = 1.5)
panel.smooth(y=qnorm(u),predict(model2), cex = 0)
# model 3
b <- pnbinom(as.vector(hcrabs$Sat), mu = predict(model3, type = "response"), size = model3$theta)
a <- pmin(b, pnbinom(as.vector(hcrabs$Sat)-1, mu = predict(model3, type = "response"), size = model3$theta))
u <- runif(length(a),min=a,max=b)
plot(predict(model3),qnorm(u), ylab = NA, xlab  = "Fitted values", main = "Horseshoe crabs NB", cex.main = 2, cex = 2, cex.lab = 1.5)
panel.smooth(y=qnorm(u),predict(model3), cex = 0)

## Leave-one-out cross-validation Horseshoe crabs
#prevent issues with factors in cross-validation
hcrabs <- data.frame(cbind(model.matrix(Sat ~ 0+Spine + Colour + Width + Weight, hcrabs), Sat = hcrabs$Sat))
model3 <- update(model3, formula = Sat~SpineBothOK+SpineOneOK+SpineNoneOK+ColourMedium+ColourDark.medium+ColourDark+Width+Weight)

SE <- NULL
for(i in 1:1000){
set.seed(i)
test <- sample(1:nrow(hcrabs), size = 1)
train <- hcrabs[-test,]
model <- update(model3, data = train)
SE <- c(SE,(train[test, "Sat"]-
  predict(model,newdata = train[test, ], type="response"))^2)
}

## Result
hist(SE, main = "Leave-one-out", xlab = "Squared error")

## K-fold cross-validation Horseshoe crabs
k = 5
SE <- NULL
shuffle <- sample(1:nrow(hcrabs), nrow(lizards))
for(i in seq(1,nrow(hcrabs),by=k)){
set.seed(i)
test <- i:min(i+k-1,nrow(hcrabs))
train <- hcrabs[shuffle, ][-test,]
model <- update(model3, data = train)
SE <- c(SE,(hcrabs[shuffle, ][test, "Sat"]-
  predict(model,newdata = hcrabs[shuffle, ][test, ], type="response"))^2)
}

## Result
hist(SE, main = "k-fold", xlab = "Squared error")

## Stratified k-fold cross-validation Horseshoecrabs
k = 5
SE <- NULL
for(i in seq(1,nrow(hcrabs),by=k)){
set.seed(i)
test <- i:min(i+k-1,nrow(hcrabs))
train <- hcrabs[order(hcrabs$Weight),][-test,]
model <- update(model3, data = train)
SE <- c(SE,(hcrabs[order(hcrabs$Weight),][test, "Sat"]-
predict(model,newdata = hcrabs[order(hcrabs$Weight),][test, ], type="response"))^2)
}

## Result
hist(SE, main = "stratified k-fold", xlab = "Squared error")