## Our example data: soil nitrogen
data(nitrogen, package="GLMsData")

head(nitrogen, 8)

## Log-normal regression in \texttt{R}
model1 <- lm(log(SoilN)~Fert, data = nitrogen)
fitted <- exp(predict(model1)) #We need to backtransform ourselves
cbind(est=coef(model1), confint(model1))

## Log-normal regression: results
plot(nitrogen$Fert,nitrogen$SoilN, xlab="FertN", ylab="SoilN", cex = 2, cex.lab=2, cex.axis=2)
segments(x0=100,x1=100,y0=0,y1=exp(predict(model1,newdata=data.frame(Fert=100))),col="red")
segments(x0=100,x1=-10,y0=exp(predict(model1,newdata=data.frame(Fert=100))),y1=exp(predict(model1,newdata=data.frame(Fert=100))),col="red")

segments(x0=200,x1=200,y0=0,y1=exp(predict(model1,newdata=data.frame(Fert=200))),col="red")
segments(x0=200,x1=-10,y0=exp(predict(model1,newdata=data.frame(Fert=200))),y1=exp(predict(model1,newdata=data.frame(Fert=200))),col="red")

lines(seq(0,max(nitrogen$Fert),length.out=100), exp(predict(model1,newdata=data.frame(Fert=seq(0,max(nitrogen$Fert),length.out=100)))),type="l",ylim=c(0,70), lwd = 2, xlab  ="FertN", ylab="SoilN", cex.lab=1.5, cex = 2)

## Inverse Gaussian GLM in \texttt{R}
model2 <- glm(SoilN~Fert, data = nitrogen, family = "inverse.gaussian")
summary(model2)

## Inverse Gaussian GLM: results
plot(seq(0,max(nitrogen$Fert),length.out=100), predict(model2,newdata=data.frame(Fert=seq(0,max(nitrogen$Fert),length.out=100)),type="response"),type="l",ylim=c(0,70), lwd = 2, xlab  ="FertN", ylab="SoilN", cex.lab=1.5, cex = 2)

points(nitrogen$Fert,nitrogen$SoilN,col="red", cex = 2,pch=16)

## Gamma GLM
model3 <- glm(SoilN~Fert, data = nitrogen, family = "inverse.gaussian")
summary(model3)

## Gamma GLM: results
plot(seq(0,max(nitrogen$Fert),length.out=100), predict(model3,newdata=data.frame(Fert=seq(0,max(nitrogen$Fert),length.out=100)),type="response"),type="l",ylim=c(0,70), lwd = 2, xlab  ="FertN", ylab="SoilN", cex.lab=1.5, cex = 2)

points(nitrogen$Fert,nitrogen$SoilN,col="red", cex = 2,pch=16)

## Tweedie GLM in \texttt{R}: finding $\xi$
out.est <- tweedie::tweedie.profile(SoilN ~ Fert, xi.vec = seq(2.01, 4, by=0.05), data = nitrogen)
out.est2 <- tweedie::tweedie.profile(SoilN ~ Fert, xi.vec =  seq(out.est$xi.max-0.5, out.est$xi.max+0.5, by=0.01), data = nitrogen)
plot(out.est2, xlab = expression(xi), ylab="Log-likelihood", type = "l")
abline(v=out.est2$xi.max, col = "red")

## Tweedie GLM in \texttt{R}: fitting
model4 <- glm(SoilN~Fert, data = nitrogen, family = statmod::tweedie(var.power = out.est2$xi.max))
summary(model4)


## All together
plot(seq(0,max(nitrogen$Fert),length.out=100), predict(model3,newdata=data.frame(Fert=seq(0,max(nitrogen$Fert),length.out=100)),type="response"),ylim=c(0,70), lwd = 2, xlab  ="FertN", ylab="SoilN", cex.lab=1.5, cex = 2, type = "n")

points(nitrogen$Fert,nitrogen$SoilN,col="red", cex = 2,pch=16)

lines(seq(0,max(nitrogen$Fert),length.out=100), exp(predict(model1,newdata=data.frame(Fert=seq(0,max(nitrogen$Fert),length.out=100)))))
lines(seq(0,max(nitrogen$Fert),length.out=100), predict(model2,newdata=data.frame(Fert=seq(0,max(nitrogen$Fert),length.out=100)),type="response"), lty = "dashed", col ="blue")
lines(seq(0,max(nitrogen$Fert),length.out=100), predict(model3,newdata=data.frame(Fert=seq(0,max(nitrogen$Fert),length.out=100)),type="response"), lty = "dotted", col = "pink")
lines(seq(0,max(nitrogen$Fert),length.out=100), predict(model4,newdata=data.frame(Fert=seq(0,max(nitrogen$Fert),length.out=100)),type="response"), lty = "dotdash", col = "purple")

legend("topleft", lty = c("solid", "dashed", "dotted", "dotdash"), col = c("black","blue","pink", "purple"), legend = c("Log-normal", "Inverse Gaussian", "Gamma", "Tweedie"))

## Ordinal example: vegetation data
data(dune,dune.env,package="vegan")
data <- cbind(y=as.factor(dune$Bracruta), dune.env)
(model5 <- MASS::polr(y~A1, data = data))

## Example: vegetation data
predict(model5,type="probs")

## Ordinal example: vegetation data
A1new <- seq(0,100,length.out=1000)
preds <- predict(model5,type="probs", newdata=data.frame(A1=A1new))
plot(1,xlim=c(min(A1new),max(A1new)),ylim=c(0,1), xlab = "A1", ylab="Predicted probability", cex.lab=1.5, cex = 2, type = "n")
lines(A1new, preds[,1], col = "red", lwd = 2)
lines(A1new, preds[,2], lwd = 2)
lines(A1new, preds[,3], lty = "dashed", lwd = 2)
lines(A1new, preds[,4], col = "red", lty="dashed", lwd = 2)
lines(A1new, preds[,5], lty = "dotted", col = "orange", lwd = 2)
legend("topleft", lty=c("solid","solid","dashed","dashed","dotted"),col=c("red","black","black","red","orange"),legend=c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5"), cex =2)

## Example: Cod parasite data
data <- read.csv("../data/MistletoeV2.csv", header = TRUE)
knitr::kable(head(data, 8), format="latex", booktabs = T)

## Example: Mistletoe infections
hist(data$No.of.mistletoes, xlab="Count", main = "Mistletoe infections")

## Example: Mistletoe infections
model6 <- glm(No.of.mistletoes~DBH, family="poisson", data = data)

DHARMa::testZeroInflation(model6)

invisible(DHARMa::testZeroInflation(model6))

DHARMa::testZeroInflation(model6, plot = FALSE)

## Example: Mistletoe infections
model7 <- glmmTMB::glmmTMB(No.of.mistletoes~DBH,ziformula=~1, family="poisson", data = data)

invisible(DHARMa::testZeroInflation(model7))

DHARMa::testZeroInflation(model7, plot = FALSE)