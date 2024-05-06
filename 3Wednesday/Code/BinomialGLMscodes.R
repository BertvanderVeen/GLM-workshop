## Binomial link functions
x<-seq(binomial(link=logit)$linkinv(-5),binomial(link=logit)$linkinv(5),length.out=1000)
plot(x=binomial()$linkfun(x),x, main = "Logit", cex.main = 5, cex.axis = 5, ylim = c(0,1), xlim = c(-5,5), type = "l", xlab=NA, ylab=NA)
abline(v= 0, h = 0, lty="dashed")
x<-seq(binomial(link=probit)$linkinv(-5),binomial(link=probit)$linkinv(5),length.out=1000)
plot(x=binomial(link=probit)$linkfun(x),x, main = "Probit", cex.main = 5, cex.axis = 5, ylim = c(0,1), xlim = c(-5,5), type = "l", xlab=NA, ylab=NA)
abline(v= 0, h = 0, lty="dashed")
x<-seq(binomial(link=cloglog)$linkinv(-5),binomial(link=cloglog)$linkinv(5),length.out=10000)
plot(x=binomial(link=cloglog)$linkfun(x),x, main = "Complementary log-log", cex.main = 5, cex.axis = 5, ylim = c(0,1), xlim = c(-5,5), type = "l", xlab=NA, ylab=NA)
abline(v= 0, h = 0, lty="dashed")
x<-seq(from=exp(-exp(-5)),to=exp(-exp(5)),length.out=1000)
plot(x=-log(-log(x)),x, main = "Log-log", cex.main = 5, cex.axis = 5, ylim = c(0,1), xlim = c(-5,5), type = "l", xlab=NA, ylab=NA)
abline(v= 0, h = 0, lty="dashed")

## Binomial link functions (2)
x<-seq(binomial(link=logit)$linkinv(-5),binomial(link=logit)$linkinv(5),length.out=1000)
plot(binomial(link=logit)$linkfun(x),x, xlab = expression(eta[i]), ylab = expression(pi[i]), type="l")
x<-seq(binomial(link=probit)$linkinv(-5),binomial(link=probit)$linkinv(5),length.out=1000)
lines(binomial(link=probit)$linkfun(x),x, lty="dashed")
x<-seq(binomial(link=cloglog)$linkinv(-5),binomial(link=cloglog)$linkinv(5),length.out=1000)
lines(binomial(link=cloglog)$linkfun(x),x, lty="dotted")
x<-seq(from=exp(-exp(5)),to=exp(-exp(-5)),length.out=1000)
lines(-log(-log(x)),x, lty=4)
abline(v= 0, lty="dashed")
legend("bottomright", lty=c(1,2,3,4), legend = c("Logit","Probit", "Complementary log-log", "Log-log"))

## Binomial link functions (2)
x<-seq(binomial(link=logit)$linkinv(-5),binomial(link=logit)$linkinv(5),length.out=1000)
plot(binomial(link=logit)$linkfun(x),x, xlab = expression(eta[i]), ylab = expression(pi[i]), type="l")
x<-seq(binomial(link=probit)$linkinv(-5),binomial(link=probit)$linkinv(5),length.out=1000)
lines(binomial(link=probit)$linkfun(x),x, lty="dashed")
x<-seq(binomial(link=cloglog)$linkinv(-5),binomial(link=cloglog)$linkinv(5),length.out=1000)
lines(binomial(link=cloglog)$linkfun(x),x, lty="dotted")
legend("bottomright", lty=c(1,2,3), legend = c("Logit","Probit", "Complementary log-log"))
lines(x = c(-2, -2), y=c(0, binomial(link=logit)$linkinv(-2)),col="red")
lines(x = c(-2, -6), y=c(binomial(link=logit)$linkinv(-2),binomial(link=logit)$linkinv(-2)),col="red")
lines(x = c(-2, -6), y=c(binomial(link=probit)$linkinv(-2),binomial(link=probit)$linkinv(-2)),col="red", lty="dashed")
lines(x = c(-2, -6), y=c(binomial(link=cloglog)$linkinv(-2),binomial(link=cloglog)$linkinv(-2)),col="red", lty="dotted")

lines(x = c(0, 0), y=c(0, binomial(link=logit)$linkinv(0)),col="red")
lines(x = c(0, 0), y=c(0, binomial(link=cloglog)$linkinv(0)),col="red", lty="dotted")
lines(x = c(0, -6), y=c(binomial(link=logit)$linkinv(0),binomial(link=logit)$linkinv(0)),col="red")
lines(x = c(0, -6), y=c(binomial(link=probit)$linkinv(0),binomial(link=probit)$linkinv(0)),col="red", lty="dashed")
lines(x = c(0, -6), y=c(binomial(link=cloglog)$linkinv(0),binomial(link=cloglog)$linkinv(0)),col="red", lty="dotted")

lines(x = c(2, 2), y=c(0, binomial(link=logit)$linkinv(2)),col="red")
lines(x = c(2, 2), y=c(0, binomial(link=probit)$linkinv(2)),col="red", lty="dashed")
lines(x = c(2, 2), y=c(0, binomial(link=cloglog)$linkinv(2)),col="red", lty="dotted")
lines(x = c(2, -6), y=c(binomial(link=logit)$linkinv(2),binomial(link=logit)$linkinv(2)),col="red")
lines(x = c(2, -6), y=c(binomial(link=probit)$linkinv(2),binomial(link=probit)$linkinv(2)),col="red", lty="dashed")
lines(x = c(2, -6), y=c(binomial(link=cloglog)$linkinv(2),binomial(link=cloglog)$linkinv(2)),col="red", lty="dotted")

## Binomial link functions: probit
eta = -1
x <- seq(-5,5,length.out=1000)
plot(y=dnorm(x),x,type="l", ylab = "dnorm(x)", xaxt="n")
polygon(c(x[x<eta],rev(x[x<eta])),c(rep(0,sum(x<eta)),rev(dnorm(x[x<eta]))), col = "red")
axis(1, at = c(-4,-2,-1,0,2,4), labels = c(-4,-2,expression(eta[i]==-1),0,2,4))

## Lizards: the data
data(lizards, package="aods3")
lizards <- lizards[-11, ]# remove siites without any lizards
head(lizards)

## Lizards: visually inspect the data
par(mfrow=c(1,2))
plot(lizards$grahami, x=lizards$Time, ylab = "Observed grahami lizards", xlab="Time")
plot(lizards$grahami, x=lizards$Site, ylab = "Observed grahami lizards", xlab="Site")

## Lizards: visually inspect the data
par(mfrow=c(1,2))
plot(lizards$grahami, x=lizards$Height, ylab = "Observed grahami lizards", xlab="Perch height")
plot(lizards$grahami, x=lizards$Diameter, ylab = "Observed grahami lizards", xlab="Perch diameter")

## Lizards: visually inspect the data
par(mfrow=c(1,2))
plot(lizards$opalinus, x=lizards$Time, ylab = "Observed opalinus lizards", xlab="Time")
plot(lizards$opalinus, x=lizards$Site, ylab = "Observed opalinus lizards", xlab="Site")

## Lizards: fit the model
model <- glm(cbind(grahami, opalinus)~Time+Diameter+Height+Site, 
             data = lizards, family="binomial")

## Lizards: interpreting parameters
signif(summary(model)$coefficients,digits = 2L)
stargazer::stargazer(model, type = "latex", ci = TRUE, font.size = "small", align = TRUE,single.row = TRUE, header = FALSE, no.space = TRUE, float = TRUE)

## Lizards: inspecting the groups
emmeans::emmeans(model, ~Time+Site, type = "response")

## Lizards: inspecting the groups visually
plot(emmeans::emmeans(model, ~Time+Site, type = "response"))+ggplot2::theme_classic()+ggplot2::xlab("Probability of observing grahami lizards")

## Lizards: parameter estimates with different link functions
logit_est <- coef(model)
model <- update(model, family = binomial(link=probit))
probit_est <- coef(model)
model <- update(model, family = binomial(link=cloglog))
cloglog_est <- coef(model)

data.frame(logit=signif(logit_est, 2L),
probit=signif(probit_est, 2L),
cloglog=signif(cloglog_est, 2L), row.names= names(coef(model)))

## Lizards: group estimates with different link functions
model <- update(model, family = binomial(link=logit))
logit_est <- emmeans::emmeans(model, ~Time+Site)
model <- update(model, family = binomial(link=probit))
probit_est <- emmeans::emmeans(model, ~Time+Site)
model <- update(model, family = binomial(link=cloglog))
cloglog_est <- emmeans::emmeans(model, ~Time+Site)

levs = expand.grid(logit_est@levels)

data.frame(logit=signif(binomial()$linkinv(logit_est@linfct%*%logit_est@bhat), 2L),
probit=signif(binomial(link=probit)$linkinv(probit_est@linfct%*%probit_est@bhat), 2L),
cloglog=signif(binomial(link=cloglog)$linkinv(cloglog_est@linfct%*%cloglog_est@bhat), 2L), row.names= paste(levs[,1],levs[,2]))

## Lizards: checking overdispersion
deviance(model)/df.residual(model)
sum(residuals(model,"pearson")^2)/df.residual(model)

## Lizards: checking overdispersion
performance::check_overdispersion(model)

## Perfect separation: example
x <- seq(-3, 3, by=0.1)
y <- x > 0
model <- glm(y ~ x, family=binomial)

## Perfect separation (2)
summary(model)

## Perfect separation: example (3)
cbind(y, signif(predict(model, type = "response"),2))