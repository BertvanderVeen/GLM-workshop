## Binomial link functions (2)
x<-seq(binomial(link=logit)$linkinv(-5),binomial(link=logit)$linkinv(5),length.out=1000)
plot(binomial(link=logit)$linkfun(x),x, xlab = expression(pi[i]), ylab = expression(eta[i]), type="l")
x<-seq(binomial(link=probit)$linkinv(-5),binomial(link=probit)$linkinv(5),length.out=1000)
lines(binomial(link=probit)$linkfun(x),x, lty="dashed")
x<-seq(binomial(link=cloglog)$linkinv(-5),binomial(link=cloglog)$linkinv(5),length.out=1000)
lines(binomial(link=cloglog)$linkfun(x),x, lty="dotted")
x<-seq(from=exp(-exp(5)),to=exp(-exp(-5)),length.out=1000)
lines(-log(-log(x)),x, lty=4)
abline(v= 0, lty="dashed")
legend("bottomright", lty=c(1,2,3,4), legend = c("Logit","Probit", "Complementary log-log", "Log-log"))

## Log-link function
x <- seq(-5,5,length.out=1000)
plot(x, exp(x), type = "l", xlab = expression(eta[i]), ylab = expression(lambda[i]))
segments(x0=0,x1=0, y0=-10, y1 = 1, col = "red")
segments(x0=0,x1=-10, y0=1, y1 = 1, col = "red")
segments(x0=2,x1=2, y0=-10, y1 =exp(2) , col = "red")
segments(x0=2,x1=-10, y0=exp(2), y1 =exp(2) , col = "red")
segments(x0=4,x1=4, y0=-10, y1 =exp(4) , col = "red")
segments(x0=4,x1=-10, y0=exp(4), y1 =exp(4) , col = "red")

## The Poisson distribution visually
par(mfrow=c(2,2))
hist(rpois(1000,1), xlab = expression(y[i]), main = expression(lambda == 1), cex.main = 2, cex.lab = 1.5)
hist(rpois(1000,5), xlab = expression(y[i]), main = expression(lambda == 5), cex.main = 2, cex.lab = 1.5)
hist(rpois(1000,10), xlab = expression(y[i]), main = expression(lambda == 10), cex.main = 2, cex.lab = 1.5)
hist(rpois(1000,100), xlab = expression(y[i]), main = expression(lambda == 100), cex.main = 2, cex.lab = 1.5)

## Horseshoe crabs: the data
data(hcrabs, package="GLMsData")
head(hcrabs, 7)
hcrabs$Col <-factor(hcrabs$Col, levels= c("LM","M","DM","D"))
levels(hcrabs$Col) <- c("Light medium","Medium","Dark medium","Dark")
hcrabs$Spine <-factor(hcrabs$Spine, levels= c("BothOK","OneOK","NoneOK"))
colnames(hcrabs)<-c("Colour","Spine","Width","Sat","Weight")

## Horseshoe crabs: the data
par(mfrow=c(2,2))
plot(jitter(log1p(Sat)) ~Weight, data = hcrabs, ylab = "log(Number of satellites+1)", cex.lab=1.5, cex.axis = 2.5, cex = 2)
plot(jitter(log1p(Sat)) ~Colour, data = hcrabs, ylab = NA, yaxt="n", cex.lab=2.5, cex.axis=2.5, cex = 2)
plot(jitter(log1p(Sat)) ~Spine, data = hcrabs, ylab = NA, yaxt="n", cex.lab=2.5, cex.axis=2.5, cex = 2)
plot(jitter(log1p(Sat)) ~Width, data = hcrabs, ylab = NA, yaxt="n", cex.lab=2.5, cex.axis=2.5, cex = 2)

What can we tell about the number of satellites?

## Horseshoe crabs: fit the model
model <- glm(Sat ~ Spine + Colour + Width + Weight, 
             family = "poisson", data = hcrabs)

## Horseshoe crabs: interpreting parameters
signif(summary(model)$coefficients,digits = 2L)

## Horseshoe crabs: interpreting parameters, centered
hcrabs$Weight<-scale(hcrabs$Weight, scale = FALSE)
hcrabs$Width<-scale(hcrabs$Width, scale = FALSE)
model <- glm(Sat ~ Spine + Colour + Width + Weight, 
             family = "poisson", data = hcrabs)
signif(summary(model)$coefficients,digits = 2L)

## Horseshoe crabs: visual interpretation
par(mfrow=c(1,3))
plot(effects::allEffects(model))

## Horseshoe crabs: Negative-binomial
modelnb <- MASS::glm.nb(Sat ~ Spine + Colour + Width + Weight,
                        data = hcrabs)

AIC(model, modelnb)

## Horseshoe crabs: comparing estimates
cbind("Poisson estimate"=signif(coef(model), 2L), "NB estimate" = signif(coef(modelnb), 2L), "Poisson SE" = signif(summary(model)$coef[,2],2L), "NB SE"= signif(summary(modelnb)$coef[,2],2L))