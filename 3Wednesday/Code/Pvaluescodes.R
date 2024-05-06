## Recap
data(lizards,package="aods3")
lizards <- lizards[-11,]

nmodel <- glm(cbind(grahami, opalinus)~Time+Site, 
             data = lizards, family="binomial")

amodel <- glm(cbind(grahami, opalinus)~Time*Site, 
             data = lizards, family="binomial")

## Lizards: LRT
(Lambda <- 2*(logLik(amodel)-logLik(nmodel)))
k <- attr(logLik(amodel),"df")-attr(logLik(nmodel),"df")
pchisq(Lambda, k,lower.tail=FALSE)

## Errors
par(mar=c(5,12,4,2))
plot(1:2,1:2,type="n", xaxt="n", yaxt="n", xaxs="i" , yaxs="i",xlab="", ylab="", xlim = c(1,2))
abline(v=1.5, h  =1.5)
text(1.25, 1.25, "Too conservative", col=2, cex=3)
text(1.75, 1.25, "Accurate", col=3, cex=3)
text(1.25, 1.75, "Accurate", col=3, cex=3)
text(1.75, 1.75, "Too optimistic", col=2, cex=3)
axis(2, c("Alternative hypothesis is true", "Null hypothesis is true"), at=c(1.25,1.75), las=1, cex.axis = 1)
axis(1, c("Null hypothesis is accepted", "Alternative hypothesis is accepted"), at=c(1.25,1.75), las=1, cex.axis = 1)

## Distribution of p-values
hist(runif(10000,0,1), breaks = 10, xaxt="n", main = "P-value distribution", xlab= "P-value")
axis(1, at = c(0.05, seq(0,1, 0.1)), labels = c(0.05, seq(0,1, 0.1)))
abline(v=0.05, col="red")

## Example: Lizards
nmodel <- glm(cbind(grahami, opalinus)~Time+Site, 
             data = lizards, family="binomial")

amodel <- update(nmodel, formula = .~Time+Site+Diameter)

## P-values by simulation
Ps <- NULL
for(i in 1:1000){
ynew <- as.matrix(stats::simulate(nmodel))
nmodel2 <- glm(ynew~Time+Site, 
             data = lizards, family="binomial")
amodel2 <- update(nmodel2, formula = .~Time+Site+Diameter)
# Store P-value
(Lambda <- 2*(logLik(amodel2)-logLik(nmodel2)))
k <- attr(logLik(amodel2),"df")-attr(logLik(nmodel2),"df")
Ps <- c(Ps, pchisq(Lambda, k,lower.tail=FALSE))
}

## P-values by simulation
Ps <- NULL
for(i in 1:1000){
set.seed(i)
ynew <- as.matrix(stats::simulate(nmodel))
nmodel2 <- glm(ynew~Time+Site, 
             data = lizards, family="binomial")
amodel2 <- update(nmodel2, formula = .~Time+Site+Diameter)
# Store P-value
(Lambda <- 2*(logLik(amodel2)-logLik(nmodel2)))
k <- attr(logLik(amodel2),"df")-attr(logLik(nmodel2),"df")
Ps <- c(Ps, pchisq(Lambda, k,lower.tail=FALSE))
}
hist(Ps, main = "P-values when M0 is true", cex.main = 3)
abline(v=0.05, col="red")

cat("P-values larger than 0.05:", sum(Ps>0.05)/1000)

Ps <- NULL
for(i in 1:1000){
set.seed(i)
ynew <- as.matrix(stats::simulate(amodel))
nmodel2 <- glm(ynew~Time+Site, 
             data = lizards, family="binomial")
amodel2 <- update(nmodel2, formula = .~Time+Site+Diameter)
# Store P-value
(Lambda <- 2*(logLik(amodel2)-logLik(nmodel2)))
k <- attr(logLik(amodel2),"df")-attr(logLik(nmodel2),"df")
Ps <- c(Ps, pchisq(Lambda, k,lower.tail=FALSE))
}
hist(Ps, main = "P-values when M1 is true", cex.main = 3)
abline(v=0.05, col="red")

cat("P-values larger than 0.05:", sum(Ps>0.05)/1000)

## To conclude