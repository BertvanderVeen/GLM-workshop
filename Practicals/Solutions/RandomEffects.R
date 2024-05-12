library(glmmTMB)
data(Owls)

plot(SiblingNegotiation ~ Nest, data = Owls)
plot(SiblingNegotiation ~ BroodSize, data = Owls)
plot(SiblingNegotiation ~ SexParent, data = Owls)
plot(SiblingNegotiation ~ ArrivalTime, data = Owls)

plot(NegPerChick ~ SexParent, data = Owls)
plot(NegPerChick ~ BroodSize, data = Owls)

model <- glmmTMB(SiblingNegotiation~ Nest + offset(log(BroodSize)), family = "poisson", data = Owls)
model1 <- glmmTMB(SiblingNegotiation~ (1|Nest) + offset(log(BroodSize)), family = "poisson", data = Owls)

library(patchwork)
library(ggeffects)
p1 <- plot(ggeffects::predict_response(model, terms = ~Nest, condition = c(Broodsize = 1), ci_level=NA))+ggplot2::theme_classic()+ ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))
p2 <- plot(ggeffects::predict_response(model1, terms = ~(1|Nest), type = "random", condition = c(Broodsize = 1), ci_level=NA))+ggplot2::theme_classic()+ ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))

p1+p2
p1/p2
p1|p2

cbind(fixed=c(fixef(model)$cond[1],fixef(model)$cond[1]+fixef(model)$cond[-1]), random=fixef(model1)$cond+ranef(model1)$cond$Nest)


model2 <- glmmTMB(SiblingNegotiation~ ArrivalTime + (1|Nest) + offset(log(BroodSize)), family = "poisson", data = Owls)
p3 <- plot(ggeffects::predict_response(model2, terms = ~ArrivalTime+(1|Nest), condition = c(Broodsize = 1), ci_level=NA, type = "random"))+ggplot2::theme_classic()+ ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))+see::scale_color_flat()+ggplot2::theme(legend.position="none")

model3 <- glmmTMB(SiblingNegotiation~ ArrivalTime + Nest + offset(log(BroodSize)), family = "poisson", data = Owls)
p4 <- plot(ggeffects::predict_response(model3, terms = ~ArrivalTime+Nest, condition = c(Broodsize = 1), ci_level=NA, type = "random"))+ggplot2::theme_classic()+ ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))+see::scale_color_flat()+ggplot2::theme(legend.position="none")

p3+p4

model4 <- glmmTMB(SiblingNegotiation~ ArrivalTime*Nest + offset(log(BroodSize)), family = "poisson", data = Owls)

model5 <- glmmTMB(SiblingNegotiation~ (ArrivalTime||Nest) + offset(log(BroodSize)), family = "poisson", data = Owls)
p5 <- plot(ggeffects::predict_response(model5, terms = ~(ArrivalTime||Nest), condition = c(Broodsize = 1), ci_level=NA, type = "random"))+ggplot2::theme_classic()+ ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))+see::scale_color_flat()+ggplot2::theme(legend.position="none")

# Check assumption - looks terrible
plot(DHARMa::simulateResiduals(model5))

model6 <- MASS::glm.nb(SiblingNegotiation ~ ArrivalTime*Nest , data = Owls)

# Try different dataset
data("Orange")

modelo <- glmmTMB(circumference ~ age + (0+age|Tree), family = tweedie, data = Orange)
p6 <- plot(ggeffects::predict_response(modelo, terms = ~age, ci_level=NA))+ggplot2::theme_classic()+ ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))+see::scale_color_flat()+ggplot2::theme(legend.position="none")
p7 <- plot(ggeffects::predict_response(modelo, terms = ~ age + (0+age|Tree), ci_level=NA, type = "random"))+ggplot2::theme_classic()+ ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1))+see::scale_color_flat()+ggplot2::theme(legend.position="none")

p6+p7

# Adaptive shrinkage
library(mvabund)
data(spider)
y <- spider$abund[,1]
spider$x
dat <- cbind(y=y, spider$x, group = 1)

models <- glmmTMB(y~(0+soil.dry+bare.sand+fallen.leaves+moss+herb.layer+reflection||group), family = nbinom2, data = dat)
models2 <- glmmTMB(y~soil.dry+bare.sand+fallen.leaves+moss+herb.layer+reflection, family = nbinom2, data = dat)

# new dataset
data(grouseticks, package = "lme4")

par(las=1,bty="l")
tvec <- c(0,1,2,5,20,40,80)
pvec <- c(4,1,3)
with(grouseticks_agg,plot(1+meanTICKS~HEIGHT,
                          pch=pvec[factor(YEAR)],
                          log="y",axes=FALSE,
                          xlab="Altitude (m)",
                          ylab="Brood mean ticks"))
axis(side=1)
axis(side=2,at=tvec+1,label=tvec)
box()
abline(v=405,lty=2)

modelg <- glmmTMB(TICKS~YEAR+HEIGHT+(1|BROOD)+(1|INDEX)+(1|LOCATION), data = grouseticks, family = poisson)
# data likelihood residuals
plot(DHARMa::simulateResiduals(modelg, seed = 2))
# random effect assumption checking
qqnorm(ranef(modelg)$cond$BROOD[,1])
qqline(ranef(modelg)$cond$BROOD[,1])

qqnorm(ranef(modelg)$cond$INDEX[,1])
qqline(ranef(modelg)$cond$INDEX[,1])

qqnorm(ranef(modelg)$cond$LOCATION[,1])
qqline(ranef(modelg)$cond$LOCATION[,1])

plot(x=qnorm(ppoints(ranef(modelg)$cond$LOCATION[,1])), y= sort(ranef(modelg)$cond$LOCATION[,1]), col = )
plot(DHARMa::getResiduals(modelg)~grouseticks$LOCATION)
plot(TICKS~HEIGHT, data = grouseticks)

points(y=grouseticks$TICKS[grouseticks$LOCATION==24],grouseticks$HEIGHT[grouseticks$LOCATION==24], col = "red")
points(y=grouseticks$TICKS[grouseticks$YEAR==96],grouseticks$HEIGHT[grouseticks$YEAR==96], col = "yellow")

# exclude location 7 and 24, something dodgy going on by the look of it
modelg2 <- glmmTMB(TICKS~YEAR+HEIGHT+(1|BROOD)+(1|INDEX)+(1|LOCATION), data = grouseticks[!grouseticks$LOCATION%in%c(7,24),], family = poisson)
qqnorm(ranef(modelg2)$cond$LOCATION[,1])
qqline(ranef(modelg2)$cond$LOCATION[,1])
plot(DHARMa::simulateResiduals(modelg2))

emmeans::emmeans(modelg, "YEAR", type = "response")
emmeans::emmeans(modelg2, "YEAR", type = "response")

# This looks fine
qqnorm(ranef(modelg2)$cond$BROOD[,1])
qqline(ranef(modelg2)$cond$BROOD[,1])

# Not quite normal but also nothing we can do!
qqnorm(ranef(modelg2)$cond$INDEX[,1])
qqline(ranef(modelg2)$cond$INDEX[,1])

# Index random effect seems non-normal, probably capturing overdispersion. NB is the way to go instead!
modelg3 <- glmmTMB(TICKS~YEAR+HEIGHT+(1|BROOD)+(1|LOCATION), data = grouseticks[!grouseticks$LOCATION%in%c(7,24),], family = nbinom2)
AIC(modelg2, modelg3)

qqnorm(ranef(modelg3)$cond$BROOD[,1])
qqline(ranef(modelg3)$cond$BROOD[,1])

# Not quite normal but also nothing we can do!
qqnorm(ranef(modelg3)$cond$LOCATION[,1])
qqline(ranef(modelg3)$cond$LOCATION[,1])

emmeans::emmeans(modelg3, "YEAR", type = "response")
