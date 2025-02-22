deposit <- read.csv("/home/bertv/GLM-workshop/data/deposit.csv")
deposit$Insecticide <- as.factor(deposit$Insecticide)
pairs(deposit)
par(mfrow=c(1,2))
# plot(Killed~Deposit, data = deposit, col = Insecticide, pch = as.numeric(Insecticide))

# Expectation: 
# Insecticide 3 kills most insects
# Killed insects increases with deposit

model <- glm(cbind(Killed, Number - Killed) ~ Insecticide*Deposit, family = "binomial", data = deposit)

pred3 <- predict(model, newdata = data.frame(Deposit = seq(1,9,length.out=1000), Insecticide = "C"), type = "response")
pred2 <- predict(model, newdata = data.frame(Deposit = seq(1,9,length.out=1000), Insecticide = "B"), type = "response")
pred1 <- predict(model, newdata = data.frame(Deposit = seq(1,9,length.out=1000), Insecticide = "A"), type = "response")

plot(Killed/Number~Deposit, data = deposit, col = Insecticide, pch = as.numeric(Insecticide), ylab = "Probability of getting killed")

lines(seq(1,9,length.out=1000), pred1, col = 1)
lines(seq(1,9,length.out=1000), pred2, col = 2)
lines(seq(1,9,length.out=1000), pred3, col = 3)
# 
# plot(model)
# plot(model, which = 4)

# Re-do without obs 6
model <- glm(cbind(Killed, Number - Killed) ~ Insecticide*Deposit, family = "binomial", data = deposit[-6,])

pred3 <- predict(model, newdata = data.frame(Deposit = seq(1,9,length.out=1000), Insecticide = "C"), type = "response")
pred2 <- predict(model, newdata = data.frame(Deposit = seq(1,9,length.out=1000), Insecticide = "B"), type = "response")
pred1 <- predict(model, newdata = data.frame(Deposit = seq(1,9,length.out=1000), Insecticide = "A"), type = "response")

plot(Killed/Number~Deposit, data = deposit, col = Insecticide, pch = as.numeric(Insecticide), ylab = "Probability of getting killed")

lines(seq(1,9,length.out=1000), pred1, col = 1)
lines(seq(1,9,length.out=1000), pred2, col = 2)
lines(seq(1,9,length.out=1000), pred3, col = 3)

# plot(model)
# plot(model, which = 4)

model <- glm(cbind(Killed, Number - Killed) ~ Insecticide*Deposit, family = "binomial", data = deposit[-6,])
model1 <- glm(cbind(Killed, Number - Killed) ~ Insecticide+Deposit, family = "binomial", data = deposit[-6,])
anova(model, model1, test="LRT")

model2 <- glm(cbind(Killed, Number - Killed) ~ Insecticide*Deposit, family = binomial(link=probit), data = deposit[-6,])
model3 <- glm(cbind(Killed, Number - Killed) ~ Insecticide*Deposit, family = binomial(link=cloglog), data = deposit[-6,])
AIC(model,model2,model3)

# just plot this ontop of our plot before
# so we can compare cloglog to the logistic regression on the data
plot(Killed/Number~Deposit, data = deposit, col = Insecticide, pch = as.numeric(Insecticide), ylab = "Probability of getting killed")

pred3 <- predict(model, newdata = data.frame(Deposit = seq(1,9,length.out=1000), Insecticide = "C"), type = "response")
pred2 <- predict(model, newdata = data.frame(Deposit = seq(1,9,length.out=1000), Insecticide = "B"), type = "response")
pred1 <- predict(model, newdata = data.frame(Deposit = seq(1,9,length.out=1000), Insecticide = "A"), type = "response")

lines(seq(1,9,length.out=1000), pred1, col = 1)
lines(seq(1,9,length.out=1000), pred2, col = 2)
lines(seq(1,9,length.out=1000), pred3, col = 3)

model3 <- glm(cbind(Killed, Number - Killed) ~ Insecticide*Deposit, family = binomial(link=cloglog), data = deposit[-6,])

pred3 <- predict(model3, newdata = data.frame(Deposit = seq(1,9,length.out=1000), Insecticide = "C"), type = "response")
pred2 <- predict(model3, newdata = data.frame(Deposit = seq(1,9,length.out=1000), Insecticide = "B"), type = "response")
pred1 <- predict(model3, newdata = data.frame(Deposit = seq(1,9,length.out=1000), Insecticide = "A"), type = "response")

lines(seq(1,9,length.out=1000), pred1, col = 1, lty = "dashed")
lines(seq(1,9,length.out=1000), pred2, col = 2, lty = "dashed")
lines(seq(1,9,length.out=1000), pred3, col = 3, lty = "dashed")

legend("bottomright", legend = c("logit: A", "logit:B", "logit:C", 
                                 "Cloglog:A", "Cloglog:B", "Cloglog:C"),
      lty = c("solid", "solid", "solid", "dashed", "dashed", "dashed"),
      col = c(3, 2, 1, 3, 2, 1)
       )

# Have a look at some R2 values
DescTools::PseudoR2(model3, which = "all")