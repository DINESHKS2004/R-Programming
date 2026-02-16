data(ChickWeight)
ChickWeight$Diet <- as.factor(ChickWeight$Diet)
model <- lm(weight ~ Time + Diet, data = ChickWeight)
summary(model)
pred <- predict(model)
mse <- mean((ChickWeight$weight - pred)^2)
mse
