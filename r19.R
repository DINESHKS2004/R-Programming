Spend <- c(100,150,200,250)
Sales <- c(10,15,20,25)

model <- lm(Sales ~ Spend)
summary(model)

predict(model, data.frame(Spend=180))
