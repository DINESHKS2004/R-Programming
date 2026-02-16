set.seed(123)

index <- sample(1:nrow(iris), 0.8*nrow(iris))
train <- iris[index, ]
test <- iris[-index, ]

log_model <- glm(Species ~ Petal.Length + Petal.Width,
                 data=train,
                 family="binomial")

pred <- predict(log_model, test, type="response")
pred_class <- ifelse(pred > 0.5, levels(iris$Species)[2],
                     levels(iris$Species)[1])

table(test$Species, pred_class)
