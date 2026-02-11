# -------------------------------
# 1. Numeric, character, logical vectors
# -------------------------------
num_vec <- c(10, 20, 30)
char_vec <- c("A", "B", "C")
log_vec <- c(TRUE, FALSE, TRUE)

print(num_vec); print(typeof(num_vec))
print(char_vec); print(typeof(char_vec))
print(log_vec); print(typeof(log_vec))

# -------------------------------
# 2. Labeled matrices
# -------------------------------
m1 <- matrix(1:25, nrow=5, byrow=TRUE)
m2 <- matrix(1:9, nrow=3, byrow=FALSE)
m3 <- matrix(1:4, nrow=2, byrow=TRUE)

print(m1); print(m2); print(m3)

# -------------------------------
# 3. 3D array
# -------------------------------
arr3d <- array(1:24, dim=c(3,4,2))
print(arr3d)

# -------------------------------
# 4. Array with dimension names
# -------------------------------
arr_named <- array(1:8,
                   dim=c(2,2,2),
                   dimnames=list(Row=c("R1","R2"),
                                 Col=c("C1","C2"),
                                 Table=c("T1","T2")))
print(arr_named)
print(arr_named["R1","C2","T1"])

# -------------------------------
# 5. Factor variables
# -------------------------------
heights <- c(150,160,170,160,150)
height_factor <- factor(heights)

letters_sample <- factor(sample(LETTERS[1:5],10,replace=TRUE))

print(height_factor)
print(letters_sample)

# -------------------------------
# 6. List with vectors, matrices, functions
# -------------------------------
my_list <- list(
  numbers = c(1,2,3),
  matrix = matrix(1:4,2,2),
  func = function(x) x^2
)

print(my_list)
print(my_list$func(5))

# -------------------------------
# 7. Basic tasks
# -------------------------------
# Factors of a number
num <- 12
factors <- c()
for(i in 1:num){
  if(num %% i == 0){
    factors <- c(factors, i)
  }
}
print(factors)

# Random integers
rand_vec <- sample(-50:50, 10)
print(rand_vec)

# FizzBuzz
for(i in 1:100){
  if(i %% 15 == 0) cat("FizzBuzz\n")
  else if(i %% 3 == 0) cat("Fizz\n")
  else if(i %% 5 == 0) cat("Buzz\n")
  else cat(i,"\n")
}

# -------------------------------
# 8. Random normal numbers & count
# -------------------------------
norm_nums <- rnorm(100)
print(summary(norm_nums))

# -------------------------------
# 9. Empty plot
# -------------------------------
plot(1:10, type="n", xlim=c(0,10), ylim=c(0,10),
     main="Empty Plot")

# -------------------------------
# 10. Data frame exam_data
# -------------------------------
exam_data <- data.frame(
  name=c("A","B","C"),
  score=c(80,75,90),
  attempts=c(1,2,1),
  qualify=c("yes","no","yes")
)

print(exam_data)

# Add row
exam_data <- rbind(exam_data,
                   data.frame(name="D",score=85,attempts=1,qualify="yes"))

# Add column
exam_data$grade <- c("B","C","A","B")

# Sort
exam_data <- exam_data[order(-exam_data$score),]
print(exam_data)

write.csv(exam_data,"exam_data.csv",row.names=FALSE)

# -------------------------------
# 11. Read CSV
# -------------------------------
data_read <- read.csv("exam_data.csv")
print(data_read)

# -------------------------------
# 12. Airquality reshape
# -------------------------------
data(airquality)

monthly_avg <- aggregate(cbind(Ozone,Solar.R,Wind,Temp)
                         ~ Month, data=airquality, mean, na.rm=TRUE)
print(monthly_avg)

# -------------------------------
# 13. Combine arrays row-wise
# -------------------------------
a1 <- array(1:6, dim=c(2,3))
a2 <- array(7:12, dim=c(2,3))
combined <- rbind(a1,a2)
print(combined)

# -------------------------------
# 14. ChickWeight dataset
# -------------------------------
data(ChickWeight)

# Sort by weight
sorted <- ChickWeight[order(ChickWeight$weight),]
print(head(sorted))

# Average weight by Diet
avg_weight <- aggregate(weight ~ Diet, data=ChickWeight, mean)
print(avg_weight)

# -------------------------------
# 15. Iris EDA
# -------------------------------
data(iris)

print(dim(iris))
print(summary(iris))
print(sapply(iris[,1:4], sd))

quantiles <- apply(iris[,1:4],2,quantile)
print(quantiles)

species_group <- aggregate(. ~ Species, data=iris, mean)
print(species_group)

# -------------------------------
# 16. USArrests dataset
# -------------------------------
data(USArrests)

print(summary(USArrests))

state_max_rape <- rownames(USArrests)[which.max(USArrests$Rape)]
print(state_max_rape)

print(max(USArrests$Murder))
print(min(USArrests$Murder))

print(cor(USArrests))

# -------------------------------
# 17. Titanic dataset
# -------------------------------
data(Titanic)
titanic_df <- as.data.frame(Titanic)

barplot(table(titanic_df$Class, titanic_df$Survived),
        main="Survival by Class")

# -------------------------------
# 18. Graphs
# -------------------------------
boxplot(iris$Sepal.Length, main="Boxplot")
hist(iris$Sepal.Length, main="Histogram")
barplot(table(iris$Species), main="Barplot")
plot(1:10,1:10,type="l",main="Line Chart")
plot(iris$Sepal.Length, iris$Petal.Length,
     main="Scatter Plot")

# -------------------------------
# 19. Regression: Sales ~ Spend
# -------------------------------
Spend <- c(10,20,30,40,50)
Sales <- c(15,25,35,45,55)

model1 <- lm(Sales ~ Spend)
summary(model1)

predict(model1, data.frame(Spend=60))

# -------------------------------
# 20. Multiple regression: ChickWeight
# -------------------------------
model2 <- lm(weight ~ Time + Diet, data=ChickWeight)
summary(model2)

pred <- predict(model2, ChickWeight)
error <- mean((ChickWeight$weight - pred)^2)
print(error)

# -------------------------------
# 21. Logistic regression on iris
# -------------------------------
set.seed(1)
train_idx <- sample(1:nrow(iris), 0.8*nrow(iris))

train <- iris[train_idx,]
test <- iris[-train_idx,]

# Convert to binary classification
train$Species <- ifelse(train$Species=="setosa",1,0)
test$Species <- ifelse(test$Species=="setosa",1,0)

log_model <- glm(Species ~ Petal.Length + Petal.Width,
                 data=train, family=binomial)

pred_prob <- predict(log_model, test, type="response")
pred_class <- ifelse(pred_prob > 0.5,1,0)

table(Predicted=pred_class, Actual=test$Species)
