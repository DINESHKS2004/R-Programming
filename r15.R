data(iris)
dim(iris)
summary(iris)
sd(iris$Sepal.Length)
quantile(iris$Petal.Length)
aggregate(Sepal.Length ~ Species, iris, mean)
iris$Category <- cut(iris$Sepal.Length,
                     breaks=c(0,5,6,8),
                     labels=c("Small","Medium","Large"))
table(iris$Category)
