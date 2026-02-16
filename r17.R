data(Titanic)

barplot(margin.table(Titanic, c(2,4)),
        main="Survival vs Class")

hist(as.numeric(dimnames(Titanic)$Age),
     main="Age Distribution")
