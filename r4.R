vec <- 1:12
arr <- array(vec, dim=c(3,4),
             dimnames=list(
               Rows=c("A","B","C"),
               Cols=c("X","Y","Z","W")
             ))
print(arr)
print(arr["B","Z"])
