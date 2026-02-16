my_list <- list(
  numbers = c(1,2,3,4),
  matrix = matrix(1:4, nrow=2),
  square = function(x) { x^2 }
)
print(my_list)
print(my_list$square(5))
