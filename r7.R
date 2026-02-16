num <- 24
factors <- c()
for(i in 1:num){
  if(num %% i == 0){
    factors <- c(factors, i)
  }
}
print(factors)     
random_numbers <- sample(-10:10, 10)
print(random_numbers)
for(i in 1:10){
  if(i %% 15 == 0){
    print("FizzBuzz")
  } else if(i %% 3 == 0){
    print("Fizz")
  } else if(i %% 5 == 0){
    print("Buzz")
  } else {
    print(i)
  }
}
