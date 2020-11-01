#'This is a description of knapsack_dynamic function
#'
#'@name knapsack_dynamic
#'
#'@description This function creates a dynamic programming to solve the knapsack problem.
#'
#'@param x a data frame has two columns and all the elements are positive numbers
#'
#'@param W a single number denotes the size of the knapsack
#'
#'@return a list contains the biggest value and the items
#'
#'@examples
#'set.seed(42)
#'n <- 2000
#'knapsack_objects <- data.frame(w = sample(1:4000, size = n , replace = TRUE),
#' v = runif(n = n, 0 , 10000))
#'l <- knapsack_dynamic(x = knapsack_objects[1:8, ], W = 3500)
#'
#'@export knapsack_dynamic

# set.seed(42)
# n <- 2000
# knapsack_objects <- data.frame(w = sample(1:4000, size = n , replace = TRUE),
#v = runif(n = n, 0 , 10000))
# system.time(knapsack_dynamic(knapsack_objects[1:8, ], W = 3500))
knapsack_dynamic <- function(x, W){
  if(! is.data.frame(x) || ncol(x) != 2 || ! is.numeric(x[, 1]) || ! is.numeric(x[, 2]) || ! all(x[, 1] > 0) || ! all(x[, 2] > 0)){
    stop("Check your input please!")
  }
a <- x
n1 <- nrow(x)
n2 <- seq(1, n1, by = 1)
n3 <- seq(1, W, by = 1)
n4 <- sort(n2, decreasing = TRUE)
t2 <- W + 1
index <- vector(length = n1)
m <- matrix(NA, n1 + 1, W + 1)
m[1, ] <- 0
m[, 1] <- 0
for (i in n2){
  for (j in n3){
    if (x[i, 1] > j){
      m[i + 1, j + 1] = m[i, j + 1]
    }else{
      m[i + 1, j + 1] = max(m[i, j + 1], (m[i, j + 1 - x[i, 1]] + x[i, 2]))
    }
  }
}
for (i in n4){
  if (m[i + 1, t2] == m[i, t2]){
    index[i] = 0
  }else{
    index[i] = 1
    t2 = t2 - x[i, 1]
  }
}
my_list <- list(value = round(m[n1 + 1, W + 1]), elements = which(index == 1))
return(my_list)
}






