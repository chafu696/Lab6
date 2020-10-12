#'Brute force method for solving the knapsack problem
#'
#'@name brute_force_knapsack
#'
#'@description This function uses a brute force algorithm to solve the knapsack problem.
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
#'v = runif(n = n, 0 , 10000))
#'bfk <- brute_force_knapsack(x = knapsack_objects[1:8, ], W = 3500)
#'
#'@export brute_force_knapsack

brute_force_knapsack <- function(x, W){
  if(! is.data.frame(x) || ncol(x) != 2 || ! is.numeric(x[, 1]) || ! is.numeric(x[, 2]) || ! all(x[, 1] > 0) || ! all(x[, 2] > 0) || W <= 0 ){
    stop("Check your input please!")
  }
  value <- 0
  for (i in 1:2^nrow(x)) {
    id <- which(as.integer(intToBits(i-1)) == 1)
    if (sum(x$w[id]) > W) next
    if (sum(x$v[id]) > value) {
      value <- sum(x$v[id])
      elements <- id
    } 
  }
  return(list("value" = round(value), "elements" = elements))
}

