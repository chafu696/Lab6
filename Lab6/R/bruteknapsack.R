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


# system.time(brute_force_knapsack(knapsack_objects[1:8, ], W = 2000))
brute_force_knapsack <- function(x, W){
  if(! is.data.frame(x) || ncol(x) != 2 || ! is.numeric(x[, 1]) || ! is.numeric(x[, 2]) || ! all(x[, 1] > 0) || ! all(x[, 2] > 0) || W <= 0 ){
    stop("Check your input please!")
  }
<<<<<<< HEAD
  value <- 0
  for (i in 1:nrow(x)) {
    id <- which(as.integer(intToBits(i-1)) == 1)
    if (sum(x$w[id]) > W) next
    if (sum(x$v[id]) > value) {
      value <- sum(x$v[id])
      elements <- id
    } 
  }
  return(list("value" = round(value), "elements" = elements))
=======
  n1 <- nrow(x)
  n2 <- seq(1, n1, by = 1)
  n3 <- as.list(n2)
  weight <- unlist(lapply(n3, function(r1, r2) combn(r2, r1, sum), x[, 1]))
  value <- unlist(lapply(n3, function(r1, r2) combn(r2, r1, sum), x[, 2]))
  index <- unlist(lapply(n3, function(r1, r2) combn(r2, r1, deparse), n2))
  t1 <- which(weight <= W)
  value2 <- max(value[t1])
  t2 <- which(value == value2)
  index1 <- index[t2]
  index1 <- gsub("[,]", "", index1)
  index2 <- substring(index1, 3, nchar(index1) - 1)
  index2 <- unlist(strsplit(index2, " "))
  my_list <- list(value = round(value2), elements = as.numeric(index2))
  return(my_list)
>>>>>>> 5f62722ffb11dfb4868d2d65067e9f93525e72ff
}
