#'This is a description of brute_force_knapsack function
#'
#'@name brute_force_knapsack
#'
#'@description This function creates a brute forced algorithm to solve the knapsack problem.
#'
#'@param x a data frame has two columns and all the elements are positive numbers
#'
#'@param W a single number denotes the size of the knapsack
#'
#'@return a list contains the biggest value and the items
#'
#'@importFrom utils combn
#'
#'@examples
#'set.seed(42)
#'n <- 2000
#'knapsack_objects <- data.frame(w = sample(1:4000, size = n , replace = TRUE),
#'v = runif(n = n, 0 , 10000))
#'l <- brute_force_knapsack(x = knapsack_objects[1:8, ], W = 3500)
#'
#'@export brute_force_knapsack


# system.time(brute_force_knapsack(knapsack_objects[1:8, ], W = 2000))
brute_force_knapsack <- function(x, W){
  if(! is.data.frame(x) || ncol(x) != 2 || ! is.numeric(x[, 1]) || ! is.numeric(x[, 2]) || ! all(x[, 1] > 0) || ! all(x[, 2] > 0)){
    stop("Check your input please!")
  }
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

  my_list <- list(value = round(value2), elements = index2)
  print(my_list, quote = FALSE)
}
