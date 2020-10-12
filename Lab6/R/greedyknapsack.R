#'A greedy method for solving the knapsack problem
#'
#'@name greedy_knapsack
#'
#'@description This function creates a greedy algorithm to solve the knapsack problem.
#'
#'@param x a data frame has two columns and all the elements are positive numbers
#'
#'@param W a single number denotes the size of the knapsack
#'
#'@return a list contains the biggest value and the items
#'
#'@examples
#'#'set.seed(42)
#'n <- 2000
#'knapsack_objects <- data.frame(w = sample(1:4000, size = n , replace = TRUE),
#'v = runif(n = n, 0 , 10000))
#'l <- greedy_knapsack(x = knapsack_objects[1:800, ], W = 3500)
#'
#'
#'@export greedy_knapsack
# set.seed(42)
# n <- 2000
# knapsack_objects <- data.frame(w = sample(1:4000, size = n , replace = TRUE), v = runif(n = n, 0 , 10000))
#system.time(greedy_knapsack(knapsack_objects[1:800, ], W = 3500))

greedy_knapsack <- function(x, W){
  if(! is.data.frame(x) || ncol(x) != 2 || ! is.numeric(x[, 1]) || ! is.numeric(x[, 2]) || ! all(x[, 1] > 0) || ! all(x[, 2] > 0) || W <= 0){
    stop("Check your input please!")
  }
  wVal <- x[, 2] / x[, 1]
  x1 <- x[order(wVal, decreasing = TRUE), ]
  val <- 0
<<<<<<< HEAD
  elements <- 0
  for (i in 1:nrow(x)){
    if (x1[i, 1] > W ){
      break
    }else{
      val <- val + x1[i, 2]
      W <- W - x1[i, 1]
      elements[i] <- rownames(x1)[i]
=======
  index <- 0
  j <- 1
  for (i in n1){
    if (x1[i, 1] > W ){
      break
    }else{
    val <- val + x1[i, 2]
    W <- W - x1[i, 1]
    index[j] <- i
    j <- j + 1
>>>>>>> 5f62722ffb11dfb4868d2d65067e9f93525e72ff
    }
  }
  return(list(value = round(val), elements = elements))
}









