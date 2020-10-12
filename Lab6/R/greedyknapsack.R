#'This is a description of greedy_knapsack function
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
  if(! is.data.frame(x) || ncol(x) != 2 || ! is.numeric(x[, 1]) || ! is.numeric(x[, 2]) || ! all(x[, 1] > 0) || ! all(x[, 2] > 0)){
    stop("Check your input please!")
  }
  x$rate <- x[, 2] / x[, 1]
  x1 <- x[order(x$rate, decreasing = TRUE), ]
  n1 <- seq(1, nrow(x), by = 1)
  val <- 0
  wei <- 0
  index <- 0
  j <- 1
  for (i in n1){
    if (x1[i, 1] > W ){
      break
    }else{
    val <- val + x1[i, 2]
    W <- W - x1[i, 1]
    wei <- wei + x1[i, 1]
    index[j] <- i
    j <- j + 1
    }
  }
  index1 <- rownames(x1)[index]
  my_list <- list(value = round(val), elements = as.numeric(index1))
  return(my_list)

}










