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
#'@param parallel default FALSE: set to TRUE if you wish to use parallel computing. this can speed up the function for larger data frames
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
#'@import parallel
#'
#'@export brute_force_knapsack

brute_force_knapsack <- function(x, W, parallel = FALSE){
  if(! is.data.frame(x) || ncol(x) != 2 || ! is.numeric(x[, 1]) || ! is.numeric(x[, 2]) || ! all(x[, 1] > 0) || ! all(x[, 2] > 0) || W <= 0 ){
    stop("Check your input please!")
  }
  if (parallel == FALSE){
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
}else if (parallel == TRUE){
  paraVal <- function(y){
    id <- which(as.integer(intToBits(y-1)) == 1)
    if (sum(x$w[id]) > W){
      return(0) }
    else {
    value <- sum(x$v[id])
    return(list(value,id))
    }
  }
  cores <- detectCores()
  cl <- makeCluster(cores)
  clusterExport(cl, c("x", "W"), envir = environment())

  m <- parLapply(cl, c(1:2^nrow(x)), function(y) paraVal(y))

  stopCluster(cl)
  #this lapply was over 10 times slower when attempting to use parLapply instead
  vals <- lapply(m, function(l) l[1])
  bestVal <- max(unlist(vals))
  bestEle <- m[[which.max(unlist(vals))]][[2]]
  return(list("value" = round(bestVal), "elements" = unlist(bestEle)))
}
}

