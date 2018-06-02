##
## top_indices.R, determine n largest values in a vector,
##     and return the corresponding vector index.
##
top <- function(x, n=10){
  result <- numeric()
  for(i in 1:n){
    j <- which.max(x)
    result[i] <- x[j]
    x[j] <- -Inf
  }
  return(match(result, x))
}
