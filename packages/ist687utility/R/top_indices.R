##
## top_indices.R, determine n largest values in a vector,
##     and return the corresponding vector index.
##
top_indices <- function(x, n=10){
  nx <- length(x)
  p <- nx-n
  xp <- sort(x, partial=p)[p]
  return(which(x > xp))
}
