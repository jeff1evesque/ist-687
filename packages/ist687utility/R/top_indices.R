##
## top_indices.R, determine n largest values in a vector,
##     and return the corresponding vector index.
##
top_indices <- function(x, end, start=0, increment=1){
  nums <- sort(x)
  return(nums(c(seq(start, end, increment))))
}
