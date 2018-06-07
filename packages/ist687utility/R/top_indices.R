##
## top_indices.R, determine n largest values in a vector,
##     and return the corresponding vector index.
##
top_indices <- function(v, end, start=0) {
  return(order(v,decreasing=T)[start:end])
}
