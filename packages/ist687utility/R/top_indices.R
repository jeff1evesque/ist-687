##
## top_indices.R, determine largest values in a vector,
##     and return the corresponding vector index.
##
top_indices <- function(v, end, start) {
  if(missing(start)) {
    start=0
  }

  return(order(v,decreasing=T)[start:end])
}
