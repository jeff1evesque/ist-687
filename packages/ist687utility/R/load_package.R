##
## load_package.R, install + install defined package
##
load_package <- function(package) {
  if (!require(package, character.only=TRUE)) install.packages(package)
  library(package, character.only = TRUE)
}
