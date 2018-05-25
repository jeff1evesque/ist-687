##
## load_package.R, install + install defined package
##
load_package <- function(package) {
  if (!require(package)) install.packages(package)
  library(package)
}
