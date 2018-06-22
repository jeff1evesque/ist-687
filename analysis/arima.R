##
## arima.R, ARIMA analysis on wikipedia traffic:
##
##     - https://www.dropbox.com/s/x14f3bg8flej1n7/train-wikipedia.csv?dl=1
##     - https://www.dropbox.com/s/o2df10dnyt3bg02/test-wikipedia.csv?dl=1
##

## set project cwd: only execute in RStudio
if (nzchar(Sys.getenv('RSTUDIO_USER_IDENTITY'))) {
  cwd <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
  setwd(cwd)
}

## utility functions
devtools::install_local(paste(cwd, sep='', '/packages/ist687utility'))
library('ist687utility')

## arima functions
devtools::install_local(paste(cwd, sep='', '/packages/ist687arima'))
library('ist687arima')

## load packages
if (!require('stringi')) install.packages('stringi', repos='http://cran.rstudio.com/')
library(stringi)

load_package(c('reshape2', 'zoo', 'Hmisc', 'forecast'))

## create dataframes
df <- munge_ist687(
  'https://www.dropbox.com/s/x14f3bg8flej1n7/train-wikipedia.csv?dl=1',
  './dataset/train.csv'
)

## row sums: determine sum for each row
rSums <- rowSums(df[,-c(1:4)])

## top 5 articles
for (i in 1:5) {
  rIndex <- top_indices(rSums, i, i)
  avgTop.m <- melt(df[rIndex, -c(1:4)])
  arima_ist687(avgTop.m$value, 6, 1, 0, 18)
}
