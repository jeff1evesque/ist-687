##
## exploratory.R, ARIMA analysis on wikipedia traffic:
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

## load packages
if (!require('stringi')) install.packages('stringi', repos='http://cran.rstudio.com/')
library(stringi)

load_package(c('zoo', 'Hmisc', 'forecast'))

## create dataframes
df <- munge_ist687(
  'https://www.dropbox.com/s/x14f3bg8flej1n7/train-wikipedia.csv?dl=1',
  './dataset/train.csv'
)

## row sums: determine sum for each row
rSums <- rowSums(df[,-c(1:4)])

## top article
rIndex.1 <- top_indices(rSums, 1, 1)
avgTop1.m <- melt(df[rIndex.1, -c(2, 4)], id.var=c('Access', 'Article'))
avgTop1.m$value <- ts(avgTop1.m$value, start=c(2015, 7), frequency=18)

## first difference: difference between one successive month
ts.d <- diff(avgTop1.m$value, 1)

## generate ar model
fit.ts.ar <- arima(avgTop1.m, order=c(1, 0, 0))

plot(avgTop1.m)

