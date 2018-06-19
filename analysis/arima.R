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

load_package(c('reshape2', 'zoo', 'Hmisc', 'forecast'))

## create dataframes
df <- munge_ist687(
  'https://www.dropbox.com/s/x14f3bg8flej1n7/train-wikipedia.csv?dl=1',
  './dataset/train.csv'
)

## row sums: determine sum for each row
rSums <- rowSums(df[,-c(1:4)])

## top article
rIndex.1 <- top_indices(rSums, 1, 1)
avgTop1.m <- melt(df[rIndex.1, -c(1:4)])
avgTop1.m$value <- ts(avgTop1.m$value, start=c(2015, 7), frequency=18)

##
## first difference: difference between one successive month
##
## Note: this is similar to the second argument to the 'arima'
##       function, implemented below.
##
ts.d <- diff(avgTop1.m$value, 1)

## plot series
plot(ts.d)

##
## generate ARI model:
##
## Note: the arima arguments include:
##
## AR: autoregression, use the dependent relationship between an observation
##     and some number of lagged observations.
##
## I: integrated, use of differencing of raw observations, or subtracting an
##     observation from one at the previous time step. The goal is to attain a
##     time series that is stationary.
##
## MA: moving average, uses the dependency between an observation and a residual
##     error from a moving average model applied to lagged observations.
##
fit.ts.ar <- arima(avgTop1.m$value, order=c(6, 1, 0))

## generate forecast: we only have 18 periods
fit.ts.arf <- forecast(fit.ts.ar, h=18)

## visualize forecast
plot(fit.ts.arf, include=18)

