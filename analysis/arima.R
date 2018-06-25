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
if (!require('stringi')) install.packagesgit ('stringi', repos='http://cran.rstudio.com/')
library(stringi)

load_package(c('reshape2', 'zoo', 'Hmisc', 'forecast', 'lmtest'))

## create dataframes
df.train <- munge_ist687(
  'https://www.dropbox.com/s/x14f3bg8flej1n7/train-wikipedia.csv?dl=1',
  './dataset/train.csv'
)
df.test <- munge_ist687(
  'https://www.dropbox.com/s/o2df10dnyt3bg02/test-wikipedia.csv?dl=1',
  './dataset/test.csv'
)

## row sums: determine sum for each row
rSums <- rowSums(df.train[,-c(1:4)])

## top 1-5, 10-15 articles
for (i in c(1:5, 11:15)) {
  rIndex <- top_indices(rSums, i, i)
  avgTop.m <- melt(df.train[rIndex, -c(2,4)])
  article.name <- avgTop.m[i, which(colnames(avgTop.m) == 'Article')]
  article.access <- avgTop.m[i, which(colnames(avgTop.m) == 'Access')]

  ## remove special characters
  article.name <- gsub('[^[^0-9A-Za-z_]', '', article.name, ignore.case = TRUE)
  article.access <- gsub('[^0-9A-Za-z_]', '', article.access, ignore.case = TRUE)

  ##
  ## determine predicted time periods
  ##
  ## Note: the test should be a continuous superset of the train dataset
  ##
  train.period <- ncol(df.test) - ncol(df.train)

  ##
  ## predict 8 time periods (i.e. months) ahead of the train dataset.
  ##
  ## Note: predicted time periods should exist in the test dataset
  ##
  prediction <- arima_ist687(
    avgTop.m$value,
    ar=6,
    i=1,
    ma=0,
    periods=18,
    ahead=train.period,
    suffix=paste0(i, '--', article.name, '-', article.access)
  )

  ## compare prediction against train
  grangertest(prediction, df.train[rIndex, -c(2,4)])
}
