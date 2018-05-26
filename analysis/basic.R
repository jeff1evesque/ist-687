##
## basic.R, analyze the the following wikipedia dataset:
##
##     - https://www.dropbox.com/s/x14f3bg8flej1n7/train_1.csv?dl=1
##     - https://www.dropbox.com/s/o2df10dnyt3bg02/train_2.csv?dl=1
##

## set project cwd
cwd <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(cwd)

## utility functions
devtools::install_local(paste(cwd, sep='', '/packages/ist687utility'))
library('ist687utility')

## load packages
load_package('reshape2')

## dataset directory
dir.create(file.path(cwd, 'dataset'), showWarnings = FALSE)

## download datasets
download_source(
  'https://www.dropbox.com/s/x14f3bg8flej1n7/train_1.csv?dl=1',
  './dataset/train_1.csv'
)
download_source(
  'https://www.dropbox.com/s/o2df10dnyt3bg02/train_2.csv?dl=1',
  './dataset/train_2.csv'
)

## create dataframes
df1 <- load_df('./dataset/train_1.csv')
df2 <- load_df('./dataset/train_2.csv')

## explode column: first column into two general columns
df1 <- cbind(
  colsplit(df1$Page, '.wikipedia.org_', c('first', 'second')),
  df1[,-which(names(df1) == 'Page')]
)
df2 <- cbind(
  colsplit(df2$Page, '.wikipedia.org_', c('first', 'second')),
  df2[,-which(names(df2) == 'Page')]
)

## explode column: second column into Article, and Language columns
df1 <- cbind(
  colsplit(df1$first, pattern=regex('_(?=[^_]+$)'), c('Article', 'Language')),
  df1[,-which(names(df1) == 'first')]
)
df2 <- cbind(
  colsplit(df2$first, pattern=regex('_(?=[^_]+$)'), c('Article', 'Language')),
  df2[,-which(names(df2) == 'first')]
)

## explode column: second column into Access, and Agent columns
df1 <- cbind(
  colsplit(df1$second, '_', c('Access', 'Agent')),
  df1[,-which(names(df1) == 'second')]
)
df2 <- cbind(
  colsplit(df2$second, '_', c('Access', 'Agent')),
  df2[,-which(names(df2) == 'second')]
)
