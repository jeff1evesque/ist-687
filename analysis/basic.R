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

## local variables
domain_regex <- '_www\.wikimedia\.org_|\.mediawiki\.org_|\.wikipedia\.org_'

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

## explode column: Page column into two general columns
df1 <- cbind(
  colsplit(df1$Page, pattern=regex(domain_regex), c('First', 'Second')),
  df1[,-which(names(df1) == 'Page')]
)
df2 <- cbind(
  colsplit(df2$Page, pattern=regex(domain_regex), c('First', 'Second')),
  df2[,-which(names(df2) == 'Page')]
)

## explode column: First column into Article, and Language columns
df1 <- cbind(
  colsplit(df1$First, pattern=regex('_(?=[^_]+$)'), c('Article', 'Language')),
  df1[,-which(names(df1) == 'First')]
)
df2 <- cbind(
  colsplit(df2$First, pattern=regex('_(?=[^_]+$)'), c('Article', 'Language')),
  df2[,-which(names(df2) == 'First')]
)

## explode column: Second column into Access, and Agent columns
df1 <- cbind(
  colsplit(df1$Second, '_', c('Access', 'Agent')),
  df1[,-which(names(df1) == 'Second')]
)
df2 <- cbind(
  colsplit(df2$Second, '_', c('Access', 'Agent')),
  df2[,-which(names(df2) == 'Second')]
)
