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
library('utility', lib.loc = paste(cwd, sep='', '/packages'))

## dataset directory
dir.create(file.path(cwd, 'dataset'), showWarnings = FALSE)

## download datasets
download_source('https://www.dropbox.com/s/x14f3bg8flej1n7/train_1.csv?dl=1', './dataset/train_1.csv')
download_source('https://www.dropbox.com/s/o2df10dnyt3bg02/train_2.csv?dl=1', './dataset/train_2.csv')

## create dataframes
df1 <- load_df('./train_1.csv')
df2 <- load_df('./train_2.csv')
