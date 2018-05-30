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
load_package('zoo')
load_package('Hmisc')
load_package('ggplot2')

## local variables
domain_regex <- '_www.wikimedia.org_|.wikimedia.org_|.mediawiki.org_|.wikipedia.org_'

## create ignored directories
dir.create(file.path(cwd, 'dataset'), showWarnings = FALSE)
dir.create(file.path(cwd, 'visualization'), showWarnings = FALSE)

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
  colsplit(df1$Page, pattern=domain_regex, c('First', 'Second')),
  df1[,-which(names(df1) == 'Page')]
)
df2 <- cbind(
  colsplit(df2$Page, pattern=domain_regex, c('First', 'Second')),
  df2[,-which(names(df2) == 'Page')]
)

## explode column: First column into Article, and Language columns
df1 <- cbind(
  colsplit(df1$First, pattern='_(?=[^_]+$)', c('Article', 'Language')),
  df1[,-which(names(df1) == 'First')]
)
df2 <- cbind(
  colsplit(df2$First, pattern='_(?=[^_]+$)', c('Article', 'Language')),
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

##
## year range: remove day, convert to year:month, then convert back to year:month:day
##     to ensure the day portion starts at 1, to allow below increment by number of days
##     in a month, via '+ monthDays(start_date1)'.
##
start_date1 <- as.Date(as.yearmon(sub('\\.[^.]+$', '', colnames(df1)[5]), format='X%Y.%m'))
start_date2 <- as.Date(as.yearmon(sub('\\.[^.]+$', '', colnames(df2)[5]), format='X%Y.%m'))
end_date1 <- as.Date(as.yearmon(sub('\\.[^.]+$', '', colnames(df1)[length(colnames(df1))]), format='X%Y.%m'))
end_date2 <- as.Date(as.yearmon(sub('\\.[^.]+$', '', colnames(df2)[length(colnames(df2))]), format='X%Y.%m'))

## aggregate dataframe
df1_aggregate <- df1
df2_aggregate <- df2

## combine columns
while (start_date1 <= end_date1) {
  ## index of columns with 'Y.M' pattern
  col_idx1 <- grep(paste0('X',format(start_date1,"%Y.%m")),names(df1_aggregate))

  ## create new aggregate columns: aggregated on month
  df1_aggregate[, paste0(format(start_date1,"%Y.%m"))] <- rowSums(df1_aggregate[,col_idx1])

  ## remove individual day columns
  df1_aggregate <- df1_aggregate[, -(col_idx1)]

  ## increment loop
  start_date1 <- start_date1 + monthDays(start_date1)
}

while (start_date2 <= end_date2) {
  ## index of columns with 'Y.M' pattern
  col_idx2 <- grep(paste0('X',format(start_date2,"%Y.%m")),names(df2_aggregate))

  ## create new aggregate columns: aggregated on month
  df2_aggregate[, paste0(format(start_date2,"%Y.%m"))] <- rowSums(df2_aggregate[,col_idx2])

  ## remove individual day columns
  df2_aggregate <- df2_aggregate[, -(col_idx2)]

  ## increment loop
  start_date2 <- start_date2 + monthDays(start_date2)
}

## convert wide to long
access.m <- melt(df1_aggregate[-c(2, 3, 4)], id='Access')

## barchart: monthly page views by access
ggplot(access.m, aes(x=variable, y=value, fill=Access)) +
  geom_bar(stat='identity') +
  labs(x = 'Year.Month', y = 'Page views', title = 'Access: Page views vs. Year.Month') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  'visualization/barchart-monthly-access.png',
  width = 16,
  height = 9,
  dpi = 100
)

## barchart: total page views by access
ggplot(access.m, aes(x=Access, y=value, fill=variable)) +
  geom_bar(stat='identity') +
  labs(x = 'Access type', y = 'Page views', title = 'Access: Page views vs. Type', color = 'Year.Month') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  'visualization/barchart-total-access.png',
  width = 16,
  height = 9,
  dpi = 100
)

##
## points: total page views by access (density)
##
## Note: boxplot renders very similar to points, since spread is
##       very large, while the interquartile range relatively
##       insignificant to the spread.
##
ggplot(access.m, aes(x=Access, y=value)) +
  geom_point(aes(fill = Access, color = Access), alpha = 0.35) +
  guides(fill=FALSE) +
  labs(x = 'Total: Access type', y = 'Page views', title = 'Access: Page views vs. Type', color = 'Access') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  'visualization/points-total-access.png',
  width = 16,
  height = 9,
  dpi = 100
)

## points: monthly page views by access (density)
ggplot(access.m, aes(x = variable)) +
  geom_point(aes(y = value, color = Access)) +
  labs(x = 'Monthly: Access type', y = 'Page views', title = 'Access: Page views vs. Access type') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  'visualization/points-monthly-access.png',
  width = 16,
  height = 9,
  dpi = 100
)

## convert wide to long
agent.m <- melt(df1_aggregate[-c(1, 3, 4)], id='Agent')

## barchart: monthly page views by agent
ggplot(agent.m, aes(x=variable, y=value, fill=Agent)) +
  geom_bar(stat='identity') +
  labs(x = 'Year.Month', y = 'Page views', title = 'Agent: Page views vs. Year.Month') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  'visualization/barchart-monthly-agent.png',
  width = 16,
  height = 9,
  dpi = 100
)

## barchart: total page views by agent
ggplot(agent.m, aes(x=Agent, y=value)) +
  geom_bar(stat='identity', position='dodge') +
  labs(x = 'Agent', y = 'Page views', title = 'Agent: Page views vs. Agent') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  'visualization/barchart-total-agent.png',
  width = 16,
  height = 9,
  dpi = 100
)
