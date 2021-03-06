##
## exploratory.R, exploratory analysis on wikipedia traffic:
##
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

load_package(c('reshape2', 'zoo', 'Hmisc', 'ggplot2', 'grid', 'gridExtra'))

## create dataframes
df <- munge_ist687(
  'https://www.dropbox.com/s/x14f3bg8flej1n7/test-wikipedia.csv?dl=1',
  './dataset/test.csv'
)

## convert wide to long
access.m <- melt(df[-c(2, 3, 4)], id='Access')

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
language.m <- melt(df[-c(1, 2, 3)], id='Language')

## barchart: monthly page views by Language
ggplot(language.m, aes(x=variable, y=value, fill=Language)) +
  geom_bar(stat='identity') +
  labs(x = 'Year.Month', y = 'Page views', title = 'Language: Page views vs. Year.Month') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  'visualization/barchart-monthly-language.png',
  width = 16,
  height = 9,
  dpi = 100
)

## barchart: total page views by language
ggplot(language.m, aes(x=Language, y=value)) +
  geom_bar(stat='identity', position='dodge') +
  labs(x = 'Language', y = 'Page views', title = 'Page views vs. Language') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  'visualization/barchart-total-language.png',
  width = 16,
  height = 9,
  dpi = 100
)

## convert wide to long
monthly_pageviews.m <- melt(colSums(df[,-c(1:4)]))

## time series: sum each row, aggregated per month
ggplot(data = monthly_pageviews.m, aes(x=rownames(monthly_pageviews.m), y=value, group=1)) +
  geom_point() +
  geom_line() +
  labs(x = 'Year.Month', y = 'Page views', title = 'Total: Page views vs. Year.Month') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(
  'visualization/timeseries-monthly-total-pageviews.png',
  width = 16,
  height = 9,
  dpi = 100
)

## time series: sum top 10 articles, aggregated per month
monthly_total_top10 <- lapply(df, function(x) sum(order(x, decreasing = FALSE)[1:10]))
monthly_total_top10 <- data.frame(monthly_total_top10)
colnames(monthly_total_top10) <- gsub('X', '', colnames(monthly_total_top10))
monthly_total_top10.m <- melt(monthly_total_top10[,-c(1:4)])

ggplot(data = monthly_total_top10.m, aes(x=variable, y=value, group=1)) +
  geom_point() +
  geom_line() +
  labs(x = 'Year.Month', y = 'Page views', title = 'Top 10 Sum: Page views vs. Year.Month') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(
  'visualization/timeseries-monthly-top10-pageviews.png',
  width = 16,
  height = 9,
  dpi = 100
)

##
## time series: top 10 articles, aggregated per month
##
## Note: top 10 articles attained by summing across rows, then selecting
##       the 10 highest values.
##
row_sums <- rowSums(df[,-c(1:4)])

## top 10 articles
row_indices_10 <- top_indices(row_sums, 10)
average_top10.m <- melt(df[row_indices_10,-c(2)], id.var=c('Article', 'Language', 'Access'))

gg_timeseries_10 <- ggplot(data = average_top10.m, aes(x=variable, y=value, group=interaction(Article, Access), color=interaction(Article, Access))) +
  geom_line() +
  labs(x = 'Year.Month', y = 'Page views', title = 'Top 10: Page views vs. Year.Month', color = 'Article') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(
  'visualization/timeseries-individual-top10-pageviews.png',
  width = 16,
  height = 9,
  dpi = 100
)

## top 10-20 articles
row_indices_20 <- top_indices(row_sums, 20, 10)
average_top20.m <- melt(df[row_indices_20,-c(2)], id.var=c('Article', 'Language', 'Access'))

ggplot(data = average_top20.m, aes(x=variable, y=value, group=interaction(Article, Access), color=interaction(Article, Access))) +
  geom_line() +
  labs(x = 'Year.Month', y = 'Page views', title = 'Top 10-20: Page views vs. Year.Month', color = 'Article') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(
  'visualization/timeseries-individual-sub20-pageviews.png',
  width = 16,
  height = 9,
  dpi = 100
)

## top 20-30 articles
row_indices_30 <- top_indices(row_sums, 30, 20)
average_top30.m <- melt(df[row_indices_30,-c(2)], id.var=c('Article', 'Language', 'Access'))

ggplot(data = average_top30.m, aes(x=variable, y=value, group=interaction(Article, Access), color=interaction(Article, Access))) +
  geom_line() +
  labs(x = 'Year.Month', y = 'Page views', title = 'Top 20-30: Page views vs. Year.Month', color = 'Article') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(
  'visualization/timeseries-individual-sub30-pageviews.png',
  width = 16,
  height = 9,
  dpi = 100
)

## top 30-40 articles
row_indices_40 <- top_indices(row_sums, 40, 30)
average_top40.m <- melt(df[row_indices_40,-c(2)], id.var=c('Article', 'Language', 'Access'))

ggplot(data = average_top40.m, aes(x=variable, y=value, group=interaction(Article, Access), color=interaction(Article, Access))) +
  geom_line() +
  labs(x = 'Year.Month', y = 'Page views', title = 'Top 30-40: Page views vs. Year.Month', color = 'Article') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(
  'visualization/timeseries-individual-sub40-pageviews.png',
  width = 16,
  height = 9,
  dpi = 100
)

## top 40-50 articles
row_indices_50 <- top_indices(row_sums, 50, 40)
average_top50.m <- melt(df[row_indices_50,-c(2)], id.var=c('Article', 'Language', 'Access'))

ggplot(data = average_top50.m, aes(x=variable, y=value, group=interaction(Article, Access), color=interaction(Article, Access))) +
  geom_line() +
  labs(x = 'Year.Month', y = 'Page views', title = 'Top 40-50: Page views vs. Year.Month', color = 'Article') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(
  'visualization/timeseries-individual-sub50-pageviews.png',
  width = 16,
  height = 9,
  dpi = 100
)

## top 50-60 articles
row_indices_60 <- top_indices(row_sums, 60, 50)
average_top60.m <- melt(df[row_indices_60,-c(2)], id.var=c('Article', 'Language', 'Access'))

ggplot(data = average_top60.m, aes(x=variable, y=value, group=interaction(Article, Access), color=interaction(Article, Access))) +
  geom_line() +
  labs(x = 'Year.Month', y = 'Page views', title = 'Top 50-60: Page views vs. Year.Month', color = 'Article') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(
  'visualization/timeseries-individual-sub60-pageviews.png',
  width = 16,
  height = 9,
  dpi = 100
)