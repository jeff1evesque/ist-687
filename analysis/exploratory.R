##
## exploratory.R, exploratory analysis on wikipedia traffic:
##
##     - https://www.dropbox.com/s/x14f3bg8flej1n7/train_1.csv?dl=1
##     - https://www.dropbox.com/s/o2df10dnyt3bg02/train_2.csv?dl=1
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
  'https://www.dropbox.com/s/x14f3bg8flej1n7/train_2.csv?dl=1',
  './dataset/train_1.csv'
)

## convert wide to long
access.m <- melt(df[-c(2, 3, 4)], id='Access')

## barchart: monthly page views by access
gg_bar(
  data=access.m,
  destfile='visualization/barchart-monthly-access.png',
  xvar='variable',
  yvar='value',
  xlbl='Year.Month',
  ylbl='Page views',
  afill='Access'
)

## barchart: total page views by access
gg_bar(
  data=access.m,
  destfile='visualization/barchart-total-access.png',
  xvar='Access',
  yvar='value',
  xlbl='Access type',
  ylbl='Page views',
  afill='variable'
)

## points: total page views by access (density)
gg_point(
  data=access.m,
  destfile='visualization/points-total-access.png',
  xvar='Access',
  yvar='value',
  xlbl='Total: Access type',
  ylbl='Page views',
  afill='Access',
  acolor='Access'
)

## points: monthly page views by access (density)
gg_point(
  data=access.m,
  destfile='visualization/points-total-access.png',
  xvar='variable',
  yvar='value',
  xlbl='Monthly: Access type',
  ylbl='Total: Access type',
  afill='Access: Page views',
  acolor='Access'
)

## convert wide to long
agent.m <- melt(df[-c(1, 3, 4)], id='Agent')

## barchart: monthly page views by agent
gg_bar(
  data=access.m,
  destfile='visualization/barchart-monthly-agent.png',
  xvar='variable',
  yvar='value',
  xlbl='Year.Month',
  ylbl='Agent: Page views',
  afill='Agent'
)

## barchart: total page views by agent
gg_bar(
  data=access.m,
  destfile='visualization/barchart-total-agent.png',
  xvar='Agent',
  yvar='value',
  xbl='Agent',
  ylbl='Page views',
  afill='Agent',
  position='dodge'
)

## convert wide to long
language.m <- melt(df[-c(1, 2, 3)], id='Language')

## barchart: monthly page views by agent
gg_bar(
  data=language.m,
  destfile='visualization/barchart-monthly-language.png',
  xvar='variable',
  yvar='value',
  xbl='Year.Month',
  ylbl='Language: Page views',
  afill='Language'
)

## barchart: total page views by language
gg_bar(
  data=language.m,
  destfile='visualization/barchart-total-language.png',
  xvar='Language',
  yvar='value',
  xbl='Language',
  ylbl='Language: Page views',
  afill='Language',
  position='dodge'
)

##
## points: total page views by agent (density)
##
## Note: boxplot renders very similar to points, since spread is
##       very large, while the interquartile range relatively
##       insignificant to the spread.
##
gg_point(
  data=agent.m,
  destfile='visualization/points-total-agent.png',
  xvar='Agent',
  yvar='value',
  xlbl='Total: Agent type',
  ylbl='Agent: Page views',
  afill='Access: Page views',
  acolor='Agent'
)

## points: monthly page views by agent (density)
gg_point(
  data=agent.m,
  destfile='visualization/points-monthly-agent.png',
  xvar='variable',
  yvar='value',
  xlbl='Monthly: Agent type',
  ylbl='Agent: Page views',
  afill='black',
  acolor='Agent'
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
