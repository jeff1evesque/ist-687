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
load_package(c('reshape2', 'zoo', 'Hmisc', 'ggplot2'))

## create dataframes
df <- munge_ist687(
  'https://www.dropbox.com/s/x14f3bg8flej1n7/train_1.csv?dl=1',
  './dataset/train_1.csv'
)

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

##
## year range: remove day, convert to year:month, then convert back to year:month:day
##     to ensure the day portion starts at 1, to allow below increment by number of days
##     in a month, via '+ monthDays(start_date1)'.
##
start_date <- as.Date(as.yearmon(sub('\\.[^.]+$', '', colnames(df)[5]), format='X%Y.%m'))
end_date <- as.Date(as.yearmon(sub('\\.[^.]+$', '', colnames(df)[length(colnames(df))]), format='X%Y.%m'))

## combine columns
while (start_date <= end_date) {
  ## index of columns with 'Y.M' pattern
  col_idx <- grep(paste0('X',format(start_date,"%Y.%m")),names(df))

  ## create new aggregate columns: aggregated on month
  df[, paste0(format(start_date,"%Y.%m"))] <- rowSums(df[,col_idx])

  ## remove individual day columns
  df <- df[, -(col_idx)]

  ## increment loop
  start_date <- start_date + monthDays(start_date)
}

## convert wide to long
access.m <- melt(df[-c(2, 3, 4)], id='Access')

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
agent.m <- melt(df[-c(1, 3, 4)], id='Agent')

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

##
## points: total page views by agent (density)
##
## Note: boxplot renders very similar to points, since spread is
##       very large, while the interquartile range relatively
##       insignificant to the spread.
##
ggplot(agent.m, aes(x=Agent, y=value)) +
  geom_point(aes(fill = Agent, color = Agent), alpha = 0.35) +
  guides(fill=FALSE) +
  labs(x = 'Total: Agent type', y = 'Page views', title = 'Agent: Page views vs. Type', color = 'Agent') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  'visualization/points-total-agent.png',
  width = 16,
  height = 9,
  dpi = 100
)

## points: monthly page views by agent (density)
ggplot(agent.m, aes(x = variable)) +
  geom_point(aes(y = value, color = Agent)) +
  labs(x = 'Monthly: Agent type', y = 'Page views', title = 'Agent: Page views vs. Type') +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  'visualization/points-monthly-agent.png',
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
row_indices <- top_indices(row_sums, 10)
average_top10.m <- melt(df[row_indices,-c(2)], id.var=c('Article', 'Language', 'Access'))

ggplot(data = average_top10.m, aes(x=variable, y=value, group=interaction(Article, Access), color=interaction(Article, Access))) +
  geom_point() +
  geom_line() +
  labs(x = 'Year.Month', y = 'Page views', title = 'Articles: Page views vs. Year.Month', color = 'Article') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(
  'visualization/timeseries-monthly-individual-pageviews.png',
  width = 16,
  height = 9,
  dpi = 100
)