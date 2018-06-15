##
## munge_ist687.R, generate necessary dataframes.
##
munge_ist687 <- function(source, filename) {
  ## local variables
  domain_regex <- '_www.wikimedia.org_|.wikimedia.org_|.mediawiki.org_|.wikipedia.org_'
  
  ## create ignored directories
  dir.create(file.path(cwd, 'dataset'), showWarnings = FALSE)
  dir.create(file.path(cwd, 'visualization'), showWarnings = FALSE)

  ## download datasets
  download_source(source, filename)

  ## create dataframe
  df <- load_df('./dataset/train_1.csv')
  
  ## explode column: Page column into two general columns
  df <- cbind(
    colsplit(df$Page, pattern=domain_regex, c('First', 'Second')),
    df[,-which(names(df) == 'Page')]
  )
  
  ## explode column: First column into Article, and Language columns
  df <- cbind(
    colsplit(df$First, pattern='_(?=[^_]+$)', c('Article', 'Language')),
    df[,-which(names(df) == 'First')]
  )
  
  ## explode column: Second column into Access, and Agent columns
  df <- cbind(
    colsplit(df$Second, '_', c('Access', 'Agent')),
    df[,-which(names(df) == 'Second')]
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

  ## remove unrelated rows: pattern match
  df <- df[-grep('^Special:', df$Article),]
  df <- df[-grep('^Especial:', df$Article),]
  df <- df[-grep('^Spezial:', df$Article),]
  df <- df[-grep('^Wikipedia:', df$Article),]

  ## load invalid nonstandard articles
  df.invalid <- load_df('./invalid-articles.csv')

  ## remove unrelated rows: exact article
  df <- df[-which(df$Article %in% df.invalid),]

  ## return dataframe
  return(df)
}
