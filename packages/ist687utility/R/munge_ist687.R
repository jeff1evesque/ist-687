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

  ## return dataframe
  return(df)
}
