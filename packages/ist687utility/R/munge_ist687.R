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
  df <- load_df(filename)
  
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

  ## get last date column
  last_date <- colnames(df)[ncol(df)]
  splitter <- strsplit(as.character(last_date), split='.', fixed=TRUE)
  pattern <- paste('^', splitter[[1]][1], '.*.', splitter[[1]][3], '$')

  ## remove columns by mm/xx/yyyy, if month is not full
  if (splitter[[1]][1] == '1' && splitter[[1]][2] != '31') {
    df <- df[-grep(pattern, colnames(df)),]
  } else if (splitter[[1]][1] == '2' && (splitter[[1]][2] != '28' || splitter[[1]][2] != '29')) {
    df <- df[-grep(pattern, colnames(df)),]
  } else if (splitter[[1]][1] == '3' && splitter[[1]][2] == '31') {
    df <- df[-grep(pattern, colnames(df)),]
  } else if (splitter[[1]][1] == '4' && splitter[[1]][2] == '30') {
    df <- df[-grep(pattern, colnames(df)),]
  } else if (splitter[[1]][1] == '5' && splitter[[1]][2] == '31') {
    df <- df[-grep(pattern, colnames(df)),]
  } else if (splitter[[1]][1] == '6' && splitter[[1]][2] == '30') {
    df <- df[-grep(pattern, colnames(df)),]
  } else if (splitter[[1]][1] == '7' && splitter[[1]][2] == '31') {
    df <- df[-grep(pattern, colnames(df)),]
  } else if (splitter[[1]][1] == '8' && splitter[[1]][2] == '31') {
    df <- df[-grep(pattern, colnames(df)),]
  } else if (splitter[[1]][1] == '9' && splitter[[1]][2] == '30') {
    df <- df[-grep(pattern, colnames(df)),]
  } else if (splitter[[1]][1] == '10' && splitter[[1]][2] == '31') {
    df <- df[-grep(pattern, colnames(df)),]
  } else if (splitter[[1]][1] == '11' && splitter[[1]][2] == '30') {
    df <- df[-grep(pattern, colnames(df)),]
  } else if (splitter[[1]][1] == '12' && splitter[[1]][2] == '31') {
    df <- df[-grep(pattern, colnames(df)),]
  }

  ## return dataframe
  return(df)
}
