##
## dataframe.R, loads sourefile into dataframe, and remove NA rows
##
load_df <- function(sourcefile) {
  df <- read.csv(sourcefile, header = TRUE)
  df <- df[complete.cases(df),]
  return(df)
}
