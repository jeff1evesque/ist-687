##
## download.R, download provided 'sourcefile' to 'destfile'.
##
download <- function(sourefile, destfile) {
  if(!file.exists(destfile)) {
    res <- tryCatch(
        download.file(
            sourcefile,
            destfile=destfile
        ),
        error=function(e) 1
    )
    load(destfile) 
  }
}
