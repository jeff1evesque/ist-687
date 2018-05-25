##
## download_source.R, download provided 'sourcefile' to 'destfile'.
##
download_source <- function(sourefile, destfile) {
  if(!file.exists(destfile)) {
    res <- tryCatch(
        download.file(
            sourcefile,
            destfile=destfile
        ),
        error=function(e) 1
    )
  }
}
