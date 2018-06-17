##
## gg_bar.R, generate ggplot2 barchart, store into png, then return
##     corresponding ggplot object.
##
gg_bar <- function(data, destfile, xvar, yvar, xlbl, ylbl, afill, dodge) {

  if (dodge) {
    ## generate ggplot
    gg <- ggplot(data, aes(x=xvar, y=yvar, fill=afill)) +
      geom_bar(stat='identity') +
      labs(x = xlbl, y = ylbl, title = paste(ylbl, ' vs ', ylbl)) +
      theme(plot.title = element_text(hjust = 0.5))
  } else {
    ## generate ggplot
    gg <- ggplot(data, aes(x=xVar, y=yVar, fill=afill)) +
      geom_bar(stat='identity', position=dodge) +
      labs(x = xlbl, y = ylbl, title = paste(ylbl, ' vs ', ylbl)) +
      theme(plot.title = element_text(hjust = 0.5))
  }

  ## save ggplot
  ggsave(
    destfile,
    width = 16,
    height = 9,
    dpi = 100
  )

  ## return ggplot
  return(gg)
}
