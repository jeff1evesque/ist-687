##
## gg_timeseries.R, generate ggplot2 point + bar chart, store into png,
##     then return corresponding ggplot object.
##
gg_timeseries <- function(data, destfile, xvar, yvar, xlbl, ylbl, agroup, prefix=NULL) {
  ## generate ggplot
  if (prefix) {
    gg <- ggplot(data = data, aes(x=xvar, y=yvar, group=agroup)) +
      geom_point() +
      geom_line() +
      labs(x = xlbl, y = ylbl, title = paste(ylbl, ' vs ', ylbl)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } else {
    gg <- ggplot(data = data, aes(x=xvar, y=yvar, group=agroup)) +
      geom_point() +
      geom_line() +
      labs(x = xlbl, y = ylbl, title = paste(prefix, ylbl, ' vs ', ylbl)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
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
