##
## gg_point.R, generate ggplot2 point chart, store into png, then return
##     corresponding ggplot object.
##
gg_point <- function(data, destfile, xvar, yvar, xlbl, ylbl, afill, acolor) {
  ## generate ggplot
  gg <- ggplot(data, aes(x=xvar, y=yvar)) +
    geom_point(aes(fill = afill, color=acolor), alpha = 0.35) +
    labs(x = xlbl, y = ylbl, title = paste(ylbl, ' vs ', ylbl)) +
    theme(plot.title = element_text(hjust = 0.5))

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
