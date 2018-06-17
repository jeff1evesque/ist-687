##
## gg_timeseries_interaction.R, generate ggplot2 point + bar chart, store
##      into png, then return corresponding ggplot object.
##
gg_timeseries_interaction <- function(data, destfile, xlbl, ylbl, prefix) {
  ## generate ggplot
  if (prefix) {
    gg <- ggplot(data = data, aes(x=variable, y=value, group=interaction(Article, Access), color=interaction(Article, Access))) +
      geom_point() +
      geom_line() +
      labs(x = xlbl, y = ylbl, title = paste(ylbl, ' vs ', ylbl)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  } else {
    gg <- ggplot(data = data, aes(x=variable, y=value, group=interaction(Article, Access), color=interaction(Article, Access))) +
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
