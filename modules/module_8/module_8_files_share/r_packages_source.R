
# functions ---------------------------------------------------------------

# hello world function:

welcome <- 
  function() {
    'hello world'
  }

# Standard error:

se <- 
  function(x) {
    sd(x) / 
      sqrt(
        length(x)
      )
  }

# plot themes -------------------------------------------------------------

# plot theme for a 7.5 x 5 inch plot

plot_theme_7.5x5 <- 
  theme(
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = 'white'),
    panel.grid = element_line(color = '#e9e9e9'),
    panel.grid.minor = element_line(linetype = 'dashed'),
    axis.line = element_line(color = 'black'
    )
  )
