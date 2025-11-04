require(ggplot2)

theme_custom <- function() {
  theme_minimal(base_family = "Helvetica") +
    theme(
      plot.title      = element_text(face = "bold"),
      plot.subtitle   = element_text(face = "italic"),
      plot.caption    = element_blank(),
      panel.spacing   = unit(1.5, "lines"),
      strip.text      = element_text(face = "bold"),
      axis.line       = element_line(color = "gray25"),
      axis.ticks      = element_line(),
      legend.position = "bottom",
      legend.title    = element_blank(),
    )
}
