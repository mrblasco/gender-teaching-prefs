require(ggplot2)

theme_custom <- function(base_size = 9, base_family = "Helvetica") {
    theme_minimal(
        base_family = base_family,
        base_size = base_size
    ) +
        theme(
            # Text
            plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(face = "italic"),
            plot.caption = element_blank(),
            axis.title = element_text(
                size = rel(0.9),
                color = "black"
            ),

            # Panels
            panel.background = element_blank(),
            panel.border = element_blank(),
            panel.grid = element_blank(),
            panel.spacing = unit(1.25, "lines"),

            # Facets
            strip.background = element_blank(),
            strip.text = element_text(face = "bold", size = rel(0.95), margin = margin(b = 4)),

            # Axes
            axis.line = element_line(
                linewidth = 0.4,
                color = "black",
            ),
            axis.ticks = element_line(
                linewidth = 0.35,
                color = "black",
            ),
            axis.ticks.length = unit(2, "pt"),

            # Legend
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.key.width = unit(.5, "in"),

            # General
            plot.margin = margin(
                t = 5, r = 5, b = 5, l = 5
            )
        )
}


# ------------------------------------------------------------
# Color palette
# ------------------------------------------------------------

my_colors <- c(
    "#1b9e77", "#d95f02",
    "#7570b3", "#e7298a",
    "#66a61e", "#e6ab02"
)

scale_color_discrete <- function(...) {
    ggplot2::scale_color_manual(values = my_colors, ...)
}
scale_fill_discrete <- function(...) {
    ggplot2::scale_fill_manual(values = my_colors, ...)
}
