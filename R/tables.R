
create_table <- function(x, ...) {
    kableExtra::kbl(x, ...) |>
        kableExtra::kable_styling(
            latex_options = c("striped", "hold_position"),
            font_size = 10,
            full_width = TRUE,
            position = "center"
        )
}