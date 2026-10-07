library(broom.mixed)
library(purrr)
library(dplyr)
library(ggplot2)


source("R/theme.R")
theme_set(theme_custom())


# ----------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

out_dir <- if (length(args) >= 1) args[1] else file.path("output", "plots")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

data_dir <- file.path("output", "03_fit_models")
files <- list.files(data_dir, pattern = "rds", full = TRUE)

# Labels
term_labels <- c(
    teamf = "Single female",
    teamff = "Female-female team",
    teammm = "Male-male team",
    teamfm = "Mixed team"
)

file_labels <- c(
    conventionality.rds = "Conventionality",
    women.rds = "Women ratio",
    atypicality.rds = "Atypicality",
    age_readings.rds = "Age readings",
    interdisc.rds = "Interdisciplinarity"
)

# Load models
extract_coeffs <- function(object) {
    broom.mixed::tidy(object, conf.int = TRUE)
}

# ----------------------------------------------------------------------
# Extract coefficients
# ----------------------------------------------------------------------

coeffs <- lapply(files, function(d) {
    bind_rows(map(readRDS(d), extract_coeffs), .id = "year")
})
names(coeffs) <- basename(files)
coeffs <- bind_rows(coeffs, .id = "file")


results <- coeffs |>
    filter(grepl("^team", term)) |>
    mutate(
        outcome = recode(file, !!!file_labels),
        contrast = recode(term, !!!term_labels),
        year = as.integer(format(as.Date(year, "%Y"), "%Y")),
        estimate_display = if_else(
            file == "women.rds",
            estimate * 100,
            estimate
        ),
        ci_low = if_else(
            file == "women.rds",
            conf.low * 100,
            conf.low
        ),
        ci_high = if_else(
            file == "women.rds",
            conf.high * 100,
            conf.high
        ),
        estimate_ci = sprintf(
            "%.2f [%.2f, %.2f]",
            estimate_display,
            ci_low,
            ci_high
        )
    )


avg_coeffs <- coeffs |>
    group_by(file, term) |>
    summarise(
        estimate = weighted.mean(estimate, 1 / std.error^2),
        std.error = sqrt(1 / sum(1 / std.error^2)),
        .groups = "drop"
    )

avg_coeffs_plot <- avg_coeffs |>
    filter(grepl("team", term)) |>
    mutate(
        outcome = recode(file, !!!file_labels),
        contrast = recode(term, !!!term_labels),
        estimate_display = if_else(file == "women.rds",
            estimate * 100,
            estimate
        ),
        conf_low = estimate_display - 1.96 * std.error,
        conf_high = estimate_display + 1.96 * std.error
    )

avg_coeffs_plot |>
    ggplot(aes(x = estimate_display, y = contrast)) +
    geom_vline(
        xintercept = 0, linewidth = 0.4,
        linetype = "dashed"
    ) +
    geom_errorbarh(
        aes(xmin = conf_low, xmax = conf_high),
        height = 0,
        linewidth = 0.5
    ) +
    geom_point(size = 2) +
    facet_wrap(~outcome, scales = "free_x", nrow = 1) +
    labs(
        x = "Estimated difference relative to single-male teams",
        y = NULL
    )

ggsave(
    file.path(out_dir, "avg_coeffs.pdf"),
    device = cairo_pdf,
    width = 7,
    height = 3,
    unit = "in"
)


p_combined <- coeffs |>
    filter(grepl("team", term)) |>
    mutate(year = as.Date(year, "%Y")) |>
    ggplot(aes(year, estimate, ymin = conf.low, ymax = conf.high, color = term)) +
    facet_grid(
        file ~ term,
        scales = "free",
        switch = "y",
        labeller = labeller(term = term_labels, file = file_labels)
    ) +
    scale_color_discrete() +
    scale_shape_manual(values = c(1, 16)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange(show.legend = FALSE, aes(shape = abs(estimate) > 2 * std.error)) +
    geom_smooth(method = "lm", aes(weight = 1 / std.error^2), formula = "y ~ x") +
    labs(
        x = "Academic year",
        y = "Outcome"
    ) +
    theme(legend.position = "none", strip.placement = "outside")


ggsave(
    file.path(out_dir, "combined.pdf"),
    device = cairo_pdf,
    width = 7,
    height = 7,
    unit = "in"
)
