# ======================================================================
# Results: publication-ready figures and supplementary tables
# ======================================================================

library(broom.mixed)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(readr)

source(here::here("R/theme.R"))
theme_set(theme_custom())

# ----------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

out_dir <- if (length(args) >= 1) {
  args[1]
} else {
  file.path("output", "plots")
}

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

data_dir <- here::here("output", "03_fit_models")

files <- list.files(
  data_dir,
  pattern = "\\.rds$",
  full.names = TRUE
)


# ----------------------------------------------------------------------
# Labels
# ----------------------------------------------------------------------

term_labels <- c(
  teamf  = "Single female",
  teamff = "Female–female team",
  teammm = "Male–male team",
  teamfm = "Mixed team"
)

file_labels <- c(
  conventionality.rds = "Conventionality",
  women.rds = "Women ratio",
  atypicality.rds = "Atypicality",
  age_readings.rds = "Age readings",
  interdisc.rds = "Interdisciplinarity"
)


# ----------------------------------------------------------------------
# Load and extract model coefficients
# ----------------------------------------------------------------------

extract_coeffs <- function(object) {
  broom.mixed::tidy(
    object,
    conf.int = TRUE,
    conf.level = 0.95
  )
}

coeffs <- map_dfr(
  files,
  function(file) {
    models <- readRDS(file)
    bind_rows(
      map(models, extract_coeffs),
      .id = "year"
    ) |>
      mutate(file = basename(file))
  }
)


# ----------------------------------------------------------------------
# Prepare publication-ready results
# ----------------------------------------------------------------------

results <- coeffs |>
  filter(grepl("^team", term)) |>
  mutate(
    # Year stored as a date in the original model list
    year = as.integer(
      format(as.Date(year, "%Y"), "%Y")
    ),
    outcome = recode(
      file,
      !!!file_labels
    ),
    contrast = recode(
      term,
      !!!term_labels
    ),

    # Display scaling only.
    # The original estimate remains unchanged.
    scale_factor = if_else(
      file == "women.rds",
      100,
      1
    ),
    estimate_display = estimate * scale_factor,
    conf_low_display = conf.low * scale_factor,
    conf_high_display = conf.high * scale_factor,

    # Convenient publication-ready representation
    estimate_ci = sprintf(
      "%.2f [%.2f, %.2f]",
      estimate_display,
      conf_low_display,
      conf_high_display
    )
  )


# ----------------------------------------------------------------------
# Order factors
# ----------------------------------------------------------------------

results <- results |>
  mutate(
    outcome = factor(
      outcome,
      levels = c(
        "Conventionality",
        "Women ratio",
        "Atypicality",
        "Age readings",
        "Interdisciplinarity"
      )
    ),
    contrast = factor(
      contrast,
      levels = c(
        "Single female",
        "Female–female team",
        "Male–male team",
        "Mixed team"
      )
    )
  )


# ======================================================================
# 1. POOLED ESTIMATES
# ======================================================================

# Inverse-variance weighted average across years.
#
# NOTE:
# This is appropriate if the year-specific estimates are being treated
# as estimates of a common underlying effect. If year-specific estimates
# are dependent, a hierarchical model is preferable.

avg_coeffs <- results |>
  group_by(file, outcome, term, contrast) |>
  summarise(
    estimate = weighted.mean(
      estimate,
      w = 1 / std.error^2,
      na.rm = TRUE
    ),
    std.error = sqrt(
      1 / sum(
        1 / std.error^2,
        na.rm = TRUE
      )
    ),
    .groups = "drop"
  ) |>
  mutate(
    conf_low = estimate - 1.96 * std.error,
    conf_high = estimate + 1.96 * std.error,
    scale_factor = if_else(
      file == "women.rds",
      100,
      1
    ),
    estimate_display = estimate * scale_factor,
    conf_low_display = conf_low * scale_factor,
    conf_high_display = conf_high * scale_factor,
    estimate_ci = sprintf(
      "%.2f [%.2f, %.2f]",
      estimate_display,
      conf_low_display,
      conf_high_display
    )
  )


# ----------------------------------------------------------------------
# Main forest plot
# ----------------------------------------------------------------------

p_main <- avg_coeffs |>
  ggplot(
    aes(
      x = estimate_display,
      y = contrast
    )
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_errorbar(
    aes(
      xmin = conf_low_display,
      xmax = conf_high_display
    ),
    orientation = "y",
    width = 0,
    linewidth = 0.55
  ) +
  geom_point(
    size = 2, shape = 21, aes(fill = file)
  ) +
  facet_wrap(
    ~outcome,
    nrow = 1,
    scales = "free_x"
  ) +
  labs(
    x = "Estimated difference relative to single-male teams",
    y = NULL
  ) +
  theme(
    legend.position = "none",
    strip.placement = "outside",
    panel.spacing = unit(1, "lines")
  )


ggsave(
  file.path(out_dir, "main_forest_plot.pdf"),
  p_main,
  device = cairo_pdf,
  width = 7.2,
  height = 1.5,
  units = "in"
)


# ======================================================================
# 2. TEMPORAL STABILITY FIGURE
# ======================================================================

# Create one plot for each outcome × contrast combination
plots <- results |>
  split(results$outcome) |>
  lapply(function(d) {
    ggplot(
      d,
      aes(
        x = year,
        y = estimate_display,
        ymin = conf_low_display,
        ymax = conf_high_display
      )
    ) +
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        linewidth = 0.35
      ) +
      geom_ribbon(
        aes(fill = contrast),
        alpha = 0.2
      ) +
      geom_point(
        aes(fill = contrast),
        size = 1.5,
        shape = 21
      ) +
      facet_grid(~contrast) +
      scale_x_continuous(
        breaks = scales::pretty_breaks(n = 5)
      ) +
      labs(
        x = "Academic year",
        y = "Difference vs single-male teams" |>
          stringr::str_wrap(width = 20)
      ) +
      theme(
        legend.position = "none"
      )
  })

# Arrange panels
p_years <- wrap_plots(
  plots,
  ncol = 1
) +
  plot_annotation(tag_levels = "A")

ggsave(
  file.path(out_dir, "supp_temporal_stability.pdf"),
  p_years,
  device = cairo_pdf,
  width = 7,
  height = 9,
  units = "in"
)



# ======================================================================
# 3. SUPPLEMENTARY TABLE: POOLED ESTIMATES
# ======================================================================

table_pooled <- avg_coeffs |>
  select(
    outcome,
    contrast,
    estimate_display,
    conf_low_display,
    conf_high_display,
    estimate_ci
  ) |>
  arrange(
    outcome,
    contrast
  )


write_csv(
  table_pooled,
  file.path(
    out_dir,
    "supp_table_pooled_estimates.csv"
  )
)


# ----------------------------------------------------------------------
# Wide version: one row per outcome
# ----------------------------------------------------------------------

table_pooled_wide <- avg_coeffs |>
  select(
    outcome,
    contrast,
    estimate_ci
  ) |>
  pivot_wider(
    names_from = contrast,
    values_from = estimate_ci
  ) |>
  arrange(outcome)


write_csv(
  table_pooled_wide,
  file.path(
    out_dir,
    "supp_table_pooled_estimates_wide.csv"
  )
)


# ======================================================================
# 4. SUPPLEMENTARY TABLE: YEAR-SPECIFIC ESTIMATES
# ======================================================================

table_year <- results |>
  select(
    outcome,
    year,
    contrast,
    estimate_display,
    conf_low_display,
    conf_high_display,
    estimate_ci
  ) |>
  arrange(
    outcome,
    year,
    contrast
  )


write_csv(
  table_year,
  file.path(
    out_dir,
    "supp_table_year_specific_estimates.csv"
  )
)


# ----------------------------------------------------------------------
# Wide year-specific table
# ----------------------------------------------------------------------

table_year_wide <- results |>
  select(
    outcome,
    year,
    contrast,
    estimate_ci
  ) |>
  pivot_wider(
    names_from = contrast,
    values_from = estimate_ci
  ) |>
  arrange(
    outcome,
    year
  )


write_csv(
  table_year_wide,
  file.path(
    out_dir,
    "supp_table_year_specific_estimates_wide.csv"
  )
)


# ======================================================================
# 5. SIMPLE MODEL INFORMATION TABLE
# ======================================================================

model_info <- coeffs |>
  filter(grepl("^team", term)) |>
  count(
    file,
    name = "n_year_specific_estimates"
  ) |>
  mutate(
    outcome = recode(
      file,
      !!!file_labels
    )
  ) |>
  select(
    outcome,
    n_year_specific_estimates
  ) |>
  arrange(outcome)


write_csv(
  model_info,
  file.path(
    out_dir,
    "supp_table_model_information.csv"
  )
)


# ======================================================================
# 6. SAVE CLEAN R OBJECTS
# ======================================================================

saveRDS(
  results,
  file.path(
    out_dir,
    "results_year_specific.rds"
  )
)

saveRDS(
  avg_coeffs,
  file.path(
    out_dir,
    "results_pooled.rds"
  )
)

# ======================================================================
# Finished
# ======================================================================

message("Results written to:", out_dir)
message("Main figure: main_forest_plot.pdf")
message("Supplementary figure: supp_temporal_stability.pdf")
message("Supplementary tables: CSV files")
