#!/usr/bin/env Rscript
#
# Montecarlo simulations.
#
# Update: Sep 27


# ----------------------------------------------------------------------
# Setup
# ----------------------------------------------------------------------
suppressWarnings({
    library(dplyr, warn.conflicts = FALSE)
    library(tidyr)
    library(ggplot2)
    library(patchwork)
})
options(warn = -1)

source(here::here("R/utils.R"))   # rank_percentile(), log_msg()
source(here::here("R/isced.R"))   # isced lookup
source(here::here("R/theme.R"))   # theme_custom()
theme_set(theme_custom())

set.seed(4881)
formats <- c("pdf", "png", "svg")

args <- commandArgs(trailingOnly = TRUE)

data_dir    <- here::here("data", "processed")
out_dir <- ifelse(length(args) >= 1, args[1], tempdir())
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------
# Utils
# ----------------------------------------------------------------------
say <- logger::log_info

year_cutoff <- 1999

team_size_cutoff <- 3

team_labels <- c(
    f = "Female",
    ff = "Female + female",
    mf = "Male + female",
    fm = "Female + male",
    mm = "Male + male",
    m = "Male"
)

# ----------------------------------------------------------------------
# Load & prepare data
# ----------------------------------------------------------------------
say("Loading data ...")
syllabi_merged <- readRDS(file.path(data_dir, "syllabi_merged.rds"))
say("Loaded {format(nrow(syllabi_merged), big.mark = ',')} rows")

ds <- syllabi_merged |>
    mutate(
        team_size = nchar(as.character(team)),
        region = dplyr::case_when(
            country %in% c(
                "DK", "DE", "AT", "BE",
                "FR", "IT", "NL", "ES", "PT",
                "PL", "ES", "IE"
            ) ~ "European Union",
            country == "CA" ~ "Canada",
            country == "US" ~ "United States",
            country == "GB" ~ "Great Britain",
            TRUE ~ "Other",
        )
    )

ds_annual <- ds |>
    count(year, team, team_size) |>
    group_by(year, team_size) |>
    mutate(
        N = sum(n),
        p = n / N,
        sex = case_when(
            team %in% c("f", "ff") ~ "Female-only",
            team %in% c("m", "mm") ~ "Male-only",
            team %in% c("fm", "mf") ~ "Mixed",
            TRUE ~ NA_character_
        )
    )

ds_long <- ds |>
    rename(Observed = team_ordered,  Simulated = team_rand) |>
    pivot_longer(
        cols = c(Simulated, Observed),
        names_to = "formation",
        values_to = "sex"
    )


# ---------------------------------------------
# Figure 1 -- Annual trends
# ---------------------------------------------
say("Analysis of syllabi by year, team and team size")

p1 <- ds_annual |>
    ggplot(aes(year, p, color = sex, shape = sex)) + 
    facet_grid(~ team_size, labeller = labeller(team_size = \(x) paste(x, "instructor(s)"))) +
    scale_y_continuous(labels = \(x) 100 * x) +
    scale_color_discrete() +
    scale_shape_discrete() +
    geom_line() +
    geom_point(size = 2.5) +
    labs(
        x = "Academic year",
        y = "Courses within team size (%)"
    )


for (ext in formats) {
    out <- file.path(out_dir, paste0("01_annual_trends.", ext))

    ggsave(
        out,
        width = 4.2,
        height = 2.5,
        units = "in"
    )

    say("Figure saved to {out}.")
}

# ---------------------------------------------
# Figure 2 -- Observed vs simulated
# ---------------------------------------------
say("Compare simulated vs observed")


p2 <- ds_long |>
    count(formation, sex, year, team_size) |>
    group_by(year, team_size) |> 
    mutate(
        N = sum(n),
        p = n / N
    ) |>
    ggplot(aes(year, p, color = formation, shape = formation)) +
    facet_wrap(
        sex ~ ., 
        scales = "free",
        labeller = labeller(sex = team_labels), 
    ) +
    scale_y_continuous(labels = \(x) 100 * x) +
    scale_color_discrete() +
    scale_shape_discrete() +
    geom_line() +
    geom_point(size = 2.5) +
    labs(
        x = "Academic year",
        y = "Courses within team size (%)"
    )

out <- ggsave(
    file.path(out_dir, "02_obs_vs_simul.pdf"),
    device = cairo_pdf,
    width = 7,
    height = 3.8,
    units = "in"
)
say("Figure saved to {out}.")


# ---------------------------------------------
# Figure 3 -- Observed vs simulated, by field
# ---------------------------------------------
say("Compare simulated vs observed, by academic field")

ds_filtered <- ds_long |>
    mutate (mixed = sex %in% c("mf", "fm")) |>
    count(formation, mixed, field, isced, stem, team_size) |>
    group_by(field, team_size) |> 
    mutate(
        N = sum(n),
        p = n / N,
        se = sqrt(p * (1-p) / n),
        conf.low = p - 1.96 * se,
        conf.high = p + 1.96 * se,
    ) |>
    filter(team_size == 2, mixed) 

say("Filtered long-data mixed-teams: {nrow(ds_filtered)} rows.")


p_base <- ds_filtered |>
    ggplot(aes(p, reorder(field, p * (formation == "Observed") ), xmin = conf.low, xmax = conf.high, color = formation, shape = formation)) + 
    ggforce::facet_col(
        ~ stringr::str_wrap(isced, 30),
        space = "free",
        scales = "free_y",
        strip.position = "top"
    ) +
    scale_color_discrete() +
    scale_shape_discrete() +
    scale_x_continuous(labels = \(x) 100 * x) +
    geom_pointrange() +
    labs(
        x = "Mixed-gender courses within two instructors (%)",
        y = NULL
    )

regex <- "Arts|Business|Engineer|Educ|Agr"
p_left <- p_base + filter(ds_filtered, grepl(regex, isced))
p_right <- p_base + filter(ds_filtered, !grepl(regex, isced))
p_combined <- (p_left + p_right) +
    plot_layout(guides = "collect", axis_titles = "collect")


out <- ggsave(
    file.path(out_dir, "03_by_field.pdf"),
    device = cairo_pdf,
    width = 7.5,
    height = 9,
    units = "in"
)
say("Figure saved to {out}.")


# ---------------------------------------------
# Figure 4 -- Observed vs simulated, by region
# ---------------------------------------------
say("Compare simulated vs observed, by region")

ds_filtered <- ds_long |>
    mutate (mixed = sex %in% c("mf", "fm")) |>
    count(formation, mixed, year, region, team_size) |>
    group_by(year, region, team_size) |> 
    mutate(
        N = sum(n),
        p = n / N,
        se = sqrt(p * (1 - p) / n),
        conf.low = p - 1.96 * se,
        conf.high = p + 1.96 * se,
    ) |>
    filter(mixed)

p3 <- ds_filtered |>
    ggplot(aes(year, p, ymin = conf.low, ymax = conf.high, color = formation, shape = formation)) + 
    facet_wrap( ~ region, scales = "free") + 
    scale_color_discrete() +
    scale_shape_discrete() +
    scale_y_continuous(labels = \(x) 100 * x, limits = c(0, NA)) + 
    geom_pointrange() +
    labs(
        y = "Mixed teams within 2-instructors (%)",
        x = "Academic year",
    )

out <- file.path(out_dir, "04_by_country.pdf")

ggsave(
    out,
    device = cairo_pdf,
    width = 7.5,
    height = 5,
    units = "in"
)

say("Figure saved to {out}.")


# ======================================================================
# 3. SUPPLEMENTARY TABLEs
# ======================================================================
ds_count_wide <- ds_long |> 
    count(sex, formation) |>
    pivot_wider(names_from = formation, values_from = n) |>
    group_by(size = nchar(sex)) |>
    mutate(
        size = paste(size, "instructor(s)"), 
        observed_pc = 100 * Observed / sum(Observed), 
        simulated_pc = 100 * Simulated/ sum(Simulated),
        diff = observed_pc - simulated_pc
    )

out <- file.path(out_dir, "supp_teams_count_wide.csv")
write.csv(ds_count_wide, out, row.names = FALSE)
say("Data saved to {out}")


# ---- by field
ds_count_wide <- ds_long |> 
    count(sex, formation, isced) |>
    pivot_wider(names_from = formation, values_from = n) |>
    group_by(size = nchar(sex), isced) |>
    mutate(
        size = paste(size, "instructor(s)"), 
        observed_pc = 100 * Observed / sum(Observed), 
        simulated_pc = 100 * Simulated/ sum(Simulated),
        diff = observed_pc - simulated_pc
    )

out <- file.path(out_dir, "supp_teams_count_field_wide.csv")
write.csv(ds_count_wide, out, row.names = FALSE)
say("Data saved to {out}")