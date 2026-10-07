# ----------------------------------------------------------------------
# Setup
# ----------------------------------------------------------------------
suppressMessages({
    library(dplyr, warn.conflicts = FALSE)
    library(tidyr)
    library(lme4)
    library(broom.mixed)
    library(stargazer)
    library(ggplot2)
    library(ggrepel)
})

log_msg <- function(format, x, ...) {
    message(sprintf(format, x, ...))
}

say <- logger::log_info


source("R/isced.R")
source("R/theme.R")
theme_set(theme_custom())

team_size_cutoff <- 3
covid_start <- 2020
covid_end <- 2021

# ----------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

data_dir <- file.path("data", "processed")
out_dir <- ifelse(length(args) >= 1, args[1], tempdir())

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

dir_v12 <- here::here("data", "interim", "v12")
data_path <- here::here("data", "processed", "montecarlo.rds")
inst_path <- file.path(dir_v12, "institutions.json")
data_sex_path <- file.path(dir_v12, "instructor_genders.csv")

team_labels <- c(
    "f" = "Female-only",
    "ff" = "Female-only",
    "m" = "Male-only",
    "mm" = "Male-only",
    "fm" = "Mixed-gender"
)

type_labels <- c(
    actual = "Observed",
    med = "Simulated (gener-neutral)"
)



# ----------------------------------------------------------------------
# Load data
# ----------------------------------------------------------------------

ds <- readRDS(data_path)

inst <- jsonlite::stream_in(file(inst_path))


sex <- read.csv(
    data_sex_path,
    header = FALSE,
    col.names = c("id", "inst_id", "year", "field", "team")
)

say(sprintf("Syllabi with gender information = %2.1f M", nrow(sex) / 1e6))

# ----------------------------------------------------------------------
# Process data
# ----------------------------------------------------------------------

sex_filtered <- sex |>
    mutate(field = factor(as.character(field))) |>
    filter(
        nchar(team) < team_size_cutoff,
        !grepl("u", team)
    )

say("Covid dataset: {nrow(sex_filtered)} rows")


sex_count <- sex_filtered |>
    count(inst_id, team, year) |>
    mutate(
        time = year - min(year),
        covid = as.integer(year >= covid_start),
        time_after_covid = pmax(0, year - covid_start),
        post_covid = ifelse(year >= covid_start, 1, 0)
    )


sex_count_wide <- sex_count %>%
    tidyr::pivot_wider(
        names_from = team,
        values_from = n,
        values_fill = 0
    )


sex_count_field <- sex_filtered %>%
    count(team, field, year) %>%
    left_join(isced_lookup, by = "field")

sex_count_field_wide <- sex_count_field %>%
    tidyr::pivot_wider(
        names_from = team,
        values_from = n,
        values_fill = 0
    ) %>%
    mutate(
        post_covid = ifelse(year >= covid_start, 1, 0)
    )


# ----------------------------------------------------------------------
# 1. Did team composition change after COVID?
# ----------------------------------------------------------------------

models <- list(
    "Single vs. Team" = 
        cbind(f + m, fm + ff + mm) ~ post_covid + (1 | inst_id),
    "Mixed vs. Same Gender" =
        cbind(fm, fm + ff + mm) ~ post_covid + (1 | inst_id),
    "Female vs. Male" =
        cbind(ff + f, m + fm + mm) ~ post_covid + (1 | inst_id)
)

fits <- purrr::map(
    models,
    ~ glmer(
        formula = .x,
        data = sex_count_wide,
        family = binomial
    )
)


modelsummary::modelsummary(
    fits, stars = TRUE, exponentiate = TRUE
)


# ----------------------------------------------------------------------
# Plot trends
# ----------------------------------------------------------------------

p_trend <- sex_count |>
    ggplot(aes(year - covid_start, n, group = as.factor(post_covid))) +
    scale_y_log10() +
    facet_grid(~team) +
    geom_smooth(method = "lm", formula = "y ~ x") +
    labs(
        x = "Time before/after COVID",
        y = "Teams"
    )

out <- file.path(out_dir, "02_trends.pdf")

ggsave(
    out,
    device = cairo_pdf, 
    unit = "in", width = 7, height = 2.4
)

say("Figure saved to {out}")

# ----------------------------------------------------------------------
# Plot sex count by field
# ----------------------------------------------------------------------

sex_count_isced <- sex_count_field %>%
    group_by(team, year, field, isced) %>%
    summarise(n = mean(n)) %>%
    tidyr::pivot_wider(names_from = team, values_from = n)

p_covid_trend <- sex_count_isced %>%
    ggplot(
        aes(x = year, y = 1 + f + ff + fm + m + mm)
    ) +
    annotate(
        "rect",
        xmin = covid_start, xmax = covid_end,
        ymin = 0, ymax = Inf,
        alpha = 0.15,
        fill = "grey70"
    ) +
    scale_y_log10() +
    facet_wrap(
        ~isced,
        scales = "free",
        labeller = labeller(
            isced = \(x) stringr::str_wrap(x, 10)
        )
    ) +
    geom_line(color = "gray75", aes(group = field), linewidth = 0.25) +
    geom_smooth(se = FALSE) +
    theme(panel.grid = element_blank()) +
    labs(
        y = "Total courses"
    )

saveRDS(p_covid_trend, file = file.path(out_dir, "plot_covid_trends.rds"))

out <- ggsave(
    file.path(out_dir, "01_covid_trends.pdf"),
    device = cairo_pdf,
    units = "in"
)
say("Figure saved to {out}.")


# ----------------------------------------------------------------------
# Binomial Regression
# ----------------------------------------------------------------------

fit_f <- glm(
    formula = cbind(f, m) ~ post_covid,
    data = sex_count_wide,
    family = binomial
)

fit_fm <- glm(
    formula = cbind(fm, mm + ff) ~ post_covid,
    data = sex_count_wide,
    family = binomial
)

fit_size <- glm(
    formula = cbind(f + m, fm + mm + ff) ~ post_covid,
    data = sex_count_wide,
    family = binomial
)

# Interrupted time series
fit_fm_its <- glm(
    cbind(fm, ff + mm) ~ time + covid + time_after_covid,
    data = sex_count_wide,
    family = binomial
)

fit_size_its <- glm(
    cbind(f + m, fm + ff + mm) ~ time + covid + time_after_covid,
    data = sex_count_wide,
    family = binomial
)

fit_fm_its <- glm(
    cbind(fm, ff + mm) ~ time + covid + time_after_covid,
    data = sex_count_wide,
    family = binomial
)

# ----------------------------------------------------------------------
# Regression resuts table
# ----------------------------------------------------------------------

models <- list(
    "Single vs Team" = fit_size,
    "Women vs Men" = fit_f,
    "Mixed-gender vs Same-gender" = fit_fm,
    "ITS (1)" = fit_fm_its,
    "ITS (2)" = fit_size_its,
    "ITS (3)" = fit_fm_its
)

stargazer(models, type = "text", dep.var.labels = names(models))

100 * (exp(coef(fit_size)[2]) - 1) # single teams drop by 27%
100 * (exp(coef(fit_f)[2]) - 1) # women vs men | single drop by 1%
100 * (exp(coef(fit_fm)[2]) - 1) # drop by 7%


# ----------------------------------------------------------------------
# Coefficient plot
# ----------------------------------------------------------------------

p_coeff <- lapply(models, broom::tidy, conf.int = TRUE) %>%
    bind_rows(.id = "depvar") %>%
    filter(grepl("covid", term)) %>%
    ggplot(
        aes(
            x = estimate, y = depvar,
            xmin = conf.low, xmax = conf.high,
        )
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_errorbar(width = 0.1) +
    geom_point() +
    scale_x_continuous(label = \(x) sprintf("%2.0f%%", 100 * (exp(x) - 1))) +
    labs(
        y = NULL,
        x = "Post-covid Difference"
    )

out <- ggsave(
    file.path(out_dir, "01_covid_coeffs.pdf"),
    device = cairo_pdf,
    width = 4.2,
    height = 1.5,
    units = "in"
)
say("Figure saved to {out}.")

# ----------------------------------------------------------------------
# Plot data montecarlo simulations
# ----------------------------------------------------------------------

plot_data <- ds %>%
    dplyr::select(year, value, team, name) %>%
    mutate(
        size = ifelse(nchar(team) == 1, "One instructor", "Two instructors"),
        team_label = team_labels[team],
        name = type_labels[name],
    ) %>%
    summarise(value = sum(value), .by = c(name, team_label, year)) %>%
    mutate(
        percent = value / sum(value),
        .by = c(name, year)
    )


p_sim <- plot_data %>%
    ggplot() +
    aes(
        x = year,
        y = percent,
        linetype = name,
        color = name,
        group = name
    ) +
    annotate(
        "rect",
        xmin = covid_start, xmax = covid_end,
        ymin = 0, ymax = Inf,
        alpha = 0.15, fill = "grey70"
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(labels = scales::percent) +
    facet_wrap(~team_label, scales = "free") +
    geom_line(size = .5) +
    geom_text_repel(
        size = 3,
        direction = "y",
        aes(label = ifelse(year == 2022, sprintf("%2.0f%%", 100 * percent), ""))
    ) +
    labs(
        x = "Year",
        y = "Syllabi per year",
        linetype = "Type",
        shape = "Type",
        color = "Type",
        title = "Trend Over Time",
        subtitle = paste0("COVID period highlighted (", covid_start, "-", covid_end, ")")
    ) +
    theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        # panel.grid.major.y = element_line(linetype = "dotted"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold")
    )

out <- ggsave(
    file.path(out_dir, "01_covid_sims.pdf"),
    device = cairo_pdf,
    width = 7,
    height = 3.4,
    units = "in"
)
say("Figure saved to {out}.")



# ----------------------------------------------------------------------
# Binomial Regression by field
# ----------------------------------------------------------------------

fit_f <- glmer(cbind(f, m) ~ post_covid + (post_covid | field), sex_count_field_wide, family = binomial)
fit_fm <- glmer(cbind(fm, mm + ff) ~ post_covid + (post_covid | field), sex_count_field_wide, family = binomial)
fit_size <- glm(cbind(f + m, fm + mm + ff) ~ post_covid, sex_count_field_wide, family = binomial)

sex_count_field_wide %>%
    mutate(
        pred = predict(fit_fm, type = "response")
    ) %>%
    filter(fm > 100, .by = field) %>%
    summarise(
        pred_prob = mean(pred), .by = c(post_covid, field)
    ) %>%
    tidyr::pivot_wider(
        values_from = pred_prob, names_from = post_covid
    ) %>%
    ggplot(
        aes(
            x = `0`, y = `1`, label = field
        )
    ) +
    geom_abline(intercept = 0, slope = 1) +
    geom_text_repel() +
    geom_point() +
    labs(
        x = "Pre-covid", y = "Post-covid"
    )

out <- ggsave(
    file.path(out_dir, "01_covid_pre_post.pdf"),
    device = cairo_pdf,
    units = "in"
)

say("Figure saved to {out}.")
