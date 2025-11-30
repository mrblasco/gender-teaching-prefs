# Libraries
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(lme4)
library(broom.mixed)
library(stargazer)
library(ggplot2)
library(ggrepel)

log_msg <- function(format, x, ...) {
    message(sprintf(format, x, ...))
}

source("R/theme.R")
theme_set(theme_custom())

covid_start <- 2020
covid_end   <- 2021

team_labels <- c(
    "f" = "Female-only",
    "ff" = "Female-only",
    "m" = "Male-only",
    "mm" = "Male-only",
    "fm" = "Mixed-gender"
)

type_labels <- c(actual = "Observed", med = "Simulated (gener-neutral)")


# ----------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------

dir_v12 <- file.path("data", "interim", "v12")
results_dir <- file.path("data", "results")
dir.create(results_dir)


data_path <- file.path("data", "processed", "montecarlo.rds")
inst_path <- file.path(dir_v12, "institutions.json") 
data_sex_path <- file.path(dir_v12, "instructor_genders.csv")

# ----------------------------------------------------------------------
# Load data
# ----------------------------------------------------------------------

ds <- readRDS(data_path)
inst <- jsonlite::stream_in(file(inst_path))
sex <- read.csv(data_sex_path, header = FALSE, col.names = c("id", "inst_id", "year", "field", "team"))

log_msg("Syllabi with gender information = %2.1f M", nrow(sex)/ 1e6)

# ----------------------------------------------------------------------
# Process data
# ----------------------------------------------------------------------

sex <- sex %>% 
    mutate(field = factor(as.character(field)))

ds_actual <- dplyr::filter(ds, name == "actual") %>% 
    dplyr::select(year, team, value)


# ----------------------------------------------------------------------
# Gender patterns
# ----------------------------------------------------------------------

sex_count <- sex %>%
    filter(nchar(team) < 3, !grepl("u", team)) %>%
    count(inst_id, team, year)

sex_count_wide <- sex_count %>%
    tidyr::pivot_wider(names_from = team, values_from = n, values_fill = 0) %>% 
    mutate(post_covid = ifelse(year >= covid_start, 1, 0))

sex_count_field <- sex %>%
    filter(nchar(team) < 3, !grepl("u", team)) %>%
    count(team, field, year)

sex_count_field_wide <- sex_count_field %>%
    tidyr::pivot_wider(
        names_from = team,
        values_from = n,
        values_fill = 0
    ) %>% 
    mutate(post_covid = ifelse(year >= covid_start, 1, 0))

# ----------------------------------------------------------------------
# Plot sex count by field
# ----------------------------------------------------------------------
source("R/isced.R")

sex_count_isced <- sex_count_field %>% 
    left_join(isced_lookup, by = "field") %>% 
    summarise(
        n = mean(n), .by = c(team, year, field, isced)
    ) %>% 
    tidyr::pivot_wider(names_from = team, values_from = n) 

p_covid_trend <- sex_count_isced %>% 
    ggplot(
        aes(
            x = year,
            y = 1 + f + ff + fm + m + mm
        )
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

saveRDS(p_covid_trend, file = file.path(results_dir, "plot_covid_trends.rds"))

# ----------------------------------------------------------------------
# Binomial Regression
# ----------------------------------------------------------------------

fit_f <- glm(cbind(f, m) ~ post_covid, sex_count_wide, family = binomial)
fit_fm <- glm(cbind(fm, mm + ff) ~ post_covid, sex_count_wide, family = binomial)
fit_size <- glm(cbind(f + m, fm + mm + ff) ~ post_covid, sex_count_wide, family = binomial)

# ----------------------------------------------------------------------
# Regression resuts table
# ----------------------------------------------------------------------

models <- list("Single vs Team" = fit_size, "Women vs Men" = fit_f, "Mixed-gender vs Same-gender" = fit_fm)
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
    facet_wrap(~term) +
    geom_errorbar(width = 0.1) +
    geom_point() + 
    scale_x_continuous(label = \(x) sprintf("%2.0f%%", 100 * (exp(x) - 1)))

p_coeff

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

    facet_wrap(~ team_label, scales = "free") +
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
        #panel.grid.major.y = element_line(linetype = "dotted"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold")
    )

p_sim


# ----------------------------------------------------------------------
# Binomial Regression by field
# ----------------------------------------------------------------------

fit_f <- glmer(cbind(f, m) ~ post_covid + (post_covid|field), sex_count_field_wide, family = binomial)
fit_fm <- glmer(cbind(fm, mm + ff) ~ post_covid+ (post_covid|field), sex_count_field_wide, family = binomial)
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
    geom_text_repel()+
    geom_point() + 
    labs(
        x = "Pre-covid", y = "Post-covid"
    )


# ----------------------------------------------------------------------
# Save 
# ----------------------------------------------------------------------

results <- list(
    "coeffs" = p_coeff,
    "simulations" = p_sim
)
saveRDS(results, file.path(results_dir, "fig_covid.rds"))
