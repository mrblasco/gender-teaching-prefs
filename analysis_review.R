# ---- setup, include = FALSE

library(dplyr)
library(ggplot2)
library(lme4)
library(broom.mixed)

source("R/theme.R")
theme_set(theme_custom())

knitr::opts_chunk$set(error = TRUE)

# ---- data, 

input <- file.path("data", "processed", "os_final.rds")
ds <- readRDS(input) %>%
    filter(
        atyp_avg != Inf,
        !is.na(atyp_avg)
    ) %>%
    mutate(
        atyp_med_sd = as.numeric(scale(atyp_med)),
        atyp_med_rnk = 100 * (rank(atyp_med) - 1) / (n() - 1),
        atyp_avg_rnk = 100 * (rank(atyp_avg) - 1) / (n() - 1),
        .by = year
    )

# ---- descriptive

summary(ds)

# ---- fit atyp

model <- atyp_med_sd ~ team + log(atyp_n) + novelty + country +
    (1 | year) + (1| field) + (1| institution)

fit <- lmer(model, data = ds)
summary(fit)

fit_rnk <- update(fit, atyp_med_rnk ~ .)
summary(fit_rnk)

fit_avg_rnk <- update(fit, atyp_avg_rnk ~ .)
summary(fit_avg_rnk)

# ---- by year

fit_by_year <- list()
for (y in seq(2000, 2019)) {
    ds_year <- filter(ds, year == y)
    fit_by_year[[as.character(y)]] <- update(fit, atyp_med_rnk ~ . - (1 | year), data = ds_year)
}

ds_coeffs <- fit_by_year %>% 
    lapply(tidy, effects = "fixed", conf.int = TRUE) %>% 
    bind_rows(.id = "year")

ds_coeffs %>%
    filter(grepl("team", term)) %>%
    ggplot(
        aes(
            y = estimate,
            ymin = conf.low,
            ymax = conf.high,
            x = year,
            color = term
        )
    ) + 
    geom_hline(yintercept = 0) + 
    facet_wrap(~term, scales = "free") + 
    geom_errorbar(width = 0.1) +
    geom_point()

# --- chekc

model <- novelty ~ team +
    (1 | year) + (1| field) + (1| institution) + (1 | country)

fit_novelty <- lmer(model, ds)
summary(fit_novelty)