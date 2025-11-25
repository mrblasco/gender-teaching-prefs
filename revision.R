# List of things to do
# - [ ] COVID data team formation
# - [ ] Explain temporal variation
# - [ ] by country, by field
# - [ ] Teaching innovativeness
# - [ ] Atypicality (why? is this a good?)
# - [ ] Pooled regression
# - [ ] log-transformed


# Setup
library(dplyr)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(MASS)
library(stargazer)
library(ggplot2)
library(parallel)
#library(glmmTMB)

code_path <- file.path("data", "processed", "class_code.rds")
data_path <- file.path("data", "processed", "os_final.rds")
novel_path <- file.path("data", "processed", "novel_v2.rds")

outdir <- file.path("data", "results")
dir.create(outdir)

source("R/theme.R")
theme_set(theme_custom())

# ---- pre process ---

ds_novel <- readRDS(novel_path)
str(ds_novel) # new work cited

# ----- data -----

ds_raw <- readRDS(data_path)
str(ds_raw)

# ---- Replicate baseline results  ---- 

# ---- Teaching Novelty ---- 

ds <- ds_raw %>%
    left_join(ds_novel) %>%
    dplyr::filter(nchar(team) < 3, year > 1998, !is.na(novel_count)) %>% 
    dplyr::mutate(
        team = relevel(factor(team), ref = "m"),
    ) %>% 
    dplyr::mutate(
        .by = year,
        novel_med_pc_year = 100 * (rank(novel_med) - 1)  / (length(novel_med) - 1)
    )
message(sprintf("Loaded n = %.2fM", nrow(ds)/1e6))

summary(ds)

system.time(
    fit0 <- glm(
        novel_med_pc_year ~ team + course_level + country + (1|field) + (1|institution) + offset(tot_count),
        data = ds,
    )
)
summary(fit0)

years <- sort(unique(ds$year))
fit_list <- list()
for (y in years) {
    init <- Sys.time()
    fit_list[[as.character(y)]] <- lmer(
        novel_med_pc_year ~ team + course_level + country + (1|field) + (1|institution),
        data = ds,
        subset = year == y
    )
    end <- Sys.time()
    message(sprintf("Year = %i fitted in %.2f seconds", y, end - init))
}

coeffs <- fit_list %>% 
    lapply(broom::tidy, conf.int = TRUE) %>% 
    dplyr::bind_rows(.id = "year")

p <- coeffs %>%
    dplyr::filter(grepl("team", term)) %>%
    dplyr::mutate(year = as.Date(year, '%Y')) %>%
    ggplot(
        aes(
            x = year,
            y = estimate,
            ymin = conf.low,
            ymax = conf.high,
            color = term
        )
    ) + 
    facet_grid(~term) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbar(width = .1) +
    geom_point()

p


# ---- Recency  ---- 

ds <- ds_raw %>%
    dplyr::filter(nchar(team) < 3, year > 1998, !is.na(novelty)) %>% 
    dplyr::mutate(
        team = relevel(factor(team), ref = "m"),
        recency = year - novelty,
        recency_pc = 100 * (rank(recency) - 1 ) / (length(recency) - 1),
        .by = year
    )

# pooled regression
system.time({
    fit <- lmer(
        recency_pc ~ team + course_level + country + year + (1|field) + (1|institution),
        data = ds
    )
})
message(sprintf("Saving to %s", "..."))
saveRDS(fit, file.path(outdir, "fit_lmer_recency_pc_by_team_course_country_year.rds"))

summary(fit)

coeffs <- tidy(fit, conf.int = TRUE)

coeffs %>% 
    filter(grepl("team", term)) %>%
    dplyr::select(
        Variable = term,
        Estimate = estimate,
        SE = std.error,
        Low = conf.low,
        High = conf.high
    ) %>%
    knitr::kable(
        caption = "Regression coefficients and 95% confidence intervals",
        digits = 2
    )

years <- sort(unique(ds$year))
fit_list <- list()
for (y in years) {
    init <- Sys.time()
    fit_list[[as.character(y)]] <- update(fit, ~ . - year, data = ds, subset = year == y)
    end <- Sys.time()
    message(sprintf("Year = %i fitted in %.2f seconds", y, end - init))
}


fit_list <- parallel::mclapply(
  years,
  function(y) {
    init <- Sys.time()
    f <- update(fit, ~ . - year, data = ds, subset = year == y)
    end <- Sys.time()
    message(sprintf("Year = %i fitted in %.2f seconds", y, end - init))
    f
  },
  mc.cores = parallel::detectCores() - 2
)

coeffs <- fit_list %>% 
    lapply(broom::tidy, conf.int = TRUE) %>% 
    dplyr::bind_rows(.id = "year")

saveRDS(coeffs, file.path(outdir, "lmer_coeffs_recency_pc_by_year.rds"))

plot_recency_pc <- coeffs %>%
    dplyr::filter(grepl("team", term)) %>%
    dplyr::mutate(year = as.Date(year, '%Y')) %>%
    ggplot(
        aes(
            x = year,
            y = estimate,
            ymin = conf.low,
            ymax = conf.high,
            color = term
        )
    ) + 
    facet_grid(~term) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbar(width = .1) +
    geom_point()

plot_recency_pc + 
    labs(
        x = "Year",
        y = "Difference in Age of readings\n(Percentile rank)"
    )

# Testing temporal patterns
coeffs %>%
    dplyr::filter(grepl("team", term)) %>%
    dplyr::mutate(year = as.Date(year, '%Y')) %>% 
    ggplot(
        aes(x = year, y = estimate, color = term),
    ) + 
    facet_grid(~term) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_smooth(
        method = "lm",
        formula = y ~ 1,
        aes(weight = 1 / (std.error^2)),
        linetype = "dashed",
        color = "black"
    ) +
    geom_smooth(
        method = "gam",
        formula = y ~ s(x, k = 6),
        mapping = aes(weight = 1/(std.error^2)), 
        color = "blue",
        se = TRUE
    )


# ----- OLD NOTES 

# Difference between course with atyp
ds <- mutate(ds_raw, has_atyp = !is.na(atyp_med)) %>% 
    filter(country == "US")
nrow(ds)


fit <- lmer(has_atyp ~ course_level + (1|field), data = ds)
summary(fit)

tidy(fit, conf.int = TRUE) %>% dplyr::select(term, estimate, conf.low, conf.high)
 
ds <- ds_raw %>% 
    filter(!is.na(atyp_med), !is.na(year), country == "US") %>% 
    mutate(
        matched_docs_pc = 100 * matched_docs / uniq_docs,
        team = relevel(factor(team), ref = "m"),
        year_ord = factor(year, ordered = TRUE),
        atyp_med = ifelse(atyp_med == Inf, NA, atyp_med),
        atyp_med_std = as.numeric(scale(atyp_med)),
        course_level = as.character(course_level) %>% 
            ifelse(is.na(.), "unknown", .) %>% 
            factor(),
    ) %>% 
    mutate(
        atyp_med_std_yr = as.numeric(scale(atyp_med)),
        atyp_med_rnk_yr = 100 * (rank(atyp_med) - 1) / (length(atyp_med) - 1),
        .by = year
    )

# --- decsriptives 

calc_stats <- function(x) {
  x <- na.omit(x)  # remove missing values
  data.frame(
    mean = mean(x),
    median = median(x),
    q05 = quantile(x, 0.05),
    q95 = quantile(x, 0.95),
    min = min(x),
    max = max(x),
    n = length(x),
    row.names = NULL
  )
}

ds_summ_stats <- ds %>% 
    dplyr::select(-id, -institution) %>%
    dplyr::select(dplyr::where(is.numeric)) %>% 
    lapply(calc_stats) %>% 
    dplyr::bind_rows(.id = "variable") 


ds_summ_stats %>% 
    dplyr::select(variable, mean, q05, median, q95, min, max) %>%
    knitr::kable(
        digits = 1,
        align = "lrrrrrrr",
        caption = "Summary statistics for numeric variables"
    )



# --- Fit atypicality using OLS 

fit0 <- lm(atyp_med_std_yr ~ team, data = ds, weights = atyp_n)

fit_no_weight <- lm(atyp_med_std_yr ~ team + course_level + country + year_ord, data = ds)
fit_weight_sqrt <- update(fit_no_weight, weights = sqrt(atyp_n))
fit_weight_n <- update(fit_no_weight, weights = atyp_n)
stargazer(
    fit_no_weight, fit_weight_sqrt, fit_weight_n,
    type = "text",
    keep = "team"
)


fit1 <- update(fit0, ~ . + course_level + country + year_ord)
summary(fit1)

fit0_rnk <- update(fit0, atyp_med_rnk_yr ~ .)
summary(fit0_rnk) # also not normal residuals

fit1_rnk <- update(fit1, atyp_med_rnk_yr ~ .)
summary(fit1_rnk)

qqnorm(residuals(fit1_rnk), pch = ".")
qqline(residuals(fit1_rnk), pch = ".")

models <- list(fit0, fit1, fit0_rnk, fit1_rnk)
stargazer(
    models, 
    keep = "team", digits = 1, type = "text", keep.stat = "n", 
    apply.coef = \(x) 100*x, apply.se = \(x) 100 * x
)

# Robust regression

fit <- MASS::rlm(
    atyp_med_rnk_yr ~ team + course_level + country + year_ord, 
    data = ds,
    weights = atyp_n
)
summary(fit)

# --- Random effects weighting 


# ---- Weights 

fit_no_weight <- lmer(
    atyp_med_rnk_yr ~ team + course_level + country + year_ord + (1|institution) + (1|field), 
    data = ds
)

qqnorm(resid(fit_no_weight))
qqline(resid(fit_no_weight))

fit_weight <- update(fit_no_weight, weights = atyp_n)

fit_weight_sqrt <- update(fit_no_weight, weights = sqrt(atyp_n))

models <- list(no_weight = fit_no_weight, weight = fit_weight, weight_sqrt = fit_weight_sqrt)
coeffs <- lapply(models, tidy, conf.int = TRUE) %>% bind_rows(.id = "weighting")

coeffs %>% 
    filter(grepl("team", term)) %>%
    mutate(
        term = recode(term,
                    "teamf" = "Female",
                    "teamff" = "Female & Female",
                    "teammm" = "Male & Male",
                    "teamfm"  = "Male & Female")   # Male is omitted baseline
    ) %>%
    ggplot(
        aes(
            x = estimate,
            y = term,
            xmin = conf.low, xmax = conf.high, 
            color = weighting, shape = weighting
        )
    ) + 
    geom_vline(xintercept = 0, linetype = 2) +
    geom_errorbar(width = 0.2) +
    geom_point(size = 3) + 
    theme_classic(base_size = 14) +
    theme(
        legend.position = "bottom", 
        panel.spacing = unit(1.25, "lines"),
        strip.background = element_rect(fill = "grey95", color = NA),
        strip.text = element_text(size = 14, face = "bold")
    ) +
    labs(
        x = "Year",
        y = "Coefficient Estimate",
        title = "Estimated Effects for Team Composition on Atypicality (% rank per year)",
        subtitle = "Baseline category (omitted): Male"
    )

lattice::dotplot(ranef(fit))


# ---- by year 

years <- sort(unique(ds$year))
coef_list <- list()
for (j in years) {
    message(sprintf("Fitting %i", j))
    fit <- update(fit_weight, ~ . + novelty + log(atyp_n) - year_ord, data = ds, subset = year == j)
    coef_list[[as.character(j)]] <- tidy(fit, conf.int = TRUE)
}

# --- viz

obs <- nrow(ds)

coef_list %>%
    dplyr::bind_rows(.id = "year") %>%
    filter(grepl("team", term)) %>%
    mutate(
        year = as.Date(year, "%Y"),
        term = recode(term,
                    "teamf" = "Female",
                    "teamff" = "Female & Female",
                    "teammm" = "Male & Male",
                    "teamfm"  = "Male & Female")   # Male is omitted baseline
    ) %>%
    ggplot() +
    aes(
        x = year,
        y = estimate,
        ymin = conf.low, ymax = conf.high,
        color = term,
        #alpha = abs(estimate) > 1.68 * std.error,
    ) + 
    geom_hline(yintercept = 0, linetype = 2) +
    geom_errorbar(width = .1) +
    geom_point(size = 3) + 
    facet_grid(~term, scales = "free")+
    theme_classic(base_size = 14) +
    theme(
        legend.position = "none", 
        panel.spacing = unit(2.25, "lines"),
        strip.background = element_rect(fill = "grey95", color = 'gray50'),
        strip.text = element_text(size = 14, face = "bold")
    ) +
    labs(
        x = "Year",
        y = "Coefficient Estimate",
        title = "Estimated Effects for Team Composition on Atypicality (% rank per year)",
        subtitle = "Baseline category (omitted): Male"
    )
