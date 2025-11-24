# - [ ] by country, by field
# - [ ] Atypicality (why? is this a good?)
# - [ ] Pooled regression 
# - [ ] log-transformed


# 
require(dplyr)
require(stargazer)
require(lme4)
library(lmerTest)
library(MASS)
library(ggplot2)
#library(glmmTMB)
library(broom.mixed)

code_path <- file.path("data", "processed", "class_code.rds")
data_path <- file.path("data", "processed", "os_final.rds")

# Load data
ds_raw <- readRDS(data_path)
str(ds_raw)


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
