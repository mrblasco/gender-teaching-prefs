# ---- setup, include = FALSE
library(yaml)
library(patchwork)
library(ggplot2)
library(dplyr)
library(lme4)
library(broom)
library(broom.mixed)

config <- yaml.load_file("config.yml")
data_dir <- config$data_dir
fig_dir <- file.path(getwd(), config$fig_dir) # full path

knitr::opts_chunk$set(
    echo = FALSE,
    messages = FALSE,
    fig.cap = "TODO: add caption here",
    fig.path = fig_dir,
    cache.path = "cache/fig2/",
    dev = c("png", "pdf"),
    dpi = 600
)

source("R/utils.R")
source("R/isced.R")
source("R/fitting.R")

# ----- theme --------------------------

source("R/theme.R")

theme_set(theme_custom())
theme_update(
    strip.text.y = element_blank(),
    legend.position = "none",
)


# ----- functions -----------------------

stem_fields <- c(
    "Astronomy", "Atmospheric Sciences",
    "Biology", "Chemistry", "Computer Science",
    "Earth Sciences", "Engineering",
    "Engineering Technician", "Mathematics",
    "Natural Resource Management", "Physics",
    "Veterinary Medicine"
)

annotate_stem <- function(fields) {
    dplyr::case_when(
        fields %in% stem_fields ~ "STEM",
        is.na(fields) ~ NA,
        TRUE ~ "Other"
    )
}

# ---- constants -----

year_cutoff <- 1999

# ---- data, include = FALSE, cache = TRUE

filename_data <- file.path(data_dir, "rds/final.rds")

ds <- readRDS(filename_data) %>%
    filter(nchar(team) < 3, year > 1999) %>% 
    mutate(
        novelty_pctl = percent_rank(year - novelty),
        intdisc_pr = percent_rank(mean_intdisc),
        total_authors = female_authors + male_authors,
        female_ratio = (female_authors + 1) / (male_authors + female_authors + 2),
        stem = annotate_stem(field),
        .by = year
    )

# ----- top-rows -------------------------------

kbl_top_rows <- head(ds) %>% 
    knitr::kable(
        caption = "Top Rows of the dataset"
    )

# ---- fit-test ----
source("R/fitting.R")

ds_auth <- filter(ds, total_authors > 0)
dim(ds_auth)

fitted_models <- list()

fit_glm <- fit_and_save_glm(
    file = "glm_qp_female_auth_team_offset.rds",
    formula = female_authors ~ team + offset(log(total_authors)),
    data = ds_auth,
    family = quasipoisson(link = "log"),
    replace = TRUE
)

broom::tidy(fit_glm) %>%
    filter(grepl("team", term)) %>%
    mutate(term = gsub("team", "Team: ", term)) %>% 
    mutate(exp_l1 = sprintf("[%2.1f%%]", 100 * (exp(estimate) - 1)))

fitted_models[[1]] <- fit_glm

fit_glm <- fit_and_save_glm(
    file = "glm_qp_female_auth_team_yr_cntry_offset.rds",
    formula = as.formula(female_authors ~ team + year + country + offset(log(total_authors))),
    data = ds_auth,
    family = quasipoisson(link = "log")
)

broom::tidy(fit_glm) %>%
    filter(grepl("team", term)) %>%
    mutate(term = gsub("team", "Team: ", term)) %>% 
    mutate(exp_l1 = sprintf("[%2.1f%%]",100 * (exp(estimate) - 1)))

fitted_models[[2]] <- fit_glm

# Model 3: year + country + field

fit_glm <- fit_and_save_glm(
    file = "glm_qp_female_auth_team_yr_cntry_field_offset.rds",
    formula = as.formula(female_authors ~ team + year + country + field + offset(log(total_authors))),
    data = ds_auth,
    family = quasipoisson(link = "log")
)


broom::tidy(fit_glm) %>%
    filter(grepl("team", term)) %>%
    mutate(term = gsub("team", "Team: ", term)) %>% 
    mutate(exp_l1 = sprintf("[%2.1f%%]", 100 * (exp(estimate) - 1)))

fitted_models[[3]] <- fit_glm


p_star <- function(x) {
    case_when(
        x < 0.001 ~ "***",
        x < 0.01 ~ "**",
        x < 0.05 ~ "*",
        TRUE ~ ""
    )
}
fitted_models
tbl_coeffs <- lapply(fitted_models, tidy) %>% 
    bind_rows(.id = "model") 

tbl_model_info <- bind_rows(lapply(fitted_models, broom::glance), .id = "model") %>% 
    tidyr::pivot_longer(-model, names_to = "term", values_to = "estimate")

tbl_coeffs_wide <- tbl_coeffs %>%
    bind_rows(tbl_model_info) %>% 
    mutate(
        estimate = sprintf("%.2f%s", estimate, p_star(p.value)),
        std.error = sprintf("(%.2f)", std.error),
        statistic = sprintf("[%.2f]", statistic),
        p.value = sprintf("%.2f", p.value),
    ) %>%
    tidyr::pivot_longer(c(-model, -term)) %>%
    tidyr::pivot_wider(names_from = model, values_fill = "") %>%
    filter(grepl("team|nobs", term)) 


tbl_coeffs_wide %>% 
    filter(name %in% c("estimate", "std.error")) %>% 
    mutate(term = ifelse(name == "estimate", term, "") %>% gsub("team", "Team: ", .)) %>%
    select(-name) %>%
    knitr::kable()


fit <- glmer(
    formula = female_authors ~ team + (1|field),
    data = ds_auth,
    family = poisson(),
    offset = log(ds_auth$total_authors),
    start = start_vals,
    verbose = TRUE
)
summary(fit)

# ----- 

ds_auth <- filter(ds, total_authors > 0)
dim(ds_auth)

fitted_models <- list()

fit_glm <- fit_and_save_glm(
    file = "glm_qp_female_auth_team_offset.rds",
    formula = female_authors ~ team + offset(log(total_authors)),
    data = ds_auth,
    family = quasipoisson(link = "log"),
    replace = TRUE
)

broom::tidy(fit_glm) %>%
    filter(grepl("team", term)) %>%
    mutate(term = gsub("team", "Team: ", term)) %>% 
    mutate(exp_l1 = sprintf("[%2.1f%%]", 100 * (exp(estimate) - 1)))

fitted_models[[1]] <- fit_glm

fit_glm <- fit_and_save_glm(
    file = "glm_qp_female_auth_team_yr_cntry_offset.rds",
    formula = as.formula(female_authors ~ team + year + country + offset(log(total_authors))),
    data = ds_auth,
    family = quasipoisson(link = "log")
)

broom::tidy(fit_glm) %>%
    filter(grepl("team", term)) %>%
    mutate(term = gsub("team", "Team: ", term)) %>% 
    mutate(exp_l1 = sprintf("[%2.1f%%]",100 * (exp(estimate) - 1)))

fitted_models[[2]] <- fit_glm

# Model 3: year + country + field

fit_glm <- fit_and_save_glm(
    file = "glm_qp_female_auth_team_yr_cntry_field_offset.rds",
    formula = as.formula(female_authors ~ team + year + country + field + offset(log(total_authors))),
    data = ds_auth,
    family = quasipoisson(link = "log")
)

broom::tidy(fit_glm) %>%
    filter(grepl("team", term)) %>%
    mutate(term = gsub("team", "Team: ", term)) %>% 
    mutate(exp_l1 = sprintf("[%2.1f%%]", 100 * (exp(estimate) - 1)))

fitted_models[[3]] <- fit_glm

# ... Recency
summary(ds$novelty) # average year of cited references
summary(ds$novelty)

# e.g., 2006 < 2025 - 5 = 2020
lm(novelty < year - 5 ~ 1, ds) # 95% have year of publication 

ds_test <- filter(ds, !is.na(intdisc_pr), year == 2010)
dim(ds_test)

fit <- lmer(
    intdisc_pr ~ team + country + (1 + team | field) + (1 | institution),
    control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE),
    verbose = TRUE,
    data = ds_test
)
summary(fit)



# ---- model, cache = TRUE, eval = FALSE

system.time(
    fit <- ds %>%
        reframe(
            bind_rows(
                age_of_readings = {
                    message("Age, N = ", n())
                    fit_model(
                        novelty_pctl ~ team + country + (1 | year) +
                            (1 | field) + (1 | institution)
                    )
                },
                intdisc = {
                    message("IntDisc, N = ", n())
                    fit_model(
                        intdisc_pr ~ team + country + (1 | year) + 
                            (1 | field) + (1 | institution)
                    )
                },
                female_ratio = {
                    message("Female Ratio, N = ", n())
                    fit_model(
                        female_ratio ~ team + country + (1 | year) +
                            (1 | field) + (1 | institution)
                    )
                },
                .id = "depvar"
            )
        )
)


# ---- model-by-year, cache = TRUE

system.time(
  fit_by_year <- ds %>%
    mutate(team = relevel(factor(team), ref = "m")) %>%
    arrange(year) %>%
    reframe(
      bind_rows(
        age_of_readings = {
          message("Age, N = ", n(), " --> ", unique(year))
          fit_model(
            novelty_pctl ~ team + country + stem +
              (1 | field) + (1 | institution)
          )
        },
        intdisc = {
          message("IntDisc, N = ", n(), " --> ", unique(year))
          fit_model(
            intdisc_pr ~ team + country + stem +
              (1 | field) + (1 | institution)
          )
        },
        female_ratio = {
          message("Female Ratio, N = ", n(), " --> ", unique(year))
          fit_model(
            female_ratio ~ team + country + stem +
              (1 | field) + (1 | institution)
          )
        },
        .id = "depvar"
      ),
      .by = c(year)
    )
)


# ---- model-by-stem, cache = TRUE

system.time(
  fit_by_stem <- ds %>%
    mutate(team = relevel(factor(team), ref = "m")) %>%
    arrange(year) %>%
    reframe(
      bind_rows(
        age_of_readings = {
          message("Age, N = ", n(), " --> ", unique(year))
          fit_model(
            novelty_pctl ~ team + country +
              (1 | field) + (1 | institution)
          )
        },
        intdisc = {
          message("IntDisc, N = ", n(), " --> ", unique(year))
          fit_model(
            intdisc_pr ~ team + country +
              (1 | field) + (1 | institution)
          )
        },
        female_ratio = {
          message("Female Ratio, N = ", n(), " --> ", unique(year))
          fit_model(
            female_ratio ~ team + country +
              (1 | field) + (1 | institution)
          )
        },
        .id = "depvar"
      ),
      .by = c(year, stem)
    )
)


# ----- plot-utils ----------------------------

rename_term <- function(term) {
  out <- dplyr::case_when(
    term == "teamf" ~ "Female alone (F)",
    term == "teamm" ~ "Male alone (M)",
    term == "teamfm" ~ "Mixed Gender (MF/FM)",
    term == "teammm" ~ "Male-Male (MM)",
    term == "teamff" ~ "Female-Female (FF)",
    TRUE ~ "Other"
  )
  factor(out, c("Male alone (M)",
                "Female alone (F)", 
                "Male-Male (MM)",
                "Mixed Gender (MF/FM)",
                "Female-Female (FF)"))
}

# ----- plot-baseline, fig.width = 7, fig.height = 3, eval = FALSE

p <- fit %>% 
  filter(grepl("team", term)) %>%
  mutate(column = rename_term(term)) %>%
  ggplot() +
  aes(
    y = column,
    x = estimate,
    pch = term,
    color = term,
    fill = term,
    xmin = conf.low,
    xmax = conf.high,
  ) +
  geom_vline(xintercept = 0, linetype = 2) +
  #geom_ribbon(linewidth = NA, alpha = 0.2) + 
  geom_linerange(size = 1.5) +
  geom_point(size = 3) +
  #scale_x_continuous(labels = function(.) sprintf("%2.0f%%", . * 100)) +
  #scale_x_continuous() + 
  labs(
    y = NULL,
    x = "Difference in Outcomes"
  )

(p + facet_grid(~depvar, scales = "free"))


# ---- plot-age-of-references, fig.cap = cap, fig.width = 7, fig.height = 3

cap <- "Estimated differences in the novelty percentile (younger = more novel) of cited readings are plotted by year and team gender composition. Results are shown for four team types: male alone (M), male-male (MM), mixed-gender (MF/FM), and female-female (FF). Estimates are derived from mixed-effects models controlling for country, field, and institution. Shaded bands indicate 95% confidence intervals. A horizontal dashed line at 0 marks no difference relative to the reference category (female alone, F)." # nolint

p <- fit_by_year %>% 
  filter(grepl("team", term), 
         depvar == "age_of_readings") %>%
  mutate(column = rename_term(term)) %>%
  ggplot() +
  aes(
    x = year,
    y = estimate,
    pch = term,
    color = term,
    fill = term,
    ymin = conf.low,
    ymax = conf.high,
  ) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_ribbon(linewidth = NA, alpha = 0.2) + 
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = function(.) sprintf("%2.0f%%", . * 100)) +
  scale_x_continuous() + 
  labs(
    x = "Year",
    y = "Difference in Age of Readings\n(Percentile Rank)"
  )

p_age_readings <- p + facet_grid(~column)
print(p_age_readings)

# ---- plot-female-ratio, fig.cap = cap, fig.width = 7, fig.height = 3

cap <- "Estimated differences in the share of cited female authors by team gender composition are plotted across years. Results are shown for four team types: male alone (M), male-male (MM), mixed-gender (MF/FM), and female-female (FF). Estimates are derived from mixed-effects models controlling for country, field, and institution. Shaded bands indicate 95% confidence intervals. The horizontal dashed line marks no difference from the reference category (female alone, F)."  # nolint

p <- fit_by_year %>% 
  filter(grepl("team", term), 
         depvar == "female_ratio") %>%
  mutate(column = rename_term(term)) %>%
  ggplot() +
  aes(
    x = year,
    y = estimate,
    pch = term,
    color = term,
    fill = term,
    ymin = conf.low,
    ymax = conf.high,
  ) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_ribbon(linewidth = NA, alpha = 0.2) + 
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = function(.) sprintf("%2.0f%%", . * 100)) +
  scale_x_continuous() + 
  labs(
    x = "Year",
    y = "Difference in Female Authors\n(%)"
  )


p_female_ratio <- p + facet_grid(~column)
print(p_female_ratio)

# ---- plot-interdisciplinarity, fig.width = 7, fig.height = 3

p <- fit_by_year %>% 
  filter(grepl("team", term), 
         depvar == "intdisc") %>%
  mutate(column = rename_term(term)) %>%
  ggplot() +
  aes(
    x = year,
    y = estimate,
    pch = term,
    color = term,
    fill = term,
    ymin = conf.low,
    ymax = conf.high,
  ) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_ribbon(linewidth = NA, alpha = 0.2) + 
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = function(.) sprintf("%2.0f%%", . * 100)) +
  scale_x_continuous() + 
  labs(
    x = "Year",
    y = "Difference in Interdisciplinarity\n(%)"
  )

p_interdisc <- p + facet_grid(~column)
print(p_interdisc)

# ---- mixed-model-by-field, cache = TRUE

system.time(
  out <- ds %>%
    mutate(
      team_size = nchar(team),
      intdisc_pr = percent_rank(mean_intdisc),
      gender = convert_gender_label(team),
      .by = year
    ) %>%
    mutate(isced = recode_field(as.character(field))) %>%
    arrange(year) %>% 
    reframe(
      {
        message(unique(year), "n = ", n())
        fit_model(
          intdisc_pr ~ team + country + (1 | institution)
        )
      },
      .by = c(year, isced)
    )
)

# ---- mixed-model-by-field-plot, fig.height = 9, fig.width = 9

p <- filter(out, grepl("team", term)) %>%
  mutate(
    column = case_when(
      term == "teamf" ~ "Female alone (F)",
      term == "teamm" ~ "Male alone (M)",
      term == "teamfm" ~ "Mixed-gender (FM/MF)",
      term == "teammm" ~ "Male-Male (MM)",
      term == "teamff" ~ "Female-Female (FF)",
      TRUE ~ "Other"
    )
  ) %>% 
  ggplot() +
  aes(
    y = estimate,
    x = year,
    pch = term,
    color = term,
    fill = term,
    ymin = conf.low,
    ymax = conf.high,
  ) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_ribbon(linewidth = NA, alpha = 0.2) +
  #geom_linerange() +
  geom_point() +
  #scale_color_manual(values = c(yes = "blue", no = "gray")) + 
  scale_y_continuous(labels = function(.) sprintf("%2.0f%%", . * 100)) +
  labs(
    y = NULL,
    x = "Difference in Interdisciplinarity (percentile rank Int. Score)"
  )

p +
  facet_grid(isced ~ column, scales = "free") + 
  theme(legend.position = "none")


