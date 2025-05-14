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

# ----- theme --------------------------

theme_set(theme_minimal())

theme_update(
  strip.text.y = element_blank(),
  legend.position = "none",
)


# ----- functions -----------------------

source("config.R")

convert_gender_label <- function(original_labels) {
  dplyr::case_match(
    original_labels,
    "f"  ~ "Female only (F)",
    "ff" ~ "Female-Female (FF)",
    "mf" ~ "Mixed Gender (MF/FM)",
    "fm" ~ "Mixed Gender (MF/FM)",
    "mm" ~ "Male-Male (MM)",
    "m"  ~ "Male only (M)",
    .default = "Unknown"
  )
}

fit_model <- function(formula) {
  fit <- lmer(
    formula,
    control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE)
  )
  if (isSingular(fit)) {
    warning("⚠️ Model is singular (overfit or sparse random effects)")
  }
  if (!is.null(fit@optinfo$conv$lme4$messages)) {
    warning("⚠️ Convergence issues detected")
  }
  coeffs <- broom::tidy(fit, conf.int = TRUE)
  return(coeffs)
}



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

# ----- data-head -------------------------------

head(ds) %>% 
  knitr::kable(
    caption = "Top Rows of the dataset"
  )

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


