
# ---- Setup, include = FALSE ------------------------------

library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)

source("config.R")

knitr::opts_chunk$set(
  echo = FALSE,
  fig.cap = "TODO: add caption here",
  fig.path = "figures/",
  cache.path = "cache/",
  dev = c("pdf", "png"),
  dpi = 600
)

# Constants
pseudo_obs <- 1
year_cutoff <- 1999
timestamp <- format(Sys.time(), "%Y%m%d")

# ---- Helpers ----------------------------------------

get_gender_composition <- function(x) {
  x <- as.character(x)
  case_when(
    grepl("m", x) & grepl("f", x) ~ "Mixed-gender",
    grepl("m", x) ~ "Male-only",
    grepl("f", x) ~ "Female-only",
    TRUE ~ "Other"
  )
}

paste_sort <- function(x) paste0(sort(x), collapse = "")

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

sep <- function(p, n) sqrt(p * (1 - p) / n)

# ------ Theme ------------------------------------

theme_custom <- function() {
  theme_minimal(base_family = "Helvetica") +
    theme(
      plot.title      = element_text(face = "bold"),
      plot.subtitle   = element_text(face = "italic"),
      plot.caption    = element_blank(),
      #panel.grid.minor = element_blank(),
      panel.spacing   = unit(1.5, "lines"),
      strip.text      = element_text(face = "bold"),
      axis.line       = element_line(color = "gray25"),
      axis.ticks      = element_line(),
      legend.position = "bottom",
      legend.title    = element_blank(),
    )
}

my_colors <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02")

scale_color_discrete <- function(...) scale_color_manual(values = my_colors, ...)
scale_fill_discrete <- function(...) scale_fill_manual(values = my_colors, ...)

theme_set(theme_custom())


# ---- Load data, cache = TRUE ----------------------------

filename_data <- "../data/rds/final.rds"

ds <- readRDS(filename_data) %>%
  filter(nchar(team) < 10) %>%
  select(field, year, team = team_ordered, team_rand) %>% 
  pivot_longer(cols = c(team, team_rand),
               names_to = "formation",
               values_to = "composition") %>%
  count(formation, composition, field, year) %>% 
  mutate(
    team_gender = get_gender_composition(composition),
    team_size = nchar(as.character(composition))
  )

format(object.size(ds), "MB")

# ----- evolution, fig.width = 7, fig.height = 3.5 ---------------

ds_count <- ds %>%
  filter(formation == "team") %>%
  count(team_size, team_gender, year, wt = n) %>%
  mutate(
    pc = (n + pseudo_obs) / (sum(n) + 2 * pseudo_obs),
    ymin = pc - 1.96 * sqrt(pc * (1 - pc) / (n + 2 * pseudo_obs)),
    ymax = pc + 1.96 * sqrt(pc * (1 - pc) / (n + 2 * pseudo_obs)),
    .by = c(year)
  ) %>%
  filter(year > year_cutoff, team_size < 3)

p <- ds_count %>%
  mutate(
    team_size = case_when(
      team_size == 1 ~ "A. One instructor",
      team_size == 2 ~ "B. Two instructors",
    )
  ) %>% 
  ggplot()  +
  aes(
    y = pc, x = year,
    color = team_gender,
    pch = team_gender,
    fill = team_gender,
    ymin = ymin, ymax = ymax,
  ) +
  geom_line() +
  geom_point(size = 2.5) +
  geom_ribbon(alpha = 0.2, linetype = 0, show.legend = FALSE) +
  geom_text(
    aes(
      label = ifelse(year == 2019, sprintf("%2.0f%%", pc * 100), ""), 
      x = 2020
    ),
    hjust = 0.25,
    size = 3,
    show.legend = FALSE
  ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(labels = function(x) sprintf("%2.0f%%", x * 100)) +
  scale_color_discrete() +
  scale_fill_discrete() +
  labs(
    x = "Academic year",
    y = "Courses per year (%)",
    color = "Team gender",
    shape = "Team gender",
    fill = "Team gender",
  )

p + facet_wrap(~ team_size, scales = "free")


# p1 <- p %+% filter(ds_count, team_size == 1)
# p2 <- p %+% filter(ds_count, team_size == 2)
# p1 + p2 +
#   plot_layout(guides = "collect", 
#               axis_titles = "collect_x") +
#   plot_annotation(tag_levels = "A")


#'
#' \clearpage
#'

# ----- montecarlo, fig.width = 7, fig.height = 4 -------

ds_count_unordered <- ds %>%
  filter(team_size < 3) %>% 
  mutate(
    composition = composition %>%
      strsplit(split = "") %>%
      sapply(paste_sort)
  ) %>%
  count(formation, composition, year, wt = n)



p <- ds_count_unordered %>%
  filter(year > year_cutoff) %>%
  mutate(pc = n / sum(n), .by = c(year, formation)) %>%
  mutate(
    formation = case_when(
      formation == "team" ~ "Actual",
      formation == "team_rand" ~ "Simulated (gender-neutral)",
    ),
  ) %>%
  ggplot() +
  aes(
    x = year, y = pc,
    color = formation,
    group = formation,
    linetype = formation,
  ) +
  scale_y_continuous(labels = function(.) sprintf("%2.0f%%", 100 * .)) +
  geom_line() + 
  ggrepel::geom_text_repel(
    aes(
      label = ifelse(year == 2019, sprintf("%2.0f%%", pc * 100), "")
    ),
    size = 3,
    segment.colour = "gray25",
    show.legend = FALSE
  ) +
  coord_cartesian(clip = "off", xlim = c(1999, 2021)) +
  scale_color_discrete() +
  scale_fill_discrete() +
  facet_wrap(~ convert_gender_label(composition), scales = "free") + 
  labs(
    x = "Academic year", y  = "Courses per year (%)"
  )


print(p)

#' 
#' 


# ----- montecarlo-order, fig.width = 5, fig.height = 3.5 -----

p <- ds %>%
  count(composition, formation, year, wt = n) %>%
  mutate(
    pc = n / sum(n),
    se = sep(pc, n),
    conf.low = pc - 1.96 * se,
    conf.high = pc + 1.96 * se,
    .by = c(year, formation)
  ) %>%
  filter(composition %in% c("mf", "fm"), year > year_cutoff) %>%
  reframe(
    .by = c(year, composition),
    diff = pc[formation == "team"] - pc[formation == "team_rand"],
    diff_se = sqrt(sum(se^2)),
    conf.low = diff - 1.96 * diff_se, conf.high = diff + 1.96 * diff_se
  ) %>% 
  mutate(
    composition = case_when(
      composition == "mf" ~ "Male first, Female second (MF)",
      composition == "fm" ~ "Female first, Male second (FM)",
    )
  ) %>% 
  ggplot(
    aes(x = year, y = diff, color = composition, fill = composition,
        ymin = conf.low, ymax = conf.high)
  ) +
  ggrepel::geom_text_repel(
    aes(
      label = ifelse(year == 2019, sprintf("%2.1f%%", diff * 100), "")
    ),
    size = 3,
    segment.colour = "gray25",
    show.legend = FALSE
  ) +
  geom_hline(yintercept = 0) +
  geom_line() +
  geom_ribbon(alpha = 0.2, linewidth = NA) +
  scale_y_continuous(labels = function(x) sprintf("%2.0f%%", 100 * x)) +
  scale_color_discrete() +
  scale_fill_discrete() +
  coord_cartesian(clip = "off", xlim = c(1999, 2021)) +
  labs(
    x = "Academic year",
    y = "Actual - Simulated (% difference)",
  )

print(p)

#' 
#' 


# ----- montecarlo-by-field, fig.width = 7, fig.height = 9 ------

ds_by_field <- ds %>%
  count(formation, composition, field, wt = n) %>%
  filter(nchar(composition) < 3)

ds_by_field_unord <- ds_by_field %>% 
  mutate(
    composition = composition %>%
      strsplit(split = "") %>%
      sapply(paste_sort)
  ) %>%
  count(formation, composition, field, wt = n) %>% 
  mutate(
    pc = n / sum(n), 
    se = sep(pc, n),
    conf.low = pc - 1.96 * se,
    conf.high = pc + 1.96 * se,
    .by = c(field, formation)
  )

ds_filtered <- ds_by_field_unord %>% 
  filter(composition == "fm") %>%
  filter(sum(n) > 1e3, .by = field) %>% 
  mutate(
    pc_diff = ave(pc, field, FUN = function(.) abs(diff(.))),
    isced = recode_field(field),
    formation = case_when(
      formation == "team" ~ "Actual",
      formation == "team_rand" ~ "Simulated (gender-neutral)",
    ),
  ) %>%
  mutate(
    isced2 = ifelse(n() < 6, "Other", isced),
    .by = isced
  )

p <- ds_filtered %>%
  ggplot(
    aes(y = reorder(field, pc), x = pc, 
        color = formation, group = formation, fill = formation,
        xmin = conf.low, xmax = conf.high)
  ) +
  geom_linerange(alpha = 0.2, linewidth = 3) +
  #geom_point() +
  geom_text(
    aes(label = round(100 * pc, 1)), 
    show.legend = FALSE
  ) +
  scale_color_discrete() +
  scale_fill_discrete() +
  scale_x_continuous(labels = function(.) sprintf("%2.0f%%", 100 * .)) +
  labs(
    y = NULL,
    x = "Courses per year (%)"
  ) +
  ggforce::facet_col(stringr::str_wrap(isced, 25) ~ ., space = "free", 
                     scales = "free_y", strip.position = "top") +
  theme(
    panel.spacing = unit(.5, "lines"),
    legend.position = "bottom"
  )


regex <- "Arts|Business|Engineer|Educ|Agr"
p1 <- p %+% filter(ds_filtered, grepl(regex, isced))
p2 <- p %+% filter(ds_filtered, !grepl(regex, isced))

(p1 + p2) + 
  plot_layout(guides = "collect", axis_titles = "collect")

#' 
#' 


# ---- Interdsciplinarity Data, cache = TRUE ----------------------------

filename_data <- "../data/rds/final.rds"

ds_intdisc <- readRDS(filename_data) %>%
  filter(nchar(team) < 3, year > year_cutoff) %>%
  mutate(intdisc_pr = percent_rank(mean_intdisc),
         .by = year)

format(object.size(ds_intdisc), "MB")

# ----- Interdisciplinarity Regression, cache = TRUE -----------------

fit_ols <- function(...) {
  message("Fitting OLS model...")
  tryCatch(
    broom::tidy(lm(...), conf.int = TRUE),
    error = function(e) message("Error: ", e$message),
    warning = function(w) message("Warning: ", w$message)
  )
}

fit_ols_intdisc <- ds_intdisc %>% 
  mutate(team_size = nchar(team),
         gender = convert_gender_label(team)) %>%
  reframe(
    fit_ols(intdisc_pr ~ country + field + gender),
    .by = c(year, team_size)
  )

# ----- Interdisciplinarity, fig.height = 3.5, fig.width = 7 -----------------

fit_ols_intdisc_labeled <- fit_ols_intdisc %>%
  filter(grepl("gender", term)) %>%
  mutate(team_size_label = case_when(
    team_size == 1 ~ "Single female instructor (F)",
    team_size == 2 ~ "Two female instructors (FF)",
    TRUE ~ paste0("Team size: ", team_size)
  )) %>%
  mutate(gender_label = gsub("^gender", "", term))


p <- fit_ols_intdisc_labeled %>% 
  ggplot(
    aes(
      x = year, y = estimate, ymin = conf.low, ymax = conf.high,
      fill = gender_label, color = gender_label, pch = gender_label
    )
  ) +
  facet_wrap(~ team_size, nrow = 1) +
  scale_color_discrete() + 
  scale_fill_discrete() +
  geom_hline(yintercept = 0, color = "brown", linetype = 2) +
  geom_ribbon(alpha = 0.2, color = NA, show.legend = FALSE) +
  geom_line() +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = function(.) sprintf("%2.0f", 100 * .)) +
  labs(
    x = "Academic year",
    y = "Effect of Gender Composition on\nInterdisciplinarity Score (percentage point)",
    title = "Interdisciplinarity",
    subtitle = "Regression coefficients and 95% confidence intervals",
  ) +
  geom_text(
    data = . %>% distinct(team_size),
    aes(
      x = -Inf, y = -Inf,
      label = case_when(
        team_size == 1 ~ "Zero = Single female instructor (F)",
        team_size == 2 ~ "Zero = Two female instructors (FF)"
      )
    ),
    inherit.aes = FALSE,
    hjust = -0.05, vjust = -1,
    color = "brown",
    size = 3.8,
    fontface = "italic"
  )  + 
  theme(
    legend.position = "right"
  )

print(p)
