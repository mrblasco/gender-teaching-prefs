# ---- Setup, include = FALSE ------------------------------
library(yaml)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(countrycode)
source("config.R")

config <- yaml.load_file("config.yml")
data_dir <- config$data_dir
fig_dir <- file.path(getwd(), config$fig_dir) # full path

dir.create(fig_dir, showWarnings = FALSE)

knitr::opts_chunk$set(
    echo = FALSE,
    fig.cap = "TODO: add caption here",
    fig.path = fig_dir,
    cache.path = "cache/",
    dev = c("png", "pdf"),
    dpi = 600,
    fig.width = 7,
    fig.height = 4
)

# Constants
pseudo_obs <- 1
year_cutoff <- 1999
timestamp <- format(Sys.time(), "%Y%m%d")

# ---- Helpers ----------------------------------------

source("R/utils.R")
source("R/theme.R")

# ------ Theme ------------------------------------

my_colors <- c("#1b9e77", "#d95f02",
               "#7570b3", "#e7298a",
               "#66a61e", "#e6ab02")

scale_color_discrete <- function(...) {
    scale_color_manual(values = my_colors, ...)
}
scale_fill_discrete <- function(...) {
    scale_fill_manual(values = my_colors, ...)
}

theme_set(theme_custom())

# ---- Load data, cache = TRUE ----------------------------

filename_data <- file.path(data_dir, "rds/final.rds")

ds <- readRDS(filename_data) %>%
    filter(nchar(team) < 10) %>%
    select(field, year, team = team_ordered, team_rand, country) %>% 
    pivot_longer(cols = c(team, team_rand),
                names_to = "formation",
                values_to = "composition") %>%
    count(formation, composition, field, year, country) %>% 
    mutate(
        team_gender = get_gender_composition(composition),
        team_size = nchar(as.character(composition)),
    )

# ---- top-rows ----------------------------

kbl_top_rows <- head(ds) %>%
    kableExtra::kbl(
        caption = "Top Rows of The Dataset"
    ) %>%
    kableExtra::kable_styling()

# ----- country ----------------------------

tbl_country <- ds %>%
    filter(formation == "team") %>%
    mutate(
        country_name = countrycode(country, "iso2c", "country.name")
    ) %>%
    bind_rows(., mutate(., country_name = "All")) %>%
    count(country_name, wt = n, sort = TRUE) %>%
    mutate(prop = 100 * n / sum(n[country_name != "All"]))

kbl_country <- tbl_country %>%
    select(
        "Country" = country_name,
        "Syllabi" = n,
        "% total" = prop
    ) %>%
    kableExtra::kbl(
        caption = "Frequency of Syllabi per Country",
        caption.short = "Frequency of Syllabi per Country",
        booktabs = TRUE,
        format.args = list(big.mark = " "),
        digits = 1,
    ) %>%
    kableExtra::kable_styling() %>% 
    kableExtra::column_spec(1, bold = TRUE)

plot_country <- tbl_country %>%
    ggplot(
        aes(
            fill = country_name == "All",
            color = country_name == "All",
            x = n,
            y = reorder(country_name, n),
            label = case_when(
                n > 1e6 ~ sprintf("%2.1fM", n / 1e6),
                n > 1e3 ~ sprintf("%2.1fk", n / 1e3),
                TRUE ~ sprintf("%i", n)
            )
        )
    ) +
    scale_x_log10(
        #labels = scales::label_math(10^.x),
        expand = c(0.1, 0.1)
    ) +
    scale_fill_manual(values = c("blue4", "gold2")) +
    scale_color_manual(values = c("white", "black")) +
    geom_col(width = 0.8, color = NA) +
    geom_text(
        position = position_stack(0),
        hjust = 0
    ) +
    labs(
        x = "Number of syllabi (log10)",
        y = NULL
    ) +
    theme(
        legend.position = "none",
        panel.grid = element_blank(),
    )

# ----- fields ----------------------------

tbl_fields <- ds %>%
    filter(formation == "team") %>% 
    count(field, wt = n) %>%
    mutate(index = row_number(),
           col = ifelse(index %% 2 == 1, "left", "right"),
           group = (index + 1) %/% 2) %>%
    select(-index) %>%
    mutate(pc = round(100 * n / sum(n), 1)) %>% 
    mutate(n = sprintf("%2.1f", n / 1e3)) %>% 
    mutate(across(c(field, n, pc), as.character)) %>% 
    pivot_wider(names_from = col, 
                values_from = c(field, n, pc), 
                values_fill =  "") %>%
    select(field_left, n_left, pc_left, field_right, n_right, pc_right) 

kbl_fields <- tbl_fields %>%
    kableExtra::kbl(
        caption = "Frequency of Syllabi by Academic Field",
        col.names = rep(c("Field", "N (thousands)", "%"), 2),
    ) %>%
    kableExtra::kable_styling()

plot_fields <- ds %>%
    filter(formation == "team") %>% 
    count(field, wt = n) %>%
    mutate(index = row_number(),
           col = ifelse(index %% 2 == 1, "left", "right"),
           group = (index + 1) %/% 2) %>% 
    ggplot(
        aes(
            x = n,
            y = reorder(field, n)
        )
    ) +
    scale_x_log10() +
    facet_wrap(~ col, scales = "free") +
    geom_col(width = 0.7) +
    labs(
        x = "Number of Syllabi (log10)",
        y = NA
    ) +
    theme(
        strip.text = element_blank()
    )

# ----- years ----------------------------

tbl_years <- ds %>%
    filter(formation == "team") %>%
    mutate(
        year_bc = ifelse(year < 2000, "1999 or older", as.character(year))
    ) %>% 
    count(year_bc, wt = n) %>%
    mutate(pc = round(100 * n / sum(n), 1))

kbl_years <- tbl_years %>% 
    mutate(n = sprintf("%2.1f", n / 1e3)) %>%
    kableExtra::kbl(
        caption = "Frequency of Syllabi per Academic Year",
        col.names = rep(c("Academic year", "N (thousands)", "%"), 1),
    ) %>%
    kableExtra::kable_styling()

plot_year <- ds %>%
    filter(formation == "team") %>%
    count(year, wt = n) %>% 
    ggplot(
        aes(
            x = year, #factor(year, ordered = TRUE),
            y = n
        )
    ) +
    scale_y_log10() +
    geom_point() +
    labs(
        y = "Number of syllabi (log10)", x = NULL
    )

# ----- composition ----------------------------

tbl_team_composition <- ds %>%
    filter(formation == "team") %>%
    count(composition, wt = n) %>%
    arrange(desc(n)) %>%
    mutate(pc = round(100 * n / sum(n), 1),
            n = sprintf("%2.1f", n / 1e3)) %>% 
    filter(nchar(composition) < 3) %>% 
    mutate(composition = convert_gender_label(composition)) 

kbl_team_composition <- tbl_team_composition %>%
    rename(
        "N (thousands)" = n,
        "Team composition" = composition,
        "%" = pc,
    ) %>%
    kableExtra::kbl(
        caption = "Frequency of Syllabi by Team Composition",
        caption.short = "Frequency of Syllabi by Team ccmposition",
        format.args = list(big.mark = " "),
    ) %>%
    kableExtra::kable_styling()

# ====================================
# Figures
# ====================================

# ----- evolution, fig.cap = cap ------------
cap <- "Evolution of Courses by Teaching Configuration. (A) proportions of courses per year with one instructor by gender (B) proportions of courses per year with two instructors by gender configuration." # nolint

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



# ----- montecarlo, fig.cap = cap -------
cap <- "Montecarlo simulations."

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

# ----- sim-by-cntry,  fig.cap = cap -------

cap <- "Montecarlo simulations by country."

ds %>%
  filter(team_size < 3, year > year_cutoff) %>%
  mutate(
    composition = composition %>%
      strsplit(split = "") %>%
      sapply(paste_sort)
  ) %>%
  mutate(
    region = case_when(
      country %in% c("DK", "DE", "AT", "BE", 
                     "FR", "IT", "NL", "ES", "PT",
                     "PL", "ES", "IE") ~ "European Union",
      country == "CA" ~ "Canada",
      country == "US" ~ "United States",
      country == "GB" ~ "Great Britain",
      TRUE ~ "Other",
    )
  ) %>% 
  count(region, formation, composition, year, wt = n) %>%
  mutate(pc = (n + 1) / (sum(n) + 2), .by = c(year, formation, region)) %>%
  filter(composition == "fm") %>%
  mutate(
    color = case_when(
      formation == "team" ~ "Actual",
      formation == "team_rand" ~ "Simulated (gender-neutral)",
    ),
  ) %>%
  ggplot(aes(year, pc, color = color, linetype = color)) +
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
  scale_y_continuous(labels = function(.) sprintf("%2.0f%%", 100 * .)) +
  scale_color_discrete() +
  scale_fill_discrete() +
  facet_wrap(~region, scales = "free") + 
  labs(
    x = "Academic year", 
    y  = "Mixed gender (MF/FM) courses\nper year (%)"
  )

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


# --------- gender-imbalance -------------------------

head(ds_by_field_unord)



ds_gender_imbalance <- ds_by_field_unord %>% 
  filter(formation == "team") %>% 
  mutate(gender = strsplit(composition, "")) %>% 
  select(gender, n, field) %>%
  tidyr::unnest(cols = gender) %>%
  count(field, gender, wt = n) %>%
  pivot_wider(names_from = gender, values_from = n) %>% 
  mutate(gender_ratio = (f + 2) / (m + f + 4), .by = field)

head(ds_gender_imbalance)

ds_simulation_diff <- ds_filtered %>% 
  filter(composition == "fm") %>% 
  select(field, formation, pc) %>% 
  pivot_wider(names_from = formation, values_from = pc)

ds_simulation_diff %>% 
  left_join(ds_gender_imbalance) %>% 
  ggplot(
    aes(gender_ratio,
        Actual - `Simulated (gender-neutral)`,
        label = field)
  ) + 
  scale_x_continuous(labels = function(.) . * 100) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggrepel::geom_text_repel() +
  labs(
    x = "Gender ratio (% women instructors)",
    y = "Actual - Simulated (Mixed gender teams)"
  )
