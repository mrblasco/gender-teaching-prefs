# ---- Setup, include = FALSE ------------------------------
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(ggforce)
library(countrycode)

knitr::opts_chunk$set(
    echo = FALSE
)

results_dir <- file.path("data", "results")

# ---- Constants ----------------------------------------

pseudo_obs <- 1
year_cutoff <- 1999

# ---- Helpers ----------------------------------------

source("R/utils.R")
source("R/theme.R")
source("R/paths.R")
source("R/labels.R")
source("R/isced.R")

save_plot <- function(plot, filename, formats = "pdf", ...) {
    stopifnot(inherits(plot, "ggplot"))
    
    saveRDS(p, filename)
    log_msg("Saved RDS: %s", filename)
    
    if (formats == "pdf") {
        pdf_filename <- paste0(tools::file_path_sans_ext(filename), ".pdf")
        ggplot2::ggsave(filename = filename, plot = plot, ...)
        log_msg("Saved PDF: %s", pdf_filename)
    }
    invisible(filename)
}

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

filename_data <- file.path(data_dir, "rds", "final.rds")

ds <- readRDS(filename_data) %>%
    filter(nchar(team) < 10) %>%
    select(field, year, team = team_ordered, team_rand, country) %>% 
    pivot_longer(
        cols = c(team, team_rand),
        names_to = "formation",
        values_to = "composition"
    ) %>%
    count(formation, composition, field, year, country) %>% 
    mutate(
        team_gender = get_gender_composition(composition),
        team_size = nchar(as.character(composition)),
    )

log_msg("%i", sum(ds$n))


ds_count <- ds %>%
    filter(formation == "team") %>%
    count(team = composition, year, wt = n)

ds_count_wide <- ds_count %>% 
    pivot_wider(names_from = team, values_from = n, values_fill = 0)

p_teams <- ggplot(ds_count, aes(x = year, y = n, color = team, linetype = team)) +
    geom_line() +
    geom_text_repel(
        data = filter(ds_count, year == 2019),
        aes(label = stringr::str_wrap(composition_labels[team], 20)),
        hjust = 0, nudge_x = 0.9, show.legend = FALSE, direction = "y",
        size = 3, segment.color = "gray75"
    ) +
    scale_y_log10() +
    scale_x_continuous(limits = c(1990, 2025))  +
    labs(
        x = "Year", y = "Syllabi (log10)"
    ) + 
    theme(legend.position = "none")

p_teams


ds_comp <- ds_count %>%
    group_by(year) %>%
    mutate(prop = n / sum(n))

p_comp <- ggplot(ds_comp, aes(x = year, y = prop, fill = team)) +
    geom_area(alpha = 0.85, linewidth = 0.2, color = "white") +
    scale_x_continuous(limits = c(1990, 2025)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
        x = "Year",
        y = "Composition of Teams (%)",
        fill = "Team"
    ) +
    scale_fill_brewer(palette = "Set3") + 
    theme_minimal(base_size = 12) +
    theme(
        legend.position = "right",
        panel.grid.minor = element_blank()
    )

p_comp 


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
    ggplot(
        aes(
            y = pc, x = year,
            color = team_gender,
            pch = team_gender,
            fill = team_gender,
            ymin = ymin, ymax = ymax,
        )
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
    ) + 
    facet_wrap(
        ~ team_size,
        scales = "free", 
        labeller = labeller(team_size = team_size_labels)
    )

save_plot(p, file.path(results_dir, "fig_evolution.rds"))

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
    facet_wrap(~ composition, scales = "free", labeller = labeller(composition = composition_labels)) + 
    labs(
        x = "Academic year", y  = "Courses per year (%)"
    )

saveRDS(p, file.path(results_dir, "fig_montecarlo.rds"))
ggsave(file.path(results_dir, "fig_montecarlo.pdf"))


# ----- sim-by-cntry,  fig.cap = cap -------

cap <- "Montecarlo simulations by country."

p <- ds %>%
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

saveRDS(p, file.path(results_dir, "fig_montecarlo_by_country.rds"))
ggsave(file.path(results_dir, "fig_montecarlo_by_country.pdf"))


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

ggsave(file.path(results_dir, "plot_ordered_teams.pdf"))

saveRDS(p, file.path(results_dir, "plot_ordered_teams.rds"))

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

p_combined <- (p1 + p2) + 
    plot_layout(guides = "collect", axis_titles = "collect")


saveRDS(p_combined, file.path(results_dir, "fig_montecarlo_by_field.rds"))
ggsave(file.path(results_dir, "fig_montecarlo_by_field.pdf"))

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
