# ======================================================================
# Representativeness of the gender distribution
# ----------------------------------------------------------------------
# Compares the female-to-male ratio of instructors in OpenSyllabus (OS)
# against two external benchmarks, to gauge how representative the OS
# sample is of the broader instructor population:
#
#   (1) OECD academic staff by gender, per country and year (Eurostat
#       educ_uoe_perd02 / OECD).            -> fig_representative_eurostat.pdf
#   (2) U.S. PhD graduates by ISCED-F broad field and year (NCES Survey
#       of Earned Doctorates).              -> fig_representative_field.pdf
#
# Reads the canonical merged dataset (data/processed/syllabi_merged.rds),
# which already carries the ISCED-F broad field in `isced`, and the two
# benchmark CSVs under data/raw/. Writes figures to <out_dir>.
#
# Usage:  Rscript scripts/06_plots_representative.R <out_dir>
# Default out_dir: output/06_plots_representative
# ======================================================================

suppressMessages({
    library(dplyr, warn.conflicts = FALSE)
    library(tidyr)
    library(ggplot2)
    library(countrycode)
})

source(here::here("R/theme.R"))  # theme_custom()
source(here::here("R/utils.R"))  # log_msg()
theme_set(theme_custom())

# ----------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) {
    args[1]
} else {
    here::here("output", "06_plots_representative")
}
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

data_path     <- here::here("data", "processed", "syllabi_merged.rds")
academic_path <- here::here("data", "raw", "tertiary_academic_staff_gender_OECD.csv")
phd_us_path   <- here::here("data", "raw", "phd_counts_by_field_year_US_isced.csv")

for (p in c(data_path, academic_path, phd_us_path)) {
    if (!file.exists(p)) stop("Required input not found: ", p)
}

# ----------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------
# Normalise field labels so OS `isced` and benchmark `field_name` join:
# lowercase, drop punctuation, drop the trailing " (icts)" parenthetical,
# collapse whitespace.
clean_field <- function(x) {
    x <- tolower(x)
    x <- gsub("\\s*\\(icts\\)", "", x)
    x <- gsub("[[:punct:]]+", "", x)
    x <- gsub("[[:space:]]+", " ", x)
    trimws(x)
}

# Count male/female instructor slots implied by a team string, weighted
# by the number of courses `n`. e.g. "fm" -> 1 man + 1 woman.
instructor_slots <- function(df) {
    df %>%
        mutate(
            men   = nchar(gsub("[f]+", "", team)) * n,
            women = nchar(gsub("[m]+", "", team)) * n
        )
}

# ----------------------------------------------------------------------
# Load OS syllabi dataset
# ----------------------------------------------------------------------
log_msg("Loading data ...")
syllabi <- readRDS(data_path)
log_msg("Loaded %s courses.", format(nrow(syllabi), big.mark = ","))

# ======================================================================
# (1) OS vs OECD academic staff --- overall, by country and year
# ======================================================================
gender_ratio_oecd <- read.csv(academic_path) %>%
    mutate(country = countrycode(country, "iso3c", "iso2c")) %>%
    select(country, year, men, women)

gender_ratio_os <- syllabi %>%
    count(country, team, year) %>%
    instructor_slots() %>%
    summarise(men = sum(men), women = sum(women),
              .by = c(country, year))

gender_ratio <- gender_ratio_os %>%
    inner_join(gender_ratio_oecd, by = c("country", "year"),
               suffix = c("_OS", "_OECD")) %>%
    mutate(
        ratio_OS   = women_OS / men_OS,
        ratio_OECD = women_OECD / men_OECD
    )

corr_by_country <- gender_ratio %>%
    group_by(country) %>%
    summarise(corr = cor(ratio_OS, ratio_OECD,
                         use = "complete.obs", method = "pearson"),
              .groups = "drop")

plot_data <- gender_ratio %>%
    filter(year > 1999) %>%
    select(country, year, ratio_OS, ratio_OECD) %>%
    pivot_longer(starts_with("ratio"), names_to = "source",
                 values_to = "ratio") %>%
    left_join(corr_by_country, by = "country") %>%
    mutate(
        data_source = ifelse(source == "ratio_OS",
                             "OpenSyllabus (OS)", "OECD/Eurostat"),
        country = countrycode(country, "iso2c", "country.name")
    )

p_all <- plot_data %>%
    ggplot(aes(x = year, y = ratio,
               linetype = data_source, color = data_source)) +
    geom_line(linewidth = 0.7) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    facet_wrap(~country) +
    scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, 0.5),
                       expand = expansion(mult = c(0.02, 0.08))) +
    scale_linetype_manual(values = c("solid", "longdash")) +
    labs(y = "Female-Male Ratio", x = "Year",
         color = "Source", linetype = "Source") +
    geom_text(
        data = plot_data %>% distinct(country, corr) %>%
            mutate(data_source = "OECD/Eurostat"),
        aes(x = -Inf, y = Inf,
            label = paste0("rho = ", sprintf("%.2f", corr))),
        hjust = -0.1, vjust = 1.2, size = 3.0, color = "black"
    ) +
    theme(panel.grid.minor = element_blank())

ggsave(file.path(out_dir, "fig_representative_eurostat.pdf"), p_all,
       device = cairo_pdf, width = 7, height = 7)
log_msg("  wrote fig_representative_eurostat.pdf")

# ======================================================================
# (2) OS vs U.S. PhD graduates --- by ISCED-F broad field and year
# ======================================================================
phd_us <- read.csv(phd_us_path) %>%
    select(country, year, field = field_name, women, men) %>%
    mutate(field = clean_field(field)) %>%
    summarise(women = sum(women), men = sum(men),
              .by = c(country, field, year))

gender_ratio_os_field <- syllabi %>%
    filter(country == "US") %>%
    count(country, team, year, isced) %>%
    mutate(field = clean_field(as.character(isced))) %>%
    summarise(n = sum(n), .by = c(country, team, year, field)) %>%
    instructor_slots() %>%
    summarise(men = sum(men), women = sum(women),
              .by = c(country, year, field))

gender_ratio_field <- gender_ratio_os_field %>%
    inner_join(phd_us, by = c("country", "year", "field"),
               suffix = c("_OS", "_PHD")) %>%
    mutate(
        ratio_OS  = women_OS / men_OS,
        ratio_PHD = women_PHD / men_PHD
    )

corr_by_field <- gender_ratio_field %>%
    group_by(field) %>%
    summarise(corr = cor(ratio_OS, ratio_PHD, use = "complete.obs"),
              .groups = "drop")

plot_data_field <- gender_ratio_field %>%
    filter(year > 1999) %>%
    select(year, field, ratio_OS, ratio_PHD) %>%
    pivot_longer(starts_with("ratio"), names_to = "source",
                 values_to = "ratio") %>%
    left_join(corr_by_field, by = "field") %>%
    mutate(
        source = ifelse(source == "ratio_OS", "OpenSyllabus (OS)", "NCES"),
        field = tools::toTitleCase(field)
    )

p_field <- plot_data_field %>%
    ggplot(aes(x = year, y = ratio, color = source, linetype = source)) +
    geom_line(linewidth = 0.7) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    facet_wrap(~stringr::str_wrap(field, 30)) +
    scale_y_continuous(limits = c(0, 3), breaks = seq(0, 2, 0.5),
                       expand = expansion(mult = c(0.02, 0.08))) +
    scale_linetype_manual(values = c("solid", "longdash")) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
    labs(y = "Female-Male Ratio", x = "Year",
         color = "Source", linetype = "Source") +
    geom_text(
        data = plot_data_field %>% distinct(field, corr) %>%
            mutate(source = "NCES"),
        aes(x = -Inf, y = Inf,
            label = paste0("rho = ", sprintf("%.2f", corr))),
        hjust = -0.1, vjust = 1.2, size = 3.0, color = "black"
    ) +
    theme(panel.grid.minor = element_blank())

ggsave(file.path(out_dir, "fig_representative_field.pdf"), p_field,
       device = cairo_pdf, width = 7, height = 7)
log_msg("  wrote fig_representative_field.pdf")

# ----------------------------------------------------------------------
# Save plot objects + correlation tables for reuse
# ----------------------------------------------------------------------
saveRDS(list(all = p_all, by_field = p_field),
        file.path(out_dir, "figs_representative.rds"))
write.csv(corr_by_country, file.path(out_dir, "corr_by_country.csv"),
          row.names = FALSE)
write.csv(corr_by_field, file.path(out_dir, "corr_by_field.csv"),
          row.names = FALSE)

log_msg("")
log_msg("Done. Representativeness figures written to %s", out_dir)
