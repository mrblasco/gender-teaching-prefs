# ======================================================================
# Supplementary "Additional Tables" --- data generation
# ----------------------------------------------------------------------
# Builds the four descriptive tables reported in the SI "Additional
# Tables" section and saves them to <out_dir> as tidy CSVs. The
# manuscript (manuscript/sections/50-supporting.Rmd) reads these CSVs and
# renders them with knitr::kable, so the paper can be built in any output
# format (PDF, HTML, Word) from the same data.
#
#   tab:SI-country      syllabi per country          -> si_table_country.csv
#   tab:SI-fields       syllabi per field            -> si_table_fields.csv
#   tab:SI-years        syllabi per academic year    -> si_table_years.csv
#   tab:SI-composition  teaching-team configurations -> si_table_composition.csv
#
# Each row of syllabi_merged.rds is one course (one team). The gender
# composition is encoded in `team` (f, m = single instructor; ff, mm, fm
# = two instructors). There is no separate `formation`/`composition`
# column, so counts are plain row counts over `team`/`country`/etc.
#
# Columns in every CSV are tidy and format-agnostic: a label column, a
# raw count `n`, and a percentage `pc`. Display formatting (counts in
# thousands, big marks, column headers) is applied in the manuscript.
#
# Usage:  Rscript scripts/90_supplementary.R <out_dir>
# Default out_dir: output/90_supplementary
# ======================================================================

suppressMessages({
    library(dplyr, warn.conflicts = FALSE)
    library(readr)
    library(ggplot2)
    library(patchwork)
})

source(here::here("R/labels.R"))  # composition_labels
source(here::here("R/theme.R"))  # composition_labels
theme_set(theme_custom())

# ----------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
out_dir <- ifelse(length(args) >= 1, args[1], tempdir())

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

data_dir <- here::here("data", "processed")

say <- function(fmt, ...) message(sprintf(fmt, ...))

# ----------------------------------------------------------------------
# Load data (one row = one course/team)
# ----------------------------------------------------------------------
say("Loading data ...")
ds <- readRDS(file.path(data_dir, "syllabi_merged.rds"))
say("Loaded %s courses.", format(nrow(ds), big.mark = ","))

country_labels <- c(
    US = "USA", GB = "Great Britain", CA = "Canada", IT = "Italy",
    PL = "Poland", NL = "Netherlands", DE = "Germany", IE = "Ireland",
    PT = "Portugal", SE = "Sweden", ES = "Spain", AT = "Austria",
    DK = "Denmark", FR = "France", BE = "Belgium"
)

save_table <- function(tidy, stem) {
    write_csv(tidy, file.path(out_dir, paste0(stem, ".csv")))
    say("  wrote %s.csv", stem)
}


# ======================================================================
# Figure SI-trends --- fluctuations
# ======================================================================
trends_data <- ds |>
    group_by(year) |>
    summarise(
        institutions = n_distinct(institution),
        syllabi = n(),
        solo_courses = sum(nchar(as.character(team)) == 1)
    )

p1 <- trends_data |>
    ggplot(aes(year, institutions)) +
    geom_col() +
    labs(
        x = "Academic year",
        y = "Academic institutions"
    )

p2 <- trends_data |>
    ggplot(aes(year, syllabi / institutions)) +
    geom_col() + 
    labs(
        x = "Academic year",
        y = "Syllabi per institution"
    )

p3 <- trends_data |>
    ggplot(aes(year, 100 * solo_courses / syllabi)) +
    geom_col() + 
    labs(
        x = "Academic year",
        y = "Solo courses (%)"
    )

p_trends <- p1 + p2 + p3 + plot_annotation(tag_levels = "A")

out <- file.path(out_dir, "supp_trends.pdf")
ggsave(
    filename = out,
    device = cairo_pdf, width = 7, height = 3, units = "in"
)
say("Figure: %s", out)



# ======================================================================
# Table SI-courselevel --- syllabi per course lelve
# ======================================================================
course_level_tbl <- ds %>%
    count(course_level, name = "n") %>%
    mutate(pc = round(100 * n / sum(n), 1)) %>%
    arrange(desc(n))

save_table(course_level_tbl, "si_course_level")

# ======================================================================
# Table SI-country --- syllabi per country
# ======================================================================
country_tbl <- ds %>%
    count(country, name = "n") %>%
    mutate(country = dplyr::recode(country, !!!country_labels,
                                   .default = "Other")) %>%
    count(country, wt = n, name = "n") %>%
    mutate(pc = round(100 * n / sum(n), 1)) %>%
    arrange(desc(n))
save_table(country_tbl, "si_table_country")

# ======================================================================
# Table SI-fields --- syllabi per field
# ======================================================================
fields_tbl <- ds %>%
    count(field, name = "n") %>%
    arrange(field) %>%
    mutate(pc = round(100 * n / sum(n), 1))
save_table(fields_tbl, "si_table_fields")

# ======================================================================
# Table SI-years --- syllabi per academic year
# ======================================================================
years_tbl <- ds %>%
    mutate(year_bc = ifelse(year < 2000, "1999 or older",
                            as.character(year))) %>%
    count(year_bc, name = "n") %>%
    mutate(pc = round(100 * n / sum(n), 1)) %>%
    arrange(year_bc)
save_table(years_tbl, "si_table_years")

# ======================================================================
# Table SI-composition --- teaching-team configurations
# ======================================================================
composition_tbl <- ds %>%
    count(composition = as.character(team), name = "n") %>%
    filter(nchar(composition) < 3) %>%
    mutate(
        label = dplyr::recode(composition, !!!composition_labels,
                              .default = "Unknown"),
        pc = round(100 * n / sum(n), 1)
    ) %>%
    arrange(desc(n)) %>%
    select(composition, label, n, pc)
save_table(composition_tbl, "si_table_composition")

# ----------------------------------------------------------------------
# Bundle the tidy tables into a single list object for reuse.
# ----------------------------------------------------------------------
saveRDS(
    list(
        country     = country_tbl,
        fields      = fields_tbl,
        years       = years_tbl,
        composition = composition_tbl
    ),
    file.path(out_dir, "si_tables.rds")
)

say("")
say("Done. Supplementary tables written to %s", out_dir)
