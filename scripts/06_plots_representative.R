library(dplyr)
library(ggplot2)
library(countrycode)
library(tidyr)

source("R/isced.R")
source("R/theme.R")
theme_set(theme_custom())

# ----------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------

results_dir <- file.path("data", "results")
dir.create(results_dir)

academic_path <- file.path("data", "raw", "tertiary_academic_staff_gender_OECD.csv")
data_path <- file.path("data", "processed", "os_final.rds")
phd_us_path <- file.path("data", "raw", "phd_counts_by_field_year_US_isced.csv")

# ----------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------

clean_field <- function(x) {
    x <- gsub("[,]+", "", tolower(x))
    gsub(" .icts.", "", x)
}

# ----------------------------------------------------------------------
# Load OS syllabi dataset
# ----------------------------------------------------------------------

syllabi <- readRDS(data_path)

# ----------------------------------------------------------------------
# U.S. PhD by field – clean & aggregate
# ----------------------------------------------------------------------

phd_us_raw <- read.csv(phd_us_path)

phd_us <- phd_us_raw %>%
    dplyr::select(country, year, field = field_name, women, men) %>%
    mutate(field = clean_field(field)) %>% 
    summarise(
        women = sum(women),
        men = sum(men),
        .by = c(country, field, year)
    )

# ----------------------------------------------------------------------
# OECD staff gender ratios
# ----------------------------------------------------------------------

gender_ratio_oecd <- read.csv(academic_path) %>%
    mutate(country = countrycode(country, "iso3c", "iso2c")) %>% 
    dplyr::select(country, year, men, women)


# ----------------------------------------------------------------------
# OpenSyllabus gender ratios (overall, no field)
# ----------------------------------------------------------------------

gender_ratio_os <- count(syllabi, country, team, year) %>% 
    dplyr::mutate(
        men = nchar(gsub("[f]+", "", team)) * n,
        women = nchar(gsub("[m]+", "", team)) * n,
    ) %>% 
    dplyr::summarise(
        men = sum(men), women = sum(women), 
        .by = c(country, year)
    )

gender_ratio <- gender_ratio_os %>% 
    inner_join(
        gender_ratio_oecd,
        by = c("country", "year"),
        suffix = c("_OS", "_OECD")
    ) %>%
    dplyr::mutate(
        share_f_OS    = women_OS / (women_OS + men_OS),
        share_f_OECD  = women_OECD / (women_OECD + men_OECD),
        ratio_OS      = women_OS / men_OS,
        ratio_OECD    = women_OECD / men_OECD
    )


# ----------------------------------------------------------------------
# Correlations by country
# ----------------------------------------------------------------------

corr_by_country <- gender_ratio %>%
    group_by(country) %>%
    summarise(
        corr = cor(
            ratio_OS, ratio_OECD, 
            use = "complete.obs", 
            method = "pearson"
        )
    )

# ----------------------------------------------------------------------
# Plot (overall)
# ----------------------------------------------------------------------

plot_data <- gender_ratio %>%
    filter(year > 1999) %>%
    dplyr::select(country, year, ratio_OS, ratio_OECD) %>%
    tidyr::pivot_longer(
        cols = starts_with("ratio"),
        names_to = "source",
        values_to = "ratio"
    ) %>%
    left_join(corr_by_country, by = "country") %>%
    dplyr::mutate(
        data_source = ifelse(source == "ratio_OS", "OpenSyllabus (OS)", "Eurostat"),
        country = countrycode(country, "iso2c", "country.name"),
    )


p_all <- plot_data %>%
    ggplot(aes(
        x = year,
        y = ratio,
        linetype = data_source,
        color = data_source
    )) +
    geom_line(linewidth = 0.7) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    facet_wrap(~ country) +
    scale_y_continuous(
        limits = c(0, 2),
        breaks = seq(0, 2, by = 0.5),
        expand = expansion(mult = c(0.02, 0.08))
    ) +
    scale_linetype_manual(values = c("solid", "longdash")) +
    labs(
        y = "Female–Male Ratio",
        x = "Year",
        title = "Female–Male Ratio of Instructors vs Academic Staff in OECD Data",
        subtitle = "Pearson correlation between OS and Eurostat series shown in each panel",
        color = "Source",
        linetype = "Source",
    ) +
    geom_text(
        data = plot_data %>% distinct(country, corr) %>% mutate(data_source = "Eurostat"),
        aes(
            x = -Inf,  # left
            y = Inf,   # top
            label = paste0("ρ = ", sprintf("%.2f", corr))
        ),
        hjust = -0.1, vjust = 1.2, size = 3.3, color = "black"
    ) +
    theme(
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        panel.grid.minor = element_blank(),
    )

pdf_filename <- file.path(results_dir, "fig_representative_eurostat.pdf")
out <- ggsave(pdf_filename)
system(paste("open", out))

# ----------------------------------------------------------------------
# Field-level comparison
# ----------------------------------------------------------------------

gender_ratio_os <- syllabi %>%
    count(country, team, year, field) %>% 
    mutate(
        field = clean_field(recode_field(as.character(field))),
    ) %>%
    count(country, team, year, field, wt = n) %>%
    dplyr::mutate(
        men = nchar(gsub("[f]+", "", team)) * n,
        women = nchar(gsub("[m]+", "", team)) * n,
    ) %>% 
    dplyr::summarise(
        men = sum(men), women = sum(women), 
        .by = c(country, year, field)
    ) 


setdiff(
    count(gender_ratio_os, field)$field,
    count(phd_us, field)$field
)

gender_ratio <- gender_ratio_os %>% 
    inner_join(
        phd_us, by = c("country", "year", "field"),
        suffix = c("_OS", "_OECD")
    ) %>%
    dplyr::mutate(
        share_f_OS    = women_OS / (women_OS + men_OS),
        share_f_OECD  = women_OECD / (women_OECD + men_OECD),
        ratio_OS      = women_OS / men_OS,
        ratio_OECD    = women_OECD / men_OECD
    )


corr_by_field <- gender_ratio %>%
    group_by(field) %>%
    summarise(
        corr = cor(ratio_OS, ratio_OECD, use = "complete.obs")
    )

# ----------------------------------------------------------------------
# Plot (by field)
# ----------------------------------------------------------------------


plot_data <- gender_ratio %>%
    filter(year > 1999) %>%
    dplyr::select(country, year, field, ratio_OS, ratio_OECD) %>%
    tidyr::pivot_longer(
        cols = starts_with("ratio"),
        names_to = "source",
        values_to = "ratio"
    ) %>%
    left_join(corr_by_field, by = "field") %>%
    dplyr::mutate(
        source = ifelse(source == "ratio_OS", "OpenSyllabus (OS)", "NCES"),
        country = countrycode(country, "iso2c", "country.name"),
    )


p_field <- plot_data %>%
    ggplot(aes(x = year, y = ratio, color = source, linetype = source)) +
    geom_line(linewidth = 0.7) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    facet_wrap(~ stringr::str_wrap(field, 30)) +
    scale_y_continuous(
        limits = c(0, 3),
        breaks = seq(0, 2, by = 0.5),
        expand = expansion(mult = c(0.02, 0.08))
    ) +
    scale_linetype_manual(values = c("solid", "longdash")) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
    labs(
        y = "Female–Male Ratio",
        title = "Female–Male Ratio of Instructors in OpenSyllabus (OS) vs PhD Graduates in the US",
        subtitle = "Pearson correlation between OS and NCES series shown in each panel",
        color = "Source", linetype = "Source"
    ) +
    geom_text(
        data = plot_data %>% distinct(field, corr) %>% mutate(source = "NCES"),
        aes(
            x = -Inf,  # left
            y = Inf,   # top
            label = paste0("ρ = ", sprintf("%.2f", corr))
        ),
        hjust = -0.1, vjust = 1.2, size = 3.3, color = "black"
    ) +
    theme(
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        panel.grid.minor = element_blank()
    )

pdf_filename <- file.path(results_dir, "fig_representative_field.pdf")
out <- ggsave(pdf_filename)
system(paste("open", out))


# ----------------------------------------------------------------------
# Save plots
# ----------------------------------------------------------------------

figures <- list(
    all = p_all,
    by_field = p_field
)
saveRDS(figures, file = file.path(results_dir, "figs_representative.rds"))
