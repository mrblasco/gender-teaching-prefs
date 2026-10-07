library(dplyr)
library(ggplot2)
library(patchwork)

source("R/theme.R")
theme_set(theme_custom())

# Load 
filename <- "data/processed/os_final.rds"
logger::log_info("Loding data from {filename}")
ds <- readRDS(filename)
logger::log_info("Data loaded: {nrow(ds)} x {ncol(ds)}")


# 1. Investigate the 2008 mixed-gender peak
#   - institution, field
#   - denominator
#   - breakdown fm/mf

uniq_year <- ds |>
    summarise(
        n_field = n_distinct(field),
        n = n_distinct(institution), 
        .by = year
    )

p_field_year <- uniq_year |>
    ggplot(aes(year, n_field)) +
    geom_line() + 
    geom_point() + 
    labs(
        x = "Year",
        y = "Academic field"
    )

p_inst_year <- uniq_year  |>
    ggplot(aes(year, n / 1e3)) +
    geom_line() + 
    geom_point() + 
    labs(
        x = "Year",
        y = "Institutions (thousands)"
    )

p_year <- ds |>
    count(year) |>
    ggplot(aes(year, n / 1e3)) +
    geom_line() + 
    geom_point() + 
    labs(
        x = "Year",
        y = "Syllabi (thousands)"
    )


p_count <- count(ds, team, year) |>
    ggplot(aes(year, n/1e3, color = team, shape = team)) +
    geom_line() +
    geom_point() + 
    scale_y_log10() + 
    labs(
        y = "Teams (thousands, log-scale)",
        x = "Year"
    )

p_count + p_inst_year + p_year + plot_annotation(tag_levels = "A")

# Bottom line: the number of institutions between 2005 and 2011 is mostly stable, the number of syllabi is steadily increasing with no peak, the number of fields is also constant over time with all 69 fields present in each year. All team configurations are also increasing steadily, except for mixed teams that appear to plattau between 2005 and 2008, to continue grow in later years at a slower rate. Thus, the increasing of other teams and theconcomitant plattau of mixed teams is what generates the peak between 2005 and 2008. This doesnot appear an artifact of the data, but a situational trend for which we don't have any solid explanation.  