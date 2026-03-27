# ----------------------------------------------------------------------
# Robustness analysis
# ----------------------------------------------------------------------

library(dplyr)
library(lme4)
library(broom.mixed)
library(knitr)
library(parallel)
library(patchwork)
library(ggplot2)

source("R/paths.R")
source("R/isced.R")
source("R/theme.R")
source("R/utils.R")
source("R/labels.R")

if (exists("theme_custom")) {
    theme_set(theme_custom())
} else {
    theme_set(theme_minimal())
}

# ----------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------

results_dir <- file.path("data", "results")
data_dir <- file.path("data", "processed")

syllabi_path <- file.path(data_dir, "os_final.rds")
novel_path <- file.path(data_dir, "novel_v2.rds")

# ----------------------------------------------------------------------
# Utils
# ----------------------------------------------------------------------
log_msg <- function(fmt, ...) {
    message(sprintf(fmt = fmt, ...))
}

center <- function(x) {
    as.numeric(scale(x, scale = FALSE, center = TRUE))
}

rank_percentile <- function(x) {
    stopifnot(all(!is.na(x)))
    100 * (rank(x) - 1) / (length(x) - 1)
}

term_labels <- c(
    "teamf" = "Woman (F)",
    "teamff" = "Two women (FF)",
    "teamm" = "Man (M)",
    "teammm" = "Two men (MM)",
    "teamfm" = "Mixed-gender (F/M)"
)

# ----------------------------------------------------------------------
# Load data
# ----------------------------------------------------------------------
syllabi <- readRDS(syllabi_path)
log_msg("Loaded syllabi: %d rows, %d cols", nrow(syllabi), ncol(syllabi))
#str(syllabi)

novelty <- readRDS(novel_path)
log_msg("Loaded novelty: %d rows, %d cols", nrow(novelty), ncol(novelty))
#str(novelty)

# ----------------------------------------------------------------------
# Merge data
# ----------------------------------------------------------------------

syllabi_merged <- syllabi %>%
    left_join(dplyr::select(novelty, -year), by = "id") %>% 
    left_join(isced_lookup, by = "field") %>%
    mutate(
        recency = year - novelty,
        team = relevel(factor(as.character(team)), ref = "m")
    )

#summary(syllabi_merged)

stopifnot(nrow(syllabi_merged) == nrow(syllabi))

# ----------------------------------------------------------------------
#  Function to fit model by year
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
#  Interdisciplinarity --- Separate regressions by year
# ----------------------------------------------------------------------

interdisc <- syllabi_merged %>%
    filter(!is.na(mean_intdisc)) %>%
    mutate(
        interdisc = rank_percentile(mean_intdisc),
        .by = c(year)
    )

fit <- lme4::lmer(
    interdisc ~ team + country + year + course_level + 
        scale(log(tot_count)) + scale(prob) + stem + (1 | field) + (1 | institution),
    data = interdisc
)

coeffs <- list(fit) %>% 
    lapply(broom::tidy, conf.int = TRUE) %>% 
    dplyr::bind_rows(.id = "model")

p_coeffs <- coeffs %>% 
    filter(grepl("team", term)) %>%
    ggplot(
        aes(
            x = estimate,
            y = term,
            xmin = conf.low,
            xmax = conf.high
        )
    ) +
    geom_vline(aes(linetype = "Men alone (M)", xintercept = 0), color = "red") +
    scale_linetype_manual(values = c("Men alone (M)" = "dashed")) +
    geom_errorbar(width = 0.1) +
    geom_point() +
    scale_y_discrete(labels = term_labels) +
    labs(
        x = "Estimated coefficient (95% CIs)", y = NULL
    )

out <- ggsave(file.path(results_dir, "robust_interdisc.pdf"))
system(paste("open", out))

# ----------------------------------------------------------------------
#  Women authors --- Separate regressions by year
# ----------------------------------------------------------------------

women <- syllabi_merged %>%
    filter(!is.na(mean_intdisc)) %>%
    mutate(
        total_authors = female_authors + male_authors,
        female_ratio = (female_authors + 1) / (male_authors + female_authors + 2),
        .by = c(year)
    )

fit <- women %>%
    lme4::lmer(
        data = .,
        female_ratio ~ team + country + year + course_level + 
            scale(log(tot_count)) + scale(prob) + stem + (1 | field) + (1 | institution)
    )

coeffs <- list(fit) %>% 
    lapply(broom::tidy, conf.int = TRUE) %>% 
    dplyr::bind_rows(.id = "model")

p_left <- coeffs %>% 
    filter(grepl("team", term)) %>%
    ggplot(
        aes(
            x = estimate,
            y = term,
            xmin = conf.low,
            xmax = conf.high
        )
    ) +
    geom_vline(aes(linetype = "Men alone (M)", xintercept = 0), color = "red") +
    scale_linetype_manual(values = c("Men alone (M)" = "dashed")) +
    geom_errorbar(width = 0.1) +
    geom_point() +
    scale_x_continuous(labels = scales::percent) +
    scale_y_discrete(labels = term_labels) +
    labs(
        x = "Estimated coefficient (95% CIs)", y = NULL
    )


model <- female_authors ~ team + year + country + field + offset(log(total_authors))

fit <- glm(model, family = quasipoisson, data = women, subset = total_authors > 0)
#summary(fit)$coef

p_right <- tidy(fit) %>% 
    filter(grepl("team", term)) %>%
    ggplot(
        aes(
            x = estimate,
            y = term,
            xmin = estimate - 2 * std.error,
            xmax = estimate + 2 * std.error
        )
    ) +
    geom_vline(aes(linetype = "Men alone (M)", xintercept = 0), color = "red") +
    scale_y_discrete(labels = term_labels) +
    geom_errorbar(width = 0.1) +
    geom_point() +
    labs(
        x = "Estimate (95% confidence)",
        y = NULL
    )

p_combined <- (p_left + p_right) + plot_annotation(tag_levels = "A")

out <- ggsave(file.path(results_dir, "fig_robustness_women.pdf"), width = 7, height = 4)
system(paste("open", out))


# ----------------------------------------------------------------------
#  Conventionality --- Separate regressions by year
# ----------------------------------------------------------------------

conventional <- syllabi_merged %>% 
    filter(!is.na(novel_med)) %>%
    mutate(
        conventional = rank_percentile(novel_med),
        conventional_log = log(novel_med),
        .by = c(year)
    )

fit <- conventional %>%
    lme4::lmer(
        data = .,
        conventional ~ team + country + year + course_level + 
            scale(log(tot_count)) + scale(prob) + stem + (1 | field) + (1 | institution)
    )

fit2 <- update(fit, conventional_log ~ .)

coeffs <- list("pr" = fit, "log" = fit2) %>% 
    lapply(broom::tidy, conf.int = TRUE) %>% 
    dplyr::bind_rows(.id = "model")

p_coeffs <- coeffs %>% 
    filter(grepl("team", term), model == "pr") %>%
    ggplot(
        aes(
            x = estimate,
            y = term,
            xmin = conf.low,
            xmax = conf.high
        )
    ) +
    geom_vline(aes(linetype = "Men alone (M)", xintercept = 0), color = "red") +
    scale_linetype_manual(values = c("Men alone (M)" = "dashed")) +
    geom_errorbar(width = 0.1) +
    geom_point() +
    scale_y_discrete(labels = term_labels) +
    labs(
        x = "Estimated coefficient (95% CIs)", y = NULL
    )


p_left <- p_coeffs
p_right <- p_coeffs %+% filter(coeffs, grepl("team", term), model == "log")
p_both <- (p_left + p_right) + plot_annotation(tag_levels = "A")

out <- ggsave(file.path(results_dir, "robust_conventional_log.pdf"), width = 7, height = 4)
system(paste("open", out))


# ----------------------------------------------------------------------
#  Atypicality --- Separate regressions by year
# ----------------------------------------------------------------------

atypical <- syllabi_merged %>%
    filter(!is.na(atyp_med), atyp_med != Inf) %>%
    mutate(
        atypical = rank_percentile(atyp_med),
        atypical_log = ifelse(atyp_med > 0, atyp_med, 1) %>% log(),
        .by = c(year)
    )

fit <- atypical %>%
    lme4::lmer(
        data = .,
        atypical ~ team + country + year + course_level + 
            scale(log(tot_count)) + scale(prob) + stem + (1 | field) + (1 | institution)
    )

fit2 <- update(fit, atypical_log ~ .)

coeffs <- list("pr" = fit, "log" = fit2) %>% 
    lapply(broom::tidy, conf.int = TRUE) %>% 
    dplyr::bind_rows(.id = "model")

p_coeffs <- coeffs %>% 
    #filter(transform == "pr") %>%
    filter(model == "pr") |>
    filter(grepl("team", term)) %>%
    ggplot(
        aes(
            x = estimate,
            y = term,
            xmin = conf.low,
            xmax = conf.high
        )
    ) +
    geom_vline(aes(linetype = "Men alone (M)", xintercept = 0), color = "red") +
    scale_linetype_manual(values = c("Men alone (M)" = "dashed")) +
    geom_errorbar(width = 0.1) +
    geom_point() +
    scale_y_discrete(labels = term_labels) +
    labs(
        x = "Estimated coefficient (95% CIs)", y = NULL
    )

p_left <- p_coeffs
p_right <- p_coeffs %+% filter(coeffs, model == "log", grepl("team", term)) 
p_both <- (p_left + p_right) + plot_annotation(tag_levels = "A")


out <- ggsave(file.path(results_dir, "robust_atypical.pdf"), width = 7, height = 4)
system(paste("open", out))


# ----------------------------------------------------------------------
#  Age of readings --- Separate regressions by year
# ----------------------------------------------------------------------

age_readings <- syllabi_merged %>%
    filter(!is.na(recency)) %>%
    mutate(
        age_readings = rank_percentile(recency),
        age_readings_log = ifelse(recency <= 0, 1, recency) %>% log(),
        .by = c(year)
    )

fit <- age_readings %>%
    lme4::lmer(
        data = .,
        age_readings ~ team + country + year + course_level + 
            scale(log(tot_count)) + scale(prob) + stem + (1 | field) + (1 | institution)
    )

fit2 <- update(fit, age_readings_log ~ .)


coeffs <- list("pr" = fit, "log" = fit2) %>% 
    lapply(broom::tidy, conf.int = TRUE) %>% 
    dplyr::bind_rows(.id = "model")

p_coeffs <- coeffs %>% 
    filter(grepl("team", term), model == "pr") %>%
    ggplot(
        aes(
            x = estimate,
            y = term,
            xmin = conf.low,
            xmax = conf.high
        )
    ) +
    geom_vline(aes(linetype = "Men alone (M)", xintercept = 0), color = "red") +
    scale_linetype_manual(values = c("Men alone (M)" = "dashed")) +
    geom_errorbar(width = 0.1) +
    geom_point() +
    scale_y_discrete(labels = term_labels) +
    labs(
        x = "Estimated coefficient (95% CIs)", y = NULL
    )


p_left <- p_coeffs
p_right <- p_coeffs %+% filter(coeffs, grepl("team", term), model == "log")
p_both <- (p_left + p_right) + plot_annotation(tag_levels = "A")

out <- ggsave(file.path(results_dir, "robust_age_readings_log.pdf"), width = 7, height = 4)
system(paste("open", out))
