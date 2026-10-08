# ======================================================================
# Heterogeneity + age/cohort confound --- PLOTTING ONLY
# ----------------------------------------------------------------------
# Reads the coefficient tables produced by scripts/09_fit_heterogeneity.R
# and draws every figure for the "Heterogeneity" and "Age and seniority
# composition" subsections, plus the derived tables and summary. No model
# is fitted here, so this script is instant and safe to re-run while
# iterating on figure styling.
#
# Inputs  (from <fit_dir>, default output/09_fit_heterogeneity/):
#   coeffs_heterogeneity.rds   full/STEM/US per-year team coeffs
#   coeffs_leading.rds         leading-instructor per-year coeffs
#   age_t1_coeffs_by_level.rds team coeffs by course level
#   age_t2_gap_vs_share.rds    per-year level-adjusted gaps
#   age_solo_share_by_level.csv, age_female_share_by_year.csv
#
# Outputs (to <out_dir>, default output/09_heterogeneity_analysis/ so the
# manuscript figure paths stay unchanged):
#   supp_heterogeneity_stem.pdf, supp_heterogeneity_us.pdf,
#   supp_leading_instructor.pdf
#   age_confound/t1_fig_gap_by_level.pdf, t1_attenuation.csv
#   age_confound/t2_fig_gap_vs_share.pdf, t2_gap_share_correlation.csv
#   summary.txt
#
# Usage:  Rscript scripts/10_plot_heterogeneity.R <out_dir> <fit_dir>
# ======================================================================

suppressMessages({
    library(dplyr, warn.conflicts = FALSE)
    library(tidyr)
    library(ggplot2)
    library(readr)
})

source(here::here("R/theme.R")) # theme_custom()
theme_set(theme_custom())

# ----------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) {
    args[1]
} else {
    here::here("output", "09_heterogeneity_analysis")
}
fit_dir <- if (length(args) >= 2) {
    args[2]
} else {
    here::here("output", "09_fit_heterogeneity")
}

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
age_dir <- file.path(out_dir, "age_confound")
dir.create(age_dir, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(file.path(fit_dir, "coeffs_heterogeneity.rds"))) {
    stop("Fit artefacts not found in '", fit_dir,
         "'. Run scripts/09_fit_heterogeneity.R first.")
}

summary_lines <- character(0)
say <- function(fmt, ...) {
    line <- sprintf(fmt, ...)
    message(line)
    summary_lines <<- c(summary_lines, line)
}

# ----------------------------------------------------------------------
# Labels
# ----------------------------------------------------------------------
term_labels <- c(
    teamf  = "Single female",
    teamff = "Female–female team",
    teammm = "Male–male team",
    teamfm = "Mixed team",
    teammf = "Mixed team (male leading)"
)

outcome_labels <- c(
    "Interdisciplinarity", "Conventionality", "Age of readings",
    "Female-authored ratio", "Atypicality"
)

age_term_labels <- c(
    "teamf"  = "Woman (F)",
    "teamff" = "Two women (FF)",
    "teamm"  = "Man (M)",
    "teammm" = "Two men (MM)",
    "teamfm" = "Mixed-gender (F/M)"
)

# ----------------------------------------------------------------------
# Load fitted coefficients
# ----------------------------------------------------------------------
say("Reading coefficient tables from %s", fit_dir)
coeffs         <- readRDS(file.path(fit_dir, "coeffs_heterogeneity.rds"))
coeffs_leading <- readRDS(file.path(fit_dir, "coeffs_leading.rds"))
t1_coeffs      <- readRDS(file.path(fit_dir, "age_t1_coeffs_by_level.rds"))
t2_gaps        <- readRDS(file.path(fit_dir, "age_t2_gap_vs_share.rds"))
solo_share     <- read_csv(file.path(fit_dir, "age_solo_share_by_level.csv"),
                           show_col_types = FALSE)
female_share_by_year <- read_csv(
    file.path(fit_dir, "age_female_share_by_year.csv"), show_col_types = FALSE)

# ======================================================================
# BLOCK A --- HETEROGENEITY FIGURES
# ======================================================================

# One row of panels per outcome, one column per contrast. The y-axis is
# freed per outcome row because the five measures live on very different
# scales (e.g. the female-author ratio spans several points while
# interdisciplinarity spans ~1); a shared axis flattens the small-scale
# rows and hides the curves.
hetero_panel <- function(df, highlight) {
    df <- df %>%
        mutate(
            group = factor(group, levels = c("All", highlight)),
            term  = factor(term, levels = names(term_labels))
        )
    ggplot(
        df,
        aes(x = as.numeric(year), y = estimate, fill = group, shape = group, color = group)
    ) +
        geom_hline(
            aes(yintercept = 0, linetype = "dashed"),
            color = "red"
        ) +
        facet_grid(outcome ~ term, labeller = labeller(term = term_labels),
                   scales = "free_y") +
        geom_pointrange(
            aes(ymin = conf.low, ymax = conf.high)
        ) +
        scale_color_discrete() +
        scale_shape_manual(values = c(2, 21)) +
        labs(x = "Academic year", y = NULL, color = NULL, fill = NULL, shape = NULL) +
        guides(linetype = "none") +
        theme(legend.position = "bottom")
}

# STEM vs full sample --------------------------------------------------
stem_df <- coeffs %>%
    filter(grepl("^team", term), !grepl("_us", model)) %>%
    mutate(group = ifelse(grepl("_stem", model), "STEM", "All"))

ggsave(
    file.path(out_dir, "supp_heterogeneity_stem.pdf"),
    hetero_panel(stem_df, "STEM"),
    device = cairo_pdf, width = 9, height = 11, units = "in"
)
say("Figure: supp_heterogeneity_stem.pdf")

# US vs full sample ----------------------------------------------------
us_df <- coeffs %>%
    filter(grepl("^team", term), !grepl("_stem", model)) %>%
    mutate(group = ifelse(grepl("_us", model), "US", "All"))

ggsave(
    file.path(out_dir, "supp_heterogeneity_us.pdf"),
    hetero_panel(us_df, "US"),
    device = cairo_pdf, width = 9, height = 11, units = "in"
)
say("Figure: supp_heterogeneity_us.pdf")

# Leading-instructor contrast ------------------------------------------
leading_df <- coeffs_leading %>%
    mutate(code = sub("^team_ordered", "", term)) %>%
    filter(code %in% c("fm", "mf")) %>%
    mutate(lead = recode(code, fm = "Female leading", mf = "Male leading"))

p_leading <- leading_df %>%
    mutate(outcome = factor(outcome, levels = outcome_labels)) %>%
    ggplot(aes(x = as.numeric(year), y = estimate, color = lead, fill = lead)) +
    geom_hline(
        aes(yintercept = 0, linetype = "Man alone (M)"),
        color = "red"
    ) +
    scale_linetype_manual(values = "dashed") +
    geom_smooth(
        method = "gam",
        formula = y ~ s(x, bs = "cs"),
        aes(weight = 1 / std.error^2),
        alpha = 0.18,
        linewidth = 0.6
    ) +
    facet_wrap(~outcome, scales = "free_y", ncol = 1) +
    labs(x = "Academic year",
         y = "Difference vs single-male teams",
         color = NULL, fill = NULL) +
    guides(linetype = "none") +
    theme(legend.position = "bottom")

ggsave(
    file.path(out_dir, "supp_leading_instructor.pdf"),
    p_leading, device = cairo_pdf, width = 7, height = 10, units = "in"
)
say("Figure: supp_leading_instructor.pdf")

# ======================================================================
# BLOCK B --- AGE / COHORT CONFOUND FIGURES & DERIVED TABLES
# ======================================================================

say("")
say("== Age confound, Task 1: female share among solo courses, by level ==")
for (i in seq_len(nrow(solo_share))) {
    say("   %-9s  female solo share = %4.1f%%  (n = %s)",
        as.character(solo_share$course_level[i]),
        solo_share$female_share[i],
        format(solo_share$n[i], big.mark = ","))
}

# Attenuation table: how much of the pooled gap is retained within level?
attn <- t1_coeffs %>%
    select(depvar, outcome, term, course_level, estimate) %>%
    pivot_wider(names_from = course_level, values_from = estimate) %>%
    mutate(
        ret_basic    = 100 * basic    / pooled,
        ret_advanced = 100 * advanced / pooled,
        ret_graduate = 100 * graduate / pooled
    )
write_csv(attn, file.path(age_dir, "t1_attenuation.csv"))

say("")
say("== Age confound, Task 1: within-level retention of the gap (%% of pooled) ==")
say("   (100%% = gap unchanged within level; ~0%% = gap explained by level)")
key_terms <- c("teamf", "teamfm", "teamff")
for (dv in unique(attn$depvar)) {
    sub <- attn %>% filter(depvar == dv, term %in% key_terms)
    for (i in seq_len(nrow(sub))) {
        say("   %-32s %-7s  basic %5.0f%%  adv %5.0f%%  grad %5.0f%%  (pooled est=%.3f)",
            sub$outcome[i], sub$term[i],
            sub$ret_basic[i], sub$ret_advanced[i], sub$ret_graduate[i],
            sub$pooled[i])
    }
}

# Task 1 figure --------------------------------------------------------
t1_plot_df <- t1_coeffs %>%
    filter(term %in% c("teamf", "teamff", "teamfm", "teammm")) %>%
    mutate(
        course_level = factor(course_level,
                              levels = c("pooled", "basic", "advanced", "graduate")),
        term = factor(term, levels = names(age_term_labels))
    )

p_t1 <- ggplot(
    t1_plot_df,
    aes(x = course_level, y = estimate, ymin = conf.low, ymax = conf.high,
        color = course_level)
) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_pointrange(linewidth = 0.4) +
    facet_grid(outcome ~ term, labeller = labeller(term = age_term_labels),
               scales = "free_y") +
    labs(x = NULL, y = "Difference vs. man alone (M)",
         title = "Team gender gaps within course level",
         subtitle = "If gaps were a seniority-via-course-type artifact, within-level estimates would collapse toward 0") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "none")

ggsave(file.path(age_dir, "t1_fig_gap_by_level.pdf"), p_t1,
       width = 11, height = 12)
say("")
say("Figure: age_confound/t1_fig_gap_by_level.pdf")

# Task 2: correlation of level-adjusted gap with female share ----------
t2_cor <- t2_gaps %>%
    group_by(outcome, depvar, term) %>%
    summarise(
        cor_gap_share = cor(estimate, female_share),
        slope_per_pp  = coef(lm(estimate ~ female_share))[2],
        .groups = "drop"
    )
write_csv(t2_cor, file.path(age_dir, "t2_gap_share_correlation.csv"))

say("")
say("== Age confound, Task 2: correlation of level-adjusted gap with female share ==")
for (i in seq_len(nrow(t2_cor))) {
    say("   %-32s %-7s  cor = %+.2f", t2_cor$outcome[i], t2_cor$term[i],
        t2_cor$cor_gap_share[i])
}

# Task 2 figure --------------------------------------------------------
p_t2 <- ggplot(t2_gaps, aes(x = year, y = estimate,
                            ymin = conf.low, ymax = conf.high)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_ribbon(alpha = 0.15) +
    geom_line() +
    geom_point(size = 0.8) +
    facet_grid(outcome ~ term, labeller = labeller(term = age_term_labels),
               scales = "free_y") +
    labs(x = "Year", y = "Level-adjusted gap vs. man alone (M)",
         title = "Level-adjusted gender gaps over time",
         subtitle = "Compare stability of the gap against the rising female share (see t2_gap_vs_share.csv)") +
    theme(legend.position = "none")

ggsave(file.path(age_dir, "t2_fig_gap_vs_share.pdf"), p_t2,
       width = 8, height = 12)
say("Figure: age_confound/t2_fig_gap_vs_share.pdf")

# ----------------------------------------------------------------------
# Write plain-text summary
# ----------------------------------------------------------------------
writeLines(summary_lines, file.path(out_dir, "summary.txt"))

message("")
message("Done plotting. Figures in ", out_dir)
