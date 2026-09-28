# ======================================================================
# Associations
# ======================================================================
#   Regression analysis
#
#   Updated: September 2026
# -----------------------------------------------------------------
suppressMessages({
    library(dplyr)
    library(tidyr)
    library(lme4)
    library(broom.mixed)
    library(ggplot2)
})

source("R/utils.R")   # rank_percentile(), log_msg()
source("R/theme.R")   # theme_custom()
theme_set(theme_custom())

set.seed(4881)

data_dir    <- file.path("data", "processed")
out_dir     <- file.path("data", "results", "associations")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

say <- logger::log_info 

term_labels <- c(
    "teamf"  = "Woman (F)",
    "teamff" = "Two women (FF)",
    "teamm"  = "Man (M)",
    "teammm" = "Two men (MM)",
    "teamfm" = "Mixed-gender (F/M)"
)

# ----------------------------------------------------------------------
# Load & prepare data (mirrors scripts/07_revision_round3.R)
# ----------------------------------------------------------------------
say("Loading data ...")
syllabi_merged <- readRDS(file.path(data_dir, "syllabi_merged.rds"))
say("Loaded {format(nrow(syllabi_merged), big.mark = ',')} rows")

# Percentile-rank the outcomes WITHIN year
analysis <- syllabi_merged %>%
    filter(!is.na(mean_intdisc)) %>%
    mutate(
        age_readings = rank_percentile(recency),
        atypical     = rank_percentile(atyp_med),
        conventional = rank_percentile(novel_med),
        interdisc    = rank_percentile(mean_intdisc),
        total_authors = female_authors + male_authors,
        female_ratio  = (female_authors + 1) / (male_authors + female_authors + 2),
        .by = c(year)
    ) %>%
    mutate(
        team = relevel(factor(as.character(team)), ref = "m"),

        # Reviewer 3 / drop unknwon
        course_level = as.character(course_level),
        course_level = ifelse(course_level == "unknown", NA, course_level),
        course_level = factor(course_level,
                              levels = c("basic", "advanced", "graduate"))
    )

say("Analysis rows (non-missing interdisciplinarity): {nrow(analysis)}")

outcomes <- c(
    interdisc    = "Interdisciplinarity",
    female_ratio = "Female-authored ratio",
    age_readings = "Age of readings (recency)",
    conventional = "Conventionality",
    atypical     = "Scientific novelty (atypicality)"
)

# ======================================================================
# TASK 1  --- Does the gender gap survive WITHIN course level?
# ======================================================================
# Strategy: for each outcome, fit ONE pooled mixed model that interacts
# team x course_level, so we recover the team contrasts separately within
# basic / advanced / graduate. Compare against the level-pooled model
# (team main effect only). If the reviewer is right, the within-level
# team contrasts should shrink toward zero relative to the pooled ones.
#
# We estimate on the full sample (all years) with year as a random
# intercept, rather than per-year, to keep the interaction model
# tractable and directly comparable across levels.
# ----------------------------------------------------------------------

# Descriptive: female / team composition by course level ----------------
comp_by_level <- analysis %>%
    filter(!is.na(course_level)) %>%
    count(course_level, team) %>%
    group_by(course_level) %>%
    mutate(pct = 100 * n / sum(n)) %>%
    ungroup()

solo_share <- analysis %>%
    filter(team %in% c("m", "f"), !is.na(course_level)) %>%
    group_by(course_level) %>%
    summarise(female_share = 100 * mean(team == "f"), n = n(), .groups = "drop")

write.csv(comp_by_level, file.path(out_dir, "t1_composition_by_level.csv"),
          row.names = FALSE)


# Model fitting ---------------------------------------------------------
# Base controls follow scripts/07_revision_round3.R but replace per-year
# regressions with a single pooled model + (1|year).
base_rhs <- "country + scale(prob) + stem + (1 | field) + (1 | institution) + (1 | year) + scale(log(tot_count))"

fit_one <- function(depvar, interact) {
    rhs_team <- if (interact) "team * course_level" else "team + course_level"
    form <- as.formula(sprintf("%s ~ %s + %s", depvar, rhs_team, base_rhs))
    df <- analysis %>% filter(!is.na(course_level), !is.na(.data[[depvar]]))
    t0 <- Sys.time()
    fit <- lme4::lmer(form, data = df, weights = sqrt(matched_docs),
                      control = lmerControl(optimizer = "bobyqa",
                                            calc.derivs = FALSE))
    say("   fitted %-13s interact=%-5s  n=%s  (%.1f min)",
        depvar, interact, format(nrow(df), big.mark = ","),
        as.numeric(difftime(Sys.time(), t0, units = "mins")))
    fit
}

# Recover team contrasts WITHIN each course level from the interaction
# model, using emmeans-style linear combinations computed by hand from
# fixef + vcov (avoids adding a new package dependency).
team_within_level <- function(fit) {
    fe  <- fixef(fit)
    V   <- as.matrix(vcov(fit))
    nm  <- names(fe)
    teams   <- c("teamf", "teamff", "teamfm", "teammm")
    levels3 <- c("basic", "advanced", "graduate")
    # course_level dummies present in the model (basic is reference)
    lvl_terms <- c(advanced = "course_leveladvanced",
                   graduate = "course_levelgraduate")
    res <- list()
    for (lv in levels3) {
        for (tm in teams) {
            v <- setNames(rep(0, length(fe)), nm)
            if (tm %in% nm) v[tm] <- 1
            if (lv != "basic") {
                ix <- paste0(tm, ":", lvl_terms[[lv]])
                if (ix %in% nm) v[ix] <- 1
            }
            est <- sum(v * fe)
            se  <- sqrt(as.numeric(t(v) %*% V %*% v))
            res[[length(res) + 1]] <- data.frame(
                course_level = lv, term = tm,
                estimate = est, std.error = se,
                conf.low = est - 1.96 * se, conf.high = est + 1.96 * se
            )
        }
    }
    bind_rows(res)
}

# Pooled (level-adjusted, no interaction) team main effects.
team_pooled <- function(fit) {
    broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE) %>%
        filter(grepl("^team", term), !grepl(":", term)) %>%
        transmute(course_level = "pooled", term,
                  estimate, std.error, conf.low, conf.high)
}


out <- fit_one("interdisc", interact = FALSE)


say("")
say("== Task 1: fitting pooled and interaction models ==")
t1_list <- list()
for (dv in names(outcomes)) {
    fit_pool <- fit_one(dv, interact = FALSE)
    fit_int  <- fit_one(dv, interact = TRUE)
    t1_list[[dv]] <- bind_rows(
        team_pooled(fit_pool),
        team_within_level(fit_int)
    ) %>% mutate(depvar = dv, outcome = outcomes[[dv]])
}
t1_coeffs <- bind_rows(t1_list)

saveRDS(t1_coeffs, file.path(out_dir, "t1_coeffs_by_level.rds"))
write.csv(t1_coeffs, file.path(out_dir, "t1_coeffs_by_level.csv"),
          row.names = FALSE)

# Attenuation summary: how much does each within-level contrast shrink
# relative to the pooled (level-adjusted) contrast?
attn <- t1_coeffs %>%
    select(depvar, outcome, term, course_level, estimate) %>%
    pivot_wider(names_from = course_level, values_from = estimate) %>%
    mutate(
        ret_basic    = 100 * basic    / pooled,
        ret_advanced = 100 * advanced / pooled,
        ret_graduate = 100 * graduate / pooled
    )
write.csv(attn, file.path(out_dir, "t1_attenuation.csv"), row.names = FALSE)

say("")
say("== Task 1: within-level retention of the team gap (%% of pooled) ==")
say("   (100%% = gap unchanged within level; ~0%% = gap explained by level)")
key_terms <- c("teamf", "teamfm", "teamff")
for (dv in names(outcomes)) {
    sub <- attn %>% filter(depvar == dv, term %in% key_terms)
    for (i in seq_len(nrow(sub))) {
        say("   %-16s %-7s  basic %5.0f%%  adv %5.0f%%  grad %5.0f%%  (pooled est=%.3f)",
            sub$outcome[i], sub$term[i],
            sub$ret_basic[i], sub$ret_advanced[i], sub$ret_graduate[i],
            sub$pooled[i])
    }
}

# Task 1 figure ---------------------------------------------------------
t1_plot_df <- t1_coeffs %>%
    filter(term %in% c("teamf", "teamff", "teamfm", "teammm")) %>%
    mutate(
        course_level = factor(course_level,
                              levels = c("pooled", "basic", "advanced", "graduate")),
        term = factor(term, levels = names(term_labels))
    )

p1 <- ggplot(t1_plot_df,
             aes(x = course_level, y = estimate,
                 ymin = conf.low, ymax = conf.high,
                 color = course_level)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_pointrange(linewidth = 0.4) +
    facet_grid(outcome ~ term,
               labeller = labeller(term = term_labels),
               scales = "free_y") +
    labs(x = NULL, y = "Difference vs. man alone (M)",
         title = "Team gender gaps within course level",
         subtitle = "If gaps were a seniority-via-course-type artifact, within-level estimates would collapse toward 0") +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "none")

ggsave(file.path(out_dir, "t1_fig_gap_by_level.pdf"), p1,
       width = 11, height = 12)

# ======================================================================
# TASK 2  --- Does the (conditioned) gap track the female share?
# ======================================================================
# Per-year, level-adjusted team gap vs. the female share of instructors
# that year. Pure composition => gap should move mechanically with the
# rising female share. We estimate per-year models (team + course_level +
# controls) and correlate the mixed-gender & female-solo gaps with the
# annual female share.
# ----------------------------------------------------------------------

# Annual female share among solo courses (proxy for feminization trend)
female_share_by_year <- analysis %>%
    filter(team %in% c("m", "f")) %>%
    group_by(year) %>%
    summarise(female_share = 100 * mean(team == "f"), .groups = "drop")

say("")
say("== Task 2: female solo share by year ==")
say("   %d: %.1f%%   ->   %d: %.1f%%",
    min(female_share_by_year$year),
    female_share_by_year$female_share[which.min(female_share_by_year$year)],
    max(female_share_by_year$year),
    female_share_by_year$female_share[which.max(female_share_by_year$year)])

per_year_gap <- function(depvar) {
    form <- as.formula(sprintf(
        "%s ~ team + course_level + country + scale(prob) + stem + (1 | field) + (1 | institution) + scale(log(tot_count))",
        depvar))
    out <- lapply(2000:2019, function(yr) {
        df <- analysis %>% filter(year == yr, !is.na(course_level),
                                  !is.na(.data[[depvar]]))
        fit <- lme4::lmer(form, data = df, weights = sqrt(matched_docs),
                          control = lmerControl(optimizer = "bobyqa",
                                                calc.derivs = FALSE))
        broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE) %>%
            filter(term %in% c("teamf", "teamfm")) %>%
            transmute(year = yr, term, estimate, std.error,
                      conf.low, conf.high)
    })
    say("   done per-year gaps for %s", depvar)
    bind_rows(out)
}

say("")
say("== Task 2: fitting per-year, level-adjusted models ==")
t2_gaps <- bind_rows(lapply(names(outcomes), function(dv) {
    per_year_gap(dv) %>% mutate(depvar = dv, outcome = outcomes[[dv]])
}))

t2_gaps <- t2_gaps %>% left_join(female_share_by_year, by = "year")
write.csv(t2_gaps, file.path(out_dir, "t2_gap_vs_share.csv"), row.names = FALSE)

# Correlation between the gap and the female share, per outcome x term.
t2_cor <- t2_gaps %>%
    group_by(outcome, depvar, term) %>%
    summarise(
        cor_gap_share = cor(estimate, female_share),
        slope_per_pp  = coef(lm(estimate ~ female_share))[2],
        .groups = "drop"
    )
write.csv(t2_cor, file.path(out_dir, "t2_gap_share_correlation.csv"),
          row.names = FALSE)

say("")
say("== Task 2: correlation of level-adjusted gap with female share ==")
say("   (Pure-composition story predicts a strong correlation; a weak")
say("    or wrong-signed one undermines it.)")
for (i in seq_len(nrow(t2_cor))) {
    say("   %-32s %-7s  cor = %+.2f", t2_cor$outcome[i], t2_cor$term[i],
        t2_cor$cor_gap_share[i])
}

# Task 2 figure ---------------------------------------------------------
p2 <- ggplot(t2_gaps, aes(x = year, y = estimate,
                          ymin = conf.low, ymax = conf.high)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_ribbon(alpha = 0.15) +
    geom_line() +
    geom_point(size = 0.8) +
    facet_grid(outcome ~ term, labeller = labeller(term = term_labels),
               scales = "free_y") +
    labs(x = "Year", y = "Level-adjusted gap vs. man alone (M)",
         title = "Level-adjusted gender gaps over time",
         subtitle = "Compare stability of the gap against the rising female share (see t2_gap_vs_share.csv)") +
    theme(legend.position = "none")

ggsave(file.path(out_dir, "t2_fig_gap_vs_share.pdf"), p2,
       width = 8, height = 12)

# ----------------------------------------------------------------------
# Write plain-text summary
# ----------------------------------------------------------------------
writeLines(summary_lines, file.path(out_dir, "summary.txt"))
say("")
say("Done. Outputs in %s", out_dir)
