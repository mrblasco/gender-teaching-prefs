# ---- setup -----
library(dplyr, warn.conflicts = FALSE)
library(logger)

source("R/isced.R") # iseced_lookup
source("R/utils.R") # log_msg

# ---- utils 
say <- logger::log_info

rank_percentile <- function(x) {
    stopifnot(length(x) > 1)    
    result <- 100 * (rank(x, na.last = "keep") - 1) / (sum(!is.na(x)) - 1)
    say("Computed percentile rank: {round(mean(is.na(result)), 2)} missing")
    result
}


# ---- paths 
results_dir <- file.path("data", "results")
data_dir    <- here::here("data", "processed")

syllabi_path <- file.path(data_dir, "os_final.rds")
novel_path  <- file.path(data_dir, "novel_v2.rds")

say(
    "Data dir: {data_dir}"
)

# ----------------------------------------------------------------------
# Load data
# ----------------------------------------------------------------------
say("==== Load data ====")
syllabi <- readRDS(syllabi_path)
novelty <- readRDS(novel_path)

say(
    "Loaded syllabi data: {nrow(syllabi)} rows, {ncol(syllabi)} cols."
)
say(
    "Loaded novelty data: {nrow(novelty)} rows, {ncol(novelty)} cols."
)


# ----------------------------------------------------------------------
# Merge and process data
# ----------------------------------------------------------------------
say("==== Merge and process ====")
syllabi_merged <- syllabi %>%
    left_join(dplyr::select(novelty, -year), by = "id") %>% 
    left_join(isced_lookup, by = "field") 

stopifnot(nrow(syllabi_merged) == nrow(syllabi))


syllabi_merged <- syllabi_merged %>%
    mutate(
        recency = year - novelty,
        team = relevel(factor(as.character(team)), ref = "m"),
        team_ordered = relevel(factor(as.character(team_ordered)), ref = "m"),
        
        # Reviewer #3 - drop unknown course levels
        course_level = dplyr::case_when(
            course_level == "unknown" ~ NA_character_,
            TRUE ~ course_level
        ),

        # Reviewer #3 - don't adjust female women proportions, if not present drop
        total_authors = female_authors + male_authors,
        female_ratio = ifelse(
            total_authors > 0,
            female_authors / total_authors,
            NA_real_
        ),

    ) |>
    group_by(year) |>
    mutate(
        intdisc_rp      = rank_percentile(mean_intdisc),
        conventional_rp = rank_percentile(novel_med),
        atyp_rp         = rank_percentile(atyp_med),
        recency_rp      = rank_percentile(recency),
    ) |>
    ungroup()


# ---- Save ---- 
say("==== Save ====")

rds_filename <- file.path(data_dir, "syllabi_merged.rds")
saveRDS(syllabi_merged, rds_filename)

say(
    "File saved to {rds_filename}"
)
