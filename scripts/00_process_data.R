# ---- setup -----
library(dplyr, warn.conflicts = FALSE)

source("R/isced.R") # iseced_lookup
source("R/utils.R") # log_msg

results_dir <- file.path("data", "results")
data_dir    <- file.path("data", "processed")

syllabi_path <- file.path(data_dir, "os_final.rds")
novel_path  <- file.path(data_dir, "novel_v2.rds")


# ----------------------------------------------------------------------
# Load data
# ----------------------------------------------------------------------
syllabi <- readRDS(syllabi_path)
log_msg("Loaded syllabi: %d rows, %d cols", nrow(syllabi), ncol(syllabi))

novelty <- readRDS(novel_path)
log_msg("Loaded novelty: %d rows, %d cols", nrow(novelty), ncol(novelty))

# ----------------------------------------------------------------------
# Merge data
# ----------------------------------------------------------------------
log_msg("Merging data...")
syllabi_merged <- syllabi %>%
    left_join(dplyr::select(novelty, -year), by = "id") %>% 
    left_join(isced_lookup, by = "field") %>%
    mutate(
        recency = year - novelty,
        team = relevel(factor(as.character(team)), ref = "m"),
        team_ordered = relevel(factor(as.character(team_ordered)), ref = "m")
    )

stopifnot(nrow(syllabi_merged) == nrow(syllabi))

# ---- Save ---- 

rds_filename <- file.path("data", "processed", "syllabi_merged.rds")
saveRDS(syllabi_merged, rds_filename)
log_msg("Saved to %s", rds_filename)
