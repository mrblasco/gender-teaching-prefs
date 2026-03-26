# Aggregate novelty by syllabis ID

library(dplyr)

path_to_novelty <- file.path("data", "syll_id_doi_atyp_match.csv.gz")
ds_novelty <- read.csv(path_to_novelty)

ds_novelty <- ds_novelty %>%
    summarise(
        atyp_avg = mean(Atyp_10pct_Z, na.rm = TRUE),
        atyp_med = median(Atyp_10pct_Z, na.rm = TRUE),
        atyp_n = sum(!is.na(Atyp_10pct_Z)),
        .by = ID
    )

outpath <- file.path("data", "processed", "atyp.rds")
saveRDS(ds_novelty, file = outpath)