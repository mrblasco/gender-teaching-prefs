library(dplyr)
library(yaml)

config <- yaml.load_file("config.yml")
data_dir <- config$data_dir

atyp <- readRDS("data/processed/atyp.rds")

filename_data <- file.path(data_dir, "rds/final.rds")

ds <- readRDS(filename_data) %>%
    dplyr::filter(nchar(team) < 3, year > 1999) %>% 
    dplyr::mutate(
        total_authors = female_authors + male_authors,
    )

ds_matched <- ds %>% 
    left_join(atyp, by = c("id"="ID"))

outpath <- file.path("data", "processed", "os_final.rds")
saveRDS(ds_matched, outpath)




