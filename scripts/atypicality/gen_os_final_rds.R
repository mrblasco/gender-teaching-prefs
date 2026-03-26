# 
library(dplyr)
library(yaml)

config <- yaml.load_file("config.yml")
data_dir <- config$data_dir

data_path <- file.path(data_dir, "rds/final.rds")
atyp_path <- file.path("data", "processed", "atyp.rds")
code_path <- file.path("data", "processed", "class_code.rds")

# Load data 
atyp <- readRDS(atyp_path)
code <- readRDS(code_path)
ds <- readRDS(data_path) 

# Process and filter data
ds <- ds %>%
    dplyr::filter(nchar(team) < 3, year > 1999) %>% 
    dplyr::mutate(
        total_authors = female_authors + male_authors,
    )


ds_matched <- ds %>% 
    left_join(atyp, by = c("id"="ID")) %>% 
    left_join(code, by = "id")

outpath <- file.path("data", "processed", "os_final.rds")
saveRDS(ds_matched, outpath)




