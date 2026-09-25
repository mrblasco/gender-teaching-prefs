# Sep 18, 2026

# Origin
origin <- "~/Documents/Projects/01_education/02_Open_Syllabus/data"
filename_data <- file.path(origin, "rds/final.rds")

ds <- readRDS(filename_data)
logger::log_info("Loaded data from {filename_data}")

saveRDS(ds, here::here("data", "raw", "syllabi.rds"))
logger::log_info("Saving {filename_data}")
