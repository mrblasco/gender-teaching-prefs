# Sep 18, 2026

# Origin
origin <- "~/Documents/Projects/01_education/02_Open_Syllabus/data/rds"

filename_data <- file.path(origin, "final.rds")
logger::log_info("Loading from {filename_data}")

# Destination
filename_raw <- here::here("data", "raw", "syllabi.rds")
filename_manifest <- here::here("data", "raw", "syllabi.yml")

# Load source data
ds <- readRDS(filename_data)

logger::log_info("Loaded data from {filename_data}")

# Save raw data
saveRDS(ds, filename_raw)

logger::log_info("Saved data to {filename_raw}")

# Create data manifest
manifest <- list(
    dataset = "syllabi",
    source = list(
        path = filename_data,
        file = basename(filename_data),
        modified = format(
            file.info(filename_data)$mtime,
            "%Y-%m-%d %H:%M:%S"
        ),
        checksum = tools::md5sum(filename_data)
    ),
    destination = list(
        path = filename_raw,
        file = basename(filename_raw),
        checksum = tools::md5sum(filename_raw)
    ),
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    provenance = list(
        script = "data/raw/01_import.R"
    )
)

yaml::write_yaml(manifest, filename_manifest)

logger::log_info("Saved data manifest to {filename_manifest}")
