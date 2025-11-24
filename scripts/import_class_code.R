# Import class codes from CSV and save into RDS

# Define file paths
input_path  <- file.path("data", "interim", "class_code.csv")
output_path <- file.path("data", "processed", "class_code.rds")

# Suppress warnings
options(warn = -1)

# Load dataset
message("Loading ", input_path, " ...")
start_time <- Sys.time()

raw_data <- read.csv(
  input_path,
  header = FALSE,
  col.names = c("id", "code"),
  stringsAsFactors = FALSE
)

end_time <- Sys.time()
message(sprintf("Dataset loaded in %.2f seconds", as.numeric(difftime(end_time, start_time, units = "secs"))))

# Function to standardise and classify class codes
standardise_class_codes <- function(codes) {
    stopifnot("Codes must be character" = is.character(codes))

    codes_clean <- toupper(codes)
    codes_clean <- gsub("[^A-Z0-9]", "", codes_clean)

    patterns <- list(
        letters_numbers          = "^[A-Z]+[0-9]+$",
        letters_numbers_letter   = "^[A-Z]+[0-9]+[A-Z]+$",
        numbers_letters          = "^[0-9]+[A-Z]+$",
        numeric_only             = "^[0-9]+$",
        letters_only             = "^[A-Z]+$",
        letter_number_letter_num = "^[A-Z][0-9][A-Z]?[0-9]+$"
    )
  
    pattern_type <- rep("unknown", length(codes_clean))
    for (name in names(patterns)) {
        matches <- grepl(patterns[[name]], codes_clean)
        pattern_type[matches] <- name
    }
  
    nums <- as.numeric(sub(".*?([0-9]+).*", "\\1", codes_clean))
    first_digit <- as.numeric(substr(as.character(nums), 1, 1))
    course_level <- rep("unknown", length(codes_clean))
    course_level[first_digit %in% c(1, 2)] <- "basic"
    course_level[first_digit %in% c(3, 4)] <- "advanced"
    course_level[first_digit > 4] <- "graduate"

    data.frame(
        original_code     = codes,
        standardised_code = codes_clean,
        pattern_type      = pattern_type,
        course_level      = course_level,
        stringsAsFactors = FALSE
    )
}

# Standardise codes
n_rows <- nrow(raw_data)
message("Standardising ", n_rows, " rows ...")
start_time <- Sys.time()

out <- standardise_class_codes(raw_data$code)

stopifnot(nrow(out) == n_rows)

end_time <- Sys.time()
message(sprintf("Conversion completed in %.2f seconds", as.numeric(difftime(end_time, start_time, units = "secs"))))

# Show summary of course levels
message("Class distribution (%):")
print(round(prop.table(table(out$course_level)) * 100, 1))

# Save final dataset
message("Saving output to ", output_path, " ...")
final_data <- data.frame(
  id = raw_data$id,
  course_level = out$course_level,
  stringsAsFactors = TRUE
)
saveRDS(final_data, file = output_path)
message("Done.")
