get_gender_composition <- function(x) {
  x <- as.character(x)
  case_when(
    grepl("m", x) & grepl("f", x) ~ "Mixed-gender",
    grepl("m", x) ~ "Male-only",
    grepl("f", x) ~ "Female-only",
    TRUE ~ "Other"
  )
}

paste_sort <- function(x) paste0(sort(x), collapse = "")

convert_gender_label <- function(original_labels) {
  dplyr::case_match(
    original_labels,
    "f"  ~ "Female only (F)",
    "ff" ~ "Female-Female (FF)",
    "mf" ~ "Mixed Gender (MF/FM)",
    "fm" ~ "Mixed Gender (MF/FM)",
    "mm" ~ "Male-Male (MM)",
    "m"  ~ "Male only (M)",
    .default = "Unknown"
  )
}

sep <- function(p, n) sqrt(p * (1 - p) / n)
