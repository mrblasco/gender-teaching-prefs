
log_msg <- function(fmt, ...) {
    message(sprintf(fmt = fmt, ...))
}

paste_sort <- function(x) {
    paste0(sort(x), collapse = "")
}

get_gender_composition <- function(x) {
    x <- as.character(x)
    dplyr::case_when(
        grepl("m", x) & grepl("f", x) ~ "Mixed-gender",
        grepl("m", x) ~ "Male-only",
        grepl("f", x) ~ "Female-only",
        TRUE ~ "Other"
    )
}


center <- function(x) {
    as.numeric(scale(x, scale = FALSE, center = TRUE))
}

rank_percentile <- function(x) {
    stopifnot(all(!is.na(x)))
    100 * (rank(x) - 1) / (length(x) - 1)
}


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
