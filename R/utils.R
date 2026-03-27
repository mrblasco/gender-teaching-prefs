
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
    if (anyNA(x)) {
        warning("Missing values detected in `x`")
    }
    idx <- !is.na(x)
    r <- rank(x[idx])
    out <- rep(NA_real_, length(x))
    out[idx] <- 100 * (r - 1) / (sum(idx) - 1)
    return(out)
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
