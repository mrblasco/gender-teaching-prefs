

ds %>%
    filter(formation == "team") %>%
    count(country, wt = n) %>%
    mutate(
        country = dplyr::case_when(
            country == "US" ~ "USA",
            country == "GB" ~ "Great Britain",
            country == "CA" ~ "Canada",
            country == "IT" ~ "Italy",
            country == "PL" ~ "Poland",
            country == "NL" ~ "Netherlands",
            country == "DE" ~ "Germany",
            country == "IE" ~ "Ireland",
            country == "PT" ~ "Portugal",
            country == "SE" ~ "Sweden",
            country == "ES" ~ "Spain",
            country == "AT" ~ "Austria",
            country == "DK" ~ "Denmark",
            country == "FR" ~ "France",
            TRUE ~ "Other"
        )
    ) %>%
    mutate(pc = round(100 * n / sum(n), 1)) %>% 
