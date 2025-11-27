#library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(parallel)

# Load data 
ds <- read.csv(
    "data/processed/v12/instructor_genders.csv",
    header = FALSE,
    col.names = c("id", "inst_id", "year", "field", "team"),
)
message(sprintf("Initial rows %.2f M", nrow(ds) / 1e6))


# Filter
ds <- dplyr::filter(ds, !grepl("u", team), nchar(team) < 3)
message(sprintf("Filtered rows %.2f M", nrow(ds) / 1e6))

# Function to randomise strings
randomise_strings <- function(strings) {
    str_letters <- unlist(strsplit(strings, split = ""))
    nchars <- sapply(strsplit(strings, ""), length)

    df <- data.frame(
        group = rep(seq_along(strings), nchars),
        random = sample(str_letters)
    )

    out <- aggregate(
        random ~ group,
        data = df,
        FUN = function(x) paste(sort(x), collapse = "")
    )

    out$random
}

set.seed(4881)
ds_small <- ds %>%
    dplyr::select(inst_id, field, year, team)
message(sprintf("File size %s", format(object.size(ds_small), "MB")))

nsims <- 25
rand_results <- parallel::mclapply(
    seq_len(nsims),
    mc.cores = 5,
    function(b) {
        t0 <- Sys.time()

        tmp <- ds_small %>%
            mutate(
                rand = randomise_strings(team),
                .by = c(inst_id, field, year)
            ) %>%
            count(year, team = rand)

        tmp$iter <- b

        td <- as.numeric(Sys.time() - t0) / 60
        message(sprintf("Iter %d done in %1.2f minutes", b, td))
        tmp
    }
)

ds_rand_all <- bind_rows(rand_results)

ds_rand_ci <- ds_rand_all %>%
    group_by(year, team) %>%
    summarise(
        med = median(n),
        lower = quantile(n, 0.025),
        upper = quantile(n, 0.975),
        .groups = "drop"
    )

ds_count <- ds %>% 
    count(year, team, name = "actual")

ds_final <- left_join(ds_rand_ci, ds_count) %>%
    tidyr::pivot_longer(c(med, actual))

outpath <- file.path("data", "processed", "montecarlo.rds")
message("saving to ", outpath)

saveRDS(ds_final, file = outpath)

# covid_start <- 2020
# covid_end   <- 2021


# ds_final %>%
#     mutate(
#         team = dplyr::case_match(
#             team,
#             "f" ~ "Female",
#             "m" ~ "Male",
#             "fm" ~ "Mixed",
#             "mm" ~ "Two males",
#             "ff" ~ "Two Females"
#         )
#     ) %>%
#     mutate(
#         upper = upper / sum(upper),
#         lower = lower / sum(lower),
#         percent = value / sum(value),
#         .by = c(name, year)
#     ) %>%
#     ggplot() +
#     aes(
#         x = year,
#         y = percent,
#         color = name,
#         group = name
#     ) +

#     # COVID annotation
#     annotate(
#         "rect",
#         xmin = covid_start, xmax = covid_end,
#         ymin = 0, ymax = Inf,
#         alpha = 0.15, fill = "grey70"
#     ) +

#     scale_color_brewer(palette = "Dark2") +
#     facet_wrap(~ team, scales = "free") +
#     geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, fill = "orange") +
#     geom_line(size = 1) +
#     geom_point(size = 2) +
#     labs(
#         x = "Year",
#         y = "Count (log scale)",
#         color = "Type",
#         title = "Trend Over Time",
#         subtitle = paste0("COVID period highlighted (", covid_start, "–", covid_end, ")")
#     ) +
#     theme_classic()


