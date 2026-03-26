library(dplyr)

d <- jsonlite::stream_in(file("data/interim/novelty.json.gz"))

d_long <- tidyr::unnest(d, cite)

d_count <- d_long %>%
    dplyr::count(cite, year, sort = TRUE) %>%
    dplyr::arrange(cite, year) %>%
    dplyr::mutate(n_cum = cumsum(n), .by = cite)

d_join <- d_long %>%
    dplyr::left_join(d_count, by = c("year", "cite")) %>%
    summarise(
        novel_count = sum(n_cum == 1),
        novel_med = median(n_cum),
        tot_count = n(),
        .by = c(id, year)
    )

outdir <- file.path("data", "processed")
saveRDS(d_join, file.path(outdir, "novel_v2.rds"))
