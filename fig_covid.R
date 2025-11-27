library(ggplot2)
library(dplyr, warn.conflicts = FALSE)

data_path <- file.path("data", "processed", "montecarlo.rds")
ds <- readRDS(data_path)

covid_start <- 2020
covid_end   <- 2021

head(ds)

# Statistics
ds_actual <- dplyr::filter(ds, name == "actual") %>% 
    dplyr::select(year, team, value)

head(ds_actual)

# Time trends
fit <- glm(value ~ year + team,  family = quasipoisson, data = ds_actual)
summary(fit)

fit_interaction <- update(fit, ~ . + year:team)
anova(fit, fit_interaction, test = "Chisq") # 


ds_wide <- ds_actual %>%
    tidyr::pivot_wider(names_from = team, values_from = value)
head(ds_wide)
chisq.test(ds_wide[, -1])  # exclude year column

######## DEscriptives V12 #################

dir_v12 <- file.path("data", "interim", "v12")
inst <- jsonlite::stream_in(file(file.path(dir_v12, "institutions.json")))
str(inst)
length(xtabs(~country, inst))

######## EVENT STUDY #################

data_sex_path <- file.path("data", "interim", "v12", "instructor_genders.csv")
sex <- read.csv(data_sex_path, header = FALSE, col.names = c("id", "inst_id", "year", "field", "team"))
str(sex)

sex_count <- sex %>%
    filter(nchar(team) < 3, !grepl("u", team)) %>%
    count(inst_id, team, year) %>%
    mutate(percent = (n + 1) / (sum(n) + 2), .by = c(inst_id, year))

sex_count %>%
    ggplot(aes(x = factor(year), y = percent)) +
    geom_boxplot()

sex_count %>%
    ggplot(aes(x = year - covid_start, y = percent, color = team)) +
    geom_smooth(method = "gam", formula = y ~ s(x, k = 4)) + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    facet_grid(~team)

######## FIGURES #################

# Process data for plotting
ds_clean <- ds %>%
    mutate(
        team = dplyr::case_match(
            team,
            "f" ~ "Female",
            "m" ~ "Male",
            "fm" ~ "Mixed",
            "mm" ~ "Two males",
            "ff" ~ "Two Females"
        ),
        name = dplyr::case_match(
            name,
            "actual" ~ "Observed",
            "med" ~ "Simulated",
            .default = name
        )
    ) %>%
    mutate(
        percent = value / sum(value),
        .by = c(name, year)
    )

head(ds_clean)

# Plots 

p <- ds_clean %>%
    ggplot() +
    aes(
        x = year,
        y = percent,
        linetype = name,
        color = name,
        group = name
    ) +

    # COVID annotation
    annotate(
        "rect",
        xmin = covid_start, xmax = covid_end,
        ymin = 0, ymax = Inf,
        alpha = 0.15, fill = "grey70"
    ) +

    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(label = scales::percent) +
    facet_wrap(~ team, scales = "free") +
    #geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, fill = "orange") +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
        x = "Year",
        y = "Syllabi per year (%)",
        linetype = "Type",
        color = "Type",
        title = "Trend Over Time",
        subtitle = paste0("COVID period highlighted (", covid_start, "–", covid_end, ")")
    ) +
    theme_classic() +
    theme(
        legend.position = "bottom"
    )

print(p)


p + aes(y = value) + labs(y = "Syllabi (thousands)") + scale_y_continuous(label = \(x) sprintf("%2.1fk", x/1e3) )



