library(readr)
library(lubridate)
library(dplyr)
library(stringr)
library(ggdist)
library(ggplot2)
library(here)
library(tidyr)
library(ggdist)
library(ggridges)
#install.packages("fitdistrplus", repos = "https://cloud.r-project.org/")
library(mgcv)
library(fitdistrplus)
library(statmod)

source(here::here("src/R/utils.R"))

data = readr::read_csv(here::here("data/processed_data/processed.csv"))
#plot_visuals = function(title, subtitle, tag, n, with_legend = FALSE, legend_name = NULL, x_axis_name = NULL, y_axis_name = NULL, use_fill = TRUE)

data = data |> 
  mutate(article_year = paste(lubridate::year(article_date)), 
         quartile_year = case_when(
           article_year == "2015" ~ sjr_2015,
           article_year == "2016" ~ sjr_2016,
           article_year == "2017" ~ sjr_2017,
           article_year == "2018" ~ sjr_2018,
           article_year == "2019" ~ sjr_2019,
           article_year == "2020" ~ sjr_2020,
           article_year == "2021" ~ sjr_2021,
           article_year == "2022" ~ sjr_2022,
           article_year == "2023" ~ sjr_2023,
           article_year == "2024" ~ sjr_2024,
           TRUE ~ NA_character_  # Default to NA if no match
         ),
         nri_year = case_when(
           article_year == 2024 ~ npi_level_24,
           article_year == 2023 ~ npi_level_23,
           article_year == 2022 ~ npi_level_22,
           article_year == 2021 ~ npi_level_21,
           article_year == 2020 ~ npi_level_20,
           article_year == 2019 ~ npi_level_19,
           article_year == 2018 ~ npi_level_18,
           article_year == 2017 ~ npi_level_17,
           article_year == 2016 ~ npi_level_16,
           article_year == 2015 ~ npi_level_15,
           TRUE ~ NA_real_
         )) |> 
  filter(article_date > as_date("2014-12-31"),
         article_date < as_date("2025-12-01"))


# data = data |>
#   sample_n(100000)


# Acceptance delay results

## Overall change in last ten years

### Rainplot

rainplot_data = data |>
  filter(!is.na(acceptance_delay)) |>
  mutate(threshold = quantile(acceptance_delay, 0.99)) |>
  filter(acceptance_delay <= threshold)

rainplot_dots_data = rainplot_data |> 
  sample_n(100000)

rainplot = rainplot_data |>
  ggplot(aes(x = as.factor(article_year), y = acceptance_delay, fill = as.factor(article_year))) +
  ggdist::stat_halfeye(
    adjust = 1,
    width = 0.6,
    justification = -.1,
    .width = 0.5,
    point_colour = NA,
    position=position_dodge(.7)
  ) +
  ggdist::stat_dots(
    data = rainplot_dots_data,
    aes(x = as.factor(article_year), y = acceptance_delay, fill = as.factor(article_year)),
    side = "left",
    justification = 1.1,
    binwidth = 0.01,
    overflow = "compress",
    alpha = 0.1,
    position=position_dodge(.7)
  ) +
  geom_boxplot(
    width = 0.2,
    outlier.color = NA,
    alpha = 1,
    fill = "white",
    color = "black",
    lwd = 0.5,
    position=position_dodge(.7)
  ) +
  plot_visuals("", "", "", 10, F, NULL, "Year of publication", "Acceptance delay") +
  coord_flip()


ggsave(width = 6, height = 4, dpi = 600, here::here("figures/analysis_figures/acceptance_delay_overall_rainplot.pdf"))
print("rainplot done")

### GAM using ggplot

## Long, inefficient and lacks detail
# acceptance_delay_plot = data |> 
#   ggplot(aes(x = article_date, y = acceptance_delay)) +
#   geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = FALSE) +
#   plot_visuals("", "", "", 1, F, NULL, "Year of publication", "Acceptance delay") +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank()
#   )
# 
# ggsave(width = 6, height = 4, dpi = 600, here::here("figures/analysis_figures/acceptance_delay_overall_loess_plot.pdf"))

### Time series

###### Simple GAM model
# Use bam() instead of gam() if it is too computationally heavy
# Use disciplines instead of journals if it is too computationally heavy

simple_gam = data |>
  filter(!is.na(article_date),
         !is.na(acceptance_delay),
         article_date >= as_date("2016-01-01"),
         article_date <= as_date("2024-12-31"),
         acceptance_delay > 0,
         accdelay_outlier == FALSE
  ) |>
  mutate(
    time_numeric = as.numeric(article_date),
    weekday = wday(article_date, week_start = 1),
    day_of_year = yday(article_date),
    month_of_year = month(article_date),
    article_year = as.numeric(lubridate::year(article_date))
  )
cat("Simple Model based on", nrow(simple_gam), "articles")

time_1 = Sys.time()

knots = list(month_of_year = c(0.5, seq(1, 12, length = 10), 12.5))
gam_model = bam(
  acceptance_delay ~ 
    s(month_of_year, bs = "cc", k = 12) + 
    s(article_year, bs = "cr", k = 8) +
    s(weekday, bs = "cc", k = 7),
  data = simple_gam,
  family = Gamma(link = "log"), 
  knots = knots, 
  nthreads = 4,
  method = "fREML"
)

concurvity_simple = concurvity(gam_model, full = TRUE)

bitmap(here::here("tables/analysis_tables/simple_gam_check.png"), type = "png16m", 
       width = 10, height = 8, units = "in", res = 200)
par(mfrow = c(2, 2))
gam.check(gam_model, rep = 10)
dev.off()

summary_output <- capture.output(summary(gam_model))
writeLines(summary_output, here::here("tables/analysis_tables/simple_gam_summary.txt"))

pdf(here::here("figures/analysis_figures/simple_gam_plot.pdf"))
plot(gam_model, pages = 1)
dev.off()

dev_expl <- 1 - gam_model$deviance / gam_model$null.deviance
cat("Percentage of deviance explained:", round(dev_expl * 100, 2), "%\n")

write.csv(concurvity_simple, here::here("tables/analysis_tables/simple_gam_concurvity.csv"))

time_2 = Sys.time()
time_taken = time_2 - time_1
cat(sprintf("Time taken to fit the GAM model: %.2f seconds\n", time_taken))

stop("simple_gam")

###### More complex GAM
# Introduce discipline and journal
# Remove journals that do not have at least 10 articles per year for the given range
# Introduce covid as an abrupt change
# filter out articles that have no information about discipline, journal
# Remove accdelay = 0

year_range <- 2016:2024
journal_10 <- data |>
  filter(!is.na(journal)) |>
  mutate(article_year = year(article_date)) |>
  filter(article_year %in% year_range) |>
  count(journal, article_year) |>
  complete(journal, article_year = year_range, fill = list(n = 0)) |>
  group_by(journal) |>
  filter(all(n >= 10)) |>
  distinct(journal) |> 
  pull(journal)

n_distinct(journal_10)

better_gam = data |>
  filter(!is.na(article_date),
         !is.na(acceptance_delay),
         !is.na(journal),
         !is.na(discipline),
         article_date >= as_date("2016-01-01"),
         article_date <= as_date("2024-12-31"),
         accdelay_outlier == FALSE,
         acceptance_delay > 0,
         journal %in% journal_10
         ) |>
  mutate(
    covid = as.factor(ifelse(article_date >= as.Date("2020-01-01"), 1, 0)),
    time_numeric = as.numeric(article_date),
    weekday = wday(article_date, week_start = 1),
    day_of_year = yday(article_date),
    month_of_year = month(article_date),
    journal = as.factor(journal),
    discipline = as.factor(discipline),
    article_year = as.numeric(lubridate::year(article_date))
  )

# Check ideal distribution

response <- better_gam$acceptance_delay
descdist(response, boot = 1000)

fit_gamma <- fitdist(response, "gamma")
fit_lnorm <- fitdist(response, "lnorm")
fit_ig <- fitdist(response, "invgauss")
fit_weibull <- fitdist(response, "weibull")
fit_exp <- fitdist(response, "exp") 

fits <- list(
  gamma = fit_gamma,
  lognormal = fit_lnorm,
  invgauss = fit_ig,
  weibull = fit_weibull,
  exponential = fit_exp
)

gof <- gofstat(fits)

results <- data.frame(
  Distribution = names(fits),
  AIC = sapply(fits, function(x) x$aic),
  BIC = sapply(fits, function(x) x$bic),
  logLik = sapply(fits, function(x) x$loglik),
  KS = gof$ks,
  CvM = gof$cvm,
  AD = gof$ad
)
 
print(results[order(results$AIC), ])

best_metrics <- c(
  AIC = results$Distribution[which.min(results$AIC)],
  BIC = results$Distribution[which.min(results$BIC)],
  logLik = results$Distribution[which.max(results$logLik)],
  KS = results$Distribution[which.min(results$KS)],
  CvM = results$Distribution[which.min(results$CvM)],
  AD = results$Distribution[which.min(results$AD)]
)

print(best_metrics)
best_distribution_counts <- table(best_metrics)
majority_best <- names(which.max(best_distribution_counts))
cat("\n Most frequent best-fitting distribution:", majority_best, "\n")
family_mapping <- list(
  gamma = Gamma(link = "log"),
  lognormal = gaussian(link = "log"),     
  invgauss = inverse.gaussian(link = "log"),
  weibull = tw(link = "log")      
)

if (!(majority_best %in% names(family_mapping))) {
  stop("No GAM family mapping found for: ", majority_best)
}

chosen_family <- family_mapping[[majority_best]]
cat("✅ GAM family used:", deparse(chosen_family$family), "\n")

time_1 = Sys.time()

knots = list(month_of_year = c(0.5, seq(1, 12, length = 10), 12.5))
gam_model = bam(
  acceptance_delay ~ 
    s(time_numeric, bs = "cr", k = 100) + 
    covid +
    s(discipline, bs = "re", k = 5) +
    s(journal, bs = "re") +
    factor(weekday, bs = "cc", k = 7),
  data = better_gam,
  family = chosen_family, 
  knots = knots, 
  nthreads = 8,
  method = "fREML"
)

concurvity(gam_model, full = TRUE)

par(mfrow = c(2, 2))
gam.check(gam_model, rep = 10)
summary(gam_model)
plot(gam_model, pages = 1)

time_2 = Sys.time()
time_taken = time_2 - time_1
cat(sprintf("Time taken to fit the GAM model: %.2f seconds\n", time_taken))

# PLot residuals

ggplot(data.frame(fitted = fitted(gam_model), 
                 residuals = residuals(gam_model)), 
       aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values", y = "Residuals") +
  theme_minimal()

ggsave(width = 6, height = 4, dpi = 600, "figures/analysis_figures/better_gam_residuals.pdf")

stop("gam_test")

## Acceptance delay by journal metrics

#### Find ideal k for general additive models

select_gam_k_under_time = function(data, x_var, y_var, 
                                    k_values = seq(5, 55, by = 10), 
                                    max_time_sec = 600, 
                                    show_plots = FALSE) {
  timings = numeric(length(k_values))
  chosen_k = NA
  
  for (i in seq_along(k_values)) {
    k = k_values[i]
    cat(sprintf("Testing k = %d...\n", k))
    
    p = ggplot(data, aes_string(x = x_var, y = y_var)) +
      geom_smooth(
        method = "gam",
        formula = y ~ s(x, k = k),
        method.args = list(select = TRUE),
        se = FALSE,
        color = "blue"
      ) +
      ggtitle(sprintf("GAM with k = %d", k))
    
    start_time = Sys.time()
    print(p)
    end_time = Sys.time()
    
    duration = as.numeric(difftime(end_time, start_time, units = "secs"))
    timings[i] = duration
    
    cat(sprintf(" → Time taken to render plot: %.3f seconds\n", duration))
    
    if (duration <= max_time_sec) {
      chosen_k = k  # update chosen_k to the largest k under max time
    } else {
      cat(" → Exceeded max time, stopping search.\n")
      break
    }
  }
  
  results = data.frame(
    k = k_values[1:i],
    time_seconds = timings[1:i]
  )
  
  cat(sprintf("\nChosen k = %d (largest k with rendering time ≤ %d seconds)\n", chosen_k, max_time_sec))
  
  return(list(chosen_k = chosen_k, timings = results))
}
result = select_gam_k_under_time(data, "article_date", "acceptance_delay")
print(result$timings)
cat("Best k:", result$chosen_k, "\n")


### Quartile

plot = data |> 
  filter(quartile_year %in% c("Q1", "Q2", "Q3", "Q4")) |> 
  ggplot(aes(x = article_date, y = acceptance_delay, fill = as.factor(quartile_year))) + 
  geom_smooth(method = "gam", formula = y ~ s(x, k = 10), se = TRUE) +
  plot_visuals("", "", "", 4, T, "Best quartile", "Year of publication", "Acceptance delay") +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  )

ggsave(width = 6, height = 4, dpi = 600, "figures/analysis_figures/acceptance_delay_quartile.pdf")

### Open Access

plot = data |> 
  filter(!is.na(open_access)) |> 
  ggplot(aes(x = article_date, y = acceptance_delay, fill = as.factor(open_access))) + 
  geom_smooth(method = "gam", formula = y ~ s(x, k = chosen_k), se = TRUE) +
  plot_visuals("", "", "", 2, T, "Open access status", "Year of publication", "Acceptance delay") +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  )

ggsave(width = 6, height = 4, dpi = 600, "figures/analysis_figures/acceptance_delay_open_access.pdf")

### Megajournal

plot = data |> 
  filter(!is.na(is_mega)) |> 
  ggplot(aes(x = article_date, y = acceptance_delay, fill = as.factor(is_mega))) + 
  geom_smooth(method = "gam", formula = y ~ s(x, k = chosen_k), se = TRUE) +
  plot_visuals("", "", "", 2, T, "Megajournal status", "Year of publication", "Acceptance delay") +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  )

ggsave(width = 6, height = 4, dpi = 600, "figures/analysis_figures/acceptance_delay_megajournal.pdf")

### Discipline

plot = data |> 
  filter(!is.na(discipline)) |> 
  ggplot(aes(x = article_date, y = acceptance_delay, fill = as.factor(discipline))) + 
  geom_smooth(method = "gam", formula = y ~ s(x, k = chosen_k), se = TRUE) +
  plot_visuals("", "", "", 4, T, "Discipline", "Year of publication", "Acceptance delay") +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  )

ggsave(width = 6, height = 4, dpi = 600, "figures/analysis_figures/acceptance_delay_discipline.pdf")

### H-index

quartile_colors <- c("Q1" = "#88CCEE", "Q2" = "#CC6677", "Q3" = "#332288", "Q4" = "#AA4499")

plot = data |> 
  filter(!is.na(h_index)) |> 
  filter(h_index > 0) |>
  filter(accdelay_outlier == FALSE) |>
  sample_n(500) |> 
  mutate(h_index_quartile = ntile(h_index, 4),
         h_index_quartile = factor(h_index_quartile, 
                                      levels = c(1, 2, 3, 4), 
                                      labels = c("Q1", "Q2", "Q3", "Q4"))) |>
  ggplot(aes(x = article_date, y = acceptance_delay, color = as.factor(h_index_quartile))) + 
  geom_smooth(method = "gam", formula = y ~ s(x, k = chosen_k), se = FALSE) +
  plot_visuals("", "", "", 4, T, "H index quartile", "Year of publication", "Acceptance delay") +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  scale_color_manual(values = quartile_colors)

ggsave(width = 6, height = 4, dpi = 600, "figures/analysis_figures/acceptance_delay_h_index.pdf")

### SJR

plot = data |> 
  filter(!is.na(sjr)) |> 
  filter(sjr > 0) |>
  filter(accdelay_outlier == FALSE) |>
  sample_n(500) |> 
  mutate(sjr_quartile = ntile(sjr, 4),
         sjr_quartile = factor(sjr_quartile, 
                                   levels = c(1, 2, 3, 4), 
                                   labels = c("Q1", "Q2", "Q3", "Q4"))) |>
  ggplot(aes(x = article_date, y = acceptance_delay, color = as.factor(sjr_quartile))) + 
  geom_smooth(method = "gam", formula = y ~ s(x, k = chosen_k), se = FALSE) +
  plot_visuals("", "", "", 4, T, "Scimago Journal Rank quartile", "Year of publication", "Acceptance delay") +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  scale_color_manual(values = quartile_colors)

ggsave(width = 6, height = 4, dpi = 600, "figures/analysis_figures/acceptance_delay_sjr.pdf")

### NRI

quartile_colors <- c("0" = "#88CCEE", "1" = "#CC6677", "2" = "#332288")

plot = data |> 
  filter(!is.na(nri_year)) |> 
  filter(accdelay_outlier == FALSE) |>
  sample_n(500) |> 
  ggplot(aes(x = article_date, y = acceptance_delay, color = as.factor(nri_year))) + 
  geom_smooth(method = "gam", formula = y ~ s(x, k = 2), se = FALSE) +
  plot_visuals("", "", "", 3, T, "NRI Rank", "Year of publication", "Acceptance delay") +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  scale_color_manual(values = quartile_colors)

ggsave(width = 6, height = 4, dpi = 600, "figures/analysis_figures/acceptance_delay_nri.pdf")

## Acceptance delay: Covid

covid_data = data |>
  dplyr::mutate(article_date_month = lubridate::floor_date(as_date(article_date), "month")) |> 
  filter(!is.na(is_covid)) |> 
  filter(
    (is_covid == TRUE & article_date > as_date("2019-11-01")) | 
      is_covid == FALSE) |> 
  dplyr::group_by(article_date_month, is_covid) |>
  dplyr::reframe(
    delay = median(
      acceptance_delay,
      na.rm = TRUE
    )
  ) |>
  tidyr::drop_na(delay)

covid_data_smooth = data |>
  dplyr::mutate(article_date_month = lubridate::floor_date(as_date(article_date), "month")) |> 
  filter(!is.na(is_covid)) |> 
  filter(
    (is_covid == TRUE & article_date > as_date("2019-11-01")) | 
      is_covid == FALSE)

covid_plot = ggplot(covid_data,
                    aes(x = as_date(article_date_month), y = delay, color = is_covid)) +
  geom_point(alpha = 0.8, size = 1) +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  geom_smooth(
    data = covid_data_smooth,
    aes(x = article_date_month, y = acceptance_delay, color = is_covid),
    method = "gam", formula = y ~ s(x, k = chosen_k), se = FALSE) +
  plot_visuals("", "Change in acceptance delays during the COVID-19 pandemic", 
               "", 2, F, "", "Year of publication", "Acceptance delay") +
  scale_color_manual(
    values = c("FALSE" = "#88CCEE", "TRUE" = "#332288"),
    labels = c("False", "True")) +
  labs(color = "COVID-19 article")

ggsave(width = 6, height = 4, dpi = 600, "figures/analysis_figures/acceptance_delay_covid.pdf")

############### Only Covid plot + quantiles

# covid_quartile_data_n = data |>
#   filter(quartile_year %in% c("Q1", "Q2", "Q3", "Q4")) |> 
#   filter(is_covid == TRUE) |> 
#   filter(acceptance_delay > 0) |> 
#   filter(article_date > as_date("2019-11-01")) |> 
#   summarize(
#     n = n()
#   )
# 
# write.csv(covid_quartile_data_n, "tables/analysis_tables/covid_quartile_data.csv")

covid_quartile_data = data |>
  filter(quartile_year %in% c("Q1", "Q2", "Q3", "Q4")) |> 
  filter(is_covid == TRUE) |> 
  filter(acceptance_delay > 0) |> 
  filter(article_date > as_date("2019-11-01")) |> 
  dplyr::group_by(quartile_year)

colors = c("Q1" = "#88CCEE", "Q2" = "#CC6677", "Q3" = "#332288", "Q4" = "#AA4499")

covid_quartile_data |> 
  ggplot(aes(x = article_date, y = acceptance_delay, color = quartile_year)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 2), se = FALSE) +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  coord_cartesian(
    ylim = c(0, 150)) +
  plot_visuals("", "Acceptance delay of covid articles based on journal quartiles", "", 4, T,
               "Best quartile", "Year of publication", "Acceptance delay") +
  scale_color_manual(values = colors) +
  labs(color = "Quartile")

ggsave(width = 6, height = 4, dpi = 600, "figures/analysis_figures/acceptance_delay_covid_quantile.pdf")


## Acceptance delay: Retractions


retracted_data <- data |> 
  filter(str_detect(publication_types, "Retracted"))

n_retracted = nrow(retracted_data)

retraction_summary <- retraction_data |> 
  summarise(
    n_total = n(),
    mean = mean(acceptance_delay, na.rm = TRUE)
  )

retraction_by_quartile <- retraction_data |> 
  filter(!is.na(quartile_year)) |>
  group_by(quartile_year) |> 
  summarise(
    n_total = n(),
    mean = mean(acceptance_delay, na.rm = TRUE),
    by_quartile.sjr_best_quartile = first(quartile_year),
    by_quartile.n_quartile = n(),
    by_quartile.mean_quartile = mean(acceptance_delay, na.rm = TRUE)
  )

retraction_by_discipline <- retraction_data |> 
  filter(!is.na(discipline)) |>
  group_by(discipline) |> 
  summarise(
    n_total = n(),
    mean = mean(acceptance_delay, na.rm = TRUE),
    by_discipline.discipline = first(discipline),
    by_discipline.n_discipline = n(),
    by_discipline.mean_discipline = mean(acceptance_delay, na.rm = TRUE)
  )

article_year_freq <- retracted_data |> 
  count(article_year) |> 
  mutate(freq = n / n_retracted) |> 
  select(article_year, freq) |> 
  arrange(article_year)

freq_vector <- article_year_freq[['freq']]
print(freq_vector)

retracted_journals = retracted_data |> 
  mutate(N = n()) |> 
  group_by(journal) |> 
  summarize(
    n = n(), 
    freq = n / N
  ) |> 
  slice(1) |> 
  ungroup()

retracted_journal_names = retracted_journals |> 
  filter(!is.na(journal)) |> 
  pull(journal)

non_retracted_data = data |> 
  filter(!str_detect(publication_types, "Retracted")) |> 
  filter(journal %in% retracted_journal_names) |> 
  left_join(article_year_freq, by = "article_year") |> 
  select(article_year, journal, freq, discipline,
         quartile_year, acceptance_delay, publication_delay, title, is_covid)

n_non_retracted = nrow(non_retracted_data)

total_sample_size = n_retracted * 2

non_retracted_sample <- NULL  
years <- 2015:2024  

for (i in seq_along(years)) { 
  year <- years[i]  
  sample_size <- round(freq_vector[i] * total_sample_size)  
  
  if (sample_size > 0) {
    temp_sample <- non_retracted_data %>%
      filter(article_year == year) %>%
      sample_n(size = min(sample_size, n()), replace = FALSE) 
    
    non_retracted_sample <- bind_rows(non_retracted_sample, temp_sample)
  }
}

nrow(non_retracted_sample)

non_retracted_sample_year <- non_retracted_sample |> 
  group_by(article_year) |> 
  summarize(
    n = n(), 
    freq = n / 19464
  ) |> 
  ungroup()

non_retracted_sample_journal = non_retracted_sample |> 
  mutate(N = n()) |> 
  group_by(journal) |> 
  summarize(
    n = n(), 
    freq = n / N
  ) |> 
  slice(1) |> 
  ungroup()

retraction_comparison_ret = retracted_data |> 
  select(acceptance_delay) |> 
  mutate(group = "Visszavont")

retraction_comparison_non_ret = non_retracted_sample |> 
  select(acceptance_delay) |> 
  mutate(group = "Nem visszavont")

retraction_comparison = rbind(retraction_comparison_ret, retraction_comparison_non_ret)

head(retraction_comparison)

ggplot(retraction_comparison, aes(x = group, y = acceptance_delay, fill = group)) +
  geom_boxplot(alpha = 0.8, outliers = FALSE, show.legend = FALSE, coef = 1.5, varwidth = FALSE)  +
  theme_classic() +
  labs(title = "",
       x = "", y = "Elfogadási késés (nap)") +
  scale_fill_manual(values = c("Visszavont" = "#CC6677", "Nem visszavont" = "#44AA99")) +
  theme(legend.position = "none",
        text = element_text(size = 16, family = "serif")
  )

ggsave(width = 4, height = 6, dpi = 600, "retraction_boxplot.pdf")

retracted_data_summary = retracted_data |> 
  summarize(
    mean_acc = mean(acceptance_delay),
    mean_pub = mean(publication_delay),
    median_acc = median(acceptance_delay),
    median_pub = median(publication_delay),
    n = n()
  )

retracted_data_summary_covid = retracted_data |> 
  filter(is_covid == TRUE) |> 
  summarize(
    mean_acc = mean(acceptance_delay),
    mean_pub = mean(publication_delay),
    median_acc = median(acceptance_delay),
    median_pub = median(publication_delay),
    n = n()
  )

non_retracted_data_summary = non_retracted_data |> 
  summarize(
    mean_acc = mean(acceptance_delay),
    mean_pub = mean(publication_delay),
    median_acc = median(acceptance_delay),
    median_pub = median(publication_delay),
    n = n()
  )

non_retracted_data_summary_covid = non_retracted_data |> 
  filter(is_covid == TRUE) |> 
  summarize(
    mean_acc = mean(acceptance_delay),
    mean_pub = mean(publication_delay),
    median_acc = median(acceptance_delay),
    median_pub = median(publication_delay),
    n = n()
  )

write.csv(retraction_summary, "retraction_summary.csv")
write.csv(retraction_by_quartile, "retraction_by_quartile.csv")
write.csv(retraction_by_discipline, "retraction_by_discipline.csv")
write.csv(retracted_journals, "retracted_journals.csv")
write.csv(non_retracted_sample_year, "non_retracted_sample_year.csv")
write.csv(non_retracted_sample_journal, "non_retracted_sample_journal.csv")
write.csv(non_retracted_sample, "non_retracted_sample.csv")
write.csv(retraction_comparison, "retraction_comparison_data.csv")
write.csv(retracted_data_summary, "retracted_data_summary.csv")
write.csv(retracted_data_summary_covid, "retracted_data_summary_covid.csv")
write.csv(non_retracted_data_summary, "non_retracted_data_summary.csv")
write.csv(non_retracted_data_summary_covid, "non_retracted_data_summary_covid.csv")

# Publication delay

## Overall change in last ten years

### Rainplot

data |> 
  mutate(
    article_year = floor_date(as_date(article_date), "year"),
    article_year = str_replace(article_year, "-01-01", ""),
    article_year = factor(article_year, levels = sort(unique(article_year), decreasing = TRUE))
  ) |> 
  filter(!is.na(publication_delay),
         !is.na(article_year),
         pubdelay_outlier == FALSE
  ) |> 
  ggplot(aes(x = publication_delay, y = factor(article_year), fill = factor(article_year))) + 
  geom_density_ridges() +
  plot_visuals(
    "",
    "Publication delays for each year",
    "",
    10,
    F,
    NULL,
    "Publication delay",
    "Year of publication"
  )

ggsave(width = 6, height = 4, dpi = 600, here::here("figures/analysis_figures/publication_delay_rainplot.pdf"))

# Robust analyses

# BELOW IS UNCHECKED

# Logarithmic heatmap of acceptance delays

acceptance_delay_heatmap = data |> 
  filter(acceptance_delay < 400) |> 
  ggplot(aes(x = article_date, y = acceptance_delay)) +
  geom_bin2d(bins = 100)

ggsave(width = 6, height = 4, dpi = 600, "figures/analysis_figures/acceptance_delay_heatmap.pdf")
  
############### % accepted within X months plots

## Daily calculation

daily_data_percentage = data |> 
  arrange(article_date) |> 
  group_by(article_date) |> 
  summarize(
    n = n(),
    six_months = sum(acceptance_delay <= 180, na.rm = TRUE) / n * 100,
    two_months = sum(acceptance_delay <= 60, na.rm = TRUE) / n * 100,
    one_months = sum(acceptance_delay <= 30, na.rm = TRUE) / n * 100,
  ) |> 
  ungroup() |> 
  mutate(timeframe = "Daily")

# Calculate daily percentage of acceptance within x days

daily_percentage_plot = daily_data_percentage |> 
  ggplot(aes(x = article_date)) +
  geom_line(aes(y = six_months, color = "6 months"), group = 1) +
  geom_line(aes(y = two_months, color = "2 months"), group = 1) +
  geom_line(aes(y = one_months, color = "1 month"), group = 1) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), 
                     limits = c(0, 100)) +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  plot_visuals("Percentage of articles accepted within a given time period", "", "", 
               3, T,"Percentage accepted within", "Date", "Percentage accepted")

ggsave(width = 6, height = 4, dpi = 600, "daily_percentage_acceptance_plot.pdf")

## Monthly calculation

monthly_data = data |> 
  mutate(
    article_year = floor_date(as_date(article_date), "year"),
    article_year = str_replace(article_year, "-01-01", ""),
    article_year = factor(article_year, levels = sort(unique(article_year), decreasing = TRUE))
  ) |>
  dplyr::mutate(article_date_month = lubridate::floor_date(as_date(article_date), "month")) |>
  dplyr::group_by(article_date_month, article_year) |> 
  select(article_date_month, article_year, article_date, acceptance_delay, publication_delay)

head(monthly_data)


#Calculate monthly percentage of acceptance within x days

monthly_data_percentage = monthly_data |> 
  group_by(article_date_month) |> 
  summarize(
    n = n(),
    six_months = sum(acceptance_delay <= 180, na.rm = TRUE) / n * 100,
    two_months = sum(acceptance_delay <= 60, na.rm = TRUE) / n * 100,
    one_months = sum(acceptance_delay <= 30, na.rm = TRUE) / n * 100,
  ) |> 
  ungroup() |> 
  mutate(timeframe = "Monthly")

head(monthly_data_percentage)

monthly_percentage_plot = monthly_data_percentage |> 
  ggplot(aes(x = article_date_month)) +
  geom_line(aes(y = six_months, color = "6 months"), group = 1) +
  geom_line(aes(y = two_months, color = "2 months"), group = 1) +
  geom_line(aes(y = one_months, color = "1 month"), group = 1) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), 
                     limits = c(0, 100)) +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  plot_visuals("Percentage of articles accepted within a given time period", "", "",
               3, T,"Percentage accepted within", "Date", "Percentage accepted")

ggsave(width = 6, height = 4, dpi = 600, "monthly_percentage_acceptance_plot.pdf")

## Combined plot

print("above is good n. 1")
#Option 1

daily_data_percentage = daily_data_percentage |> rename(date = article_date)
monthly_data_percentage = monthly_data_percentage |> rename(date = article_date_month)

print("above is good n. 2")

# Create the combined plot
combined_plot = ggplot() +
  # Daily data (dashed, transparent)
  geom_line(data = daily_data_percentage, aes(x = date, y = six_months, color = "6 months", linetype = "Daily"), alpha = 0.3) +
  geom_line(data = daily_data_percentage, aes(x = date, y = two_months, color = "2 months", linetype = "Daily"), alpha = 0.3) +
  geom_line(data = daily_data_percentage, aes(x = date, y = one_months, color = "1 month", linetype = "Daily"), alpha = 0.3) +
  
  # Monthly data (solid, bold)
  geom_line(data = monthly_data_percentage, aes(x = date, y = six_months, color = "6 months", linetype = "Monthly"), size = 1.2) +
  geom_line(data = monthly_data_percentage, aes(x = date, y = two_months, color = "2 months", linetype = "Monthly"), size = 1.2) +
  geom_line(data = monthly_data_percentage, aes(x = date, y = one_months, color = "1 month", linetype = "Monthly"), size = 1.2) +
  
  # Axis & labels
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  # Line type & color legend
  scale_linetype_manual(values = c("Daily" = "dashed", "Monthly" = "solid")) +
  
  labs(
    x = "Article Date",
    y = "Percentage of Accepted Articles",
    color = "Acceptance Delay",
    linetype = "Timeframe"
  ) +
  theme_minimal()

ggsave(width = 6, height = 4, dpi = 600, "plot_test_1.pdf")

