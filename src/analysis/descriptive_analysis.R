#### Descriptive analysis

library(readr)
library(lubridate)
library(dplyr)
library(stringr)
library(ggdist)
library(viridis)
library(ggplot2)
library(papaja)
library(tidyr)
library(ggridges)
library(here)
library(forcats)
library(scales)

source(here::here("src/R/utils.R"))

data = readr::read_csv(here::here("data/processed_data/processed.csv"))

data = data |> 
  filter(accdelay_outlier == FALSE,
         pubdelay_outlier == FALSE )


# Number of articles

## All articles

all_article_number = data |> 
  mutate(article_year = paste(lubridate::year(article_date))) |> 
  filter(!is.na(article_year)) |>
  group_by(article_year) |>
  summarise(article_number = n(),
            all_articles = nrow(data),
            percentage = n() / all_articles * 100)

## Covid articles

covid_article_number = data |> 
  mutate(article_year = paste(lubridate::year(article_date))) |> 
  filter(!is.na(article_year)) |> 
  group_by(article_year) |>
  summarise(
    all_articles = nrow(data),
    covid_number_per_year = sum(is_covid == TRUE),
    all_covid_articles = nrow(data |> filter(is_covid == TRUE)),
    percentage_covid = sum(is_covid == TRUE) / all_covid_articles * 100,
    percentage_all = sum(is_covid == TRUE) / all_articles * 100
    )

## Open access articles

open_access_article_number = data |> 
  mutate(article_year = paste(lubridate::year(article_date))) |> 
  filter(!is.na(article_year)) |>
  group_by(article_year) |>
  summarise(
    all_articles = nrow(data),
    all_articles_this_year = n(),
    all_open_access_articles = nrow(data |> filter(open_access == TRUE)),
    open_access_number_per_year = sum(open_access == TRUE),
    percentage_open_access = sum(open_access == TRUE) / all_open_access_articles * 100,
    percentage_all = sum(open_access == TRUE) / all_articles * 100,
    percentage_this_year = sum(open_access == TRUE) / all_articles_this_year * 100
    )

## Megajournal articles

megajournal_article_number = data |> 
  mutate(article_year = paste(lubridate::year(article_date))) |> 
  filter(!is.na(article_year)) |>
  group_by(article_year) |>
  summarise(
    all_articles = nrow(data),
    all_articles_this_year = n(),
    all_megajournal_articles = nrow(data |> filter(is_mega == TRUE)),
    megajournal_number_per_year = sum(is_mega == TRUE),
    percentage_megajournal = sum(is_mega == TRUE) / all_megajournal_articles * 100,
    percentage_all = sum(is_mega == TRUE) / all_articles * 100,
    percentage_this_year = sum(is_mega == TRUE) / all_articles_this_year * 100
  )

## Retracted articles

retracted_article_number = data |> 
  mutate(article_year = paste(lubridate::year(article_date))) |> 
  filter(!is.na(article_year)) |>
  group_by(article_year) |>
  summarise(
    all_articles = nrow(data),
    all_articles_this_year = n(),
    all_retracted_articles = nrow(data |> filter(str_detect(publication_types, "Retracted"))),
    retracted_number_per_year = sum(str_detect(publication_types, "Retracted")),
    percentage_retracted = sum(str_detect(publication_types, "Retracted")) / all_retracted_articles * 100,
    percentage_all = sum(str_detect(publication_types, "Retracted")) / all_articles * 100,
    percentage_this_year = sum(str_detect(publication_types, "Retracted")) / all_articles_this_year * 100
  )

## Discipline

discipline_article_number = data |> 
  mutate(article_year = paste(lubridate::year(article_date))) |> 
  filter(!is.na(article_year),
         !is.na(discipline)) |>
  group_by(article_year, discipline) |>
  summarise(
    all_articles = nrow(data),
    discipline_number_per_year = n(),
    percentage_all = n() / all_articles * 100
  )

## Norwegian publication indicator

data_with_years <- data |> 
  mutate(
    article_year = year(article_date),
    npi_year = case_when(
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
    )
  ) |> 
  select(article_date, article_year, npi_year, title)

year_totals <- data_with_years |> 
  group_by(article_year) |> 
  summarise(
    all_articles = n(),
    all_npi_articles = sum(!is.na(npi_year)),
    .groups = "drop"
  )

npi_article_number <- data_with_years |> 
  group_by(article_year, npi_year) |> 
  summarise(
    count = n(),
    .groups = "drop"
  ) |> 
  left_join(year_totals, by = "article_year") |> 
  mutate(
    percentage_all = count / all_articles * 100,
    percentage_npi = ifelse(is.na(npi_year), NA_real_, count / all_npi_articles * 100)
  )

write.csv(all_article_number, here::here("tables/analysis_tables/article_number_all.csv"), row.names = FALSE)
write.csv(covid_article_number, here::here("tables/analysis_tables/article_number_covid.csv"), row.names = FALSE)
write.csv(open_access_article_number, here::here("tables/analysis_tables/article_number_open_access.csv"), row.names = FALSE)
write.csv(megajournal_article_number, here::here("tables/analysis_tables/article_number_megajournal.csv"), row.names = FALSE)
write.csv(retracted_article_number, here::here("tables/analysis_tables/article_number_retracted.csv"), row.names = FALSE)
write.csv(discipline_article_number, here::here("tables/analysis_tables/article_number_discipline.csv"), row.names = FALSE)
write.csv(npi_article_number, here::here("tables/analysis_tables/article_number_npi.csv"), row.names = FALSE)


# Distribution of articles

## Distribution through time

monthly_publications = data |> 
  dplyr::mutate(
    article_date_month = lubridate::floor_date(as_date(article_date), "month")) |> 
  group_by(article_date_month) |>
  ggplot(aes(x = article_date_month)) +
  geom_histogram() +
  plot_visuals("", "", "", 1,  FALSE, NULL, "Article date", "Count") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))

ggsave(here::here("figures/analysis_figures/monthly_distribution.pdf"))

## Distribution of discipline

discipline_distribution = data |> 
  filter(!is.na(discipline)) |>
  mutate(discipline = fct_infreq(discipline)) |> 
  ggplot(aes(x = as.factor(discipline), fill = discipline)) +
  geom_bar() +
  plot_visuals("", "", "", 4,  FALSE, NULL, "Discipline", "Count") +
  scale_x_discrete(labels = c(
    "health_sciences" = "Health Sciences",
    "life_sciences" = "Life Sciences",
    "physical_sciences" = "Physical Sciences",
    "social_sciences_and_humanities" = "Social Sciences and Humanities"
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))
  
ggsave(here::here("figures/analysis_figures/discipline_distribution.pdf"))

## Distribution of quartiles

quartile_data <- data |>
  mutate(article_year = year(article_date)) |> 
  pivot_longer(
    cols = starts_with("sjr_"),
    names_to = "sjr_year",
    names_prefix = "sjr_",
    values_to = "quartile"
  ) |>
  filter(as.integer(sjr_year) == article_year, !is.na(quartile))

ggplot(quartile_data, aes(x = as.factor(quartile), fill = quartile)) +
  geom_bar() +
  facet_wrap(~ article_year) +
  plot_visuals("", "Ammount of articles each year by quartiles", "", 4,
               FALSE, NULL, "Quartile", "Count") +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))

ggsave(here::here("figures/analysis_figures/quartile_distribution.pdf"))

# Distribution of journals

## By article number

journal_distribution = data |> 
  group_by(journal) |>
  ggplot(aes(x = reorder(journal, table(journal)[journal]))) +
  geom_bar(width = 0.5) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

ggsave(here::here("figures/analysis_figures/journal_article_number_distribution.pdf"))
           
## By mean and median delays

### Acceptance delay

acceptance_delay_distribution = data |>
  group_by(journal) |>
  summarise(
    mean = mean(acceptance_delay, na.rm = TRUE),
    median = median(acceptance_delay, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(cols = c(mean, median), names_to = "statistic", values_to = "delay") |>
  ggplot(aes(x = delay, fill = statistic)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = quantile(delay, 0.25)), linetype = "dotted", linewidth = 1, color = "#332288") +
  geom_vline(aes(xintercept = quantile(delay, 0.75)), linetype = "dotted", linewidth = 1, color = "#332288") +
  annotate("text", x = 41, y = 0.017, label = "Q1", vjust = -1.5, color = "black") +
  annotate("text", x = 78, y = 0.017, label = "Q3", vjust = -1.5, color = "black") +
  geom_rug(alpha = 0.1) +
  scale_fill_manual(
    values = c("mean" = "#88CCEE", "median" = "#CC6677"),
    labels = c("Mean", "Median"),
    name = "Statistic"
  ) +
  plot_visuals("", "Acceptance delay distribution for journals", "", 2, T, "", "Acceptance delay", "Density")

ggsave(here::here("figures/analysis_figures/acceptance_delay_distribution.pdf"))

### Publication delay

publication_delay_distribution = data |>
  group_by(journal) |>
  summarise(
    mean = mean(publication_delay, na.rm = TRUE),
    median = median(publication_delay, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(cols = c(mean, median), names_to = "statistic", values_to = "delay") |>
  ggplot(aes(x = delay, fill = statistic)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = quantile(delay, 0.25)), linetype = "dotted", linewidth = 1, color = "#332288") +
  geom_vline(aes(xintercept = quantile(delay, 0.75)), linetype = "dotted", linewidth = 1, color = "#332288") +
  annotate("text", x = 8, y = 0.026, label = "Q1", vjust = -1.5, color = "black") +
  annotate("text", x = 41, y = 0.026, label = "Q3", vjust = -1.5, color = "black") +
  geom_rug(alpha = 0.1) +
  scale_fill_manual(
    values = c("mean" = "#88CCEE", "median" = "#CC6677"),
    labels = c("Mean", "Median"),
    name = "Statistic"
  ) +
  plot_visuals("", "Publication delay distribution for journals", "", 2, T, "", "Publication delay", "Density")

ggsave(here::here("figures/analysis_figures/publication_delay_distribution.pdf"))

# General information about delays

summary_data <- data |> 
  summarise(mean_acceptance_delay = round(mean(acceptance_delay, na.rm = TRUE), 3),
            mean_publication_delay = round(mean(publication_delay, na.rm = TRUE), 3),
            sd_acceptance_delay = round(sd(acceptance_delay, na.rm = TRUE), 3),
            sd_publication_delay = round(sd(publication_delay, na.rm = TRUE), 3),
            median_acceptance_delay = round(median(acceptance_delay, na.rm = TRUE), 3),
            median_publication_delay = round(median(publication_delay, na.rm = TRUE), 3),
            quantile_25_acceptance_delay = round(quantile(acceptance_delay, 0.25, na.rm = TRUE), 3),
            quantile_75_acceptance_delay = round(quantile(acceptance_delay, 0.75, na.rm = TRUE), 3),
            quantile_25_publication_delay = round(quantile(publication_delay, 0.25, na.rm = TRUE), 3),
            quantile_75_publication_delay = round(quantile(publication_delay, 0.75, na.rm = TRUE), 3),
            min_acceptance_delay = round(min(acceptance_delay, na.rm = TRUE), 3),
            max_acceptance_delay = round(max(acceptance_delay, na.rm = TRUE), 3),
            min_publication_delay = round(min(publication_delay, na.rm = TRUE), 3),
            max_publication_delay = round(max(publication_delay, na.rm = TRUE), 3),
            iqr_acceptance_delay = round(IQR(acceptance_delay, na.rm = TRUE), 3),
            iqr_publication_delay = round(IQR(publication_delay, na.rm = TRUE), 3),
            n = n(),
            `1_months_acceptance_delay` = sum(acceptance_delay <= 30, na.rm = TRUE) / n * 100,
            `2_months_acceptance_delay` = sum(acceptance_delay <= 60, na.rm = TRUE) / n * 100,
            `6_months_acceptance_delay` = sum(acceptance_delay <= 180, na.rm = TRUE) / n * 100,
            `1_year_acceptance_delay` = sum(acceptance_delay <= 365, na.rm = TRUE) / n * 100,
            `1_month_publication_delay` = sum(publication_delay <= 30, na.rm = TRUE) / n * 100,
            `2_months_publication_delay` = sum(publication_delay <= 90, na.rm = TRUE) / n * 100,
            `6_months_publication_delay` = sum(publication_delay <= 180, na.rm = TRUE) / n * 100,
            `1_year_publication_delay` = sum(publication_delay <= 365, na.rm = TRUE) / n * 100
  ) |> 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

## Delays yearly

year_table = data |> 
  dplyr::mutate(article_year = paste(year(article_date))) |> 
  filter(!is.na(acceptance_delay),
         !is.na(publication_delay),
         !is.na(article_year)) |> 
  group_by(article_year) |> 
  summarise(
    n = n(),
    mean_acc_delay = mean(acceptance_delay, na.rm = TRUE),
    median_acc_delay = median(acceptance_delay, na.rm = TRUE),
    mean_pub_delay = mean(publication_delay, na.rm = TRUE),
    median_pub_delay = median(publication_delay, na.rm = TRUE)
  )

## Delays based on quartiles

quartile_table <- data |> 
  mutate(article_year = paste(year(article_date)), 
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
         )) |> 
  filter(!is.na(article_year),
         !is.na(quartile_year)) |> 
  group_by(article_year, quartile_year) |> 
  summarise(
    year = article_year,
    n = n(),
    mean_acc_delay = mean(acceptance_delay, na.rm = TRUE),
    median_acc_delay = median(acceptance_delay, na.rm = TRUE),
    mean_pub_delay = mean(publication_delay, na.rm = TRUE),
    median_pub_delay = median(publication_delay, na.rm = TRUE),
    `2_months_acc` = sum(acceptance_delay <= 60, na.rm = TRUE) / n * 100,
    `6_months_acc` = sum(acceptance_delay <= 180, na.rm = TRUE) / n * 100,
    `1_year_acc` = sum(acceptance_delay <= 365, na.rm = TRUE) / n * 100,
    `2_months_pub` = sum(publication_delay <= 60, na.rm = TRUE) / n * 100,
    `6_months_pub` = sum(publication_delay <= 180, na.rm = TRUE) / n * 100,
    `1_year_pub` = sum(publication_delay <= 365, na.rm = TRUE) / n * 100
  ) |> 
  unique() |> 
  select(year, quartile_year, n, mean_acc_delay, mean_pub_delay, 
         median_acc_delay, median_pub_delay, 
         `2_months_acc`, `6_months_acc`, `1_year_acc`, 
         `2_months_pub`, `6_months_pub`, `1_year_pub`)

write.csv(summary_data, here::here("tables/analysis_tables/delay_summary.csv"), row.names = FALSE)
write.csv(year_table, here::here("tables/analysis_tables/delay_summary_yearly.csv"))
write.csv(quartile_table, here::here("tables/analysis_tables/delay_summary_quartile.csv"))

## Delays based on discipline, megajournals, and open access

group_vars <- c("discipline", "is_mega", "open_access", "is_covid")

# Loop over each variable
for (var in group_vars) {
  # Dynamically build the group_by expression
  grouped_table <- data |>
    filter(!is.na(.data[[var]])) |> 
    group_by(.data[[var]]) |>  # dynamic grouping
    summarise(
      n = n(),
      mean_acc_delay = mean(acceptance_delay, na.rm = TRUE),
      median_acc_delay = median(acceptance_delay, na.rm = TRUE),
      mean_pub_delay = mean(publication_delay, na.rm = TRUE),
      median_pub_delay = median(publication_delay, na.rm = TRUE), 
      `2_months_acc` = sum(acceptance_delay <= 60, na.rm = TRUE) / n * 100,
      `6_months_acc` = sum(acceptance_delay <= 180, na.rm = TRUE) / n * 100,
      `1_year_acc` = sum(acceptance_delay <= 365, na.rm = TRUE) / n * 100,
      `2_months_pub` = sum(publication_delay <= 60, na.rm = TRUE) / n * 100,
      `6_months_pub` = sum(publication_delay <= 180, na.rm = TRUE) / n * 100,
      `1_year_pub` = sum(publication_delay <= 365, na.rm = TRUE) / n * 100,
      .groups = "drop"
    )
  
  # Create dynamic file name
  file_name <- paste0("tables/analysis_tables/delay_summary_", var, ".csv")
  
  write.csv(grouped_table, here::here(file_name), row.names = FALSE)
}

# General information about all other variables

### Fix APC
data = data |> 
  mutate(apc = if_else(is.na(apc_amount) | apc_amount == "", FALSE, TRUE))


## Logical variable summary

logical_variables = c("is_mega", "open_access", "is_covid", "apc")
summary_list <- list()
  
  for (var in logical_variables) {
    x <- data[[var]]
    
    summary_list[[var]] <- tibble(
      variable = var,
      is_false = sum(x == FALSE, na.rm = TRUE),
      is_true = sum(x == TRUE, na.rm = TRUE),
      is_na = sum(is.na(x))
    )
}

Logical_variables_summary <- bind_rows(summary_list)

## Numeric variable summary

numeric_variables = c("h_index", "rank", "sjr", "npi_level_24" )
summary_list <- list()

for (var in numeric_variables) {
  x <- data[[var]]
  
  summary_list[[var]] <- tibble(
    variable = var,
    min = min(x, na.rm = TRUE),
    q1 = quantile(x, 0.25, na.rm = TRUE),
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    q3 = quantile(x, 0.75, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    iqr = IQR(x, na.rm = TRUE)
  )
}

numeric_variables_summary <- bind_rows(summary_list)

write.csv(Logical_variables_summary, here::here("tables/analysis_tables/summary_logical_variables.csv"), row.names = FALSE)
write.csv(numeric_variables_summary, here::here("tables/analysis_tables/summary_numeric_variables.csv"), row.names = FALSE)

## Quartile change through time

sjr_numeric <- data |>
  mutate(across(starts_with("sjr_"), ~ as.numeric(gsub("Q", "", .))))

quartile_analysis <- sjr_numeric |>
  group_by(journal) |> 
  summarise(
    q_2024 = sjr_2024,
    q_average = round(mean(c_across(starts_with("sjr_")), na.rm = TRUE), 3)
  ) |> 
  unique()

## npi change through time

npi_analysis <- data |>
  group_by(journal) |> 
  summarise(
    npi_2024 = npi_level_24,
    npi_average = round(mean(c_across(starts_with("npi_level_")), na.rm = TRUE), 3) 
  ) |> 
  unique()

write.csv(quartile_analysis, here::here("tables/analysis_tables/summary_quartile_analysis.csv"), row.names = FALSE)
write.csv(npi_analysis, here::here("tables/analysis_tables/summary_npi_analysis.csv"), row.names = FALSE)

## Retracted article summary

retracted_article_summary = data |> 
  filter(str_detect(publication_types, "Retracted")) |> 
  summarise(
    n = n(),
    mean_acc_delay = mean(acceptance_delay, na.rm = TRUE),
    mean_pub_delay = mean(publication_delay, na.rm = TRUE),
    median_acc_delay = median(acceptance_delay, na.rm = TRUE),
    median_pub_delay = median(publication_delay, na.rm = TRUE),
    sd_acc_delay = sd(acceptance_delay, na.rm = TRUE),
    sd_pub_delay = sd(publication_delay, na.rm = TRUE)
  )

write.csv(retracted_article_summary, here::here("tables/analysis_tables/summary_retracted_article.csv"), row.names = FALSE)

retracted_data <- data |> 
  filter(str_detect(publication_types, "Retracted")) |> 
  mutate(article_year = paste(lubridate::year(article_date)))

n_retracted = nrow(retracted_data)

article_year_freq <- retracted_data |> 
  count(article_year) |> 
  mutate(freq = n / n_retracted) |> 
  select(article_year, freq) |> 
  arrange(article_year)

freq_vector <- article_year_freq[['freq']]

retracted_journals = retracted_data |> 
  mutate(N = n()) |> 
  group_by(journal) |> 
  summarize(
    n = n(), 
    freq = n / N
  ) |> 
  slice(1) |> 
  ungroup()

write.csv(retracted_journals, here::here("tables/analysis_tables/summary_retracted_journals.csv"), row.names = FALSE)

#Percentile plot:

percentile_function <- function(df) {
  probs <- seq(0.1, 0.9, 0.1)
  dplyr::tibble(
    percentile = 100 * probs,
    delay = quantile(df$acceptance_delay, probs = probs, na.rm = TRUE)
  )
}

acceptance_data_quantile <- data |>
  mutate(article_date_month = lubridate::floor_date(as_date(article_date), "month")) |>
  dplyr::group_by(article_date_month) |>
  dplyr::group_modify(~ percentile_function(.x))

percentile_colors <- c(
  "10" = "#88CCEE",
  "20" = "#CC6677",
  "30" = "#332288",
  "40" = "#AA4499",
  "50" = "#44AA99",
  "60" = "#999933",
  "70" = "#882255",
  "80" = "#661100",
  "90" = "#6699CC"
)

percentile_labels <- paste0(seq(10, 90, by = 10), "th")

ggplot(acceptance_data_quantile, aes(x = article_date_month, y = delay, color = factor(percentile), group = percentile)) +
  geom_line(size = 1) +
  plot_visuals(
    "Acceptance delay percentiles over time",
    "Percentiles of acceptance delay by month",
    "",
    11, 
    T,
    NULL,
    "Month",
    "Acceptance delay (days)"
  ) +
  scale_color_manual(
    values = percentile_colors,
    labels = percentile_labels,
    name = "Percentile"
  ) 

ggsave(here::here("figures/analysis_figures/acceptance_delay_percentiles.pdf"))

### Megajournal names

megajournals_names <- c(
  "PLOS ONE", "Scientific Reports", "BMJ Open", "PeerJ",
  "Royal Society Open Science", "F1000Research", "GigaScience",
  "SAGE Open", "Heliyon", "SAGE Open Medicine",
  "SAGE Open Medical Case Reports", "SAGE Open Nursing",
  "SAGE Open Medicine", "SAGE Open Engineering", "Springer Plus,
  SAGE Open", "IEEE Access", "G3", "Biology Open", "Elementa,
  Science of the Antropoce", "AIP Advances", "Journal of Engineering",
  "Sustainability", "International Journal of Molecular Sciences",
  "Sensors", "Energies", "Molecules", "Science of the Total Environment",
  "eLife", "FEBS Open Bio", "mBio", "ACS Omega"
)


