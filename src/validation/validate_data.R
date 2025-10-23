library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(here)
library(validate)

source(here::here("src/R/utils.R"))
#plot_visuals = function(title, subtitle, tag, n, with_legend = FALSE, legend_name = NULL, x_axis_name = NULL, y_axis_name = NULL, use_fill = TRUE)

articles = read_csv(here::here("data/processed_data/processed.csv"))

readr::write_csv(articles, here::here("data/processed_data/processed.csv"))

nrow(articles)

outlier_delays = articles |>
   summarise(
     accdelay_outlier_lower = sum(acceptance_delay <= 1, na.rm = TRUE),
     accdelay_outlier_upper = sum(acceptance_delay >= 1095, na.rm = TRUE),
     pubdelay_lower = sum(publication_delay <= 1, na.rm = TRUE),
     pubdelay_upper = sum(publication_delay >= 1095, na.rm = TRUE),
     accdelay_outlier = sum(acceptance_delay <= 1 | acceptance_delay >= 1095, na.rm = TRUE),
     pubdelay_outlier = sum(publication_delay <= 1 | publication_delay >= 1095, na.rm = TRUE)
   )

write.csv(outlier_delays, here::here("tables/validation_tables/outlier_delays.csv"), row.names = FALSE)

articles = articles |>
   filter(acceptance_delay >= 1,
          acceptance_delay <= 1095,
          publication_delay >= 1,
          publication_delay <= 1095)

nrow(articles)

articles = articles |> 
  filter(article_date >= as.Date("2016-01-01"),
         article_date <= as.Date("2025-06-01"))

nrow(articles)

############### Check column types

type_rules <- validator(
  is_covid            = is.logical(is_covid),
  received            = is.Date(received),
  article_date        = is.Date(article_date),
  acceptance_delay    = is.numeric(acceptance_delay),
  is_psych            = is.logical(is_psych),
  is_mega             = is.logical(is_mega),
  issn_linking        = is.character(issn_linking),
  h_index             = is.numeric(h_index),
  open_access         = is.logical(open_access),
  publication_delay   = is.numeric(publication_delay),
  title               = is.character(title),
  journal             = is.character(journal),
  sjr                 = is.numeric(sjr),
  rank                = is.numeric(rank),
  discipline          = is.character(discipline),
  asjc                = is.numeric(asjc),
  npi_discipline      = is.character(npi_discipline),
  npi_field           = is.character(npi_field),
  npi_level_16        = is.numeric(npi_level_16),
  npi_level_17        = is.numeric(npi_level_17),
  npi_level_18        = is.numeric(npi_level_18),
  npi_level_19        = is.numeric(npi_level_19),
  npi_level_20        = is.numeric(npi_level_20),
  npi_level_21        = is.numeric(npi_level_21),
  npi_level_22        = is.numeric(npi_level_22),
  npi_level_23        = is.numeric(npi_level_23),
  npi_level_24        = is.numeric(npi_level_24),
  is_series           = is.logical(is_series),
  established         = is.numeric(established),
  country             = is.character(country),
  keywords            = is.character(keywords),
  apc                 = is.character(apc),
  apc_amount          = is.character(apc_amount)
)


############### Check missing articles

missingness <- validator(
  is_covid            = !is.na(is_covid),
  received            = !is.na(received),
  article_date        = !is.na(article_date),
  acceptance_delay    = !is.na(acceptance_delay),
  is_psych            = !is.na(is_psych),
  is_mega             = !is.na(is_mega),
  issn_linking        = !is.na(issn_linking),
  h_index             = !is.na(h_index),
  open_access         = !is.na(open_access),
  publication_delay   = !is.na(publication_delay),
  title               = !is.na(title),
  journal             = !is.na(journal),
  sjr                 = !is.na(sjr),
  rank                = !is.na(rank),
  discipline          = !is.na(discipline),
  asjc                = !is.na(asjc),
  npi_discipline      = !is.na(npi_discipline),
  npi_field           = !is.na(npi_field),
  npi_level_16        = if (established <= 2015) !is.na(npi_level_16),
  npi_level_17        = if (established <= 2016) !is.na(npi_level_17),
  npi_level_18        = if (established <= 2017) !is.na(npi_level_18),
  npi_level_19        = if (established <= 2018) !is.na(npi_level_19),
  npi_level_20        = if (established <= 2019) !is.na(npi_level_20),
  npi_level_21        = if (established <= 2020) !is.na(npi_level_21),
  npi_level_22        = if (established <= 2021) !is.na(npi_level_22),
  npi_level_23        = if (established <= 2022) !is.na(npi_level_23),
  npi_level_24        = if (established <= 2023) !is.na(npi_level_24),
  sjr_2016            = if (established <= 2015) !is.na(sjr_2016),
  sjr_2017            = if (established <= 2016) !is.na(sjr_2017),
  sjr_2018            = if (established <= 2017) !is.na(sjr_2018),
  sjr_2019            = if (established <= 2018) !is.na(sjr_2019),
  sjr_2020            = if (established <= 2019) !is.na(sjr_2020),
  sjr_2021            = if (established <= 2020) !is.na(sjr_2021),
  sjr_2022            = if (established <= 2021) !is.na(sjr_2022),
  sjr_2023            = if (established <= 2022) !is.na(sjr_2023),
  sjr_2024            = if (established <= 2023) !is.na(sjr_2024),
  is_series           = !is.na(is_series),
  established         = !is.na(established),
  country             = !is.na(country),
  keywords            = !is.na(keywords),
  apc                 = !is.na(apc[open_access == TRUE]),
  apc_amount          = !is.na(apc_amount[open_access == TRUE])
)


############### Check range

range_checks <- validator(
  publication_types   = grepl("Journal Article", publication_types),
  received            = in_range(received, min = as.Date("2013-01-01"), max = as.Date("2025-06-01")),
  article_date        = in_range(article_date, min = as.Date("2016-01-01"), max = as.Date("2025-06-01")),
  acceptance_delay    = in_range(acceptance_delay, min = 1, max = 1095),  # 1 day to 3 years
  publication_delay   = in_range(publication_delay, min = 1, max = 1095),
  established         = in_range(established, min = 1500, max = 2025),
  h_index             = in_range(h_index, min = 0, max = 1500),
  sjr                 = in_range(sjr, min = 0, max = 150),
  rank                = in_range(rank, min = 1, max = 35000),
  asjc                = in_range(asjc, min = 1000, max = 4999),
  npi_level_16        = if (established <= 2015) in_range(npi_level_16, min = 0, max = 2),
  npi_level_17        = if (established <= 2016) in_range(npi_level_17, min = 0, max = 2),
  npi_level_18        = if (established <= 2017) in_range(npi_level_18, min = 0, max = 2),
  npi_level_19        = if (established <= 2018) in_range(npi_level_19, min = 0, max = 2),
  npi_level_20        = if (established <= 2019) in_range(npi_level_20, min = 0, max = 2),
  npi_level_21        = if (established <= 2020) in_range(npi_level_21, min = 0, max = 2),
  npi_level_22        = if (established <= 2021) in_range(npi_level_22, min = 0, max = 2),
  npi_level_23        = if (established <= 2022) in_range(npi_level_23, min = 0, max = 2),
  npi_level_24        = if (established <= 2023) in_range(npi_level_24, min = 0, max = 2),
  sjr_2016            = if (established <= 2015) sjr_2016 %in% c("Q1", "Q2", "Q3", "Q4"),
  sjr_2017            = if (established <= 2016) sjr_2017 %in% c("Q1", "Q2", "Q3", "Q4"),
  sjr_2018            = if (established <= 2017) sjr_2018 %in% c("Q1", "Q2", "Q3", "Q4"),
  sjr_2019            = if (established <= 2018) sjr_2019 %in% c("Q1", "Q2", "Q3", "Q4"),
  sjr_2020            = if (established <= 2019) sjr_2020 %in% c("Q1", "Q2", "Q3", "Q4"),
  sjr_2021            = if (established <= 2020) sjr_2021 %in% c("Q1", "Q2", "Q3", "Q4"),
  sjr_2022            = if (established <= 2021) sjr_2022 %in% c("Q1", "Q2", "Q3", "Q4"),
  sjr_2023            = if (established <= 2022) sjr_2023 %in% c("Q1", "Q2", "Q3", "Q4"),
  sjr_2024            = if (established <= 2023) sjr_2024 %in% c("Q1", "Q2", "Q3", "Q4")
)


############### Check article uniqueness

uniqueness = validator(
  is_unique(title)
)


############### Save validations as figures

cf_type = confront(articles, type_rules)
summary(cf_type)

pdf(file = here::here("figures/validation_figures/validation_type.pdf"))
plot = plot(cf_type)
dev.off()


cf_missing = confront(articles, missingness)
summary(cf_missing)

pdf(file = here::here("figures/validation_figures/validation_missing.pdf"))
plot = plot(cf_missing)
dev.off()


cf_range = confront(articles, range_checks)
summary(cf_range)

pdf(file = here::here("figures/validation_figures/validation_range.pdf"))
plot = plot(cf_range)
dev.off()


cf_unique = confront(articles, uniqueness)
summary(cf_unique)

pdf(file = here::here("figures/validation_figures/validation_unique.pdf"))
plot = plot(cf_unique)
dev.off()


############### Number of journals

journal_n = articles |>
  filter(!is.na(journal)) |> 
  dplyr::summarize(distinct_journals = n_distinct(journal))

write.csv(journal_n, here::here("tables/validation_tables/journal_n.csv"), row.names = F)

############### Articles per journal

journal_n_article = articles |>
  filter(!is.na(journal)) |> 
  dplyr::group_by(journal) |>
  dplyr::summarise(number = n()) |> 
  dplyr::arrange(desc(number))

less_than_10_articles_per_journal = journal_n_article |> 
  dplyr::filter(number < 10) |> 
  dplyr::summarise(total_journals = n_distinct(journal),
                   percentage_less_than_10_articles = round((total_journals / nrow(journal_n_article)) * 100, 2)
  )

write.csv(journal_n_article, here::here("tables/validation_tables/journal_articles_n.csv"), row.names = F)
write.csv(less_than_10_articles_per_journal, here::here("tables/validation_tables/journal_articles_10.csv"), row.names = T)

############### By article number

article_number_logarithmic = journal_n_article |> 
  ggplot(aes(x = number)) +
  geom_density() +
  scale_x_log10(
    breaks = c(1, 10, 100, 1000, 10000, 100000),
    labels = c("1", "10", "100", "1.000", "10.000", "100.000")
  ) +
  plot_visuals("", "", "", 1,  FALSE, NULL, "log10(Article number)", "Journal distribution")

ggsave(width = 6, height = 4, dpi = 600, here::here("figures/validation_figures/article_per_journal_logarithmic.pdf"))

############### Articles per year

articles_year = articles |>
  mutate(article_date_year = floor_date(as_date(article_date), "year"),
         article_date_year = str_replace(article_date_year, "-01-01", "")) |> 
  group_by(article_date_year) |> 
  dplyr::summarise(title = n())

write.csv(articles_year, here::here("tables/validation_tables/articles_per_year.csv"), row.names = FALSE)

histogram_plot_year = articles_year |>
  ggplot2::ggplot(aes(x = factor(article_date_year), y = title)) +
  ggplot2::geom_bar(stat = "identity") +
  plot_visuals("", "Density plot of articles per year", "", 1,  FALSE, NULL, "Article date", "Number of articles")

ggsave(here::here("figures/validation_figures/histogram_year.pdf"))

############### Articles per month

articles_per_month = articles |>
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  dplyr::group_by(article_date_month) |>
  dplyr::summarise(title = n())

write.csv(articles_per_month, here::here("tables/validation_tables/articles_per_month.csv"), row.names = FALSE)

articles_per_month_plot = articles |>
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  dplyr::mutate(article_date_month = as_date(article_date_month)) |> 
  ggplot(aes(x = article_date_month)) +
  geom_bar() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  plot_visuals("", "", "", 1,  FALSE, NULL, "Article date", "Count")

ggsave(here::here("figures/validation_figures/histogram_plot_month.pdf"))

############### Articles per day

daily_articles = articles |>
  dplyr::group_by(article_date) |>
  dplyr::summarise(title = n()) |> 
  dplyr::mutate(article_year = paste(lubridate::year(article_date)))

daily_articles_plot = daily_articles |> 
  filter(!is.na(article_date)) |> 
  ggplot2::ggplot(aes(x = article_date, y = title)) +
  geom_bar(stat = "identity") +
  plot_visuals("", "Articles per day", "", 1,  FALSE, NULL, "Article date", "Number of articles") + 
  facet_wrap(~ article_year, ncol = 4, scales = "free_x") +
  theme(
   axis.text.x = element_blank() 
  )

ggsave(here::here("figures/validation_figures/daily_articles.pdf"))

############### Covid articles

covid_articles = articles |> 
  filter(article_date >= as.Date("2019-01-01")) |> 
  filter(is_covid == TRUE) |> 
  summarise(n = n(),
            percentage = round((n / nrow(articles)) * 100, 2))

write.csv(covid_articles, here::here("tables/validation_tables/covid_articles.csv"), row.names = FALSE)

############### Acceptance Delays per month

monthly_acceptance_delay = articles |>
  filter(!is.na(article_date),
         article_date <= as.Date("2025-06-01")) |> 
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  mutate(article_date_month = as_date(article_date_month)) |> 
  dplyr::group_by(article_date_month) |>
  dplyr::summarise(acceptance_delay = mean(acceptance_delay)) |> 
  ungroup() |> 
  ggplot(aes(x = article_date_month, y = acceptance_delay)) +
  geom_segment(aes(x = article_date_month, xend = article_date_month, y = 0, yend = acceptance_delay)) + 
  geom_point(color = "black", size = 1.5) +
  plot_visuals("", "", "", 10, F, NULL, "Acceptance delay", "Year") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("figures/validation_figures/acceptance_delay_month.pdf"))

############### Publication Delays per month

monthly_publication_delay = articles |>
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  mutate(article_date_month = as_date(article_date_month)) |> 
  dplyr::group_by(article_date_month) |>
  dplyr::summarise(publication_delay = mean(publication_delay)) |> 
  ungroup() |> 
  ggplot(aes(x = article_date_month, y = publication_delay)) +
  geom_segment(aes(x = article_date_month, xend = article_date_month, y = 0, yend = publication_delay), color = "red") + 
  geom_point(color = "black", size = 1.5) +
  labs(x = "Month", y = "Average Publication Delay", title = "Average Publication Delay by Month") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("figures/validation_figures/publication_delay_month.pdf"))

############### Discipline check

wos_discipline = articles |> 
  filter(!is.na(discipline)) |> 
  group_by(discipline) |> 
  summarize(count = n())

npi_discipline = articles |> 
  filter(!is.na(npi_discipline)) |> 
  group_by(npi_discipline) |> 
  summarize(count = n())

agreement_between_discipline = articles |> 
  filter(!is.na(discipline), !is.na(npi_discipline),
         discipline != "multidisciplinary") |>
  mutate(agreement = case_when(
    discipline == "health_sciences" & npi_discipline == "Health Sciences"  ~ 1,
    discipline == "life_sciences" & npi_discipline == "Health Sciences"  ~ 1,
    discipline == "physical_sciences" & npi_discipline == "Natural Sciences and Engineering"  ~ 1,
    discipline == "social_sciences_and_humanities" & npi_discipline == "Humanities"  ~ 1,
    TRUE ~ 0
  )) |> 
  summarize(agreement = sum(agreement),
            total = nrow(articles),
            percentage = (agreement / total) * 100)

write.csv(wos_discipline, here::here("tables/validation_tables/wos_discipline.csv"), row.names = FALSE)
write.csv(npi_discipline, here::here("tables/validation_tables/npi_discipline.csv"), row.names = FALSE)
write.csv(agreement_between_discipline, here::here("tables/validation_tables/agreement_between_disciplines.csv"), row.names = FALSE)

############### Each year curve on same graph

articles_graph = articles |>
  filter(!is.na(acceptance_delay),
         !is.na(article_date),
         article_date <= as.Date("2025-05-30")) |>
  dplyr::mutate(article_date_year = lubridate::year(article_date)) |>
  dplyr::mutate(month = lubridate::month(article_date)) |>
  dplyr::group_by(article_date_year, month) |>
  dplyr::summarize(mean_acceptance_delay = mean(acceptance_delay, na.rm = TRUE))

articles_graph |> 
  ggplot(aes(x = month, y = mean_acceptance_delay, colour = factor(article_date_year))) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = 1:12, labels = month.name) +
  plot_visuals("", "Acceptance delays per year", "", 10,  TRUE, "Year", "Month", "Acceptance delay") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here::here("figures/validation_figures/acceptance_delay_per_year.pdf"))

############### Distribution of delays per year

articles |> 
  mutate(article_date_year = factor(lubridate::year(article_date))) |> 
  ggplot(aes(x = acceptance_delay, colour = article_date_year)) + 
  geom_density(alpha = 0.2) +
  scale_x_continuous(limits = c(0, 730), breaks = c(50, 100, 365, 730)) +
  plot_visuals("", "Distribution of acceptance delays per year", "", 10,  T, "Year", "Acceptance delay", "Density")

ggsave(here::here("figures/validation_figures/acceptance_delay_distribution_per_year.pdf"))

############### Saving validated data

readr::write_csv(articles, "/users/zsimi/pubdelays/data/processed_data/processed.csv")

stop("Analysis done")