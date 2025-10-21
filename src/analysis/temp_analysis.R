library(readr)
library(lubridate)
library(dplyr)
library(stringr)
library(ggdist)
library(ggplot2)
library(tidyr)
library(ggdist)
library(here)
library(ggridges)
library(scales)


data = readr::read_csv("/users/zsimi/processed.csv")
scimago = readr::read_csv("/users/zsimi/pubdelays/src/data/scimago/scimago.csv")
non_retracted_data = readr::read_csv("/users/zsimi/pubdelays/src/analysis/non_retracted_sample.csv")



set.seed(123)

# data = data |> 
#   sample_n(500000)

########## Quartile fix

scimago = scimago |> 
  select(issn_linking, sjr_2023, sjr_2022, sjr_2021, sjr_2020, sjr_2019, sjr_2018, sjr_2017, sjr_2016, sjr_2015)


data = data |> 
  left_join(scimago, by = "issn_linking", keep = FALSE)
  

# Load custom functions
source(here::here("src/analysis/utils.R"))
#plot_visuals <- function(title, subtitle, tag, n, with_legend = FALSE, legend_name = NULL, x_axis_name = NULL, y_axis_name = NULL, use_fill = TRUE)

data = data |> 
  filter(article_date > as_date("2014-12-31"),
         article_date < as_date("2024-12-01"))

data <- data |> 
  mutate(article_year = format(article_date, "%Y"),
         article_year = as.character(article_year),
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
           TRUE ~ NA_character_  # Default to NA if no match
         ))

# Now plot using the correct quartile for each year
plot <- data |> 
  filter(quartile_year %in% c("Q1", "Q2", "Q3", "Q4")) |> 
  ggplot(aes(x = article_date, y = acceptance_delay, colour = as.factor(quartile_year))) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 5), se = TRUE)

ggsave(width = 6, height = 4, dpi = 600, "correct_quartile_plot.pdf")

#Wrong quartile for comparison
plot <- data |> 
  filter(sjr_best_quartile %in% c("Q1", "Q2", "Q3", "Q4")) |> 
  ggplot(aes(x = article_date, y = acceptance_delay, colour = as.factor(sjr_best_quartile))) + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 5), se = TRUE)

ggsave(width = 6, height = 4, dpi = 600, "wrong_quartile_plot.pdf")

############### Retraction fix

#Create data

retracted_data <- data |> 
  filter(str_detect(publication_types, "Retracted"))

n_retracted = nrow(retracted_data)

article_year_freq <- retracted_data |> 
  count(article_year) |> 
  mutate(freq = n / n_retracted) |> 
  select(article_year, freq) |> 
  arrange(article_year)

write.csv(article_year_freq, "article_year_freq.csv")

freq_vector <- article_year_freq[['freq']]
print(freq_vector)

retracted_journals = retracted_data |> 
  mutate(N = n()) |> 
  group_by(journal_title) |> 
  summarize(
    n = n(), 
    freq = n / N
  ) |> 
  slice(1) |> 
  ungroup()

write.csv(retracted_journals, "retracted_journals.csv")

retracted_journal_names = retracted_journals |> 
  filter(!is.na(journal_title)) |> 
  pull(journal_title)

non_retracted_data = data |> 
  filter(!str_detect(publication_types, "Retracted")) |> 
  filter(journal_title %in% retracted_journal_names) |> 
  left_join(article_year_freq, by = "article_year") |> 
  select(article_year, journal_title, freq, discipline,
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

write.csv(non_retracted_sample_year, "non_retracted_sample_year.csv")

non_retracted_sample_journal = non_retracted_sample |> 
  mutate(N = n()) |> 
  group_by(journal_title) |> 
  summarize(
    n = n(), 
    freq = n / N
  ) |> 
  slice(1) |> 
  ungroup()

write.csv(non_retracted_sample_journal, "non_retracted_sample_journal.csv")

write.csv(non_retracted_sample, "non_retracted_sample.csv")


retraction_comparison_ret = retracted_data |> 
  select(acceptance_delay) |> 
  mutate(group = "Visszavont")

retraction_comparison_non_ret = non_retracted_sample |> 
  select(acceptance_delay) |> 
  mutate(group = "Nem visszavont")

retraction_comparison = rbind(retraction_comparison_ret, retraction_comparison_non_ret)

write.csv(retraction_comparison, "retraction_comparison_data.csv")

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


non_retracted_data = readr::read_csv("/users/zsimi/pubdelays/src/analysis/non_retracted_sample.csv")

retracted_data_summary = retracted_data |> 
  summarize(
    mean_acc = mean(acceptance_delay),
    mean_pub = mean(publication_delay),
    median_acc = median(acceptance_delay),
    median_pub = median(publication_delay),
    n = n()
  )

write.csv(retracted_data_summary, "retracted_data_summary.csv")

retracted_data_summary_covid = retracted_data |> 
  filter(is_covid == TRUE) |> 
  summarize(
    mean_acc = mean(acceptance_delay),
    mean_pub = mean(publication_delay),
    median_acc = median(acceptance_delay),
    median_pub = median(publication_delay),
    n = n()
  )

write.csv(retracted_data_summary_covid, "retracted_data_summary_covid.csv")

non_retracted_data_summary = non_retracted_data |> 
  summarize(
    mean_acc = mean(acceptance_delay),
    mean_pub = mean(publication_delay),
    median_acc = median(acceptance_delay),
    median_pub = median(publication_delay),
    n = n()
  )

write.csv(non_retracted_data_summary, "non_retracted_data_summary.csv")

non_retracted_data_summary_covid = non_retracted_data |> 
  filter(is_covid == TRUE) |> 
  summarize(
    mean_acc = mean(acceptance_delay),
    mean_pub = mean(publication_delay),
    median_acc = median(acceptance_delay),
    median_pub = median(publication_delay),
    n = n()
  )

write.csv(non_retracted_data_summary_covid, "non_retracted_data_summary_covid.csv")

stop()

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
  plot_visuals("Percentage of articles accepted within a given time period", "", "", 3, T,"Percentage accepted within", "Date", "Percentage accepted", F)

ggsave(width = 6, height = 4, dpi = 600, "daily_percentage_acceptance_plot.pdf")

## Monthly calculation

monthly_data = data |> 
  mutate(
    article_date_year = floor_date(as_date(article_date), "year"),
    article_date_year = str_replace(article_date_year, "-01-01", ""),
    article_date_year = factor(article_date_year, levels = sort(unique(article_date_year), decreasing = TRUE))
  ) |>
  dplyr::mutate(article_date_month = lubridate::floor_date(as_date(article_date), "month")) |>
  dplyr::group_by(article_date_month, article_date_year) |> 
  select(article_date_month, article_date_year, article_date, acceptance_delay, publication_delay)

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
  plot_visuals("Percentage of articles accepted within a given time period", "", "", 3, T,"Percentage accepted within", "Date", "Percentage accepted", F)

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

stop("below is untested")
############### Loess test plot



acceptance_data_discipline <- data |>
  filter(!is.na(discipline)) |> 
  dplyr::mutate(
    article_date_month = lubridate::floor_date(as_date(article_date), "month"))

discipline_plot <- ggplot(
  acceptance_data_discipline,
  aes(
    x = as_date(article_date_month),
    y = acceptance_delay, group = discipline,
    colour = discipline
  )
) +
  geom_smooth(method = "loess",
    show.legend = TRUE,
    na.rm = TRUE,
    span = 0.3,
    se = TRUE
  ) +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  plot_visuals("Ábra 10", "Cikkek elfogadási késének eloszlása diszciplinák szerint", "", 5, T, "Diszciplina", "Megjelenési év", "Elfogadási késés", F) +
  scale_colour_manual(values = c("health_sciences" = "#88CCEE", "life_sciences" = "#332288", "multidisciplinary" = "#CC6677", "physical_sciences" = "#AA4499", "social_sciences_and_humanities" = "#44AA99"),
                      labels = c("Egészségtudományok", "Élettudományok", "Multidiszciplináris", "Fizikai tudományok", "Társadalomtudományok\nés bölcsészet"))

ggsave(width = 6, height = 4, dpi = 600, "plots/discipline_plot.pdf")
print("Discipline plot done")

stop()



############### publication delay within 1 month



data |> 
  filter(publication_delay <=31) |> 
  mutate(
    article_date_year = floor_date(as_date(article_date), "year"),
    article_date_year = str_replace(article_date_year, "-01-01", ""),
    article_date_year = factor(article_date_year, levels = sort(unique(article_date_year), decreasing = TRUE))
  ) |>
  filter(article_date_year == 2023) |> 
  ggplot(aes(x = publication_delay)) + 
  geom_histogram(binwidth = 1, position = "stack") +
  plot_visuals(
    "Ábra x",
    "Publikálási késések eloszlása 2023-ban",
    "",
    1,
    F,
    NULL,
    "Publikálási késés",
    "N",
    TRUE
  )

ggsave(width = 6, height = 4, dpi = 600, "plots/publication_density_1month.pdf")
print("publication density done")



############### Covid retraction data



covid_n = data |> 
  filter(is_covid == TRUE) |> 
  summarise(
    n_total = n()
  )
write.csv(covid_n, "tables/covid_n.csv")

retraction_covid_data = data |> 
  filter(str_detect(publication_types, "Retracted")) |> 
  group_by(is_covid)

nrow(retraction_covid_data)

retraction_data_table = retraction_covid_data |> 
  summarise(
    n_total = n(),
    mean = mean(acceptance_delay, na.rm = TRUE),
    by_quartile = retraction_covid_data |> 
      group_by(sjr_best_quartile) |> 
      summarise(
        n_quartile = n(),
        mean_quartile = mean(acceptance_delay, na.rm = TRUE)
      ),
    by_discipline = retraction_covid_data |> 
      group_by(discipline) |> 
      summarise(
        n_discipline = n(),
        mean_discipline = mean(acceptance_delay, na.rm = TRUE)
      )
  )
write.csv(retraction_data_table, "tables/retraction_covid_data.csv")



############### Monthly changes



monthly_data = data |> 
  mutate(
    article_date_year = floor_date(as_date(article_date), "year"),
    article_date_year = str_replace(article_date_year, "-01-01", ""),
    article_date_year = factor(article_date_year, levels = sort(unique(article_date_year), decreasing = TRUE))
  ) |>
  dplyr::mutate(article_date_month = lubridate::floor_date(as_date(article_date), "month")) |>
  dplyr::group_by(article_date_month, article_date_year) |>
  dplyr::reframe(
    delay = median(
      acceptance_delay,
      na.rm = TRUE
    )
  ) |>
  tidyr::drop_na(delay)

monthly_plot <- ggplot(monthly_data,
                          aes(
                            x = as_date(article_date_month),
                            y = delay, colour = factor(article_date_year)
                          )
) +
  geom_point(alpha = 0.8, size = 1) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%m"
  )
  
ggsave(width = 6, height = 4, dpi = 600, "plots/monthly_data_acceptance.pdf")
stop()
