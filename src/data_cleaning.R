library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

data = readr::read_tsv("/users/zsimi/pubdelays/filtered_articles.tsv")

############### Print the name of the columns

colnames(data)

############### Check if there are multiple articles with the same name

multiple <- data |>
  dplyr::group_by(title) |>
  dplyr::summarise(articles = n()) |>
  dplyr::arrange(desc(articles))

print(multiple)
print("multiple articles checked")

############### Articles per month

result = data |>
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  dplyr::group_by(article_date_month) |>
  dplyr::summarise(title = n())

print(result)
print("Articles per month done")
write.csv(result, "articles_per_month.csv", row.names = FALSE)

############### Density plot

histogram_plot_month = data |>
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  ggplot2::ggplot(aes(x = article_date_month)) +
  ggplot2::geom_bar() +
  ggplot2::labs(title = "Density plot of articles per month", x = "Month", y = "Number of articles") +
  ggplot2::theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

ggsave("histogram_plot_month.pdf")
print("Histogram plot month done")


############### Articles per year

articles_year <- data |>
  dplyr::mutate(article_date_year = lubridate::year(article_date)) |>
  dplyr::group_by(article_date_year) |>
  dplyr::summarise(title = n())

print(articles_year)
print("Articles per year done")
write.csv(articles_year, "articles_per_year.csv", row.names = FALSE)

histogram_plot_year = data |>
  dplyr::mutate(article_date_year = lubridate::year(article_date)) |>
  ggplot2::ggplot(aes(x = article_date_year)) +
  ggplot2::geom_bar() +
  ggplot2::labs(title = "Density plot of articles per year", x = "Year", y = "Number of articles") +
  ggplot2::theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

ggsave("histogram_plot_year.pdf")
print("Histogram plot year done")

############### Daily articles

daily_articles <- data |>
  dplyr::mutate(article_date_day = lubridate::day(article_date)) |>
  dplyr::group_by(article_date_day) |>
  dplyr::summarise(title = n())

print(daily_articles)

daily_articles <- data |>
  dplyr::group_by(article_date) |>
  dplyr::summarise(title = n())

print(daily_articles)

print("Articles per day done")
write.csv(daily_articles, "articles_per_day.csv", row.names = FALSE)

############### Count the Bottom and top 1% acceptance delay

acceptance_1perc_bottom <- data %>%
  filter(!is.na(acceptance_delay)) %>%
  filter(acceptance_delay <= quantile(acceptance_delay, 0.01)) 
acceptance_1perc_top <- data %>%
  filter(!is.na(acceptance_delay)) %>%
  filter(acceptance_delay >= quantile(acceptance_delay, 0.99))

acceptance_1perc_top = acceptance_1perc_top |> 
  mutate(nrows = nrow(acceptance_1perc_top))
acceptance_1perc_bottom = acceptance_1perc_bottom |> 
  mutate(nrows = nrow(acceptance_1perc_bottom))
write.csv(acceptance_1perc_bottom, "acceptance_delay_bottom_1_percent.csv", row.names = FALSE)
write.csv(acceptance_1perc_top, "acceptance_delay_top_1_percent.csv", row.names = FALSE)


acceptance_1perc_year = data |> 
  dplyr::filter(!is.na(acceptance_delay)) |>
  dplyr::mutate(article_date_year = lubridate::year(article_date)) |>
  group_by(article_date_year) |> 
  summarise(
    bottom_1 = quantile(acceptance_delay, 0.01, na.rm = T),
    top_1 = quantile(acceptance_delay, 0.99, na.rm = T)
  )
print(acceptance_1perc_year)
write.csv(acceptance_1perc_year, "acceptance_1perc_year.csv", row.names = FALSE)

print("Bottom-top 1% acceptance delay done")


############### Count the Bottom and top 1% publication delay

publication_1perc_bottom <- data %>%
  filter(!is.na(publication_delay)) %>%
  filter(publication_delay <= quantile(publication_delay, 0.01)) 
publication_1perc_top <- data %>%
  filter(!is.na(publication_delay)) %>%
  filter(publication_delay >= quantile(publication_delay, 0.99))

publication_1perc_top = publication_1perc_top |> 
  mutate(nrows = nrow(publication_1perc_top))
publication_1perc_bottom = publication_1perc_bottom |> 
  mutate(nrows = nrow(publication_1perc_bottom))
write.csv(publication_1perc_bottom, "publication_delay_bottom_1_percent.csv", row.names = FALSE)
write.csv(publication_1perc_top, "publication_delay_top_1_percent.csv", row.names = FALSE)


publication_1perc_year = data |> 
  dplyr::filter(!is.na(publication_delay)) |>
  dplyr::mutate(article_date_year = lubridate::year(article_date)) |>
  group_by(article_date_year) |> 
  summarise(
    bottom_1 = quantile(publication_delay, 0.01, na.rm = T),
    top_1 = quantile(publication_delay, 0.99, na.rm = T)
  )
print(publication_1perc_year)
write.csv(publication_1perc_year, "publication_1perc_year.csv", row.names = FALSE)

print("Bottom-top 1% publication delay done")

############### Count articles per journal

journal_n_article = data |>
  dplyr::group_by(journal_title) |>
  dplyr::summarise(title = n()) |>
  dplyr::arrange(desc(journal_title))

print(journal_n_article)
write.csv(journal_n_article, "journal_articles_n.csv", row.names = F)
print("Articles per journal done")

############### Number of journals

journal_n <- data |>
  dplyr::summarize(n_distinct_journals = n_distinct(journal_title))
 
print(journal_n)
write.csv(journal_n, "journal_n.csv", row.names = F)
print("Journal count done")

############### Acceptance Delays per month

result = data |>
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  dplyr::group_by(article_date_month) |>
  dplyr::summarise(acceptance_delay = mean(acceptance_delay)) |> 
  ungroup()

print(result)
print("Acceptance delay per month done")
write.csv(result, "acceptance_delay_per_month.csv", row.names = FALSE)

result$article_date_month <- as.Date(result$article_date_month, format = "%Y-%m-%d")
ggplot(result, aes(x = article_date_month, y = acceptance_delay)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Month", y = "Average Acceptance Delay", title = "Average Acceptance Delay by Month") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("acceptance_delay_month.pdf")
print("Acceptance delay per month plot done")

ggplot(result, aes(x = article_date_month, y = acceptance_delay)) +
  geom_segment(aes(x = article_date_month, xend = article_date_month, y = 0, yend = acceptance_delay), color = "red") + 
  geom_point(color = "black", size = 1.5) +
  labs(x = "Month", y = "Average Acceptance Delay", title = "Average Acceptance Delay by Month") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("acceptance_delay_month_lollipop.pdf")
print("Acceptance delay per month lollipop plot done")

############### Acceptance Delays per year

result = data |>
  dplyr::mutate(article_date_year = lubridate::year(article_date)) |>
  dplyr::group_by(article_date_year) |>
  dplyr::summarise(acceptance_delay = mean(acceptance_delay)) |> 
  ungroup()

print(result)
print("Acceptance delay per year done")
write.csv(result, "acceptance_delay_per_year.csv", row.names = FALSE)

############### Publication Delays per month

result = data |>
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  dplyr::group_by(article_date_month) |>
  dplyr::summarise(publication_delay = mean(publication_delay)) |> 
  ungroup()

print(result)
print("publication delay per month done")
write.csv(result, "publication_delay_per_month.csv", row.names = FALSE)

############### Publication Delays per year

result = data |>
  dplyr::mutate(article_date_year = lubridate::year(article_date)) |>
  dplyr::group_by(article_date_year) |>
  dplyr::summarise(publication_delay = mean(publication_delay)) |> 
  ungroup()

print(result)
print("publication delay per year done")
write.csv(result, "publication_delay_per_year.csv", row.names = FALSE)

############### Compare filtered and unfiltered article number

articles_unfiltered <- jsonlite::fromJSON("/users/zsimi/pubmed_medline_articles.json")
unfiltered_raw_n = nrow(articles_unfiltered)
paste("There are", unfiltered_raw_n, "number of rows in the unfiltered dataset")
articles_unfiltered <- articles_unfiltered |>
  tidyr::unnest(history) |>
  dplyr::mutate(keywords = stringr::str_replace_all(keywords, ";", ",")) |>
  dplyr::mutate(
    publication_types = stringr::str_extract_all(publication_types, "(?<=:)[^;]+") |> 
      # concatenate types with commas
      lapply(function(x) paste(x, collapse = ", ")) |>
      unlist()
  ) |>
  dplyr::filter(!(is.na(received) & is.na(accepted))) |>
  dplyr::mutate_at(c("pubdate", "received", "revised", "accepted", "pubmed", "medline", "entrez", "article_date"), 
                   # ensure year-month-date format, append 01 where month or day is missing
                   lubridate::as_date, format=c("%Y", "%Y-%m", "%Y-%m-%d")
  ) |>
  dplyr::filter(stringr::str_detect(publication_types, pattern="Journal Article")) |>
  dplyr::mutate(issn_linking = stringr::str_replace_all(issn_linking, "-", "")) |>
  dplyr::filter(!is.na(issn_linking)) |>
  dplyr::mutate(acceptance_delay = as.numeric(difftime(accepted, received, units = "days"))) |>
  dplyr::mutate(publication_delay = as.numeric(dplyr::if_else(!is.na(article_date), difftime(article_date, accepted, units = "days"), difftime(pubdate, accepted, units = "days")))) |>
  dplyr::filter((acceptance_delay > 0 & publication_delay > 0) | (acceptance_delay > 0 & is.na(publication_delay)) | (is.na(acceptance_delay) & publication_delay > 0) | (is.na(acceptance_delay) & is.na(publication_delay))) |>
  dplyr::select(-`pmc-release`, -retracted, -aheadofprint, -ecollection)

print("Articles processed.")

articles_unfiltered = articles_unfiltered |> 
  dplyr::mutate(article_date = floor_date(as_date(article_date)), "month") |> 
  dplyr::mutate(received = floor_date(as_date(article_date)), "month") |> 
  dplyr::filter(!is.na(article_date) & !is.na(acceptance_delay)) |> 
  dplyr::mutate(article_date = as_date(article_date)) |> 
  dplyr::mutate(received = as_date(received)) |> 
  dplyr::select(received, article_date, acceptance_delay, publication_delay, title) |> 
  dplyr::distinct()

print("data filtered")

unfiltered_n = nrow(articles_unfiltered)
filtered_n = nrow(data)
paste("There are", unfiltered_n, "number of rows in the unfiltered dataset")
paste("There are", filtered_n, "number of rows in the filtered dataset")

#Compare filtered and unfiltered acceptance delay

articles_filtered_compare = data |>
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  dplyr::select(acceptance_delay, article_date_month, publication_delay)

articles_unfiltered_compare = articles_unfiltered |> 
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  dplyr::select(article_date_month, acceptance_delay, publication_delay)

min_date <- min(min(articles_unfiltered_compare$article_date_month), min(articles_filtered_compare$article_date_month))
max_date <- max(max(articles_unfiltered_compare$article_date_month), max(articles_filtered_compare$article_date_month))

ggplot() +
  geom_line(data = articles_filtered_compare, aes(x = article_date_month, y = acceptance_delay), color = "blue") +
  geom_line(data = articles_unfiltered_compare, aes(x = article_date_month, y = acceptance_delay), color = "red") +
  scale_x_date(limits = c(min_date, max_date)) +
  labs(title = "Line Plot with Two Datasets",
       x = "Article Date",
       y = "acceptance_delay") +
  theme_minimal()

ggsave("dataset_compare_acceptance_delay.pdf")
print("Acceptance delay between the two datasets done")


##############################################################################################################

articles_filtered_compare = data |>
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  dplyr::select(acceptance_delay, article_date_month, publication_delay) |> 
  dplyr::group_by(article_date_month) |>
  dplyr::summarise(acceptance_delay = mean(acceptance_delay)) |> 
  ungroup()


articles_unfiltered_compare = articles_unfiltered |>
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  dplyr::select(acceptance_delay, article_date_month, publication_delay) |> 
  dplyr::group_by(article_date_month) |>
  dplyr::summarise(acceptance_delay = mean(acceptance_delay)) |> 
  ungroup()

articles_unfiltered$article_date_month <- as.Date(articles_unfiltered$article_date_month, format = "%Y-%m-%d")
articles_filtered$article_date_month <- as.Date(articles_filtered$article_date_month, format = "%Y-%m-%d")


ggplot() +
  geom_line(data = articles_filtered_compare, aes(x = article_date_month, y = acceptance_delay), color = "blue") +
  geom_line(data = articles_unfiltered_compare, aes(x = article_date_month, y = acceptance_delay), color = "red") +
  scale_x_date(limits = c(min_date, max_date)) +
  labs(title = "Line Plot with Two Datasets",
       x = "Article Date",
       y = "acceptance_delay") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("acceptance_delay_two_datasets.pdf")
print("Acceptance delay for two datasets done")


################################################################################################################

#Compare filtered and unfiltered publication delay

#Investigate other columns

 
#test difference between filtered and unfiltered

ggplot() +
  geom_density(aes(x = acceptance_delay, color = "Full Dataset"), data = articles_unfiltered) +
  geom_density(aes(x = acceptance_delay, color = "Subset Dataset"), data = articles_filtered) +
  labs(title = "Acceptance Delay Distribution",
       x = "Acceptance Delay",
       y = "Density") +
  scale_color_manual(name = "Dataset", values = c("Full Dataset" = "blue", "Subset Dataset" = "red"))

ggsave("dataset_distributions.pdf")
print("Distributions for two datasets done")

ks_test_result <- ks.test(articles_unfiltered$acceptance_delay, articles_filtered$acceptance_delay)
print(ks_test_result)

stop("test 2.c")

#Discipline check

result = data |> 
  group_by(discipline) |> 
  summarize(count = n())

print(result)
print("Discipline check done")
write.csv(result, "discipline_count.csv", row.names = FALSE)

stop("test 2.d")

#Each year curve on same graph

data |>
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date), "01", sep = "-")) |>
  dplyr::mutate(article_date_year = lubridate::year(article_date)) |>
  dplyr::mutate(month = lubridate::month(article_date)) |>
  dplyr::group_by(article_date_year, month) |>
  dplyr::summarize(mean_acceptance_delay = mean(acceptance_delay, na.rm = TRUE)) |>
  ggplot(aes(x = month, y = mean_acceptance_delay, colour = factor(article_date_year))) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = 1:12, labels = month.name) +
  labs(title = "Mean Acceptance Delay by Month for Each Year",
       x = "Month",
       y = "Mean Acceptance Delay",
       colour = "Year") +
  ylim(100, 150) +
  theme_minimal()


ggsave("acceptance_delay_per_year.pdf")
print("Acceptance delay per year done")
#Check if acc and pub delay aligns with the timeline

#Check if journal publishes same values

#Distribution of delays per year

data |> 
  mutate(article_date_year = paste(lubridate::year(article_date))) |> 
  ggplot(aes(x = acceptance_delay, colour = article_date_year)) |> 
  geom_density()

stop("test 6")

#Top, middle, bottom journal values compared

##By SJR

result <- data |> 
  group_by(journal) |> 
  mutate(
    quantiles = quantile(sjr, probs = c(0.25, 0.75)),
    group = case_when(
      article_number <= quantiles[1] ~ "Bottom 25%",
      article_number > quantiles[1] & sjr <= quantiles[2] ~ "Middle 50%",
      article_number > quantiles[2] ~ "Upper 25%"
    )
  ) |> 
  select(-quantiles) |> 
  group_by(group) |> 
  summarize(avg_acceptance_delay = mean(avg_acceptance_delay))

print(result)
write.csv(result, "journal_rank_quantiles_acceptance_delay.csv")

stop("test 5")

##By article number

result <- data |> 
  group_by(journal) |> 
  summarize(
    article_number = n(),
    avg_acceptance_delay = mean(acceptance_delay)
  ) |> 
  mutate(
    quantiles = quantile(article_number, probs = c(0.25, 0.75)),
    group = case_when(
      article_number <= quantiles[1] ~ "Bottom 25%",
      article_number > quantiles[1] & article_number <= quantiles[2] ~ "Middle 50%",
      article_number > quantiles[2] ~ "Upper 25%"
    )
  ) |> 
  select(-quantiles) |> 
  group_by(group) |> 
  summarize(avg_acceptance_delay = mean(avg_acceptance_delay))

print(result)
write.csv(result, "journal_n_articles_quantiles_acceptance_delay.csv")

stop("test 3")

##By rank

result <- data |> 
  group_by(journal) |> 
  mutate(
    quantiles = quantile(rank, probs = c(0.25, 0.75)),
    group = case_when(
      article_number <= quantiles[1] ~ "Bottom 25%",
      article_number > quantiles[1] & rank <= quantiles[2] ~ "Middle 50%",
      article_number > quantiles[2] ~ "Upper 25%"
    )
  ) |> 
  select(-quantiles) |> 
  group_by(group) |> 
  summarize(avg_acceptance_delay = mean(avg_acceptance_delay))

print(result)
write.csv(result, "journal_rank_quantiles_acceptance_delay.csv")

stop("test 4")