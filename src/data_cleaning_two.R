library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

data = readr::read_tsv("/users/zsimi/pubdelays/filtered_articles.tsv")

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
  dplyr::sample_n(100000)
  dplyr::select(acceptance_delay, article_date_month, publication_delay)

print("check 1")

articles_unfiltered_compare = articles_unfiltered |> 
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  dplyr::select(article_date_month, acceptance_delay, publication_delay)

print("check 2")

ggplot() +
  geom_line(data = articles_filtered_compare, aes(x = article_date_month, y = acceptance_delay), color = "blue") +
  geom_line(data = articles_unfiltered_compare, aes(x = article_date_month, y = acceptance_delay), color = "red") +
  labs(title = "Line Plot with Two Datasets",
       x = "Article Date",
       y = "acceptance_delay") +
  theme_minimal() +
  ylim = 500

print("check 3")

ggsave("dataset_compare_acceptance_delay.pdf")
print("Acceptance delay between the two datasets done")

stop("first plot done")
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

print("second plot proceeded")

ggplot() +
  geom_line(data = articles_filtered_compare, aes(x = article_date_month, y = acceptance_delay), color = "blue") +
  geom_line(data = articles_unfiltered_compare, aes(x = article_date_month, y = acceptance_delay), color = "red") +
  labs(title = "Line Plot with Two Datasets",
       x = "Article Date",
       y = "acceptance_delay") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("acceptance_delay_two_datasets.pdf")
print("Acceptance delay for two datasets done")
