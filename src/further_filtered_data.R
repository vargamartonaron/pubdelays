library(lubridate)
library(readr)
library(dplyr)

original_data = readr::read_tsv("/users/zsimi/pubdelays/journal_articles_everything.tsv")

colnames(original_data)


megajournals <- c(
  "24701343", "21583226", "20466390", "20446055", "23251026",
  "22115463", "21601836", "21693536", "20513305", "21678359",
  "19326203", "20545703", "21582440", "20452322", "20566700",
  "23915447", "22991093", "24058440", "21508925", "2050084X",
  "20461402"
)

data = original_data |> 
  dplyr::mutate(article_date = floor_date(as_date(article_date)), "month") |> 
  dplyr::mutate(received = floor_date(as_date(article_date)), "month") |> 
  dplyr::filter(received >= "2016-01-01" & received <= "2023-06-01") |>
  dplyr::filter(article_date >= "2016-01-01" & article_date <= "2023-06-01") |> 
  dplyr::filter(!is.na(article_date) & !is.na(acceptance_delay)) |> 
  dplyr::filter(acceptance_delay <= 700 & acceptance_delay >= 10) |> 
  dplyr::mutate(is_psych = ifelse(((3200 <= `all_science_journal_classification_codes_(asjc)`) & (3207 >= `all_science_journal_classification_codes_(asjc)`)), TRUE, FALSE),
                covid_time = if_else(article_date < as.Date("2019-12-01"), "Before", "After"),
                is_mega = case_when(
                  issn %in% megajournals ~ TRUE,
                  TRUE ~ FALSE
                ))|> 
  dplyr::mutate(article_date = as_date(article_date)) |> 
  dplyr::mutate(received = as_date(received)) |> 
  dplyr::select(is_covid, received, article_date, acceptance_delay, is_psych, covid_time, is_mega, issn, h_index, publication_delay, "does_the_journal_comply_to_doaj's_definition_of_open_access?", title, journal_title, sjr, rank, discipline, doaj_seal, "all_science_journal_classification_codes_(asjc)") |> 
  dplyr::distinct()

print("data filtered")

class(data$article_date)
head(data$article_date)

readr::write_tsv(data, "filtered_articles.tsv")

stop("Analysis done")


