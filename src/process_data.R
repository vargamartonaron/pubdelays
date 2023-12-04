setwd("~/pubdelays")
library(groundhog)
# using groundhog for the sake of reproducibility, info at https://groundhogr.com/

packages <- c("dplyr", "jsonlite", "fuzzyjoin", "tidyr", "readr", "lubridate", "stringr")

groundhog.library(packages, "2023-12-04")

# import json and csv data

articles <- jsonlite::fromJSON("/users/usumusu/pubmed_medline_articles.json")
print("JSON parsed into dataframe.")
scimago <- readr::read_csv("/users/usumusu/scimagojr_2022.csv")
print("Journal data parsed into dataframe.")

covid_synonyms <- c('covid',
                     'covid-19',
                     'coronavirus disease 19',
                     'sars-cov-2',
                     '2019-ncov',
                     '2019ncov',
                     '2019-n-cov',
                     '2019n-cov',
                     'ncov-2019',
                     'n-cov-2019',
                     'coronavirus-2019',
                     'wuhan pneumonia',
                     'wuhan virus',
                     'wuhan coronavirus',
                     'coronavirus 2')

covid_pattern <- paste0("\\b(?:", paste(covid_synonyms, collapse = "|"), ")\\b")

replication_synonyms <- c('replication, replicating, replication of, replication study')

replication_pattern <- paste0("\\b(?:", paste(replication_synonyms, collapse = "|"), ")\\b")

# unnest and ensure types

articles <- articles |>
  tidyr::unnest(history) |>
  dplyr::mutate(keywords = str_replace_all(keywords, ";", ",")) |>
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
  dplyr::mutate(is_covid = dplyr::if_else((stringr::str_detect(title, regex(covid_pattern, ignore_case = TRUE)) | stringr::str_detect(keywords, regex(covid_pattern, ignore_case = TRUE))), TRUE, FALSE)) |>
  dplyr::mutate(is_replication = dplyr::if_else((stringr::str_detect(title, regex(replication_pattern, ignore_case = TRUE)) | stringr::str_detect(keywords, regex(replication_pattern, ignore_case = TRUE))), TRUE, FALSE))

print("Articles processed.")

scimago <- scimago |>
  dplyr::rowwise() |>
  dplyr::mutate(areas = paste(dplyr::c_across(tidyselect::starts_with("Areas"))[!is.na(c_across(starts_with("Areas")))], collapse=", ")) |>
  dplyr::ungroup() |>
  dplyr::select(-Sourceid, -Type, -Areas_1, -Areas_2, -Areas_3, -Areas_4, -Areas_5, -Region, -Publisher, -`Total Docs.`, -`Ref. / Doc.`, -`Total Docs. (3years)`) |>
  dplyr::rename(journal_title = Title)

print("Scimago processed.")

joined <- scimago |>
  fuzzyjoin::regex_inner_join(articles, by=c(Issn = "issn_linking")) |>
  dplyr::select(-Issn, -journal)

print("Joined dataframes.")

names(joined) <- gsub(" ", "_", tolower(names(joined)))

print("Writing to csv.")
readr::write_csv(joined, "journal_articles.csv")