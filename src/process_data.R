install.packages("groundhog")
library(groundhog)
# using groundhog for the sake of reproducibility, info at https://groundhogr.com/

packages <- c("tidyverse", "jsonlite", "future", "progress", "fuzzyjoin")

groundhog.library(packages, "2023-12-03")

# import json and csv data

articles <- jsonlite::fromJSON("/home/martonaronvarga/Downloads/pubmed_medline_articles.json")
print("JSON parsed into dataframe.")
scimago <- readr::read_csv("/home/martonaronvarga/GitHub/pubdelays/Data/scimagojr_2022.csv")
print("Journal data parsed into dataframe.")

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
  dplyr::mutate(acceptance_delay = difftime(accepted, received, units = "days")) |>
  dplyr::mutate(publication_delay = as.numeric(ifelse(!is.na(article_date), difftime(article_date, accepted, units = "days"), difftime(pubdate, accepted, units = "days")))) |>
  dplyr::filter(acceptance_delay < 0 | publication_delay < 0)

scimago <- scimago |>
  dplyr::rowwise() |>
  dplyr::mutate(areas = paste(dplyr::c_across(tidyselect::starts_with("Areas"))[!is.na(c_across(starts_with("Areas")))], collapse=", ")) |>
  dplyr::ungroup() |>
  dplyr::select(-Sourceid, -Type, -Areas_1, -Areas_2, -Areas_3, -Areas_4, -Areas_5, -Region, -Publisher, -`Total Docs.`, -`Ref. / Doc.`, -`Total Docs. (3years)`) |>
  dplyr::rename(journal_title = Title)

joined <- scimago |>
  fuzzyjoin::regex_inner_join(articles, by=c(Issn = "issn_linking")) |>
  dplyr::select(-Issn, -journal)

names(joined) <- gsub(" ", "_", tolower(names(joined)))




