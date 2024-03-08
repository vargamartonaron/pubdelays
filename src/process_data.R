setwd("~/pubdelays")
library(groundhog)
# using groundhog for the sake of reproducibility, info at https://groundhogr.com/

packages <- c("dplyr", "jsonlite", "fuzzyjoin", "tidyr", "readr", "lubridate", "stringr")

groundhog.library(packages, "2023-12-02")

# import json and csv data

articles <- read_tsv("D:/University/ELTE/MetaScienceLab/Publication delay/journal_articles_sliced_100k.tsv")
articles <- jsonlite::fromJSON("/users/usumusu/pubmed_medline_articles.json")
print(nrow(articles))
print("JSON parsed into dataframe.")
scimago <- readr::read_csv("/users/usumusu/scimagojr_2022.csv")
print("Journal data parsed into dataframe.")
webofscience = readr::read_csv()
print("Web of Science parsed into dataframe.")
doaj = readr::read_csv("https://s3.eu-west-2.amazonaws.com/doaj-data-cache/journalcsv__doaj_20240308_1620_utf8.csv")
#This only works today, we probably need an offline csv
print("Directory of Open Access Journals parsed into dataframe.")

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

replication_synonyms <- c('replication', 'replicating', 'replication of', 'replication study')

replication_pattern <- paste0("\\b(?:", paste(replication_synonyms, collapse = "|"), ")\\b")

# unnest and ensure types

#articles = articles |> 
  #Cut the first 10000 rows
  #dplyr::slice(1:10000) |>
  #tidyr::drop_na(article_date) |>
  #only preserve years and months from the date variable
  #dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-"))

articles <- articles |>
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
  dplyr::mutate(is_covid = dplyr::if_else((stringr::str_detect(title, stringr::regex(covid_pattern, ignore_case = TRUE)) | stringr::str_detect(keywords, stringr::regex(covid_pattern, ignore_case = TRUE))), TRUE, FALSE)) |>
  dplyr::mutate(is_replication = dplyr::if_else((stringr::str_detect(title, stringr::regex(replication_pattern, ignore_case = TRUE)) | stringr::str_detect(keywords, stringr::regex(replication_pattern, ignore_case = TRUE))), TRUE, FALSE)) |>
  dplyr::select(-`pmc-release`, -retracted, -aheadofprint, -ecollection)

print("Articles processed.")

scimago <- scimago |>
  dplyr::rowwise() |>
  dplyr::ungroup() |>
  dplyr::select(-Sourceid, -Type, -Areas_1, -Areas_2, -Areas_3, -Areas_4, -Areas_5, -Region, -Publisher, -`Total Docs.`, -`Ref. / Doc.`, -`Cites / Doc. (2years)`, -`SJR Best Quartile`) |>
  dplyr::rename(journal_title = Title)

print("Scimago processed.")

webofscience <- webofscience |>
  dplyr::rowwise() |>
  dplyr::ungroup() |>
  dplyr::mutate(
    issn = paste(webofscience$'Print-ISSN', webofscience$'E-ISSN', sep = ", ")) |>
  dplyr::filter(`Source Type` == "Journal") |>
  tidyr::unite("discipline", c(`Top level:\n\nHealth Sciences`, `Top level:\n\nPhysical Sciences`,  `Top level:\n\nSocial Sciences`, `Top level:\n\nLife Sciences`), na.rm = TRUE, sep = ", ") |> 
  dplyr::mutate(discipline = ifelse(`All Science Journal Classification Codes (ASJC)` == 1000, "multidisciplinary", discipline),
                discipline = ifelse(grepl(",", discipline), "multidisciplinary", discipline)) |>
  dplyr::select('Source Title', 'issn', 'Open Access status', 'Source Type', 'All Science Journal Classification Codes (ASJC)', discipline)

print("Web of Science processed.")

doaj <- doaj |> 
  dplyr::select(`Journal ISSN (print version)`, `Journal EISSN (online version)`, `Review process`, `APC`, `APC amount`, `DOAJ Seal`, `Does the journal comply to DOAJ's definition of open access?`) |> 
  dplyr::mutate(issn = paste(doaj$`Journal ISSN (print version)`, doaj$`Journal EISSN (online version)`, sep = ", ")) |> 
  dplyr::mutate(issn = stringr::str_replace_all(issn, "-", "")) |> 
  dplyr::mutate(issn = stringr::str_replace_all(issn, "NA", "")) |> 
  dplyr::select(-`Journal ISSN (print version)`, -`Journal EISSN (online version)`)
  
print("DOAJ processed.")
  
joined_scimago <- scimago |>
  fuzzyjoin::regex_inner_join(articles, by = c(Issn = "issn_linking"))  |>
  dplyr::select(-Issn)

print("Scimago joined")

joined_wos <- webofscience |>
  fuzzyjoin::regex_inner_join(articles, by = c('issn'= "issn_linking")) |>
  dplyr::select(-issn, -`Source Type`, -`Source Title`)

print("Web of Science joined")

joined_doaj <- doaj |>
  fuzzyjoin::regex_inner_join(articles, by = c(issn = "issn_linking")) |>
  dplyr::select(-issn)
#This is not good
print("DOAJ joined")

joined = joined_doaj |> 
  distinct(title, .keep_all = TRUE)
#We have to rewrite this so the name is the same as the last dataset, which contains everything
print("Joined dataframes.")
print(nrow(joined))

names(joined) <- gsub(" ", "_", tolower(names(joined)))

print("Writing to tsv.")
readr::write_tsv(joined, "journal_articles.tsv")