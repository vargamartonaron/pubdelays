setwd("~/pubdelays")
#library(groundhog)
# using groundhog for the sake of reproducibility, info at https://groundhogr.com/

#packages <- c("dplyr", "jsonlite", "fuzzyjoin", "tidyr", "readr", "lubridate", "stringr")

#groundhog.library(packages, "2023-12-02")
# import json and csv data

library(dplyr)
library(readr)
library(jsonlite)
library(tidyr)
library(lubridate)
library(stringr)
library(fuzzyjoin)

#articles <- read_tsv("D:/University/ELTE/MetaScienceLab/Publication delay/journal_articles_sliced_100k.tsv")
articles <- jsonlite::fromJSON("/users/zsimi/pubmed_medline_articles.json")
print(nrow(articles))
print("JSON parsed into dataframe.")
scimago <- readr::read_csv("/users/zsimi/scimagojr_2022.csv")
print("Journal data parsed into dataframe.")
webofscience <- readr::read_csv("/users/zsimi/ext_list_February_2024.csv")
print("Web of Science parsed into dataframe.")
doaj <- readr::read_csv("/users/zsimi/doaj.csv")
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
  dplyr::filter((acceptance_delay > 0 & publication_delay > 0) |
                  (acceptance_delay > 0 & is.na(publication_delay)) |
                  (is.na(acceptance_delay) & publication_delay > 0) |
                  (is.na(acceptance_delay) & is.na(publication_delay))) |>
  dplyr::mutate(article_date = floor_date(as_date(article_date)), "month") |> 
  dplyr::mutate(received = floor_date(as_date(article_date)), "month") |> 
  dplyr::filter(received >= "2016-01-01" & received <= "2023-06-01") |>
  dplyr::filter(article_date >= "2016-01-01" & article_date <= "2023-06-01") |> 
  dplyr::filter(!is.na(article_date) & !is.na(acceptance_delay)) |> 
  dplyr::filter(acceptance_delay >= quantile(acceptance_delay, 0.01) & 
                  acceptance_delay <= quantile(acceptance_delay, 0.99)) |>
  dplyr::filter(publication_delay >= quantile(publication_delay, 0.01) & 
                  publication_delay <= quantile(publication_delay, 0.99)) |>
  dplyr::mutate(is_covid = dplyr::if_else((stringr::str_detect(title, stringr::regex(covid_pattern, ignore_case = TRUE)) |
                                           stringr::str_detect(keywords, stringr::regex(covid_pattern, ignore_case = TRUE))), TRUE, FALSE)) |>
  dplyr::mutate(is_replication = dplyr::if_else((stringr::str_detect(title, stringr::regex(replication_pattern, ignore_case = TRUE)) |
                                                 stringr::str_detect(keywords, stringr::regex(replication_pattern, ignore_case = TRUE))), TRUE, FALSE)) |>
  dplyr::select(-`pmc-release`, -retracted, -aheadofprint, -ecollection)

#Limit for faster run times
articles_small = articles |> 
  sample_n(10000)

readr::write_tsv(articles_small, "articles_raw_10000.tsv")
print("10000 Articles processed.")

readr::write_tsv(articles, "articles_raw.tsv")
print("Articles processed.")


scimago <- scimago |>
  dplyr::rowwise() |>
  dplyr::ungroup() |>
  dplyr::select(-Sourceid, -Type, -Areas_1, -Areas_2, -Areas_3, -Areas_4, -Areas_5, -Region, -Publisher, -`Total Docs.`, -`Ref. / Doc.`, -`Cites / Doc. (2years)`, -`SJR Best Quartile`) |>
  dplyr::rename(journal_title = Title)

print("Scimago processed.")

webofscience <- webofscience |>
  dplyr::mutate(
    issn = paste(webofscience$'Print-ISSN', webofscience$'E-ISSN', sep = ", ")) |>
  dplyr::filter(`Source Type` == "Journal") |>
  tidyr::unite("discipline",
	      c(`Top level:\n\nHealth Sciences`,
		 `Top level:\n\nPhysical Sciences`,  `Top level:\n\nSocial Sciences`, `Top level:\n\nLife Sciences`), na.rm = TRUE, sep = ", ") |> 
dplyr::mutate(discipline = ifelse(`All Science Journal Classification Codes (ASJC)` == 1000, "multidisciplinary", discipline),
                discipline = ifelse(grepl(",", discipline), "multidisciplinary", discipline)) |>
  dplyr::select('Source Title', 'issn', 'Open Access status', 'Source Type', 'All Science Journal Classification Codes (ASJC)', discipline)

print("Wos processed.") 

doaj <- doaj |> 
  dplyr::select(`Journal ISSN (print version)`, `Journal EISSN (online version)`, `Review process`, `APC`, `APC amount`, `DOAJ Seal`, `Does the journal comply to DOAJ's definition of open access?`) |> 
  dplyr::mutate(issn = paste(`Journal ISSN (print version)`, `Journal EISSN (online version)`, sep = ", ")) |> 
  dplyr::mutate(issn = stringr::str_replace_all(issn, "-", "")) |> 
  dplyr::mutate(issn = stringr::str_replace_all(issn, "NA", "")) |> 
  dplyr::mutate(issn = ifelse(startsWith(issn, ", "), substr(issn, 3, nchar(issn)), issn)) |> 
  dplyr::select(-`Journal ISSN (print version)`, -`Journal EISSN (online version)`)

  
print("DOAJ processed.")

  
joined_scimago <- scimago |>
 fuzzyjoin::regex_inner_join(articles, by = c('Issn' = "issn_linking"))  |>
  dplyr::select(-Issn)

readr::write_tsv(joined_scimago, "articles_scimago.tsv")
print("Scimago joined")

joined_wos <- webofscience |>
  fuzzyjoin::regex_inner_join(joined_scimago, by = c('issn'= "issn_linking")) |>
  dplyr::select(-issn, -`Source Type`, -`Source Title`)

readr::write_tsv(joined_wos, "articles_scimago_wos.tsv")
print("Web of Science joined")

joined_doaj <- doaj |>
  fuzzyjoin::regex_inner_join(joined_wos, by = c('issn' = "issn_linking")) |>
  dplyr::select(-issn)

readr::write_tsv(joined_doaj, "articles_scimago_wos_doaj.tsv")
print("Directory of Open Access joined")


print(paste("There are", nrow(articles), "articles without linking with outher datasets."))
print(nrow(articles))
print(paste("There are", nrow(joined_scimago), "articles with scimago."))
print(nrow(joined_scimago))

print(paste("There are", nrow(joined_wos), "articles with scimago and Web of Science."))
print(nrow(joined_wos))
print(paste("There are", nrow(joined_doaj), "articles with scimago, Web of Science, and DOAJ."))
print(nrow(joined_doaj))


names(joined_doaj) = gsub(" ", "_", tolower(names(joined_doaj)))

colnames(joined_doaj)

megajournals <- c(
  "24701343", "21583226", "20466390", "20446055", "23251026",
  "22115463", "21601836", "21693536", "20513305", "21678359",
  "19326203", "20545703", "21582440", "20452322", "20566700",
  "23915447", "22991093", "24058440", "21508925", "2050084X",
  "20461402"
)

data = joined_doaj |> 
  dplyr::mutate(is_psych = ifelse(((3200 <= `all_science_journal_classification_codes_(asjc)`) &
                                     (3207 >= `all_science_journal_classification_codes_(asjc)`)), TRUE, FALSE),
                covid_time = if_else(article_date < as.Date("2019-12-01"), "Before", "After"),
                is_mega = case_when(
                  issn_linking %in% megajournals ~ TRUE,
                  TRUE ~ FALSE
                ))|> 
  dplyr::mutate(article_date = as_date(article_date)) |> 
  dplyr::mutate(received = as_date(received)) |> 
  dplyr::select(is_covid, received, article_date, acceptance_delay, is_psych, covid_time, is_mega, issn_linking, h_index, publication_delay,
                "does_the_journal_comply_to_doaj's_definition_of_open_access?", title, journal_title, sjr, rank, discipline, doaj_seal, "all_science_journal_classification_codes_(asjc)") |> 
  dplyr::distinct()

print("data filtered")

class(data$article_date)
head(data$article_date)

readr::write_tsv(data, "filtered_articles.tsv")

data_10000 = data |> 
  sample_n(10000)

readr::write_tsv(data_10000, "filtered_articles_10000.tsv")

stop("Analysis done")
