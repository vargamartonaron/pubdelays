setwd("~/pubdelays")
library(groundhog)
# using groundhog for the sake of reproducibility, info at https://groundhogr.com/

#packages <- c("dplyr", "jsonlite", "fuzzyjoin", "tidyr", "readr", "lubridate", "stringr")

#groundhog.library(packages, "2023-12-02")
# import json and csv data

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
  dplyr::mutate(
    issn = paste(webofscience$'Print-ISSN', webofscience$'E-ISSN', sep = ", ")) |>
  dplyr::filter(`Source Type` == "Journal") |>
  tidyr::unite("discipline",
	      c(`Top level:\n\nHealth Sciences`,
		 `Top level:\n\nPhysical Sciences`,  `Top level:\n\nSocial Sciences`, `Top level:\n\nLife Sciences`), na.rm = TRUE, sep = ", ") |> 
dplyr::mutate(discipline = ifelse(`All Science Journal Classification Codes (ASJC)` == 1000, "multidisciplinary", discipline),
                discipline = ifelse(grepl(",", discipline), "multidisciplinary", discipline)) |>
  dplyr::mutate(is_psych = ifelse((3200 <= `All Science Journal Classification Codes (ASJC)`) & (3207 >= `All Science Journal Classification Codes (ASJC)`), TRUE, FALSE)) |>
  dplyr::select('Source Title', 'issn', 'Open Access status', 'Source Type', 'All Science Journal Classification Codes (ASJC)', discipline)

print("Wos done.") 
 

#doaj <- doaj |> 
 # dplyr::select(`Journal ISSN (print version)`, `Journal EISSN (online version)`, `Review process`, `APC`, `APC amount`, `DOAJ Seal`, `Does the journal comply to DOAJ's definition of open access?`) |> 
#  dplyr::mutate(issn = paste(doaj$`Journal ISSN (print version)`, doaj$`Journal EISSN (online version)`, sep = ", ")) |> 
#  dplyr::mutate(issn = stringr::str_replace_all(issn, "-", "")) |> 
#  dplyr::mutate(issn = stringr::str_replace_all(issn, "NA", "")) |> 
#  dplyr::select(-`Journal ISSN (print version)`, -`Journal EISSN (online version)`)

doaj <- doaj |> 
  dplyr::select(`Journal ISSN (print version)`, `Journal EISSN (online version)`, `Review process`, `APC`, `APC amount`, `DOAJ Seal`, `Does the journal comply to DOAJ's definition of open access?`) |> 
  dplyr::mutate(issn = paste(`Journal ISSN (print version)`, `Journal EISSN (online version)`, sep = ", ")) |> 
  dplyr::mutate(issn = stringr::str_replace_all(issn, "-", "")) |> 
  dplyr::mutate(issn = stringr::str_replace_all(issn, "NA", "")) |> 
  dplyr::mutate(issn = ifelse(startsWith(issn, ", "), substr(issn, 3, nchar(issn)), issn)) |> 
  dplyr::select(-`Journal ISSN (print version)`, -`Journal EISSN (online version)`)

  
print("DOAJ processed.")
#warnings()
  
joined_scimago <- scimago |>
 fuzzyjoin::regex_inner_join(articles, by = c('Issn' = "issn_linking"))  |>
  dplyr::select(-Issn)

print("Scimago joined")

joined_wos <- webofscience |>
  fuzzyjoin::regex_inner_join(joined_scimago, by = c('issn'= "issn_linking")) |>
  dplyr::select(-issn, -`Source Type`, -`Source Title`)

print("Web of Science joined")

joined_doaj <- doaj |>
  fuzzyjoin::regex_inner_join(joined_wos, by = c('issn' = "issn_linking")) |>
  dplyr::select(-issn)


print(nrow(articles))
print(nrow(joined_scimago))
print(nrow(joined_wos))
print(nrow(joined_doaj))
names(joined_doaj) = gsub(" ", "_", tolower(names(joined_doaj)))


megajournals <- c(
  "24701343", "21583226", "20466390", "20446055", "23251026",
  "22115463", "21601836", "21693536", "20513305", "21678359",
  "19326203", "20545703", "21582440", "20452322", "20566700",
  "23915447", "22991093", "24058440", "21508925", "2050084X",
  "20461402"
)

quantiles <- quantile(original_data$acceptance_delay, probs = c(0.01, 0.99))
print(quantiles)

original_data = joined_doaj

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
