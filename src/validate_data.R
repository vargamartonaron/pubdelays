library(validate)
library(tidyverse)

articles <- read_tsv("/users/usumusu/pubdelays/data/journal_articles.tsv")

type_rules <- validator(
  is.logical(is_covid),
  is.Date(received),
  is.Date(article_date),
  is.numeric(acceptance_delay),
  is.logical(is_psych),
  is.character(covid_time),
  is.logical(is_mega),
  is.character(issn),
  is.numeric(h_index),
  is.numeric(publication_delay),
  is.logical(open_access_doaj),
  is.character(title),
  is.character(journal_title),
  is.numeric(sjr),
  is.numeric(rank),
  is.character(discipline),
  is.character(doaj_seal),
  is.numeric(asjc)
)

missingness <- validator(
  !is.na(is_covid),
  !is.na(received),
  !is.na(article_date),
  !is.na(acceptance_delay),
  !is.na(is_psych),
  !is.na(covid_time),
  !is.na(is_mega),
  !is.na(issn),
  !is.na(h_index),
  !is.na(publication_delay),
  !is.na(open_access_doaj),
  !is.na(title),
  !is.na(journal_title),
  !is.na(sjr),
  !is.na(rank),
  !is.na(discipline),
  !is.na(doaj_seal),
  !is.na(asjc)
)

range_checks <- validator(
  in_range(received, min=as.Date("2006-01-01"), max=as.Date("2023-06-01")),
  in_range(article_date, min=as.Date("2006-01-01"), max=as.Date("2023-06-01")),
  in_range(acceptance_delay, min=0, max=700),
  in_range(publication_delay, min=0, max=700)
)

uniqueness <- validator(
  is_unique(title)
)
