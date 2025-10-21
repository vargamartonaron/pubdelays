library("dplyr")
library("glue")
library("jsonlite")
library("fuzzyjoin")
library("tidyr")
library("readr")
library("lubridate")
library("stringr")
library("tibble")
library("purrr")
#install.packages("tools", repos = "http://cran.us.r-project.org")
library("tools")


json_files <- list.files(
  path = "/users/zsimi/pubdelays/data/raw_data/pubmed/jsons",
  pattern = "\\.xml\\.gz\\.json$",
  all.files = TRUE,
  full.names = TRUE
)

print(json_files)

important_cols <- c(
  "history",
  "journal",
  "pubdate",
  "publication_types",
  "issn_linking"
)

file <- commandArgs(trailingOnly = TRUE)

articles <- jsonlite::fromJSON(file) |>
   tibble::as_tibble()


#articles <- map_dfr(json_files, function(file) {
#  dat <- fromJSON(file)
#  dat <- as_tibble(dat)
#})

if (!all(important_cols %in% colnames(articles))) {
  message("Skipping file ", file, ": Missing important columns")
  stop()
}


scimago <- readr::read_csv("/users/zsimi/pubdelays/data/processed_data/scimago.csv")
webofscience <- readr::read_csv("/users/zsimi/pubdelays/data/processed_data/web_of_science.csv")
doaj <- readr::read_csv("/users/zsimi/pubdelays/data/processed_data/doaj.csv")
npi <- readr::read_csv("/users/zsimi/pubdelays/data/processed_data/norwegian_list.csv")

print("test_1")

covid_synonyms <- c(
  "covid",
  "covid-19",
  "coronavirus disease 19",
  "sars-cov-2",
  "2019-ncov",
  "2019ncov",
  "2019-n-cov",
  "2019n-cov",
  "ncov-2019",
  "n-cov-2019",
  "coronavirus-2019",
  "wuhan pneumonia",
  "wuhan virus",
  "wuhan coronavirus",
  "coronavirus 2"
)

covid_pattern <- paste0(
  "\\b(?:", paste(covid_synonyms, collapse = "|"), ")\\b"
)

replication_synonyms <- c(
  "replication",
  "replicating",
  "replication of",
  "replication study"
)

replication_pattern <- paste0(
  "\\b(?:", paste(replication_synonyms, collapse = "|"), ")\\b"
)

articles <- articles |>
  tidyr::unnest(history)

if (!all(c("received", "accepted") %in% colnames(articles))) {
  message("Skipping file ", file, ": Missing unnested important var")
  stop()
}

r_length <- nrow(articles)

print("test_2")

articles <- articles |>
  dplyr::mutate(keywords = stringr::str_replace_all(keywords, ";", ",")) |>
  dplyr::mutate(
    publication_types = stringr::str_extract_all(
      publication_types, "(?<=:)[^;]+"
    ) |>
      lapply(function(x) paste(x, collapse = ", ")) |>
      unlist()
  ) |>
  dplyr::filter(!(is.na(received) | is.na(accepted))) |>
  dplyr::mutate_at(c(
    "pubdate",
    "received",
    "accepted",
    "pubmed",
    "medline",
    "entrez",
    "article_date"
  ),
  # ensure year-month-date format, append 01 where month or day is missing
  lubridate::as_date, format = c("%Y", "%Y-%m", "%Y-%m-%d")
  ) |>
  dplyr::filter(
    stringr::str_detect(publication_types, pattern = "Journal Article")
  ) |>
  dplyr::mutate(
    issn_linking = stringr::str_replace_all(issn_linking, "-", "")
  ) |>
  dplyr::filter(!is.na(issn_linking)) |>
  dplyr::filter(
    !(
      received >= article_date |
        accepted >= article_date |
        accepted <= received
    )
  ) |>
  dplyr::mutate(
    acceptance_delay = as.numeric(
      difftime(accepted, received, units = "days")
    )
  ) |>
  dplyr::mutate(
    publication_delay = as.numeric(
      dplyr::if_else(!is.na(article_date),
        difftime(article_date, accepted, units = "days"),
        difftime(pubdate, accepted, units = "days")
      )
    )
  ) |>
  dplyr::filter(
    !(is.na(acceptance_delay) & is.na(publication_delay)) &
      !(acceptance_delay < 0 | publication_delay < 0)
  ) |>
  dplyr::mutate(
    is_covid = dplyr::if_else(
      (
        stringr::str_detect(
          title,
          stringr::regex(covid_pattern, ignore_case = TRUE)
        ) |
          stringr::str_detect(
            keywords, stringr::regex(covid_pattern, ignore_case = TRUE)
          )
      ),
      TRUE,
      FALSE
    )
  ) |>
  dplyr::mutate(
    is_replication = dplyr::if_else(
      (
        stringr::str_detect(
          title,
          stringr::regex(replication_pattern, ignore_case = TRUE)
        ) |
          stringr::str_detect(
            keywords,
            stringr::regex(replication_pattern, ignore_case = TRUE)
          )
      ),
      TRUE,
      FALSE
    )
  )


f_filter <- nrow(articles)

print("test_3")

joined_doaj <- articles |>
  dplyr::left_join(
    scimago, join_by("issn_linking"), suffix = c("", "_scimago")
  ) |>
  dplyr::left_join(
    webofscience, join_by("issn_linking"), suffix = c("", "_wos")
  ) |>
  dplyr::left_join(
    doaj, join_by("issn_linking"), suffix = c("", "_doaj")
  ) |> 
  dplyr::left_join(
    npi, join_by("issn_linking"), suffix = c("", "_npi")
  ) 

names(joined_doaj) <- gsub(" ", "_", tolower(names(joined_doaj)))
joined_doaj <- joined_doaj[!duplicated(names(joined_doaj))]

megajournals <- c(
  "24701343", "21583226", "20466390", "20446055", "23251026",
  "22115463", "21601836", "21693536", "20513305", "21678359",
  "19326203", "20545703", "21582440", "20452322", "20566700",
  "23915447", "22991093", "24058440", "21508925", "2050084X",
  "20461402"
)


data <- joined_doaj |>
  dplyr::mutate(
    is_psych = ifelse(
      (
        (3200 <= `asjc`) &
          (3207 >= `asjc`)
      ),
      TRUE,
      FALSE
    ),
    is_mega = case_when(
      issn_linking %in% megajournals ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  dplyr::mutate(
    open_access = dplyr::case_when(
      `does_the_journal_comply_to_doaj's_definition_of_open_access?` == "Yes" ~ TRUE,
      open_access_status == "Unpaywall Open Acess" ~ TRUE,
      npi_open_access == "DOAJ" ~ TRUE,
      TRUE ~ FALSE
    )
  ) |> 
  mutate(ceased = is.numeric(ceased)
  ) |> 
  filter(is.na(ceased) | ceased <= 2014,
         is_conference == 0,
         received >= as.Date("2015-01-01")
  ) |>
  dplyr::select(
    is_covid,
    received,
    article_date,
    acceptance_delay,
    is_psych,
    is_mega,
    issn_linking,
    h_index,
    open_access,
    publication_delay,
    sjr_2024, sjr_2023, sjr_2022, sjr_2021, sjr_2020, sjr_2019, sjr_2018, sjr_2017, sjr_2016, sjr_2015,
    publication_types,
    title,
    journal,
    sjr,
    rank,
    discipline,
    asjc,
    npi_discipline,
    npi_field,
    npi_level_24, npi_level_23, npi_level_22, npi_level_21, npi_level_20, npi_level_19, npi_level_18, npi_level_17, npi_level_16, npi_level_15,
    is_series,
    established,
    country,
    keywords,
    apc,
    apc_amount
  ) |>
  dplyr::distinct(title, .keep_all = TRUE) # keep unique articles only

s_filter <- nrow(data)

filters <- tibble::tibble(
  raw_length = r_length,
  first_filter = f_filter,
  second_filter = s_filter
)

print("data filtered")


readr::write_tsv(data, glue::glue("{file}.tsv"))

stop("test_5")

#readr::write_csv(filters, glue::glue("{json_files}_filters.csv"))

for (json_file in json_files) {

  base_name <- basename(json_file)

  output_file <- file.path("/users/zsimi/pubdelays/data/processed_data/", paste0(base_name, ".tsv"))

  write_tsv(data, output_file)
}

print("test_5")
