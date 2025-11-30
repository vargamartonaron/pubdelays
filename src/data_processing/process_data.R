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
retraction_watch <- readr::read_csv("/users/zsimi/pubdelays/data/processed_data/retraction_watch.csv")

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

joined_doaj <- joined_doaj |> 
  mutate(article_year = as.numeric(paste(lubridate::year(article_date)))) |> 
  mutate(
    quartile_year = as.character(case_when(
      article_year == 2015 ~ quartile_2015,
      article_year == 2016 ~ quartile_2016,
      article_year == 2017 ~ quartile_2017,
      article_year == 2018 ~ quartile_2018,
      article_year == 2019 ~ quartile_2019,
      article_year == 2020 ~ quartile_2020,
      article_year == 2021 ~ quartile_2021,
      article_year == 2022 ~ quartile_2022,
      article_year == 2023 ~ quartile_2023,
      article_year == 2024 ~ quartile_2024,
      article_year == 2025 ~ quartile_2024,
      TRUE ~ NA_character_  # Default to NA if no match
    ))) |> 
  mutate(npi_year = as.numeric(case_when(
    article_year == 2025 ~ npi_level_24,
    article_year == 2024 ~ npi_level_24,
    article_year == 2023 ~ npi_level_23,
    article_year == 2022 ~ npi_level_22,
    article_year == 2021 ~ npi_level_21,
    article_year == 2020 ~ npi_level_20,
    article_year == 2019 ~ npi_level_19,
    article_year == 2018 ~ npi_level_18,
    article_year == 2017 ~ npi_level_17,
    article_year == 2016 ~ npi_level_16,
    article_year == 2015 ~ npi_level_15,
    TRUE ~ NA_real_
  ))) |> 
  mutate(rank_year = as.numeric(case_when(
    article_year == 2025 ~ rank_2024,
    article_year == 2024 ~ rank_2024,
    article_year == 2023 ~ rank_2023,
    article_year == 2022 ~ rank_2022,
    article_year == 2021 ~ rank_2021,
    article_year == 2020 ~ rank_2020,
    article_year == 2019 ~ rank_2019,
    article_year == 2018 ~ rank_2018,
    article_year == 2017 ~ rank_2017,
    article_year == 2016 ~ rank_2016,
    article_year == 2015 ~ rank_2015,
    TRUE ~ NA_real_
  ))) |> 
  mutate(h_index_year = as.numeric(case_when(
    article_year == 2025 ~ h_index_2024,
    article_year == 2024 ~ h_index_2024,
    article_year == 2023 ~ h_index_2023,
    article_year == 2022 ~ h_index_2022,
    article_year == 2021 ~ h_index_2021,
    article_year == 2020 ~ h_index_2020,
    article_year == 2019 ~ h_index_2019,
    article_year == 2018 ~ h_index_2018,
    article_year == 2017 ~ h_index_2017,
    article_year == 2016 ~ h_index_2016,
    article_year == 2015 ~ h_index_2015,
    TRUE ~ NA_real_
  ))) |> 
  # mutate(sjr_year = as.numeric(case_when(
  #   article_year == 2025 ~ as.numeric(sjr_2024),
  #   article_year == 2024 ~ as.numeric(sjr_2024),
  #   article_year == 2023 ~ as.numeric(sjr_2023),
  #   article_year == 2022 ~ as.numeric(sjr_2022),
  #   article_year == 2021 ~ as.numeric(sjr_2021),
  #   article_year == 2020 ~ as.numeric(sjr_2020),
  #   article_year == 2019 ~ as.numeric(sjr_2019),
  #   article_year == 2018 ~ as.numeric(sjr_2018),
  #   article_year == 2017 ~ as.numeric(sjr_2017),
  #   article_year == 2016 ~ as.numeric(sjr_2016),
  #   article_year == 2015 ~ as.numeric(sjr_2015),
  #   TRUE ~ NA_real_
  # ))) |> 
  select(-ends_with(c(
    "15", "16", "17", "18", "19",
    "20", "21", "22", "23", "24", "25"
  )))
  
head(joined_doaj)

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
         received >= as.Date("2013-01-01")
  ) |>
  dplyr::select(
    is_covid,
    received,
    article_date,
    acceptance_delay,
    is_psych,
    is_mega,
    issn_linking,
    h_index_year,
    open_access,
    publication_delay,
    publication_types,
    title,
    journal,
    quartile_year,
    rank_year,
    discipline,
    asjc,
    npi_discipline,
    npi_field,
    npi_year,
    is_series,
    established,
    country,
    keywords,
    apc,
    apc_amount,
    doi
  ) |>
  dplyr::distinct(title, .keep_all = TRUE) # keep unique articles only

s_filter <- nrow(data)

filters <- tibble::tibble(
  raw_length = r_length,
  first_filter = f_filter,
  second_filter = s_filter
)

print("data filtered")

# Retraction watch test

data = data |> 
  left_join(retraction_watch |> 
              mutate(merge_key = coalesce(doi, retraction_doi)),
            by = c("doi" = "merge_key"),
            suffix = c("", "_retraction"))


data <- data |>
  mutate(is_retracted = as.logical(if_else(!is.na(reason) | !is.na(retraction_nature), TRUE, FALSE))) |> 
  mutate(article_date = case_when(
    !is.na(retraction_date) ~ as.Date(original_date),
    TRUE ~ article_date
  )) |>
  select(
    -original_date,
    -title_retraction,
    -retraction_doi,
    -doi_retraction
  )

nrow(data)
colnames(data)
head(data)

readr::write_tsv(data, glue::glue("{file}.tsv"))

stop("test_5")

#readr::write_csv(filters, glue::glue("{json_files}_filters.csv"))

#for (json_file in json_files) {

#  base_name <- basename(json_file)

#  output_file <- file.path("/users/zsimi/pubdelays/data/processed_data/", paste0(base_name, ".tsv"))

#  write_tsv(data, output_file)
#}

#print("test_5")
