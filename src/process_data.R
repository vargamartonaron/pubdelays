library("dplyr")
library("glue")
library("jsonlite")
library("fuzzyjoin")
library("tidyr")
library("readr")
library("lubridate")
library("stringr")


json_files <- list.files(
  path = "../data/",
  pattern = "\\.xml\\.gz\\.json$",
  all.files = TRUE,
  full.names = TRUE
)

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


if (!all(important_cols %in% colnames(articles))) {
  message("Skipping file ", file, ": Missing important columns")
  stop()
}

scimago <- readr::read_csv2("/users/usumusu/pubdelays/data/scimago.csv")
webofscience <- readr::read_csv("/users/usumusu/pubdelays/data/ext_list.csv")
doaj <- readr::read_csv("/users/usumusu/pubdelays/data/doaj.csv")

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

scimago <- scimago |>
  dplyr::select(
    -Sourceid,
    -Type,
    -Region,
    -Publisher,
    -`Total Docs. (2023)`,
    -`Total Docs. (3years)`,
    -`Ref. / Doc.`,
    -`Cites / Doc. (2years)`
  ) |>
  dplyr::rename(journal_title = Title) |>
  tidyr::separate_longer_delim(Issn, delim = ", ") |>
  dplyr::rename(issn_linking = Issn) |>
  dplyr::group_by(issn_linking) |>
  dplyr::slice(1) |>
  dplyr::ungroup()


webofscience <- webofscience |>
  dplyr::mutate(
    issn = paste(webofscience$`ISSN`, webofscience$`EISSN`, sep = ", ")
  ) |>
  dplyr::filter(`Source Type` == "Journal") |>
  dplyr::rename(
    asjc = `All Science Journal Classification Codes (ASJC)`,
    issn_linking = issn
  ) |>
  tidyr::separate_longer_delim(
    `asjc`,
    delim = "; "
  ) |>
  tidyr::separate_longer_delim(
    issn_linking,
    delim = ", "
  ) |>
  dplyr::mutate(
    discipline = dplyr::case_when(
      asjc == 1000 ~ "multidisciplinary",
      asjc <= 1111 ~ "life_sciences",
      asjc <= 1213 ~ "social_sciences_and_humanities",
      asjc <= 1315 ~ "life_sciences",
      asjc <= 1410 ~ "social_sciences_and_humanities",
      asjc <= 1712 ~ "physical_sciences",
      asjc <= 1804 ~ "social_sciences_and_humanities",
      asjc <= 1913 ~ "physical_sciences",
      asjc <= 2003 ~ "social_sciences_and_humanities",
      asjc <= 2312 ~ "physical_sciences",
      asjc <= 2406 ~ "life_sciences",
      asjc <= 2614 ~ "physical_sciences",
      asjc <= 2748 ~ "health_sciences",
      asjc <= 2809 ~ "life_sciences",
      asjc <= 2923 ~ "health_sciences",
      asjc <= 3005 ~ "life_sciences",
      asjc <= 3109 ~ "physical_sciences",
      asjc <= 3322 ~ "social_sciences_and_humanities",
      asjc <= 3616 ~ "health_sciences",
      TRUE ~ "NA"
    )
  ) |>
  dplyr::mutate(
    is_psych = ifelse(
      (3200 <= `asjc`) &
        (3207 >= `asjc`),
      TRUE,
      FALSE
    )
  ) |>
  dplyr::select(
    "Source Title",
    "issn_linking",
    "Open Access Status",
    "Source Type",
    "asjc",
    "discipline"
  ) |>
  dplyr::group_by(issn_linking) |>
  dplyr::slice(1) |>
  dplyr::ungroup()


doaj <- doaj |>
  dplyr::select(
    `Journal ISSN (print version)`,
    `Journal EISSN (online version)`,
    `Review process`,
    `APC`,
    `APC amount`,
    `DOAJ Seal`,
    `Does the journal comply to DOAJ's definition of open access?`
  ) |>
  dplyr::mutate(issn = paste(
    `Journal ISSN (print version)`,
    `Journal EISSN (online version)`,
    sep = ", "
  )
  ) |>
  dplyr::mutate(issn = stringr::str_replace_all(issn, "-", "")) |>
  dplyr::mutate(issn = stringr::str_replace_all(issn, "NA", "")) |>
  dplyr::mutate(
    issn = ifelse(
      startsWith(issn, ", "),
      substr(issn, 3, nchar(issn)),
      issn
    )
  ) |>
  dplyr::select(
    -`Journal ISSN (print version)`,
    -`Journal EISSN (online version)`
  ) |>
  dplyr::rename(issn_linking = issn) |>
  tidyr::separate_longer_delim(
    issn_linking,
    delim = ", "
  ) |>
  dplyr::group_by(issn_linking) |>
  dplyr::slice(1) |>
  dplyr::ungroup()


f_filter <- nrow(articles)

joined_doaj <- articles |>
  dplyr::left_join(
    scimago, join_by("issn_linking"), suffix = c("", "_scimago")
  ) |>
  dplyr::left_join(
    webofscience, join_by("issn_linking"), suffix = c("", "_wos")
  ) |>
  dplyr::left_join(
    doaj, join_by("issn_linking"), suffix = c("", "_doaj")
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
      "does_the_journal_comply_to_doaj's_definition_of_open_access?" == "Yes" ~ TRUE,
      "open_access_status" == "Unpaywall Open Acess" ~ TRUE,
      TRUE ~ FALSE
    )
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
    sjr_best_quartile,
    publication_types,
    title,
    journal_title,
    sjr,
    rank,
    discipline,
    "asjc"
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
readr::write_csv(filters, glue::glue("{file}_filters.csv"))
