library("dplyr")
library("tidyr")
library("readr")


doaj_raw = readr::read_csv("/users/zsimi/pubdelays/data/raw_data/directory_of_open_access_journals/doaj_2025_05_15.csv",
                       col_names = TRUE)


doaj <- doaj_raw |>
  dplyr::select(
    `Journal title`,
    `Journal ISSN (print version)`,
    `Journal EISSN (online version)`,
    `Review process`,
    `APC`,
    `APC amount`,
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


readr::write_csv(doaj, "/users/zsimi/pubdelays/data/processed_data/doaj.csv", 
                 col_names = TRUE)