library("dplyr")
library("tidyr")
library("readr")


wos_raw = readr::read_csv("/users/zsimi/pubdelays/data/raw_data/web_of_science/wos.csv",
                           col_names = TRUE)


webofscience <- wos_raw |>
  dplyr::mutate(
    issn = paste(wos_raw$`Print-ISSN`, wos_raw$`E-ISSN`, sep = ", ")
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
    "Open Access status",
    "Source Type",
    "asjc",
    "discipline"
  ) |>
  dplyr::group_by(issn_linking) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

readr::write_csv(webofscience, "/users/zsimi/pubdelays/data/processed_data/web_of_science.csv", 
                 col_names = TRUE)