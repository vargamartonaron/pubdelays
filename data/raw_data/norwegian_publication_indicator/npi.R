library("dplyr")
library("fuzzyjoin")
library("readr")

npi = readr::read_csv2("/users/zsimi/pubdelays/data/raw_data/norwegian_publication_indicator/norwegian_list_2025_05_14.csv",
                      col_names = TRUE)


npi = npi |> 
  select(
    -journal_id,
    -`International Title`,
    -`Publishing Agreement`,
    -`Level 2026`,
    -`Level 2014`, -`Level 2013`,
    -`Level 2012`, -`Level 2011`, -`Level 2010`,
    -`Level 2009`, -`Level 2008`, -`Level 2007`,
    -`Level 2006`, -`Level 2005`, -`Level 2004`,
    -publisher_id,
    -`Publishing Company`,
    -Publisher,
    -URL,
    -`Last Updated`
  ) |> 
  rename(
    npi_title = `Original Title`,
    print_issn = `Print ISSN`,
    online_issn = `Online ISSN`,
    npi_open_access = `Open Access`,
    npi_discipline = `NPI Academic Discipline`,
    npi_field = `NPI Scientific Field`,
    npi_level_25 = `Level 2025`,
    npi_level_24 = `Level 2024`,
    npi_level_23 = `Level 2023`,
    npi_level_22 = `Level 2022`,
    npi_level_21 = `Level 2021`,
    npi_level_20 = `Level 2020`,
    npi_level_19 = `Level 2019`,
    npi_level_18 = `Level 2018`,
    npi_level_17 = `Level 2017`,
    npi_level_16 = `Level 2016`,
    npi_level_15 = `Level 2015`,
    country_of_publication = `Country of Publication`,
    language = Language,
    is_conference = `Conference Proceedings`,
    is_series = Series,
    established = Established,
    ceased = Ceased
  )

npi = npi |> 
  dplyr::mutate(issn = paste(
    print_issn,
    online_issn,
    sep = ", "
  )) |> 
  dplyr::mutate(issn = stringr::str_replace_all(issn, "-", "")) |>
  dplyr::mutate(issn = stringr::str_replace_all(issn, "NA", "")) |>
  dplyr::mutate(
    issn = ifelse(
      startsWith(issn, ", "),
      substr(issn, 3, nchar(issn)),
      issn
    )
  ) |> 
  dplyr::rename(issn_linking = issn) |> 
  dplyr::select(-print_issn, -online_issn, npi_title) |> 
  tidyr::separate_longer_delim(
    issn_linking,
    delim = ", "
  ) |>
  dplyr::group_by(issn_linking) |>
  dplyr::slice(1) |>
  dplyr::ungroup()


readr::write_csv(npi, "/users/zsimi/pubdelays/data/processed_data/norwegian_list.csv", 
                 col_names = TRUE)