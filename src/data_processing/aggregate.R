library(readr)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
library(glue)

file_list <- list.files(
  path = "/users/zsimi/pubdelays/data/raw_data/pubmed/jsons/",
  pattern = ".\\.json\\.tsv$",
  all.files = TRUE,
  full.names = TRUE
)
print(file_list)

list_df <- lapply(
  file_list,
  function(x) {
    data <- readr::read_tsv(
      x,
      col_types = cols(
        "l", # covid
        "D", # received
        "D", # article_date
        "n", # acceptance_delay
        "l", # is_psych
        "l", # is_mega
        "c", # issn_linking
        "n", # h_index
        "l", # open_access
        "n", # publication_delay
        "c", # sjr_2024
        "c", # sjr_2023
        "c", # sjr_2022
        "c", # sjr_2021
        "c", # sjr_2020
        "c", # sjr_2019
        "c", # sjr_2018
        "c", # sjr_2017
        "c", # sjr_2016
        "c", # sjr_2015
        "c", # publication_types
        "c", # title
        "c", # journal
        "n", # sjr
        "n", # rank
        "c", # discipline
        "n", # asjc
        "c", # npi_discipline
        "c", # npi_field
        "n", # npi_level_24
        "n", # npi_level_23
        "n", # npi_level_22
        "n", # npi_level_21
        "n", # npi_level_20
        "n", # npi_level_19
        "n", # npi_level_18
        "n", # npi_level_17
        "n", # npi_level_16
        "n", # npi_level_15
        "l", # is_series
        "n", # established
        "c", # country
        "c", # keywords
        "c", # apc
        "c" # apc_amount
      )
    )
    return(data)
  }
)

print("test_1")

df = list_df |> 
  dplyr::bind_rows()

print("test_2")

print(nrow(df))

glimpse(df)


# filter z_score > 3 outliers
#Only mutate
df <- df |>
  dplyr::mutate(accdelay_outlier = dplyr::case_when(
    abs(acceptance_delay - mean(acceptance_delay, na.rm = TRUE)) / sd(acceptance_delay, na.rm = TRUE) > 3 ~ TRUE,
    TRUE ~ FALSE
  )) |>
  dplyr::mutate(pubdelay_outlier = dplyr::case_when(
    abs(publication_delay - mean(publication_delay, na.rm = TRUE)) / sd(publication_delay, na.rm = TRUE) > 3 ~ TRUE, 
    TRUE ~ FALSE
  )) |>
  dplyr::distinct(title, .keep_all = TRUE) # keep unique titles only


print(nrow(df))
print(nrow(filter(df, sjr_2024 == "Q1")))
#number of q1 articles

#readr::write_csv(df, glue::glue("../data/processed_data/processed.csv"))

print("test_3")

readr::write_csv(df, "/users/zsimi/pubdelays/data/processed_data/processed.csv")

