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
        "n", # h_index_year
        "l", # open_access
        "n", # publication_delay
        "c", # publication_types
        "c", # title
        "c", # journal
        "c", # quartile_year
        "n", # rank_year
        "c", # discipline
        "n", # asjc
        "c", # npi_discipline
        "c", # npi_field
        "n", # npi_year
        "l", # is_series
        "n", # established
        "c", # country
        "c", # keywords
        "c", # apc
        "c", # apc_amount
        "c", # doi
        "c", # retraction nature
        "c", # retraction reason
        "D", # retraction date
        "l"  # is_retracted
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

df <- df |>
  dplyr::distinct(title, .keep_all = TRUE) # keep unique titles only

print(nrow(df))

print("test_3")

readr::write_csv(df, "/users/zsimi/pubdelays/data/processed_data/processed.csv")

