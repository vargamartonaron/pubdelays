library(tidyverse)

file_list <- list.files(
  path = "../data",
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
        "l",
        "D",
        "D",
        "n",
        "l",
        "l",
        "c",
        "n",
        "l",
        "n",
        "c",
        "c",
        "c",
        "c",
        "n",
        "n",
        "c",
        "n"
      ),
      show_col_types = FALSE
    )
    return(data)
  }
)

df <- list_df |>
  dplyr::bind_rows()


print(nrow(df))

glimpse(df)
# filter z_score > 3 outliers

df <- df |>
  dplyr::filter(
    3 > abs(acceptance_delay - mean(acceptance_delay, na.rm = TRUE)) / sd(acceptance_delay, na.rm = TRUE),
    3 > abs(publication_delay - mean(publication_delay, na.rm = TRUE)) / sd(publication_delay, na.rm = TRUE)
  ) |>
  dplyr::distinct(title, .keep_all = TRUE) # keep unique titles only

print(nrow(df))
print(nrow(filter(df, sjr_best_quartile == "Q1")))
#number of q1 articles

readr::write_csv(df, glue::glue("../data/processed.csv"))
