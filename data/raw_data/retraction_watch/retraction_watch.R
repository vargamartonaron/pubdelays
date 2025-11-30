library(readr)
library(dplyr)
library(lubridate)

retraction_watch = readr::read_csv("/users/zsimi/pubdelays/data/raw_data/retraction_watch/retraction_watch.csv",
                       col_names = TRUE)

head(retraction_watch)

is.Date(retraction_watch$RetractionDate)
is.Date(retraction_watch$OriginalPaperDate)

retraction_watch = retraction_watch |> 
  select(RetractionDate, OriginalPaperDate, Title, OriginalPaperDOI, RetractionDOI, RetractionNature, Reason) |>
  mutate(
    retraction_date = as.Date(as.POSIXct(RetractionDate, format = "%m/%d/%Y %H:%M")),
    original_date   = as.Date(as.POSIXct(OriginalPaperDate, format = "%m/%d/%Y %H:%M"))
  ) |> 
  filter(
    retraction_date >= as.Date("2015-01-01") & 
      original_date   >= as.Date("2013-01-01")
  ) |> 
  select(-RetractionDate, -OriginalPaperDate) |> 
  rename(
    title = Title,
    doi = OriginalPaperDOI,
    retraction_doi = RetractionDOI,
    retraction_nature = RetractionNature,
    reason = Reason
  )


write_csv(retraction_watch, "/users/zsimi/pubdelays/data/processed_data/retraction_watch.csv")

head(retraction_watch)