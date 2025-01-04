library(tidyverse)

csv_files <- list.files(
  path = "../data/",
  pattern = "*filters\\.csv$",
  all.files = TRUE,
  full.names = TRUE
)

count <- 0

for (file in csv_files) {
  data <- read_csv(file, show_col_types = FALSE)
  count <- count + as.numeric(data$second_filter)
}
print(count)
