library(dplyr)
library(parallel)
library(readr)
library(lubridate)
library(ggplot2)

detectCores()

###################################
##### Load data
###################################

data = readr::read_tsv("/users/zsimi/pubdelays/filtered_articles.tsv")

#set.seed(42) # Set seed for reproducibility

data <- data %>%
  filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-11-01')) |> 
  mutate(article_date_numeric = as.numeric(article_date))


###################################
##### Simple regression
###################################

start_time = proc.time()

model <- lm(acceptance_delay ~ poly(article_date, 10), data = data)

end_time = proc.time()

total_time = end_time - start_time

print(total_time)

regression <- function(data) {
  model <- lm(acceptance_delay ~ poly(article_date, 10), data = data)
  
  return(summary(model))
}


core_settings <- c(1, 2, 4)

results <- lapply(core_settings, function(cores) {
  start_time <- proc.time()
  
  messages <- mclapply(1, function(x) {
    regression_summary <- regression(data)
    return("Regression done")
  }, mc.cores = cores)
  
  end_time <- proc.time()
  
  total_time <- end_time - start_time
  
  list(cores = cores, message = messages[[1]], time = total_time["elapsed"])
})

for (result in results) {
  cat("Cores used:", result$cores, "\n")
  cat("Message:", result$message, "\n")
  cat("Execution time (seconds):", result$time, "\n\n")
}