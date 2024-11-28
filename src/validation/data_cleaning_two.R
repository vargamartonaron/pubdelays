library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(fuzzyjoin)

data_joined = readr::read_tsv("/users/zsimi/pubdelays/filtered_articles.tsv")

data_raw = readr::read_tsv("/users/zsimi/pubdelays/articles_raw.tsv")

journal_metric = readr::read_csv("/users/zsimi/pubdelays/journal_metrics.csv", col_names = TRUE)

colnames(data_joined)
colnames(data_raw)
colnames(journal_metric)

#################################################################################################
####################################  UNTESTED  #################################################
#################################################################################################

summarized_data_acc = data_joined |> 
  filter(article_date >= as_date("2023-01-01") & article_date <= as_date("2023-12-31")) |> 
  group_by(journal_title) |> 
  summarize(acceptance_delay_pubmed = median(acceptance_delay))



print("data summarized")
colnames(summarized_data_acc)
write.csv(summarized_data_acc, "pubmed_journals.csv")

stop("bruh")

summarized_data_acc$journal_title <- trimws(summarized_data_acc$journal_title)
summarized_data_acc$journal_title <- tolower(summarized_data_acc$journal_title)
summarized_data_acc$journal_title <- gsub("[^[:alnum:] ]", "", summarized_data_acc$journal_title)
journal_metric$journal_title <- trimws(journal_metric$journal_title)
journal_metric$journal_title <- tolower(journal_metric$journal_title)
journal_metric$journal_title <- gsub("[^[:alnum:] ]", "", journal_metric$journal_title)
summarized_data_acc$journal_title <- gsub("\\s+", "_", trimws(summarized_data_acc$journal_title))
journal_metric$journal_title <- gsub("\\s+", "_", trimws(journal_metric$journal_title))


journal_metric = journal_metric |> 
  filter(date == "2023")

df_merged = dplyr::inner_join(journal_metric, summarized_data_acc, by = "journal_title")

print("data merged")
colnames(df_merged)

df_merged <- df_merged %>%
  mutate(acceptance_delay_diff = abs(acceptance_delay_pubmed - acceptance_delay))

print("merged data compared")
write.csv(df_merged, "comp_temp.csv")


stop("acceptance delay done")


#Check if acc and pub delay aligns with the timeline

#Check if journal publishes same values

##Distribution of best quartile articles

