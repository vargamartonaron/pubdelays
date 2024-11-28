library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

data = readr::read_tsv("/users/zsimi/pubdelays/filtered_articles.tsv")

############### Print the name of the columns

colnames(data)

############### Check if there are multiple articles with the same name

multiple <- data |>
  dplyr::group_by(title) |>
  dplyr::summarise(articles = n()) |>
  dplyr::arrange(desc(articles))

print(multiple)
print("multiple articles checked")

############### Articles per month

result = data |>
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  dplyr::group_by(article_date_month) |>
  dplyr::summarise(title = n())

print(result)
print("Articles per month done")
write.csv(result, "articles_per_month.csv", row.names = FALSE)

############### Density plot

histogram_plot_month = data |>
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  ggplot2::ggplot(aes(x = article_date_month)) +
  ggplot2::geom_bar() +
  ggplot2::labs(title = "Density plot of articles per month", x = "Month", y = "Number of articles") +
  ggplot2::theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

ggsave("histogram_plot_month.pdf")
print("Histogram plot month done")


############### Articles per year

articles_year <- data |>
  dplyr::mutate(article_date_year = lubridate::year(article_date)) |>
  dplyr::group_by(article_date_year) |>
  dplyr::summarise(title = n())

print(articles_year)
print("Articles per year done")
write.csv(articles_year, "articles_per_year.csv", row.names = FALSE)

histogram_plot_year = data |>
  dplyr::mutate(article_date_year = lubridate::year(article_date)) |>
  ggplot2::ggplot(aes(x = article_date_year)) +
  ggplot2::geom_bar() +
  ggplot2::labs(title = "Density plot of articles per year", x = "Year", y = "Number of articles") +
  ggplot2::theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

ggsave("histogram_plot_year.pdf")
print("Histogram plot year done")

############### Daily articles

daily_articles <- data |>
  dplyr::mutate(article_date_day = lubridate::day(article_date)) |>
  dplyr::group_by(article_date_day) |>
  dplyr::summarise(title = n())

print(daily_articles)

daily_articles <- data |>
  dplyr::group_by(article_date) |>
  dplyr::summarise(title = n())

print(daily_articles)

print("Articles per day done")
write.csv(daily_articles, "articles_per_day.csv", row.names = FALSE)

############### Count the Bottom and top 1% acceptance delay

acceptance_1perc_bottom <- data %>%
  filter(!is.na(acceptance_delay)) %>%
  filter(acceptance_delay <= quantile(acceptance_delay, 0.01)) 
acceptance_1perc_top <- data %>%
  filter(!is.na(acceptance_delay)) %>%
  filter(acceptance_delay >= quantile(acceptance_delay, 0.99))

acceptance_1perc_top = acceptance_1perc_top |> 
  mutate(nrows = nrow(acceptance_1perc_top))
acceptance_1perc_bottom = acceptance_1perc_bottom |> 
  mutate(nrows = nrow(acceptance_1perc_bottom))
write.csv(acceptance_1perc_bottom, "acceptance_delay_bottom_1_percent.csv", row.names = FALSE)
write.csv(acceptance_1perc_top, "acceptance_delay_top_1_percent.csv", row.names = FALSE)


acceptance_1perc_year = data |> 
  dplyr::filter(!is.na(acceptance_delay)) |>
  dplyr::mutate(article_date_year = lubridate::year(article_date)) |>
  group_by(article_date_year) |> 
  summarise(
    bottom_1 = quantile(acceptance_delay, 0.01, na.rm = T),
    top_1 = quantile(acceptance_delay, 0.99, na.rm = T)
  )
print(acceptance_1perc_year)
write.csv(acceptance_1perc_year, "acceptance_1perc_year.csv", row.names = FALSE)

print("Bottom-top 1% acceptance delay done")


############### Count the Bottom and top 1% publication delay

publication_1perc_bottom <- data %>%
  filter(!is.na(publication_delay)) %>%
  filter(publication_delay <= quantile(publication_delay, 0.01)) 
publication_1perc_top <- data %>%
  filter(!is.na(publication_delay)) %>%
  filter(publication_delay >= quantile(publication_delay, 0.99))

publication_1perc_top = publication_1perc_top |> 
  mutate(nrows = nrow(publication_1perc_top))
publication_1perc_bottom = publication_1perc_bottom |> 
  mutate(nrows = nrow(publication_1perc_bottom))
write.csv(publication_1perc_bottom, "publication_delay_bottom_1_percent.csv", row.names = FALSE)
write.csv(publication_1perc_top, "publication_delay_top_1_percent.csv", row.names = FALSE)


publication_1perc_year = data |> 
  dplyr::filter(!is.na(publication_delay)) |>
  dplyr::mutate(article_date_year = lubridate::year(article_date)) |>
  group_by(article_date_year) |> 
  summarise(
    bottom_1 = quantile(publication_delay, 0.01, na.rm = T),
    top_1 = quantile(publication_delay, 0.99, na.rm = T)
  )
print(publication_1perc_year)
write.csv(publication_1perc_year, "publication_1perc_year.csv", row.names = FALSE)

print("Bottom-top 1% publication delay done")

############### Count articles per journal

journal_n_article = data |>
  dplyr::group_by(journal_title) |>
  dplyr::summarise(title = n()) |>
  dplyr::arrange(desc(journal_title))

print(journal_n_article)
write.csv(journal_n_article, "journal_articles_n.csv", row.names = F)
print("Articles per journal done")

############### Number of journals

journal_n <- data |>
  dplyr::summarize(n_distinct_journals = n_distinct(journal_title))
 
print(journal_n)
write.csv(journal_n, "journal_n.csv", row.names = F)
print("Journal count done")

############### Acceptance Delays per month

result = data |>
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  dplyr::group_by(article_date_month) |>
  dplyr::summarise(acceptance_delay = mean(acceptance_delay)) |> 
  ungroup()

print(result)
print("Acceptance delay per month done")
write.csv(result, "acceptance_delay_per_month.csv", row.names = FALSE)

result$article_date_month <- as.Date(result$article_date_month, format = "%Y-%m-%d")
ggplot(result, aes(x = article_date_month, y = acceptance_delay)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Month", y = "Average Acceptance Delay", title = "Average Acceptance Delay by Month") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("acceptance_delay_month.pdf")
print("Acceptance delay per month plot done")

ggplot(result, aes(x = article_date_month, y = acceptance_delay)) +
  geom_segment(aes(x = article_date_month, xend = article_date_month, y = 0, yend = acceptance_delay), color = "red") + 
  geom_point(color = "black", size = 1.5) +
  labs(x = "Month", y = "Average Acceptance Delay", title = "Average Acceptance Delay by Month") +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("acceptance_delay_month_lollipop.pdf")
print("Acceptance delay per month lollipop plot done")

############### Acceptance Delays per year

result = data |>
  dplyr::mutate(article_date_year = lubridate::year(article_date)) |>
  dplyr::group_by(article_date_year) |>
  dplyr::summarise(acceptance_delay = mean(acceptance_delay)) |> 
  ungroup()

print(result)
print("Acceptance delay per year done")
write.csv(result, "acceptance_delay_per_year.csv", row.names = FALSE)

############### Publication Delays per month

result = data |>
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  dplyr::group_by(article_date_month) |>
  dplyr::summarise(publication_delay = mean(publication_delay)) |> 
  ungroup()

print(result)
print("publication delay per month done")
write.csv(result, "publication_delay_per_month.csv", row.names = FALSE)

############### Publication Delays per year

result = data |>
  dplyr::mutate(article_date_year = lubridate::year(article_date)) |>
  dplyr::group_by(article_date_year) |>
  dplyr::summarise(publication_delay = mean(publication_delay)) |> 
  ungroup()

print(result)
print("publication delay per year done")
write.csv(result, "publication_delay_per_year.csv", row.names = FALSE)

############### Compare filtered and unfiltered article number

data_raw = readr::read_tsv("/users/zsimi/pubdelays/articles_raw.tsv")

unfiltered_n = nrow(data_raw)
filtered_n = nrow(data)
paste("There are", unfiltered_n, "number of rows in the unfiltered dataset")
paste("There are", filtered_n, "number of rows in the filtered dataset")

#Compare filtered and unfiltered acceptance delay

data_joined = data

articles_joined_compare = data_joined |>
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  dplyr::mutate(article_date_month = as_date(article_date_month)) |> 
  filter(!is.na(acceptance_delay),
         !is.na(article_date_month)) |> 
  dplyr::select(acceptance_delay, article_date_month, publication_delay)

print("check 1")

articles_raw_compare = data_raw |>
  dplyr::mutate(article_date = as_date(article_date)) |> 
  dplyr::mutate(received = as_date(received)) |> 
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  dplyr::mutate(article_date_month = as_date(article_date_month)) |> 
  filter(!is.na(acceptance_delay),
         !is.na(article_date_month)) |> 
  dplyr::select(article_date_month, acceptance_delay, publication_delay)

print("check 2")

#Create the monthly means

monthly_means_joined <- articles_joined_compare |>
  mutate(YearMonth = floor_date(article_date_month, "month"))  |>    
  group_by(YearMonth) |>                            
  summarize(MeanValue1 = mean(acceptance_delay))                

monthly_means_raw <- articles_raw_compare |>
  mutate(YearMonth = floor_date(article_date_month, "month")) |> 
  group_by(YearMonth) |>                           
  summarize(MeanValue2 = mean(acceptance_delay))              

print("check 3")

#Create the plot

paste("There are", n_distinct(pull(monthly_means_raw, YearMonth)), "number of different months in the raw dataset")
paste("There are", n_distinct(pull(monthly_means_joined, YearMonth)), "number of different months in the joined dataset")


ggplot() +
  geom_line(data = monthly_means_joined, aes(x = YearMonth, y = MeanValue1, color = "Joined Data")) + 
  geom_line(data = monthly_means_raw, aes(x = YearMonth, y = MeanValue2, color = "Raw data")) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m",
               limits = as_date(c("2016-01-01", "2023-01-01"))) + 
  labs(title = "Monthly Mean Line Chart from Two Separate Datasets", 
       x = "Date", y = "Acceptance delay (days)",
       colour = "Datasets") +
  scale_color_manual(values = c("Joined Data" = "blue", "Raw data" = "red")) +
  ylim(0, 300) +
  theme_minimal()

print("check 4")

ggsave("dataset_compare_acceptance_delay.pdf")
print("Acceptance delay between the two datasets done")

################################################################################################################

#Compare filtered and unfiltered publication delay

articles_raw_compare = articles_raw_compare |> 
  filter(!is.na(publication_delay))

articles_joined_compare = articles_joined_compare |> 
  filter(!is.na(publication_delay))

#Create the monthly means

monthly_means_joined <- articles_joined_compare |>
  mutate(YearMonth = floor_date(article_date_month, "month"))  |>    
  group_by(YearMonth) |>                            
  summarize(MeanValue1 = mean(publication_delay))                

monthly_means_raw <- articles_raw_compare |>
  mutate(YearMonth = floor_date(article_date_month, "month")) |> 
  group_by(YearMonth) |>                           
  summarize(MeanValue2 = mean(publication_delay))              

print("check 5")

#Create the plot


ggplot() +
  geom_line(data = monthly_means_joined, aes(x = YearMonth, y = MeanValue1, color = "Joined Data")) + 
  geom_line(data = monthly_means_raw, aes(x = YearMonth, y = MeanValue2, color = "Raw data")) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m",
               limits = as_date(c("2016-01-01", "2023-01-01"))) + 
  labs(title = "Monthly Mean Line Chart from Two Separate Datasets", 
       x = "Date", y = "Publication delay (days)",
       colour = "Datasets") +
  scale_color_manual(values = c("Joined Data" = "blue", "Raw data" = "red")) +
  ylim(0, 75) +
  theme_minimal()

print("check 6")

ggsave("dataset_compare_publication_delay.pdf")
print("Publication delay between the two datasets done")


#test difference between filtered and unfiltered


articles_joined_compare = data_joined |>
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  dplyr::mutate(article_date_month = as_date(article_date_month)) |> 
  filter(!is.na(acceptance_delay),
         !is.na(article_date_month)) |> 
  dplyr::select(acceptance_delay, article_date_month, publication_delay)

print("check 1")

articles_raw_compare = data_raw |>
  dplyr::mutate(article_date = as_date(article_date)) |> 
  dplyr::mutate(received = as_date(received)) |> 
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-")) |>
  dplyr::mutate(article_date_month = as_date(article_date_month)) |> 
  filter(!is.na(acceptance_delay),
         !is.na(article_date_month)) |> 
  dplyr::select(article_date_month, acceptance_delay, publication_delay)

print("check 2")

ggplot() +
  geom_density(aes(x = acceptance_delay, color = "Full Dataset"), data = articles_raw_compare) +
  geom_density(aes(x = acceptance_delay, color = "Subset Dataset"), data = articles_joined_compare) +
  labs(title = "Acceptance Delay Distribution",
       x = "Acceptance Delay",
       y = "Density") +
  scale_color_manual(name = "Dataset", values = c("Full Dataset" = "blue", "Subset Dataset" = "red"))

ggsave("dataset_distributions.pdf")
print("Distributions for two datasets done")

#Discipline check

result = data_joined |> 
  group_by(discipline) |> 
  summarize(count = n())

print(result)
print("Discipline check done")
write.csv(result, "discipline_count.csv", row.names = FALSE)



#Each year curve on same graph

data_joined_graph = data_joined |>
  filter(!is.na(acceptance_delay),
         !is.na(article_date)) |> 
  dplyr::mutate(article_date_year = lubridate::year(article_date)) |>
  dplyr::mutate(month = lubridate::month(article_date)) |>
  dplyr::group_by(article_date_year, month) |>
  dplyr::summarize(mean_acceptance_delay = mean(acceptance_delay, na.rm = TRUE))

write.csv(data_joined_graph, "data_for_graph.csv")


print("check 1")

data_joined_graph |> 
  ggplot(aes(x = month, y = mean_acceptance_delay, colour = factor(article_date_year))) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = 1:12, labels = month.name) +
  labs(title = "Mean Acceptance Delay by Month for Each Year",
       x = "Month",
       y = "Mean Acceptance Delay",
       colour = "Year") +
  ylim(0, 300) +
  theme_minimal()


ggsave("acceptance_delay_per_year.pdf")
print("Acceptance delay per year done")

#Distribution of delays per year

data_joined |> 
  mutate(article_date_year = factor(lubridate::year(article_date))) |> 
  ggplot(aes(x = acceptance_delay, color = article_date_year)) + 
  geom_density()

ggsave("dist_per_year.pdf")
print("Distributions for all years done")

##By article number

data_joined |> 
  group_by(journal_title) |> 
  mutate(article_number = n()) |> 
  ggplot(aes(x = article_number)) +
  geom_density() +
  ylab("Density of journals")

ggsave("article_n_per_journal.pdf")
print("Articles per journal done")


#################################################################################################
####################################  UNTESTED  #################################################
#################################################################################################

#Investigate other columns

#Check if acc and pub delay aligns with the timeline

#Check if journal publishes same values

##Distribution of journals by best quartile

