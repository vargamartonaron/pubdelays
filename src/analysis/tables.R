library(readr)
library(lubridate)
library(dplyr)
library(stringr)
#install.packages("ggdist", repos = "https://cloud.r-project.org/")
library(ggdist)
#install.packages("tidyr", repos = "https://cloud.r-project.org/")
library(tidyr)
#install.packages("ggdist", repos = "https://cloud.r-project.org/")
library(ggdist)

data = readr::read_csv("/users/zsimi/processed.csv")
colnames(data)

data = data |> 
  filter(article_date > as_date("2014-12-31"),
         article_date < as_date("2024-12-01"))

#Overall table

delay_table = data |> 
  summarise(
    n = n(),
    n_acc = sum(!is.na(acceptance_delay)),
    n_pub = sum(!is.na(publication_delay)),
    mean_acc_delay = mean(acceptance_delay, na.rm = TRUE),
    median_acc_delay = median(acceptance_delay, na.rm = TRUE),
    sd_acc_delay = sd(acceptance_delay, na.rm = TRUE),
    q1_acc_delay = quantile(acceptance_delay, 0.25, na.rm = TRUE),
    q3_acc_delay = quantile(acceptance_delay, 0.75, na.rm = TRUE),
    iqr_acc_delay = IQR(acceptance_delay, na.rm = TRUE),
    mean_pub_delay = mean(publication_delay, na.rm = TRUE),
    median_pub_delay = median(publication_delay, na.rm = TRUE),
    sd_pub_delay = sd(publication_delay, na.rm = TRUE),
    q1_pub_delay = quantile(publication_delay, 0.25, na.rm = TRUE),
    q3_pub_delay = quantile(publication_delay, 0.75, na.rm = TRUE),
    iqr_pub_delay = IQR(publication_delay, na.rm = TRUE),
  )

print("delay_table done")
write.csv(delay_table, "tables/delay_table.csv")
 
#Year table

year_table = data |> 
  dplyr::mutate(
    article_date_year = floor_date(as_date(article_date), "year"),
    article_date_year = str_replace(article_date_year, "-01-01", "")
    ) |>
  group_by(article_date_year) |> 
  summarise(
    n = n(),
    mean_acc_delay = mean(acceptance_delay, na.rm = TRUE),
    mean_pub_delay = mean(publication_delay, na.rm = TRUE)
  )

print("year_table done")
write.csv(year_table, "tables/year_table.csv")

#Month table

month_table = data |> 
  dplyr::mutate(
    article_date_month = floor_date(as_date(article_date), "month")
  ) |>
  group_by(article_date_month) |> 
  summarise(
    n = n(),
    mean_acc_delay = mean(acceptance_delay, na.rm = TRUE),
    mean_pub_delay = mean(publication_delay, na.rm = TRUE)
  )

print("month_table done")
write.csv(month_table, "tables/month_table.csv")

#Quartile table

quartile_table = data |> 
  filter(sjr_best_quartile %in% c("Q1", "Q2", "Q3", "Q4")) |> 
  group_by(sjr_best_quartile) |> 
  summarise(
    n = n(),
    mean_acc_delay = mean(acceptance_delay, na.rm = TRUE),
    mean_pub_delay = mean(publication_delay, na.rm = TRUE)
  )

print("quartile_table done")
write.csv(quartile_table, "tables/quartile_table.csv")

#Discipline table

discipline_table = data |> 
  filter(!is.na(discipline)) |> 
  group_by(discipline) |> 
  summarise(
    n = n(),
    mean_acc_delay = mean(acceptance_delay, na.rm = TRUE),
    mean_pub_delay = mean(publication_delay, na.rm = TRUE)
  )

print("discipline_table done")
write.csv(discipline_table, "tables/discipline_table.csv")


#Open access table

open_access_table = data |> 
  group_by(open_access) |> 
  summarise(
    n = n(),
    mean_acc_delay = mean(acceptance_delay, na.rm = TRUE),
    mean_pub_delay = mean(publication_delay, na.rm = TRUE)
  )

print("open_access_table done")
write.csv(open_access_table, "tables/open_access_table.csv")

#Megajournal table

megajournal_table = data |> 
  mutate(article_date_year = floor_date(as_date(article_date), "year"),
         article_date_year = str_replace(article_date_year, "-01-01", "")) |> 
  group_by(article_date_year) |> 
  group_by(is_mega) |> 
  summarise(
    n = n(),
    mean_acc_delay = mean(acceptance_delay, na.rm = TRUE),
    mean_pub_delay = mean(publication_delay, na.rm = TRUE)
  )

print("megajournal_table done")
write.csv(megajournal_table, "tables/megajournal_table.csv")

#Retraction table

retraction_table = data |> 
  filter(str_detect(publication_types, "Retracted")) |> 
  summarise(
    n = n(),
    mean_acc_delay = mean(acceptance_delay, na.rm = TRUE),
    mean_pub_delay = mean(publication_delay, na.rm = TRUE)
  )

print("retraction_table done")
write.csv(retraction_table, "tables/retraction_table.csv")


#Acceptance delay table

acceptance_table = data |> 
  summarise(
    n = n(),
    `2_months` = sum(acceptance_delay <= 60, na.rm = TRUE) / n * 100,
    `6_months` = sum(acceptance_delay <= 180, na.rm = TRUE) / n * 100,
    `1_year` = sum(acceptance_delay <= 365, na.rm = TRUE) / n * 100
  )


print("Acceptance delay table done")
write.csv(acceptance_table, "tables/acceptance_table.csv")


#Publication delay table

publication_table = data |> 
  summarise(
    n = n(),
    `2_months` = sum(publication_delay <= 60, na.rm = TRUE) / n * 100,
    `6_months` = sum(publication_delay <= 180, na.rm = TRUE) / n * 100,
    `1_year` = sum(publication_delay <= 365, na.rm = TRUE) / n * 100
  )

print("publication_table done")
write.csv(publication_table, "tables/publication_table.csv")

#Delay for disciplines table

delay_discipline_table = data |> 
  filter(!is.na(discipline)) |> 
  group_by(discipline) |> 
  summarise(
    n = n(),
    `2_months_acc` = sum(acceptance_delay <= 60, na.rm = TRUE) / n * 100,
    `6_months_acc` = sum(acceptance_delay <= 180, na.rm = TRUE) / n * 100,
    `1_year_acc` = sum(acceptance_delay <= 365, na.rm = TRUE) / n * 100,
    `2_months_pub` = sum(publication_delay <= 60, na.rm = TRUE) / n * 100,
    `6_months_pub` = sum(publication_delay <= 180, na.rm = TRUE) / n * 100,
    `1_year_pub` = sum(publication_delay <= 365, na.rm = TRUE) / n * 100
  )


print("Delay discipline table done")
write.csv(delay_discipline_table, "tables/delay_discipline_table.csv")
