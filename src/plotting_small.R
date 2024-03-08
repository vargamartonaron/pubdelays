library(cowplot)
library(ggdist)
library(readr)
library(ggplot2)
library(papaja)
library(dplyr)
library(tidyr)
library(viridis)
library(lubridate)

setwd('~/pubdelays')

articles <- readr::read_tsv('/users/usumusu/pubdelays/journal_articles.tsv')

articles = articles |> 
  #Cut the first 10000 rows
  #dplyr::slice(1:10000) |>
  tidyr::drop_na(article_date) |>
  #only preserve years and months from the date variable
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-"))

articles = joined_wos |> 
  #Cut the first 10000 rows
  #dplyr::slice(1:10000) |>
  tidyr::drop_na(article_date) |>
  #only preserve years and months from the date variable
  dplyr::mutate(article_date_month = paste(lubridate::year(article_date), lubridate::month(article_date),"01", sep = "-"))

acceptance_data <- articles |>
  dplyr::group_by(article_date_month) |>
  dplyr::reframe(delay = median(acceptance_delay, na.rm=T)) |>
  dplyr::filter(article_date_month >= lubridate::as_date('2016-01-01') & article_date_month <= lubridate::as_date('2023-12-01')) |>
  tidyr::drop_na(delay) |>
  filter(delay <= 700)

acceptance_data_discipline <- articles |>
  dplyr::group_by(article_date_month, discipline) |>
  dplyr::reframe(delay = median(acceptance_delay, na.rm=T)) |>
  dplyr::filter(article_date_month >= lubridate::as_date('2016-01-01') & article_date_month <= lubridate::as_date('2023-12-01')) |>
  tidyr::drop_na(delay) |>
  filter(delay <= 700)

acceptance_data_journal <- articles |>
  dplyr::group_by(article_date_month, journal_title) |>
  dplyr::reframe(delay = median(acceptance_delay, na.rm=T)) |>
  dplyr::filter(article_date_month >= lubridate::as_date('2016-01-01') & article_date_month <= lubridate::as_date('2023-12-01')) |>
  tidyr::drop_na(delay)

acceptance_plot <- ggplot(acceptance_data, aes(x = as_date(article_date_month), y = delay)) +
  geom_point(alpha=1/5) +
  scale_x_date(date_breaks = '1 years', date_labels = '%Y') +
  ylim(0,400)+
  labs(y = "Elfogadási késés mediánja (nap)", x = "Dátum", title = "Elfogadási késés") 
ggsave('acceptance_plot.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

acceptance_plot_journal <- ggplot(acceptance_data_journal, aes(x = as_date(article_date_month), y = delay)) +
  geom_smooth(show.legend = F, na.rm = TRUE, span = 3, se = T)+
  scale_x_date(date_breaks = '1 years', date_labels = '%Y') +
  ylim(0,400)+
  labs(y = "Elfogadási késés mediánja (nap)", x = "Dátum", title = "Elfogadási késés") 

ggsave('acceptance_plot_journal.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")


bin_plot <- ggplot(acceptance_data_journal, aes(x = as_date(article_date_month), y = delay)) +
  geom_bin2d(bins = 80) +
  scale_fill_gradient(low = "#FFFFFF",
                      high = "#132B43") +
  geom_density2d() +
  ylim(0, 400)
ggsave('bin_plot.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")
print(bin_plot)


discipline_plot <- ggplot(acceptance_data_discipline, aes(x = as_date(article_date_month), y = delay, group = discipline, colour = discipline)) +
  geom_smooth(show.legend = T, na.rm = TRUE, span = 3, se = T) +
  scale_x_date(date_breaks = '1 years', date_labels = '%Y') +
  ylim(0,400)+
  labs(y = "Median Acceptance delay (days)", x = "Date", title = "Acceptance delay among disciplines")

ggsave('discipline_plot.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

print(discipline_plot) 

#Need: percentile plot, open access or not plot, megajournal plot
#Percentile plot:

percentile_function <- function(df) {
  # Return delay percentiles from df
  probs = seq(0.05, 1, 0.025)
  dplyr::tibble(
    percentile = 100 * probs,
    delay = quantile(joined_wos$acceptance_delay, probs=probs, na.rm=TRUE)
  )
}

acceptance_data_quantile = acceptance_data %>%
  dplyr::group_by(delay, article_date_month) %>%
  dplyr::do(percentile_function(.))

#Works until this, doesn't work after this

acceptance_plot_quantile <- ggplot(acceptance_data_quantile, aes(x = as_date(article_date_month), y = delay)) +
  geom_smooth(aes(group = NULL)) +
  geom_smooth(data = dplyr::filter(acceptance_data_quantile, percentile == 50), color = "black") +
  geom_line(data = dplyr::filter(acceptance_data_quantile, percentile %in% c(25, 75)), color = "blue")

print(acceptance_plot_quantile)

#Open access plot

open_access_plot <- ggplot(joined_wos, aes(x = as_date(article_date_month), y = acceptance_delay, color = `Open Access status`)) +
  geom_smooth(show.legend = T, na.rm = TRUE, span = 3, se = T) +
  scale_x_date(date_breaks = '1 years', date_labels = '%Y') +
  ylim(0,400)+
  labs(y = "Median Acceptance delay (days)", x = "Date", title = "Acceptance delay by Open Access status")

print(open_access_plot)


#Megajournal plot

megajournals <- c("PLOS ONE", "Scientific Reports", "BMJ Open", "PeerJ", "Royal Society Open Science", "F1000Research", "GigaScience", "SAGE Open", "Heliyon", "SAGE Open Medicine", "SAGE Open Medical Case Reports", 
                  "SAGE Open Nursing", "SAGE Open Medicine", "SAGE Open Engineering", "Springer Plus, SAGE Open", "IEEE Access", "G3", "Biology Open", "Elementa, Science of the Antropoce", "AIP Advances", "Journal of Engineering",
                  "Sustainability", "International Journal of Molecular Sciences", "Sensors", "Energies", "Molecules", "Science of the Total Environment", "eLife", "FEBS Open Bio", "mBio", "ACS Omega")
#I have to check which is really a megajournal and which is not, then we need their issn-s so we can filter.
