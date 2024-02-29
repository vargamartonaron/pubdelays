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


acceptance_data <- articles |>
  dplyr::group_by(article_date_month) |>
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
