library(readr)
library(ggplot2)
library(papaja)
library(dplyr)
library(tidyr)
library(extrafont)
library(viridis)
library(ggridges)
library(geomorph)
library(lubridate)

setwd('~/pubdelays')
#extrafont::font_import(paths='~/times.ttf', prompt = FALSE)
#loadfonts()


articles <- readr::read_tsv('/users/usumusu/pubdelays/journal_articles.tsv')

acceptance_data <- articles |>
  dplyr::group_by(article_date) |>
  dplyr::filter(lubridate::year(article_date) >= 2016 & lubridate::year(article_date) <= 2022) |>
  dplyr::reframe(delay=median(acceptance_delay, na.rm=T)) |>
  tidyr::drop_na(delay)

  
acceptance_plot <- ggplot(acceptance_data, aes(x = article_date, y = delay)) +
  geom_point(alpha = 1/5) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(y = "Elfogadási késés mediánja (nap)", x = "Dátum", title = "Elfogadási késés") +
  theme(axis.text.x = element_text(size = 16, family = "Times", hjust = 1),
        axis.text.y = element_text(size = 16, family = "Times"),
        axis.title = element_text(size = 22, family = "Times")) +
  theme_apa() + 
  ylim(0, 300)

  ggsave('acceptance_plot.pdf', width = 7, height = 7, scale = 0.9, dpi = 200)
  
covid_acceptance_data <- articles |>
  dplyr::group_by(article_date, is_covid) |>
  dplyr::filter(lubridate::year(article_date) >= 2019 & lubridate::year(article_date) <= 2022) |>
  dplyr::reframe(delay = median(acceptance_delay, na.rm = TRUE)) |>
  tidyr::drop_na(delay)
  
covid_acceptance_plot <- ggplot(covid_acceptance_data, aes(x = article_date, y = delay, color = is_covid)) +
  geom_point(alpha = 1/5) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(y = "Elfogadási késés mediánja (nap)", x = "Dátum", color = "Covid cikkek", title = "Elfogadási késés") +
  theme(axis.text.x = element_text(size = 16, family = "Times", hjust = 1),
        axis.text.y = element_text(size = 16, family = "Times"),
        axis.title = element_text(size = 22, family = "Times")) +
  theme_apa() + 
  ylim(0, 300) +
  scale_color_viridis(option = "plasma")

ggsave('covid_acceptance_plot.pdf', width = 7, height = 7, scale = 0.9, dpi = 200)

publication_data <- articles |>
  dplyr::group_by(article_date) |>
  dplyr::filter(lubridate::year(article_date) >= 2016 & lubridate::year(article_date) <= 2022) |>
  dplyr::reframe(delay=median(publication_delay, na.rm=T)) |>
  tidyr::drop_na(delay)

publication_plot <- ggplot(publication_data, aes(x = article_date, y = delay)) +
  geom_point(alpha = 1/5) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(y = "Publikációs késés mediánja (nap)", x = "Dátum", title = "Publikációs késés") +
  theme(axis.text.x = element_text(size = 16, family = "Times", hjust = 1),
        axis.text.y = element_text(size = 16, family = "Times"),
        axis.title = element_text(size = 22, family = "Times")) +
  theme_apa() + 
  ylim(0, 300)

ggsave('publication_plot.pdf', width = 7, height = 7, scale = 0.9, dpi = 200)


covid_publication_data <- articles |>
  dplyr::group_by(article_date, is_covid) |>
  dplyr::filter(lubridate::year(article_date) >= 2019 & lubridate::year(article_date) <= 2022) |>
  dplyr::reframe(delay = median(publication_delay, na.rm = TRUE)) |>
  tidyr::drop_na(delay)

covid_publication_plot <- ggplot(covid_publication_data, aes(x = article_date, y = delay, color = is_covid)) +
  geom_point(alpha = 1/5) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(y = "Publikációs késés mediánja (nap)", x = "Dátum", color = "Covid cikkek", title = "Publikációs késés") +
  theme(axis.text.x = element_text(size = 16, family = "Times", hjust = 1),
        axis.text.y = element_text(size = 16, family = "Times"),
        axis.title = element_text(size = 22, family = "Times")) +
  theme_apa() + 
  ylim(0, 300) +
  scale_color_viridis(option = "plasma")

ggsave('covid_publication_plot.pdf', width = 7, height = 7, scale = 0.9, dpi = 200)

top_10_megajournals <- articles |>
  dplyr::group_by(journal_title) |>
  dplyr::reframe(total_docs = `total_docs._(3years)`,
                 n_articles = n()) |>
  dplyr::arrange(desc(total_docs)) |>
  dplyr::slice_max(order_by = total_docs, n = 10)

top_10_megajournals_plot <- top_10_megajournals |>
  ggplot(aes(x = reorder(journal_title, -total_docs), y = total_docs, fill = total_docs)) +
  geom_col() +
  scale_fill_viridis_c(option = "plasma", name = 'Dok. - ok') +
  labs(y = "Megjelent dokumentumok 2019 - 2022", x = "Folyóirat", title = "Mega - folyóiratok") +
  theme(axis.text.x = element_text(size = 16, family = "Times", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 16, family = "Times"),
        axis.title = element_text(size = 22, family = "Times")) +
  theme_apa()

ggsave('top_10_megajournals_plot.pdf', width = 7, height = 7, scale = 0.9, dpi = 200)

top_10_h_index <- articles |>
  dplyr::group_by(journal_title) |>
  dplyr::reframe(h_index = h_index) |>
  dplyr::slice_max(order_by = h_index, n = 10)

top_10_h_index_plot <- top_10_h_index |>
  ggplot(aes(x = reorder(journal_title, -h_index), y = h_index, fill = h_index)) +
  geom_col() +
  scale_fill_viridis_c(option = "plasma", name = "h index") +
  labs(y = "h index", x = "Folyóirat", title = "Top 10 h index folyóirat") +
  theme(axis.text.x = element_text(size = 16, family = "Times", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 16, family = "Times"),
        axis.title = element_text(size = 22, family = "Times")) +
  theme_apa()

ggsave('top_10_h_index_plot.pdf', width = 7, height = 7, scale = 0.9, dpi = 200)

discipined_articles <- articles |>
  tidyr::separate_longer_delim(areas, delim = ",") |>
  dplyr::mutate(areas = trimws(areas)) |>
  # getting invalid rows.
  dplyr::filter(!grepl("^\\d+$", areas) & areas != "FALSE" & !is.na(areas))

relevant_areas <- c('Medicine', 'Economics', 'Biochemistry', 'Physics and Astronomy', 'Engineering', 'Arts and Humanities', 'Psychology', 'Neuroscience', 'Mathematics', 'Computer Science', 'Pharmacology')

acceptance_density_disciplines_plot <- discipined_articles |>
  dplyr::filter(lubridate::year(article_date) >= 2016 & lubridate::year(article_date) <= 2022) |>
  dplyr::filter(areas %in% relevant_areas) |>
  tidyr::drop_na(acceptance_delay) |>
  ggplot(aes(x = acceptance_delay, y = reorder(areas, after_stat(density)), fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, gradient_lwd = 1.) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = expand_scale(mult = c(0.01, 0.25))) +
  scale_fill_viridis_c(name = "Késés", option = "plasma") +
  labs(title = "Elfogadási késés diszciplinánként", subtitle = "Egy cikk több diszciplinába is tartozhat, a folyóirat alapján") +
  theme_ridges(font_size = 13, grid = TRUE) +
  theme(axis.title.y = element_blank(), text = element_text(family = "Times"))

ggsave('discipline_ridgeplot.pdf', width = 12, height = 16, scale = 0.9, dpi = 200)

standard_dev_across_disciplines_acceptance <- discipined_articles |>
  dplyr::filter(lubridate::year(article_date) >= 2016 & lubridate::year(article_date) <= 2022) |>
  tidyr::drop_na(acceptance_delay) |>
  dplyr::group_by(areas) |>
  dplyr::reframe(mean_delay = mean(acceptance_delay),
          sd_delay = sd(acceptance_delay)) |>
  dplyr::mutate(coeff_of_var = sd_delay / mean_delay) |>
  ggplot(aes(x = as.numeric(factor(disciplines)), y = coeff_of_var, fill = coeff_of_var, label = areas)) +
  stat_summary_2d(geom = "raster", bins = 30, aes(fill = ..level..),
                  col = "white", alpha = 0.7) +
  scale_fill_viridis_c(option = "plasma", trans = "log") +
  labs(title = "Deviation Raster Plot Across Disciplines",
       x = "Discipline", y = "Coefficient of variation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times"),
        axis.text.y = element_text(family = "Times"))

ggsave('deviation_raster_plot.pdf', dpi = 200)