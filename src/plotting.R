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
library(hexbin)

setwd('~/pubdelays')
#extrafont::font_import(paths='~/times.ttf', prompt = FALSE)
#loadfonts()


articles <- readr::read_tsv('/users/usumusu/pubdelays/journal_articles.tsv')
#articles <- read_tsv('/home/martonaronvarga/GitHub/ppk_expcourse/journal_articles.tsv')

acceptance_data <- articles |>
  dplyr::group_by(article_date) |>
  dplyr::reframe(delay = median(acceptance_delay, na.rm=T)) |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-12-01')) |>
  tidyr::drop_na(delay)

  
acceptance_plot <- ggplot(acceptance_data, aes(x = article_date, y = delay)) +
  geom_point(alpha = 1/5) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(y = "Elfogadási késés mediánja (nap)", x = "Dátum", title = "Elfogadási késés") +
  theme_apa(base_family = "Times", base_size = 32) + 
  ylim(40, 160)

ggsave('acceptance_plot.pdf', scale = 0.9, width = 16, height = 9, units = "in", dpi = 200)
  
covid_delay_data <- articles |>
  dplyr::group_by(article_date) |>
  dplyr::reframe(covid_acceptance_delay = median(acceptance_delay[is_covid], na.rm = TRUE),
                 covid_publication_delay = median(publication_delay[is_covid], na.rm = TRUE)) |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-12-01')) |>
  tidyr::drop_na(covid_acceptance_delay) |>
  tidyr::drop_na(covid_publication_delay)

non_covid_delay_data <- articles |>
  dplyr::group_by(article_date) |>
  dplyr::reframe(non_covid_acceptance_delay = median(acceptance_delay[!is_covid], na.rm = TRUE),
                 non_covid_publication_delay = median(publication_delay[!is_covid], na.rm = TRUE)) |>
  tidyr::drop_na(non_covid_acceptance_delay) |>
  tidyr::drop_na(non_covid_publication_delay)

joined_delay_data <- left_join(non_covid_delay_data, covid_delay_data, by = join_by(article_date)) |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-12-01'))
  
covid_acceptance_plot <- ggplot(joined_delay_data) +
  geom_point(alpha = 0.5, aes(x = article_date, y = covid_acceptance_delay, color = "Covid")) +
  geom_point(alpha = 0.5, aes(x = article_date, y = non_covid_acceptance_delay, color = "Nem Covid")) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(y = "Elfogadási késés mediánja (nap)", x = "Dátum", color = NULL, title = "Elfogadási késés") +
  theme_apa(base_family = "Times", base_size = 32) +
  scale_color_manual(values = c("Covid" = viridis_pal(option = "plasma")(2)[2], "Nem Covid" = viridis_pal(option = "plasma")(2)[1])) +
  ylim(0, 160)

ggsave('covid_acceptance_plot.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

publication_data <- articles |>
  dplyr::group_by(article_date) |>
  dplyr::reframe(delay=median(publication_delay, na.rm=T)) |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-12-01')) |>
  tidyr::drop_na(delay)

publication_plot <- ggplot(publication_data, aes(x = article_date, y = delay)) +
  geom_point(alpha = 1/5) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(y = "Publikációs késés mediánja (nap)", x = "Dátum", title = "Publikációs késés") +
  theme_apa(base_family = "Times", base_size = 32) +
  ylim(0, 70)

ggsave('publication_plot.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

covid_publication_plot <- ggplot(joined_delay_data, aes(x = article_date)) +
  geom_point(alpha = 0.5, aes(y = covid_publication_delay, color = "Covid")) +
  geom_point(alpha = 0.65, aes(y = non_covid_publication_delay, color = "Nem Covid")) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(y = "Publikációs késés mediánja (nap)", x = "Dátum", color = NULL, title = "Publikációs késés") +
  theme_apa(base_family = "Times", base_size = 32) +
  scale_color_manual(values = c("Covid" = viridis_pal(option = "viridis")(2)[2], "Nem Covid" = viridis_pal(option = "viridis")(2)[1])) +
  ylim(0, 70)
  

ggsave('covid_publication_plot.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

top_10_megajournals <- articles |>
  dplyr::group_by(journal_title) |>
  dplyr::reframe(total_docs = first(as.numeric(`total_docs._(3years)`))) |>
  dplyr::arrange(desc(total_docs)) |>
  dplyr::slice_head(n = 10)

top_10_megajournals_articles <- articles |>
  filter(journal_title %in% top_10_megajournals$journal_title)

top_10_megajournals_plot <- top_10_megajournals |>
  ggplot(aes(y = reorder(journal_title, -total_docs), x = total_docs, fill = total_docs)) +
  geom_col() +
  scale_fill_viridis_c(option = "plasma", name = "Dok.-ok 2019-2022") +
  labs(y = NULL, x = NULL, title = "Mega - folyóiratok") +
  theme_apa(base_family = "Times", base_size = 32)

ggsave('top_10_megajournals_plot.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

delays_megajournals_acceptance_delay <- top_10_megajournals_articles |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-12-01')) |>
  tidyr::drop_na(acceptance_delay) |>
  dplyr::filter(acceptance_delay > 0 & acceptance_delay < 150) |>
  ggplot(aes(x = acceptance_delay, y = reorder(journal_title, -acceptance_delay), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 0, panel_scaling = TRUE) +
  scale_x_continuous(expansion(c(0, 0))) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
  scale_fill_viridis_c(name = "Késés", option = "plasma") +
  labs(title = "Elfogadási késés a mega - folyóiratokban") +
  theme_ridges(font_size = 12, grid = TRUE, font_family = "Times") +
  theme(axis.title.y = element_blank(), text = element_text(family = "Times"),
        axis.title.x = element_blank()) +
  theme_apa(base_family = "Times", base_size = 32)

ggsave('top_10_megajournal_acceptance_delay.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

delays_megajournals_publication_delay <- top_10_megajournals_articles |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-12-01')) |>
  tidyr::drop_na(publication_delay) |>
  dplyr::filter(publication_delay > 0 & publication_delay < 100) |>
  ggplot(aes(x = publication_delay, y = reorder(journal_title, -publication_delay), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 1., panel_scaling = TRUE) +
  scale_x_continuous(expansion(c(0, 0))) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
  scale_fill_viridis_c(name = "Késés", option = "plasma") +
  labs(title = "Publikációs késés a mega - folyóiratokban") +
  theme_ridges(font_size = 12, grid = TRUE, font_family = "Times") +
  theme(axis.title.y = element_blank(), text = element_text(family = "Times"),
        axis.title.x = element_blank()) +
  theme_apa(base_family = "Times", base_size = 32)

ggsave('top_10_megajournal_publication_delay.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

top_10_h_index_journals <- articles |>
  group_by(journal_title) |>
  reframe(h_index = first(as.numeric(h_index))) |>
  arrange(desc(h_index)) |>
  slice_head(n = 10)

top_h_index_articles <- articles |>
  filter(journal_title %in% top_10_h_index_journals$journal_title)

top_10_h_index_plot <- top_10_h_index_journals |>
  ggplot(aes(y = reorder(journal_title, -h_index), x = h_index, fill = h_index)) +
  geom_col() +
  scale_fill_viridis_c(option = "plasma", name = NULL) +
  labs(title = "Top 10 h-index folyóirat", x = NULL, y = NULL) +
  theme_apa(base_family = "Times", base_size = 32)
  
ggsave('top_10_h_index_plot.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

delays_h_index_acceptance_delay <- top_h_index_articles |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-12-01')) |>
  tidyr::drop_na(acceptance_delay) |>
  dplyr::filter(acceptance_delay > 0 & acceptance_delay < 200) |>
  ggplot(aes(x = acceptance_delay, y = reorder(journal_title, -acceptance_delay), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 1., panel_scaling = TRUE) +
  scale_x_continuous(expansion(c(0, 0))) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
  scale_fill_viridis_c(name = "Késés", option = "plasma") +
  labs(title = "Elfogadási késés a top 10 h-index lapokban") +
  theme_ridges(font_size = 12, grid = TRUE, font_family = "Times") +
  theme(axis.title.y = element_blank(), text = element_text(family = "Times"),
        axis.title.x = element_blank()) +
  theme(plot.title = element_text(size = 24)) +
  theme_apa(base_family = "Times", base_size = 32)
  

ggsave('top_10_h_index_acceptance_delay.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

delays_h_index_publication_delay <- top_h_index_articles |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-12-01')) |>
  tidyr::drop_na(publication_delay) |>
  dplyr::filter(publication_delay > 0 & publication_delay < 100) |>
  ggplot(aes(x = publication_delay, y = reorder(journal_title, -publication_delay), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 1., panel_scaling = TRUE) +
  scale_x_continuous(expansion(c(0, 0))) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
  scale_fill_viridis_c(name = "Késés", option = "plasma") +
  labs(title = "Publikációs késés a top 10 h-index lapokban") +
  theme_ridges(font_size = 12, grid = TRUE, font_family = "Times") +
  theme(axis.title.y = element_blank(), text = element_text(family = "Times"),
        axis.title.x = element_blank()) +
  theme_apa(base_family = "Times", base_size = 32)

ggsave('top_10_h_index_publication_delay.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

discipined_articles <- articles |>
  tidyr::separate_longer_delim(areas, delim = ",") |>
  dplyr::mutate(areas = trimws(areas)) |>
  # getting invalid rows.
  dplyr::filter(!grepl("^\\d+$", areas) & areas != "FALSE" & !is.na(areas))

relevant_areas <- c('Medicine', 'Economics', 'Biochemistry', 'Physics and Astronomy', 'Engineering', 'Arts and Humanities', 'Psychology', 'Neuroscience', 'Mathematics', 'Computer Science', 'Pharmacology')

acceptance_density_disciplines_plot <- discipined_articles |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-12-01')) |>
  dplyr::filter(areas %in% relevant_areas) |>
  tidyr::drop_na(acceptance_delay) |>
  dplyr::filter(acceptance_delay > 0 & acceptance_delay < 200) |>
  ggplot(aes(x = acceptance_delay, y = reorder(areas, -acceptance_delay), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 1., panel_scaling = TRUE) +
  scale_x_continuous(expansion(0, 0)) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
  scale_fill_viridis_c(name = "Késés", option = "plasma") +
  labs(title = "Elfogadási késés diszciplinánként", subtitle = "Egy cikk több diszciplinába is tartozhat, a folyóirat alapján") +
  theme_ridges(font_size = 12, grid = TRUE, font_family = "Times") +
  theme(axis.title.y = element_blank(), text = element_text(family = "Times"),
        axis.title.x = element_blank()) +
  theme_apa(base_family = "Times", base_size = 32)


ggsave('discipline_ridgeplot.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

standard_dev_across_disciplines_acceptance <- discipined_articles |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-12-01')) |>
  tidyr::drop_na(acceptance_delay) |>
  dplyr::group_by(article_date, areas) |>
  dplyr::reframe(mean_delay = mean(acceptance_delay),
                 sd_delay = sd(acceptance_delay),
                 coeff_of_var = sd_delay / mean_delay)

standard_dev_across_disciplines_acceptance_plot <- standard_dev_across_disciplines_acceptance |>
  dplyr::filter(!is.na(coeff_of_var)) |>
  ggplot(aes(y = factor(areas), x = article_date, z = coeff_of_var)) +
  stat_summary_2d(geom = "raster", bins = 30, alpha = 0.8, fun = "identity") +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  scale_fill_viridis_c(option = "plasma", trans = "log", name = "Koeff", labels = scales::number_format(scale = 1, accuracy = 0.01)) +
  labs(title = "Variációs koefficiens az évek során, területenként",
       x = NULL, y = NULL, subtitle="Minél nagyobb a koefficiens, annál variábilisabb volt az adott napon az adott terület.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = "Times"),
        axis.text.y = element_text(family = "Times")) +
  theme_apa(base_family = "Times", base_size = 32)

ggsave('deviation_raster_plot.pdf', dpi = 200, width = 16, height = 9, units = "in")
