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
library(ggdark)

setwd('~/pubdelays')
#extrafont::font_import(paths='~/times.ttf', prompt = FALSE)
#loadfonts()

invert_geom_defaults()
articles <- readr::read_tsv('/users/usumusu/pubdelays/filtered_articles.tsv')
#articles <- read_tsv('/home/martonaronvarga/GitHub/ppk_expcourse/journal_articles.tsv')

acceptance_data <- articles |>
  dplyr::group_by(article_date) |>
  dplyr::reframe(delay = median(acceptance_delay, na.rm=T)) |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-11-01')) |>
  tidyr::drop_na(delay)

  
acceptance_plot <- ggplot(acceptance_data, aes(x = article_date, y = delay, colour = after_stat(y))) +
  geom_point(alpha = 1/3) +
  scale_color_viridis() +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(y = "Delay (day)", x = "Date", title = "Acceptance Delay") +
  ylim(80, 150) +
  dark_mode(theme_apa(base_family = "Times", base_size = 24)) + 
  geom_hline(yintercept = 100, linetype = "dashed", color = "#BF616A", linewidth = 2) +
  theme(legend.position = "none")

ggsave('acceptance_plot.pdf', scale = 0.9, width = 16, height = 9, units = "in", dpi = 200)
  
covid_delay_data <- articles |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-11-01')) |>
  dplyr::group_by(article_date) |>
  dplyr::reframe(covid_acceptance_delay = median(acceptance_delay[is_covid], na.rm = TRUE),
                 covid_publication_delay = median(publication_delay[is_covid], na.rm = TRUE)) |>
  tidyr::drop_na(covid_acceptance_delay) |>
  tidyr::drop_na(covid_publication_delay)

non_covid_delay_data <- articles |>
  dplyr::group_by(article_date) |>
  dplyr::reframe(non_covid_acceptance_delay = median(acceptance_delay[!is_covid], na.rm = TRUE),
                 non_covid_publication_delay = median(publication_delay[!is_covid], na.rm = TRUE)) |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-11-01')) |>
  tidyr::drop_na(non_covid_acceptance_delay) |>
  tidyr::drop_na(non_covid_publication_delay)

joined_delay_data <- left_join(non_covid_delay_data, covid_delay_data, by = join_by(article_date)) |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-11-01'))
  
covid_acceptance_plot <- ggplot(joined_delay_data) +
  geom_point(alpha = 1/2, aes(x = article_date, y = covid_acceptance_delay, color = "Contains Covid keyword")) +
  geom_point(alpha = 1/3, aes(x = article_date, y = non_covid_acceptance_delay, color = "Does not contain Covid keyword")) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ylim(20, 150) +
  labs(y = "Delay (day)", x = "Date", color = NULL, title = "Acceptance delay in light of covid") +
  dark_mode(theme_apa(base_family = "Times", base_size = 24)) +
  scale_color_manual(values = c("Contains Covid keyword" = viridis_pal(option = "viridis")(2)[1], "Does not contain Covid keyword" = viridis_pal(option = "viridis")(2)[2])) +
  theme(legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 32),
        legend.position = "none")

ggsave('covid_acceptance_plot.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

publication_data <- articles |>
  dplyr::group_by(article_date) |>
  dplyr::reframe(delay=median(publication_delay, na.rm=T)) |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-11-01')) |>
  tidyr::drop_na(delay)

publication_plot <- ggplot(publication_data, aes(x = article_date, y = delay, colour = after_stat(y))) +
  geom_point(alpha = 1/3) +
  scale_color_viridis() +
  geom_hline(yintercept = 20, linetype = "dashed", color = "#BF616A", linewidth = 2) +
  ylim(0, 50) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(y = "Delay (day)", x = "Date", title = "Publication delay") +
  dark_mode(theme_apa(base_family = "Times", base_size = 24)) +
  theme(leged.position = "none")

ggsave('publication_plot.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

covid_publication_plot <- ggplot(joined_delay_data, aes(x = article_date)) +
  geom_point(alpha = 1/2, aes(y = covid_publication_delay, color = "Contains Covid keyword")) +
  geom_point(alpha = 1/3, aes(y = non_covid_publication_delay, color = "Does not contain Covid keyword")) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ylim(0, 50) +
  labs(y = "Delay (day)", x = "Date", color = NULL, title = "Publication delay") +
  dark_mode(theme_apa(base_family = "Times", base_size = 24)) +
  scale_color_manual(values = c("Contains Covid keyword" = viridis_pal(option = "viridis")(2)[1], "Does not contain Covid keyword" = viridis_pal(option = "viridis")(2)[2])) +
  theme(legend.key.size = unit(2, "cm"),
        legend.position = "none")
  

ggsave('covid_publication_plot.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

top_10_megajournals <- articles |>
  dplyr::group_by(journal_title) |>
  dplyr::filter(is_mega = TRUE)

top_10_megajournals_articles <- articles |>
  filter(journal_title %in% top_10_megajournals$journal_title)

delays_megajournals_acceptance_delay <- top_10_megajournals_articles |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-11-01')) |>
  tidyr::drop_na(acceptance_delay) |>
  dplyr::filter(acceptance_delay > 0 & acceptance_delay < 300) |>
  ggplot(aes(x = acceptance_delay, y = reorder(journal_title, -acceptance_delay), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 0, panel_scaling = TRUE, quantile_lines = TRUE, quantiles = 2) +
  scale_fill_viridis_c(name = "Delay", option = "magma") +
  dark_mode(theme_apa(base_family = "Times", base_size = 24)) +
  labs(title = "Acceptance delay in top-10 mega-journals", x = NULL, y = NULL) +
  theme(axis.title.x = element_blank())

ggsave('top_10_megajournal_acceptance_delay.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

delays_megajournals_publication_delay <- top_10_megajournals_articles |>
 dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-11-01')) |>
 tidyr::drop_na(publication_delay) |>
 dplyr::filter(publication_delay > 0 & publication_delay < 200) |>
 ggplot(aes(x = publication_delay, y = reorder(journal_title, -publication_delay), fill = after_stat(x))) +
 geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 1., panel_scaling = TRUE, quantile_lines = TRUE, quantiles = 2) +
 scale_fill_viridis_c(name = "Delay", option = "magma") +
 dark_mode(theme_apa(base_family = "Times", base_size = 24)) +
 labs(title = "Publication delay in top-10 mega-journals", x = NULL, y = NULL) +
 theme(axis.title.x = element_blank())

ggsave('top_10_megajournal_publication_delay.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

top_10_h_index_journals <- articles |>
  group_by(journal_title) |>
  reframe(h_index = first(as.numeric(h_index))) |>
  arrange(desc(h_index)) |>
  slice_head(n = 10)

top_h_index_articles <- articles |>
  filter(journal_title %in% top_10_h_index_journals$journal_title)

delays_h_index_acceptance_delay <- top_h_index_articles |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-11-01')) |>
  tidyr::drop_na(acceptance_delay) |>
  dplyr::filter(acceptance_delay > 0 & acceptance_delay < 300)

delays_h_index_acceptance_delay_plot <- delays_h_index_acceptance_delay |>
  ggplot(aes(x = acceptance_delay, y = reorder(journal_title, -acceptance_delay, sum), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 1., panel_scaling = TRUE, quantile_lines = TRUE, quantiles = 2) + 
  scale_fill_viridis_c(name = "Delay", option = "magma") +
  scale_y_discrete(expand = expansion(mult = c(0.1, 0.2))) +
  dark_mode(theme_apa(base_family = "Times", base_size = 16)) +
  labs(title = "Acceptance delay in the top-10 h-index journals", x = NULL, y = NULL) +
  theme(axis.title.x = element_blank())
  

ggsave('top_10_h_index_acceptance_delay.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

delays_h_index_publication_delay <- top_h_index_articles |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-11-01')) |>
  tidyr::drop_na(publication_delay) |>
  dplyr::filter(publication_delay > 0 & publication_delay < 200) |>
  ggplot(aes(x = publication_delay, y = reorder(journal_title, publication_delay), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 1., panel_scaling = TRUE, quantile_lines = TRUE, quantiles = 2) +
  scale_fill_viridis_c(name = "Delay", option = "magma") +
  xlim(0, 100) +
  scale_y_discrete(expand = expansion(mult = c(0.1, 0.2))) +
  dark_mode(theme_apa(base_family = "Times", base_size = 16)) +
  labs(title = "Publication delay in the top-10 h-index journals", x = NULL, y = NULL) +
  theme(axis.title.x = element_blank())

ggsave('top_10_h_index_publication_delay.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")


