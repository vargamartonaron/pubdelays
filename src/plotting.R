library(readr)
library(ggplot2)
library(papaja)
library(dplyr)
library(tidyr)
setwd('~/pubdelays')

articles <- readr::read_tsv('/users/usumusu/pubdelays/journal_articles.tsv')

acceptance_data <- articles |>
  dplyr::mutate(plotyear = dplyr::if_else(!is.na(article_date), lubridate::year(article_date), lubridate::year(pubdate)))|>
  dplyr::select(plotyear, acceptance_delay) |>
  tidyr::drop_na(acceptance_delay)
  

acceptance_plot <- ggplot(acceptance_data, aes(x = plotdate, y = acceptance_delay, group = 1)) +
  geom_smooth(method = "loess", se = TRUE, span = 1, color = "black", linewidth = 1.2) +
  geom_point(alpha = 1/ 20) +
  labs(y = "Acceptance delay (days)", x = "Year") +
  theme_apa() +
  theme(axis.text.x = element_text(size = 16, family = "Times", hjust = 1),
        axis.text.y = element_text(size = 16, family = "Times"),
        axis.title = element_text(size = 22, family = "Times")) +
  ylim(0, 200)

  ggsave('acceptance_plot.jpeg', width = 7, height = 7, scale = 0.9)