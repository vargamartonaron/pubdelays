#### Descriptive analysis

library(readr)
library(lubridate)
#install.packages("dplyr", repos = "https://cloud.r-project.org/")
library(dplyr)
#install.packages("stringr", repos = "https://cloud.r-project.org/")
library(stringr)
#install.packages("cowplot")
#library(cowplot)
#install.packages("ggdist", repos = "https://cloud.r-project.org/")
library(ggdist)
#install.packages("viridis", repos = "https://cloud.r-project.org/")
library(viridis)
#install.packages("ggplot2", repos = "https://cloud.r-project.org/")
library(ggplot2)
#install.packages("papaja", repos = "https://cloud.r-project.org/")
library(papaja)
#install.packages("tidyr", repos = "https://cloud.r-project.org/")
library(tidyr)
#install.packages("ggridges", repos = "https://cloud.r-project.org/")
library(ggridges)
#install.packages("geomorph", repos = "https://cloud.r-project.org/")
library(geomorph)
#install.packages("hexbin", repos = "https://cloud.r-project.org/")
library(hexbin)

data = readr::read_tsv("/users/zsimi/pubdelays/journal_articles_everything.tsv")

# Mean before and after COVID
colnames(data)

filtered_data = data |> 
  filter(!is.na(article_date) & !is.na(acceptance_delay)) |> 
  filter(acceptance_delay <= 730 & acceptance_delay >= 7) |> 
  mutate(article_date = floor_date(as_date(article_date), "month"),
         covid_group = if_else(article_date < as.Date("2019-12-01"), "Before", "After")) |> 
  mutate(is_psych = ifelse(((3200 <= `all_science_journal_classification_codes_(asjc)`) & (3207 >= `all_science_journal_classification_codes_(asjc)`)), TRUE, FALSE)) |> 
  group_by(covid_group)

mean_data <- filtered_data |> 
  summarise(mean_acceptance_delay = mean(acceptance_delay, na.rm = TRUE))

# Print the mean values for each group
print(mean_data)

sink("summary_stats.txt")

# Calculate mean
mean_data <- filtered_data |> 
  summarise(mean_acceptance_delay = mean(acceptance_delay))

# Print number of rows in data and filtered_data
num_rows_data <- nrow(data)
num_rows_filtered <- nrow(filtered_data)

# Calculate median
median_data <- filtered_data |> 
  summarise(median_acceptance_delay = median(acceptance_delay))

# Calculate standard deviation
sd_data <- filtered_data |> 
  summarise(sd_acceptance_delay = sd(acceptance_delay))

# Print the results
cat("Mean acceptance delay:\n")
cat(mean_data$mean_acceptance_delay)

cat("\n\nNumber of rows in data:", num_rows_data)
cat("\nNumber of rows in filtered data:", num_rows_filtered)

cat("\n\nMedian acceptance delay:\n")
cat(median_data$median_acceptance_delay)

cat("\n\nStandard deviation of acceptance delay:\n")
cat(sd_data$sd_acceptance_delay)

# Close the connection
sink()

#Covid articles

sink("covid.txt")
covid_data_false = filtered_data |> 
  filter(is_covid == FALSE) |> 
  summarise(mean_acceptance_delay = mean(acceptance_delay),
            median_acceptance_delay = median(acceptance_delay),
            sd_acceptance_delay = sd(acceptance_delay)) |> 
  mutate(is_covid = "False")

covid_data_true = filtered_data |> 
  filter(is_covid == TRUE) |> 
  summarise(mean_acceptance_delay = mean(acceptance_delay),
            median_acceptance_delay = median(acceptance_delay),
            sd_acceptance_delay = sd(acceptance_delay)) |> 
  mutate(is_covid = "True")

covid_data = rbind(covid_data_false, covid_data_true)
print(covid_data)
sink()

colnames(filtered_data)

megajournals <- c(
  "24701343", "21583226", "20466390", "20446055", "23251026",
  "22115463", "21601836", "21693536", "20513305", "21678359",
  "19326203", "20545703", "21582440", "20452322", "20566700",
  "23915447", "22991093", "24058440", "21508925", "2050084X",
  "20461402"
)

# Aggregate article_date variable to months as new variable
articles <- filtered_data |>
  dplyr::mutate(
    article_date_month = lubridate::floor_date(as_date(article_date), "month"),
    open_access_status = case_when(
      `does_the_journal_comply_to_doaj's_definition_of_open_access?` == "Yes" ~ TRUE,
      `does_the_journal_comply_to_doaj's_definition_of_open_access?` == "No" ~ FALSE,
      is.na(`does_the_journal_comply_to_doaj's_definition_of_open_access?`) == TRUE ~ FALSE,
      TRUE ~ FALSE
    ),
    is_mega = case_when(
      issn %in% megajournals ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  dplyr::filter(
    article_date_month >= lubridate::as_date('2016-01-01') & article_date_month <= lubridate::as_date('2023-12-01')
  )

# Plot data
acceptance_data <- articles |>
  dplyr::group_by(article_date_month) |>
  dplyr::reframe(
    delay = median(
      acceptance_delay,
      na.rm = TRUE
    )
  ) |>
  tidyr::drop_na(delay) |>
  dplyr::filter(delay <= 700)

acceptance_data_discipline <- articles |>
  dplyr::group_by(
    article_date_month,
    discipline,
    is_psych
  ) |>
  dplyr::reframe(
    delay = median(
      acceptance_delay,
      na.rm = TRUE
    )
  ) |>
  dplyr::filter(
    article_date_month >= lubridate::as_date("2016-01-01") &
      article_date_month <= lubridate::as_date("2023-12-01")
  ) |>
  tidyr::drop_na(delay) |>
  dplyr::filter(delay <= 700)

acceptance_data_journal <- articles |>
  dplyr::group_by(
    article_date_month,
    journal_title
  ) |>
  dplyr::reframe(
    delay = median(acceptance_delay, na.rm = TRUE)
  ) |>
  dplyr::filter(
    article_date_month >= lubridate::as_date("2016-01-01") &
      article_date_month <= lubridate::as_date("2022-12-01")
  ) |>
  tidyr::drop_na(delay)

acceptance_plot <- ggplot(acceptance_data,
                          aes(
                            x = as_date(article_date_month),
                            y = delay
                          )
) +
  geom_point(alpha = 1 / 5) +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  geom_hline(
    yintercept = 100,
    linetype = "dashed",
    color = "#BF616A",
    linewidth = 2
  ) +
  ylim(
    0,
    400
  ) +
  labs(
    y = "Acceptance delay (day)",
    x = "Date",
    title = "Acceptance delay"
  ) +
  theme_apa(base_family = "Times", base_size = 32)

ggsave(
  "acceptance_plot.pdf",
  scale = 0.9,
  dpi = 200,
  width = 16,
  height = 9,
  units = "in"
)

acceptance_plot_journal <- ggplot(
  acceptance_data_journal,
  aes(
    x = as_date(article_date_month),
    y = delay
  )
) +
  geom_smooth(
    show.legend = FALSE,
    na.rm = TRUE,
    span = 3,
    se = TRUE
  ) +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  ylim(
    0,
    400
  ) +
  labs(
    y = "Acceptance delay median (day)",
    x = "Date", title = "Acceptance delay"
  ) +
  theme_apa(base_family = "Times", base_size = 32)



ggsave(
  "acceptance_plot_journal.pdf",
  scale = 0.9, dpi = 200,
  width = 16,
  height = 9,
  units = "in"
)


bin_plot <- ggplot(
  acceptance_data_journal,
  aes(
    x = as_date(article_date_month),
    y = delay
  )
) +
  geom_bin2d(bins = 80) +
  scale_fill_gradient(
    low = "#FFFFFF",
    high = "#132B43"
  ) +
  geom_density2d() +
  ylim(
    0,
    400
  ) +
  theme_apa(base_family = "Times", base_size = 32)



ggsave(
  "bin_plot.pdf",
  scale = 0.9,
  dpi = 200,
  width = 16,
  height = 9,
  units = "in"
)

discipline_plot <- ggplot(
  acceptance_data_discipline,
  aes(
    x = as_date(article_date_month),
    y = delay, group = discipline,
    colour = discipline
  )
) +
  geom_smooth(
    show.legend = TRUE,
    na.rm = TRUE,
    span = 3,
    se = TRUE
  ) +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  ylim(
    0,
    400
  ) +
  labs(
    y = "Median Acceptance delay (days)",
    x = "Date", title = "Acceptance delay among disciplines"
  ) +
  theme_apa(base_family = "Times", base_size = 32)



ggsave(
  "discipline_plot.pdf",
  scale = 0.9,
  dpi = 200,
  width = 16,
  height = 9,
  units = "in"
)

open_access_plot <- ggplot(articles,
                           aes(
                             x = as_date(article_date),
                             y = acceptance_delay,
                             color = `open_access_status`
                           )
) +
  geom_smooth(
    show.legend = TRUE,
    na.rm = TRUE,
    span = 3,
    se = TRUE
  ) +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  ylim(
    0,
    400
  ) +
  labs(
    y = "Acceptance delay median (day)",
    x = "Date",
    title = "Acceptance delay based on Open Access status"
  ) +
  theme_apa(base_family = "Times", base_size = 20)



ggsave(
  "open_access_plot.pdf",
  scale = 0.9,
  dpi = 200,
  width = 16,
  height = 9,
  units = "in"
)

# psych and other disciplines plot
is_psychology_plot <- ggplot(
  acceptance_data_discipline,
  aes(
    x = as_date(article_date_month),
    y = delay
  )
) +
  geom_line(
    data = dplyr::filter(acceptance_data_discipline, is_psych == TRUE)
  ) +
  geom_line(
    data = dplyr::filter(acceptance_data_discipline, is_psych == FALSE),
    aes(
      x = as_date(article_date_month),
      y = delay,
      color = discipline
    )
  ) +
  geom_point() +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  ylim(
    0,
    400
  ) +
  labs(
    y = "Acceptance delay median (day)",
    x = "Date",
    title = "Acceptance delay in psychological and other journals"
  ) +
  theme_apa(base_family = "Times", base_size = 20)



ggsave(
  "is_psychology_plot.pdf",
  scale = 0.9,
  dpi = 200,
  width = 16,
  height = 9,
  units = "in"
)
all_mega_data <- articles |>
  dplyr::group_by(article_date_month, is_mega) |>
  dplyr::reframe(delay = median(acceptance_delay, na.rm = TRUE))
print(all_mega_data)

# mega journal plot
megajournal_plot <- ggplot(
  all_mega_data,
  aes(
    x = as_date(article_date_month),
    y = delay,
    color = is_mega
  )
) +
  geom_line(
  ) +
  geom_point() +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  ylim(
    0,
    400
  ) +
  labs(
    y = "Acceptance delay median (day)",
    x = "Date",
    title = "Acceptance delay in mega-journals"
  ) +
  theme_apa(base_family = "Times", base_size = 20)



ggsave(
  "megajournal_plot.pdf",
  scale = 0.9,
  dpi = 200,
  width = 16,
  height = 9,
  units = "in"
)
megajournal_data <- articles |>
  dplyr::filter(
    is_mega == TRUE
  ) |>
  dplyr::group_by(journal_title, article_date_month) |>
  dplyr::reframe(delay = median(acceptance_delay))

# only the megajorunals
megajournal_plot_2 <- ggplot(
  megajournal_data,
  aes(
    x = as_date(article_date_month),
    y = delay,
    color = journal_title
  )
) +
  geom_line(
  ) +
  geom_point() +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  ylim(
    0,
    400
  ) +
  labs(
    y = "Acceptance delay median (day)",
    x = "Date",
    title = "Acceptance delay in mega-journals"
  ) +
  theme_apa(base_family = "Times", base_size = 20)



ggsave(
  "megajournal_plot_2.pdf",
  scale = 0.9,
  dpi = 200,
  width = 16,
  height = 9,
  units = "in"
)
#Need: percentile plot
#Percentile plot:

percentile_function <- function(df) {
  # Return delay percentiles from df
  probs <- seq(
    0.05,
    1,
    0.025
  )
  dplyr::tibble(
    percentile = 100 * probs,
    delay = quantile(
      articles$acceptance_delay,
      probs = probs,
      na.rm = TRUE
    )
  )
}

acceptance_data_quantile <- acceptance_data %>%
  dplyr::group_by(
    delay,
    article_date_month
  ) |>
  dplyr::do(
    percentile_function(.)
  )
# Acceptance delay quantile plot
quantile_plot <- ggplot(
  acceptance_data_quantile,
  aes(
    x = as_date(article_date_month),
    y = delay
  )
) +
  geom_line(
    aes(
      y = delay,
      color = factor(percentile)
    )
  ) +
  scale_x_date(
    date_breaks = "1 years",
    date_labels = "%Y"
  ) +
  labs(
    y = "Elfogadási késés mediánja (nap)",
    x = "Dátum",
    title = "Elfogadási késés kvantilise"
  ) +
  theme_apa(base_family = "Times", base_size = 32)



ggsave(
  "quantile_plot.pdf",
  scale = 0.9,
  dpi = 200,
  width = 16,
  height = 9,
  units = "in"
)



megajournals_names <- c(
  "PLOS ONE", "Scientific Reports", "BMJ Open", "PeerJ",
  "Royal Society Open Science", "F1000Research", "GigaScience",
  "SAGE Open", "Heliyon", "SAGE Open Medicine",
  "SAGE Open Medical Case Reports", "SAGE Open Nursing",
  "SAGE Open Medicine", "SAGE Open Engineering", "Springer Plus,
  SAGE Open", "IEEE Access", "G3", "Biology Open", "Elementa,
  Science of the Antropoce", "AIP Advances", "Journal of Engineering",
  "Sustainability", "International Journal of Molecular Sciences",
  "Sensors", "Energies", "Molecules", "Science of the Total Environment",
  "eLife", "FEBS Open Bio", "mBio", "ACS Omega"
)

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
  ylim(40, 160) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "#BF616A", linewidth = 2)

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
  scale_color_manual(values = c("Covid" = viridis_pal(option = "viridis")(2)[2], "Nem Covid" = viridis_pal(option = "viridis")(2)[1])) +
  ylim(0, 160) +
  theme(legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 32),
        legend.position = c(0.3, 0.3))

ggsave('covid_acceptance_plot.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

publication_data <- articles |>
  dplyr::group_by(article_date) |>
  dplyr::reframe(delay=median(publication_delay, na.rm=T)) |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-12-01')) |>
  tidyr::drop_na(delay)

publication_plot <- ggplot(publication_data, aes(x = article_date, y = delay)) +
  geom_point(alpha = 1/5) +
  geom_hline(yintercept = 20, linetype = "dashed", color = "#BF616A", linewidth = 2) +
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
  ylim(0, 70) +
  theme(legend.key.size = unit(2, "cm"),
        legend.position = c(0.8, 0.8))


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
  theme_apa(base_family = "Times", base_size = 16) +
  labs(y = NULL, x = NULL, title = "Mega - folyóiratok") +
  theme(axis.title.x = element_blank())

ggsave('top_10_megajournals_plot.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

delays_megajournals_acceptance_delay <- top_10_megajournals_articles |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-12-01')) |>
  tidyr::drop_na(acceptance_delay) |>
  dplyr::filter(acceptance_delay > 0 & acceptance_delay < 150) |>
  ggplot(aes(x = acceptance_delay, y = reorder(journal_title, -acceptance_delay), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 0, panel_scaling = TRUE, quantile_lines = TRUE, quantiles = 2) +
  scale_x_continuous(expansion(c(0, 0))) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
  scale_fill_viridis_c(name = "Késés", option = "plasma") +
  theme_ridges(font_size = 12, grid = TRUE, font_family = "Times") +
  theme_apa(base_family = "Times", base_size = 24) +
  labs(title = "Elfogadási késés a mega - folyóiratokban", x = NULL, y = NULL) +
  theme(axis.title.x = element_blank())

ggsave('top_10_megajournal_acceptance_delay.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

delays_megajournals_publication_delay <- top_10_megajournals_articles |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-12-01')) |>
  tidyr::drop_na(publication_delay) |>
  dplyr::filter(publication_delay > 0 & publication_delay < 100) |>
  ggplot(aes(x = publication_delay, y = reorder(journal_title, -publication_delay), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 1., panel_scaling = TRUE, quantile_lines = TRUE, quantiles = 2) +
  scale_x_continuous(expansion(c(0, 0))) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
  scale_fill_viridis_c(name = "Késés", option = "plasma") +
  theme_ridges(font_size = 12, grid = TRUE, font_family = "Times") +
  theme_apa(base_family = "Times", base_size = 24) +
  labs(title = "Publikációs késés a mega - folyóiratokban", x = NULL, y = NULL) +
  theme(axis.title.x = element_blank())

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
  theme_apa(base_family = "Times", base_size = 16) +
  labs(title = "Top 10 h-index folyóirat", x = NULL, y = NULL) +
  theme(axis.title.x = element_blank())

ggsave('top_10_h_index_plot.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

delays_h_index_acceptance_delay <- top_h_index_articles |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-12-01')) |>
  tidyr::drop_na(acceptance_delay) |>
  dplyr::filter(acceptance_delay > 0 & acceptance_delay < 200)

delays_h_index_acceptance_delay_plot <- delays_h_index_acceptance_delay |>
  ggplot(aes(x = acceptance_delay, y = reorder(journal_title, -acceptance_delay, sum), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 1., panel_scaling = TRUE, quantile_lines = TRUE, quantiles = 2) +
  scale_x_continuous(expansion(c(0, 0))) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
  scale_fill_viridis_c(name = "Késés", option = "plasma") +
  theme_ridges(font_size = 12, grid = TRUE, font_family = "Times") +
  theme_apa(base_family = "Times", base_size = 16) +
  labs(title = "Elfogadási késés a top 10 h-index lapokban", x = NULL, y = NULL) +
  theme(axis.title.x = element_blank())


ggsave('top_10_h_index_acceptance_delay.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")

delays_h_index_publication_delay <- top_h_index_articles |>
  dplyr::filter(article_date >= lubridate::as_date('2016-01-01') & article_date <= lubridate::as_date('2022-12-01')) |>
  tidyr::drop_na(publication_delay) |>
  dplyr::filter(publication_delay > 0 & publication_delay < 100) |>
  ggplot(aes(x = publication_delay, y = reorder(journal_title, publication_delay), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 1., panel_scaling = TRUE, quantile_lines = TRUE, quantiles = 2) +
  scale_x_continuous(expansion(c(0, 0))) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.25))) +
  scale_fill_viridis_c(name = "Késés", option = "plasma") +
  theme_ridges(font_size = 12, grid = TRUE, font_family = "Times") +
  theme_apa(base_family = "Times", base_size = 16) +
  labs(title = "Publikációs késés a top 10 h-index lapokban", x = NULL, y = NULL) +
  theme(axis.title.x = element_blank())

ggsave('top_10_h_index_publication_delay.pdf', scale = 0.9, dpi = 200, width = 16, height = 9, units = "in")
