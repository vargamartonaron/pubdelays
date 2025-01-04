library(cowplot)
library(ggdist)
library(readr)
library(ggplot2)
library(papaja)
library(dplyr)
library(tidyr)
library(viridis)
library(lubridate)

# Load data
articles <- readr::read_csv(
  "/users/usumusu/pubdelays/data/processed.csv"
)

megajournals <- c(
  "24701343", "21583226", "20466390", "20446055", "23251026",
  "22115463", "21601836", "21693536", "20513305", "21678359",
  "19326203", "20545703", "21582440", "20452322", "20566700",
  "23915447", "22991093", "24058440", "21508925", "2050084X",
  "20461402"
)

# Aggregate article_date variable to months as new variable
articles <- articles |>
  dplyr::mutate(
    article_date_month = lubridate::floor_date(as_date(article_date), "month")
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
    color = open_access
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
