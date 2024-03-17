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
articles <- readr::read_tsv(
  "/home/martonaronvarga/GitHub/pubdelays/Data/journal_articles_sliced_200k.tsv"
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
    article_date_month = lubridate::floor_date(as_date(article_date), "month"),
    open_access_status = case_when(
      `does_the_journal_comply_to_doaj's_definition_of_open_access` == "Yes" ~ TRUE,
      `does_the_journal_comply_to_doaj's_definition_of_open_access` == "No" ~ FALSE,
      is.na(`does_the_journal_comply_to_doaj's_definition_of_open_access`) == TRUE ~ FALSE
    ),
    is_mega = case_when(
      issn_linking %in% megajournals ~ TRUE,
      TRUE ~ FALSE
    ),
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
  dplyr::filter(
    article_date_month >= lubridate::as_date("2016-01-01") &
      article_date_month <= lubridate::as_date("2023-12-01")
  ) |>
  tidyr::drop_na(delay) |>
  dplyr::filter(delay <= 700)

acceptance_data_discipline <- articles |>
  dplyr::group_by(
    article_date_month,
    discipline
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
      article_date_month <= lubridate::as_date("2023-12-01")
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
  ylim(
    0,
    400
  ) +
  labs(
    y = "Elfogadási késés mediánja (nap)",
    x = "Dátum", title = "Elfogadási késés"
  )

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
    y = "Elfogadási késés mediánja (nap)",
    x = "Dátum", title = "Elfogadási késés"
  )

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
  )

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
  )

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
    y = "Elfogadási késés mediánja (nap)",
    x = "Dátum",
    title = "Elfogadási késés Open Access státusz szerint"
  )

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
  articles,
  aes(
    x = as_date(article_date),
    y = acceptance_delay,
    # only display where is_psych is TRUE
    color = is_psych == TRUE
  )
) +
  geom_smooth(
    show.legend = TRUE,
    na.rm = TRUE,
    span = 3,
    se = TRUE
  ) +
  geom_smooth(
    data = dplyr::filter(articles, is_psych == FALSE),
    aes(
      x = as_date(article_date),
      y = acceptance_delay,
      color = discipline
    ),
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
    y = "Elfogadási késés mediánja (nap)",
    x = "Dátum",
    title = "Elfogadási késés Pszichológiai és nem pszichológiai folyóiratokban"
  )

ggsave(
  "is_psychology_plot.pdf",
  scale = 0.9,
  dpi = 200,
  width = 16,
  height = 9,
  units = "in"
)

# mega journal plot
megjournal_plot <- ggplot(
  articles,
  aes(
    x = as_date(article_date),
    y = acceptance_delay,
    # only display where is_mega is TRUE
    color = is_mega == TRUE
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
    y = "Elfogadási késés mediánja (nap)",
    x = "Dátum",
    title = "Elfogadási késés megajournalokban"
  )

ggsave(
  "megjournal_plot.pdf",
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
