
# setup -------------------------------------------------------------------

library(tidyverse)

# Read and pre-process data:

co2_df <- 
  read_csv(
    "data/row/co2_mm_mlo.csv", 
    skip = 40
  ) %>% 
  select(
    year:month, 
    co2 = average
  ) %>% 
  mutate(day = 1) %>% 
  unite(
    "date",
    c(year, month, day),
    sep = "-",
    remove = FALSE
  ) %>% 
  mutate(
    date = as_date(date)
  )

# plot the data -----------------------------------------------------------

co2_df %>% 
  filter(
    between(year, 1960, 2000),
    month %in%
      c(
        1:2, 
        6:8,
        12)
  ) %>% 
  mutate(
    season =
      if_else(
        month %in% 6:8,
        "Summer",
        "Winter"
      )
  ) %>% 
  ggplot() +
  aes(
    x = date, 
    y = co2, 
    color = Month
  ) +
  geom_point(size = 2.5) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~season) +
  theme_bw() +
  labs(
    title = 'Atmospheric carbon dioxide concentration, Mauna Loa: 1960-2020',
    x = 'Year',
    y = bquote('Monthly average '*CO[2]*' (ppm)')) +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 20),
    axis.title.x = element_text(vjust = -1.5),
    axis.title.y = element_text(vjust = 1.5),
    plot.title = 
      element_text(
        size = 28, 
        vjust = 2
      ),
    strip.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 14),
    plot.margin = 
      unit(
        c(
          0.75,
          0.35,
          0.75,
          0.35
        ), 
        "in"
      )
  )

# Save plot:

ggsave('co2.png', width = 13.33, height = 7.5)


