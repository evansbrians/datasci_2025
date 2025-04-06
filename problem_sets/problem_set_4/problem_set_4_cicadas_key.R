
# Script file for problem set 4: Cicada emergence

# question 1 --------------------------------------------------------------

# Before opening your script file for this problem set, change the name of
# `problem_set_4.R` to "problem_set_4_[last name]_[first name].R" using a snake
# case naming convention. *Note: You will submit this script file as your
# assignment*.

# question 2 --------------------------------------------------------------

# Open the script file in RStudio and attach the core tidyverse packages to your
# current R session.

library(tidyverse)

# 3 -----------------------------------------------------------------------

# Read in `data/raw/cicadas_2021.rds` and globally assign the list to the
# name cicada_list:

cicada_list <- 
  read_rds("data/raw/cicadas_2021.rds")

# 4 -----------------------------------------------------------------------

# In a single, chained analysis with no intermediate assignments:

# * Subset `cicada_list` to `cicada_observations_2021`
# * Subset to observations in Maryland, Virginia, and the District of 
#   Columbia ("MD", "VA", and "DC");
# * Subset to observations where data values are not `NA`;
# * Subset to research grade observations (variable = `quality_grade`);
# * Change the variable name `scientific_name` to `species`
# * Remove the columns `city`, `state`, and `quality_grade`;
# * Globally assign the resultant object to the name `cicada_research_quality`.

cicada_research_quality <- 
  cicada_list %>% 
  pluck("cicada_observations_2021") %>% 
  filter(
    state %>% 
      str_detect("MD|VA|DC"),
    !is.na(date),
    quality_grade == "research"
  ) %>% 
  select(
    date,
    species = scientific_name,
    address
  )

# 5 -----------------------------------------------------------------------

# This study is focused on the three species of Brood X cicada. Without using
# `filter()`, please:

# * Subset `cicada_research_quality` to Brood X species;
# * Globally assign the resultant object to the name `brood_x_observations`.

brood_x_observations <- 
  cicada_research_quality %>% 
  semi_join(
    cicada_list %>% 
      pluck("brood_x"),
    by = "species"
  )

# 6 -----------------------------------------------------------------------

# Unfortunately for us, "Park" is a common street name and there is a type of
# road in the area called a "parkway" (which may be abbreviated as "pkwy").
# Let's explore values in the `address` variable. Without using `filter()`,
# extract a vector of addresses from the data frame `brood_x_observations` 
# such that the extracted values:

# * Starts with the word "Park" or "park", *or*
# * Contain the strings "Parkway", "parkway", "Pkwy", or "pkwy";
# * Are globally assigned to the name `not_parks`.

not_parks <-
  brood_x_observations %>% 
  pull(address) %>% 
  keep(
    ~ str_detect(.x, "^[Pp]ark|[Pp](ar)?kwa?y")
  )

# 7 -----------------------------------------------------------------------

# Subset `brood_x_observations` such that:

# * The `address` variable contains "park", "Park", or "Zoo";
# * The `address` is *not* found in the vector assigned to the name
#   `not_parks`;
# * The resultant object is a tibble data frame that is globally assigned to 
#   the name `brood_x_parks`.

brood_x_parks <- 
  brood_x_observations %>% 
  filter(
    str_detect(address, "[Pp]ark|Zoo"),
    !address %in% not_parks
  )

# 8 -----------------------------------------------------------------------

# Using `brood_x_parks`, generate a summary table that:

# * Provides the number of observations of brood X cicada within parks where 
#   more than 40 cicadas have been observed;
# * Is arranged from the highest to lowest number of cicada observations;
# * Is globally assigned to the name `brood_x_park_summary`.

brood_x_park_summary <- 
  brood_x_parks %>% 
  filter(
    n() > 40,
    .by = address
  ) %>% 
  summarize(
    n = n(),
    .by = address
  ) %>% 
  arrange(
    desc(n)
  )

# 9 -----------------------------------------------------------------------

# We want to start placing our ARUs at the four locations with the greatest
# number of cicada observations and at the locations where brood X cicadas
# emerged the earliest. We will use a density plot to visualize the time of
# emergence. Using `brood_x_park_summary` and `brood_x_parks`, in a single 
# piped code block:

# * Subset `brood_x_parks` to the four parks with the greatest number of
#   cicada observations;
# * Subset the resultant object to the observations in April of 2021;
# * Generate a density plot, in which:
#   * The date variable is mapped to the x-axis;
#   * The fill color is mapped to `species`;
#   * The y-axis represents the statistical densities of the species 
#     occurrence, with each species on top of one another (i.e., stacked);
#   * Each park is placed within its own facet;
#   * All of the facets are positioned in a single column;
#   * The scale of the y-axis is determined by the range of density values 
#     within a given park and is displayed from 0 to a value of 0.05 above the
#     maximum density for that park;
#   * The scale of the x-axis does *not* vary by park;
#   * The color scale associated with `species` is determined using 
#     `scale_fill_brewer()`;
#   * Your plot is titled "Density distribution of Brood X cicada observations
#     by date"
#   * The x-axis is labeled as "Date";
#   * The y-axis is labeled "Density";
#   * The legend of your plot is labeled "Species";
#   * The background of the plot is not gray, does not contain grid lines, and
#     is bordered with a black line;
#   * All text in your plot is in Times New Roman;
#   * The plot title is in 14 pt font;
#   * The facet strip text and axis title is in 12 pt font;
#   * The axis labels are in 10 pt font

brood_x_parks %>% 
  semi_join(
    brood_x_park_summary %>% 
      slice_max(n, n = 4),
    by = "address"
  ) %>% 
  filter(
    str_detect(date, "2021-04")
  ) %>% 
  ggplot() +
  aes(
    x = date,
    fill = species,
  ) + 
  geom_density(position = "stack") +
  facet_wrap(
    ~ address,
    ncol = 1,
    scales = "free_y"
  ) +
  scale_y_continuous(
    expand =
      expansion(
        add = c(0, 0.05)
      )
  ) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Density distribution of Brood X cicada observations by date",
    x = "Date",
    y = "Density",
    fill = "Species"
  ) +
  theme(
    panel.background = 
      element_rect(
        color = "#000000",
        fill = "#ffffff"
      ),
    panel.grid = element_blank(),
    strip.background = element_rect(color = "black"),
    # axis.line = 
    #   element_line(
    #     color = "black",
    #     linewidth = 0.5
    #   ),
    text = element_text(family = "Times"),
    plot.title = element_text(size = 14),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12),
    # axis.text = element_text(size = 10),
  )

