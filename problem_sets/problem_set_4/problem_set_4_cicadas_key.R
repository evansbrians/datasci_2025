
# Script file for problem set 4: Cicada emergence

# question 1 --------------------------------------------------------------

# Before opening your script file for this problem set, change the name of
# the `problem_set_4.R` to "problem_set_4_[last name]_[first name].R" using
# a snake case naming convention. *Note: You will submit this script file 
# as your assignment*.

# question 2 --------------------------------------------------------------

# Open the script file in RStudio and attach the tidyverse metapackage to 
# your current R session.

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
# * Globally assign the name cicada_research_quality to the resultant 
#   object.

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
# `filter()` please:

# * Subset the `cicada_research_quality` to Brood X species;
# * Globally assign the name `brood_x_observations` to the resultant object.

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

# Subset brood_x to such that the resultant object:

# * The `address` variable contains "park", "Park", or "Zoo";
# * The `address` is *not* found in the vector assigned to the name
#   `not_parks`;
# * Is globally assigned to the name `brood_x_parks`.

brood_x_parks <- 
  brood_x_observations %>% 
  filter(
    str_detect(address, "[Pp]ark|Zoo"),
    !address %in% not_parks
  )

# 8 -----------------------------------------------------------------------

# Using `brood_x_parks`, provide a summary table that:

# * Provides the number of observations of brood X cicada within parks where 
#   more than 40 cicadas have been observed
# * Is arranged from the highest to lowest number of cicada observations
# * Is globally assigned to the name brood_x_park_summary

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
# emergence. Use brood_x_park summary to subset brood_x_parks to the
# observations at the four parks with the greatest number of cicada
# observations. Then generate a density plot in which:

# * Your plot is titled "Density distribution of Brood X cicada emergence by
#   date"
# * The date variable is mapped to the x axis and labeled as "Date"
# * The y-axis is density and is labeled "Density"
# * The legend of your plot is labeled "Species"
# * The fill color mapped to species using `scale_fill_brewer()`
# * Each park is placed on its own facet
# * All of the facets are positioned in a single column
# * The scale of the y-axis is determined by the range of density values within
#   a given park
# * The densities of the species are displayed on top of one another (i.e., 
#   stacked)
# * The background of the plot is not gray and does not contain grid lines.
# * The x and y axis are colored black and 0.5 mm wide (see `?element_line`)
# * All text in your plot is in Times New Roman

brood_x_parks %>% 
  semi_join(
    brood_x_park_summary %>% 
      slice_max(n, n = 4),
    by = "address"
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
    scales = "free"
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(
    expand = expansion(
      add = c(0, 0.05)
      )
  ) +
  labs(
    title = "Density distribution of Brood X cicada emergence by date",
    x = "Date",
    y = "Density",
    fill = "Species"
  ) +
  theme(
    panel.background = 
      element_rect(
        color = "#000000",
        fill = "#ffffff"),
    panel.grid = element_blank(),
    axis.line = 
      element_line(
        color = "black",
        linewidth = 0.5
      ),
    text = element_text(family = "Times")
  )

