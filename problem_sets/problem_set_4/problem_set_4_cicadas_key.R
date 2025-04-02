
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

# Using `brood_x_parks`, provide a unique vector of locations (variable =
# `address`) in which more than 40 cicadas have been observed:

brood_x_parks %>% 
  filter(
    n() > 40,
    .by = address
  ) %>% 
  pull(address) %>% 
  unique()

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
  geom_density(alpha = 0.8, position = "stack") +
  facet_wrap(
    ~ address,
    ncol = 1,
    scales = "free"
  ) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Density distribution of Brood X cicada emergence by date",
    x = "Date",
    y = "Density"
  ) +
  theme(
    panel.background = element_blank(),
    axis.line = 
      element_line(
        color = "black",
        linewidth = 0.5
      ),
    strip.background = element_rect(color = "black")
  )

