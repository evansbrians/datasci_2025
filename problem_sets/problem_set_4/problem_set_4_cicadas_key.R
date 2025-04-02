
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
# * Subset to research grade observations (variable = `quality_grade`);
# * Change the variable name `scientific_name` to `species`
# * Remove the columns `city`, `state`, and `quality_grade`;
# * Globally assign the name cicada_research_quality to the resultant 
#   object.

cicada_research_quality <-
  cicada_list %>% 
  pluck("cicada_observations_2021") %>% 
  filter(
    state %in% 
      c(
        "DC", 
        "MD", 
        "VA"
      ),
    quality_grade == "research"
  ) %>% 
  select(
    date,
    species = scientific_name, 
    address
  )

# Or:

cicada_research_quality <- 
  cicada_list %>% 
  pluck("cicada_observations_2021") %>% 
  filter(
    state %>% 
      str_detect("MD|VA|DC"),
    quality_grade == "research"
  ) %>% 
  select(
    date,
    species = scientific_name,
    address
  )

# 5 -----------------------------------------------------------------------

# This study is focused on Brood X cicada. Without using `filter()` please:
  
# * Subset the data to Brood X species (*Magicicada cassini*, *Magicicada
#   septendecim*, and *Magicicada septendecula*);
# * Assign the name `brood_x_observations` to the resultant object.

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

# * The `address` variable contains "park" or "Park";
# * The `address` is *not* found in the vector assigned to the name
#   `not_parks`;
# * Is globally assigned to the name `brood_x_parks`.

brood_x_parks <- 
  brood_x_observations %>% 
  filter(
    str_detect(address, "[Pp]ark"),
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

