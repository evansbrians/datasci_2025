
# Answer key for problem set 3: Counting caterpillars

# question 1 --------------------------------------------------------------

# Before opening your script file for this problem set, change the name of
# the `problem_set_3.R` to "problem_set_3_[last name]_[first name].R" using
# a snake case naming convention. *Note: You will submit this script file 
# as your assignment*.

# question 2 --------------------------------------------------------------

# Open the script file in RStudio and attach the tidyverse metapackage to 
# your current R session.

library(tidyverse)

# question 3 --------------------------------------------------------------

# Read in the file `caterpillars_count.rds` and assign the names of the list
# items to the global environment.

read_rds("data/raw/caterpillars_count.rds") %>% 
  list2env(envir = .GlobalEnv)

# question 4 --------------------------------------------------------------

# Subset the `sites` data frame to where `region` is `DC` (District of
# Columbia), `MD` (Maryland), or `VA` (Virginia and globally assign the name
# `sites_dmv` to the resultant object.

sites_dmv <- 
  sites %>% 
  filter(region == "VA")

# [[-0.75]] Incorrect: This only subset the data to "VA" (see key).

# question 5 --------------------------------------------------------------

# How many caterpillars has Caterpillars Count counted? Please provide your
# answer as a one-value numeric vector.

observations %>% 
  filter(arthropod == "caterpillar") %>% 
  summarize(n = n()) %>% 
  pull()

# [[-0.50]] Incorrect: Because `n()` calculates the number of rows, and the
# count can be greater than one for each row, this did not return the total
# number of caterpillars.

# [[-0.10]] Code parsimony: The `summarize()` function was not necessary.

# question 6 --------------------------------------------------------------

# Create a summary table that displays the number of surveys by year and 
# arrange from the highest to lowest number of surveys per year.

surveys %>% 
  group_by(
    year = year(date)
  ) %>% 
  summarize(
    n = n()
  ) %>% 
  arrange(
    desc(n)
  )

# [[-0.50]] `group_by()` is not among the functions that you may use for this
# assignment.

# question 7 --------------------------------------------------------------

# Subset the survey_locations data frame to records that are located within the
# District of Columbia, Maryland, or Virginia. For full credit, please complete
# this such that:

# * No columns are added to, or removed from, `survey_locations`
# * The `filter` function is not used to subset rows
# * The `select` function is not used to subset columns

survey_locations %>% 
  inner_join(
    sites_dmv,
    by = "site_id"
  )

# [[-1.0]] Incorrect: This should have been a `semi_join()` because
# `inner_join()` adds columns to the resultant object.

# question 8 --------------------------------------------------------------

# Please generate a summary table that provides the average (mean) number of
# caterpillars observed in "Beat sheet" and "Visual" surveys
# (`observation_method`).

# [[No points removed]] Not answered.

# question 9 --------------------------------------------------------------

# Please generate a bar plot that displays the number of surveys conducted
# in the District of Columbia, Maryland, and Virginia in 2024. Plot the data
# such that:

# * Your x-aesthetic is region and is labeled "Region"
# * Your y-axis is labeled “Count”
# * The y-axis ranges from 0 to 2500
# * The plot includes a descriptive title

# [[-2.50]] Not answered.

# question 10 --------------------------------------------------------------

# Please generate a summary table that provides the total number of arthropods
# counted per state in the District of Columbia, Maryland, and Virginia in 2024.

surveys %>% 
  filter(year(date) == 2024)

# [[-2.0]] Some points for filter, but see key.

# [[-0.25]] Code formatting: Include no more than one prefix function per line
# of code.
