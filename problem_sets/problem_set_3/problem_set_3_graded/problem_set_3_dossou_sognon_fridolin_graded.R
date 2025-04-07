
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

# Read in the file `caterpillars_count.rds`s and assign the names of the list
# items to the global environment.

read_rds("data/raw/caterpillars_count.rds") %>% 
  list2env(envir = .GlobalEnv)

# question 4 --------------------------------------------------------------

# Subset the `sites` data frame to where `region` is `DC` (District of
# Columbia), `MD` (Maryland), or `VA` (Virginia and globally assign the name
# `sites_dmv` to the resultant object.

sites_dmv <- 
  sites %>% 
  filter(
    region %in% 
      c(
        "DC", 
        "MD", 
        "VA"
      )
  )

# question 5 --------------------------------------------------------------

# How many caterpillars has Caterpillars Count counted? Please provide your
# answer as a one-value numeric vector.

observations %>% 
  filter(arthropod == "caterpillar") %>% 
  select(arthropod_quantity) %>% 
  sum()

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
  semi_join(
    sites_dmv, 
    by = 
      c("site_id" = "site_id")
  )

# [[-0.20]] Code parsimony: It is not necessary to use `c("site_id" =
# "site_id")`, unless the two key columns names are different.

# question 8 --------------------------------------------------------------

# Please generate a summary table that provides the average (mean) number of
# caterpillars observed in "Beat sheet" and "Visual" surveys
# (`observation_method`).

observations %>% 
  filter(arthropod == "caterpillar") %>% 
  select(survey_id:arthropod_quantity) %>% 
  left_join(
    surveys %>% 
      select(survey_id, observation_method), 
    by = "survey_id"
  ) %>% 
  group_by(observation_method) %>% 
  summarize(
    mean = mean(arthropod_quantity)
  )

# [[No points removed]] Incorrect: A `full_join()` was necessary here to
# maintain observations with no caterpillars.

# [[No points removed]] `group_by()` is not among the functions that you may use
# for this assignment.

# question 9 --------------------------------------------------------------

# Please generate a bar plot that displays the number of surveys conducted
# in the District of Columbia, Maryland, and Virginia in 2024. Plot the data
# such that:

# * Your x-aesthetic is region and is labeled "Region"
# * Your y-axis is labeled “Count”
# * The y-axis ranges from 0 to 2500
# * The plot includes a descriptive title

# subsetting the surveys data frame to surveys conducted in 2024 
# (the target table)

surveys %>%
  filter(
    year(date) == "2024"
  ) %>% 
  
  # adding the "site_id" column from the source table survey_locations
  
  left_join(
    survey_locations %>% 
      select(branch_id:site_id), 
    by = "branch_id"
  ) %>% 
  
  # adding the "region" column from the source table sites_dmv  
  
  inner_join(
    sites_dmv %>% 
      select(site_id, region), 
    by = "site_id"
  ) %>% 
  
  # plotting the number of surveys conducted in the region "DC", "MD" and "VA" 
  # in 2024
  
  ggplot() + 
  aes(x = region) + 
  geom_bar() + 
  scale_y_continuous(
    limits = c(0, 2500)
  ) + 
  labs(
    x = "Region", 
    y = "Count", 
    title = "Number of surveys conducted in the District of Columbia, Maryland, 
    and Virginia in 2024"
  )

# [[-0.25]] Incorrect: Your y-axis goes below 0 and above 2500.

# [[-0.25]] Code parsimony: It was not necessary necessary to quote
# 2024 in year.

# question 10 --------------------------------------------------------------

# Please generate a summary table that provides the total number of arthropods
# counted per state in the District of Columbia, Maryland, and Virginia in 2024.

# selecting the columns of interest in observations data frame (target table)

observations %>% 
  select(survey_id, arthropod_quantity) %>% 
  
  # subsetting the surveys data frame to surveys conducted in 2024 
  # and adding the date and the branch_id columns from the surveys source table
  
  inner_join(
    surveys %>%
      filter(
        year(date) == "2024"
      ) %>% 
      select(survey_id:date), 
    by = "survey_id"
  ) %>% 
  
  # adding the "site_id" column from the source table survey_locations
  
  left_join(
    survey_locations %>% 
      select(branch_id:site_id), 
    by = "branch_id"
  ) %>%
  
  # adding the "region" column from the source table sites_dmv  
  
  inner_join(
    sites_dmv %>% 
      select(site_id, region), 
    by = "site_id"
  ) %>% 
  
  # generating a summary table that provides the total number of arthropods
  # state in the District of Columbia, Maryland, and Virginia in 2024
  
  group_by(region) %>% 
  summarize(
    number_of_arthropods = sum(arthropod_quantity)
  )

# [[-1.25]] `group_by()` is not among the functions that you may use for this
# assignment.

# [[-0.25]] Code parsimony: It was not necessary necessary to quote 2024 in
# year.
