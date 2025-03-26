
# Answer key for problem set 3

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

# Read in the `caterpillars_count.rds` and assign the names of the list to the
# global environment.

read_rds("data/raw/caterpillars_count.rds") %>% 
  list2env(.GlobalEnv)

# question 4 --------------------------------------------------------------

# How many caterpillars has Caterpillars Count counted? Please provide your
# answer as a one-value numeric vector.

observations %>% 
  filter(arthropod == "caterpillar") %>% 
  pull(arthropod_quantity) %>% 
  sum()

# question 5 --------------------------------------------------------------

# Please make a summary data frame of the total number of sites in the District
# of Columbia, Maryland, and Virginia and arrange the table alphabetically by
# region.

sites %>% 
  filter(
    region %in%
      c(
        "DC",
        "MD", 
        "VA"
      )
  ) %>% 
  summarize(
    n = n(),
    .by = region
  ) %>% 
  arrange(region)

# question 6 --------------------------------------------------------------

# Subset the survey_locations data frame to those that are within sites in the
# District of Columbia, Maryland, or Virginia. For full credit, please complete
# this such that:

# * No columns are added to `survey_locqtions`;
# * No function is nested more than two levels deep;
# * The `select` function is not used to remove columns.

sites %>% 
  filter(
    region %in%
      c(
        "DC",
        "MD", 
        "VA"
      )
  ) %>% 
  semi_join(
    survey_locations,
    .,
    by = "site_name"
  )

# question 7 --------------------------------------------------------------

# Please make a summary data frame of the total number of surveys conducted in
# the District of Columbia, Maryland, and Virginia in 2024 and arrange the table
# from the highest to lowest number of surveys.

survey_locations %>% 
  inner_join(
    sites %>% 
      filter(
        region %in%
          c(
            "DC",
            "MD", 
            "VA"
          )
      ),
    by = "site_name"
  ) %>% 
  left_join(
    surveys,
    by = "survey_location"
  ) %>% 
  summarize(
    n = n(),
    .by = region
  ) %>% 
  arrange(
    desc(n)
  )

# question 8 --------------------------------------------------------------

# Please generate a summary table that provides the average (mean) number of
# caterpillars observed in "Beat sheet" and "Visual" surveys. Note that to
# properly address this question you will have to do something about those `NA`
# values!

observations %>% 
  filter(arthropod == "caterpillar") %>%
  select(survey_id, arthropod_quantity) %>% 
  full_join(
    surveys,
    by = "survey_id"
  ) %>% 
  mutate(
    arthropod_quantity = replace_na(arthropod_quantity, 0)
  ) %>% 
  summarize(
    average_caterpillars = mean(arthropod_quantity),
    .by = observation_method
  )

# Or, without mutate:

observations %>% 
  filter(arthropod == "caterpillar") %>% 
  select(survey_id, arthropod_quantity) %>% 
  full_join(
    surveys,
    by = "survey_id"
  ) %>% 
  summarize(
    average_caterpillars = 
      arthropod_quantity %>% 
      replace_na(0) %>% 
      mean(),
    .by = observation_method
  )

# question 9 --------------------------------------------------------------

# Please generate a summary table that provides the total number of arthropods
# counted per state in Maryland, DC, and Virginia in 2024.

observations %>% 
  select(survey_id, arthropod_quantity) %>% 
  inner_join(
    surveys %>% 
      filter(
        year(date) == 2024
      ) %>% 
      select(survey_id, survey_location),
    by = "survey_id"
  ) %>% 
  inner_join(
    survey_locations %>% 
      select(survey_location, site_name),
    by = "survey_location"
  ) %>% 
  inner_join(
    sites %>% 
      select(site_name, region) %>% 
      filter(
        region %in% 
          c(
            "MD",
            "DC",
            "VA"
          )
      ),
    by = "site_name"
  ) %>% 
  summarize(
    n_dmv = sum(arthropod_quantity),
    .by = region
  )

# question 10 -------------------------------------------------------------

