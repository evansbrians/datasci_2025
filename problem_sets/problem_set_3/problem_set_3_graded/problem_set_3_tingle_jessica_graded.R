
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
  list2env(.GlobalEnv)

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
  summarize(
    n = n(),
    .by = arthropod
  ) %>% 
  filter(arthropod == "caterpillar") %>% 
  pull(n)

# [[-0.50]] Incorrect: Because `n()` calculates the number of rows, and the
# count can be greater than one for each row, this did not return the total
# number of caterpillars.

# [[-0.10]] Code parsimony: The `summarize()` function was not necessary.
  
# question 6 --------------------------------------------------------------

# Create a summary table that displays the number of surveys by year and 
# arrange from the highest to lowest number of surveys per year.

surveys %>% 
  mutate(
    year = year(date)
  ) %>% 
  summarize(
    n = n(),
    .by = year
  ) %>% 
  arrange(
    desc(n)
  )

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
    by = "site_id"
  )

# question 8 --------------------------------------------------------------

# Please generate a summary table that provides the average (mean) number of
# caterpillars observed in "Beat sheet" and "Visual" surveys
# (`observation_method`).

# Note from JLT: I detected no NAs for arthropod quantity,
# and I do see plenty of 0s,
# so I don't think I'm accidentally excluding zero values in the code below

observations %>% 
  left_join(
    surveys %>% 
      select(survey_id, observation_method),
    by = "survey_id"
  ) %>%
  filter(arthropod == "caterpillar") %>% 
  summarize(
    mean_caterpillars = mean(arthropod_quantity),
    .by = observation_method
  )

# [[No points removed]] Incorrect: 
# * To maintain zeros, you would have needed to filter to "caterpillar" prior
#   to joining the data.
# * A `full_join()` was necessary here to maintain observations with no 
#   caterpillars.

# question 9 --------------------------------------------------------------

# Please generate a bar plot that displays the number of surveys conducted
# in the District of Columbia, Maryland, and Virginia in 2024. Plot the data
# such that:

# * Your x-aesthetic is region and is labeled "Region"
# * Your y-axis is labeled “Count”
# * The y-axis ranges from 0 to 2500
# * The plot includes a descriptive title

# Subset surveys to 2024, keeping only the necessary columns

surveys %>% 
  select(branch_id, date) %>% 
  filter(
    year(date) == 2024
    ) %>% 
  
  # Use a join to add column with survey regions for DC, MD, and VA
  
  inner_join(
    
    # Generate a data frame of survey locations in DC, MD, and VA
    # by joining survey_locations, which shares a key with surveys (branch_id),
    # with sites_dmv, which does not share a key with surveys
    
    survey_locations %>% 
      select(branch_id, site_id) %>% 
      inner_join(
        sites_dmv %>% 
          select(site_id, region),
        by = "site_id"
      ),
    by = "branch_id"
  ) %>% 
  
  # Summarize to get the number of surveys for each region
  
  summarize(
    n = n(),
    .by = region
  ) %>% 
  
  # Plot
  
  ggplot() +
  aes(
    x = region,
    y = n
  ) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    limits = c(0, 2500),
    expand = c(0, 0)
  ) +
  labs(
    title = "Number of arthropod surveys in 2024",
    x = "Region",
    y = "Count"
  )

# Virginia for the win!  :)

# [[-0.25]] Code formatting: Closing parentheses should be indented to the same
# level as the start of the function.

# question 10 --------------------------------------------------------------

# Please generate a summary table that provides the total number of arthropods
# counted per state in the District of Columbia, Maryland, and Virginia in 2024.

# Subset surveys to 2024, ditching the unnecessary columns

surveys %>% 
  select(!observation_method) %>% 
  filter(
    year(date) == 2024
  ) %>% 
  
  # Use a join to add column with survey regions for DC, MD, and VA
  
  inner_join(
    
    # Generate a data frame of survey locations in DC, MD, and VA
    # by joining survey_locations, which shares a key with surveys (branch_id),
    # with sites_dmv, which does not share a key with surveys
    
    survey_locations %>% 
      select(branch_id, site_id) %>% 
      inner_join(
        sites_dmv %>% 
          select(site_id, region),
        by = "site_id"
      ),
    by = "branch_id"
  ) %>% 
  
  # Keep only the survey_id and region columns,
  # join them with relevant columns of observations
  
  select(survey_id, region) %>% 
  
  inner_join(
    observations %>% 
      select(survey_id, arthropod_quantity),
    by = "survey_id"
  ) %>% 
  
  # Create summary table of the number of arthropods counted per region
  
  summarize(
    total_arthropods = 
      sum(arthropod_quantity),
    .by = region
  )

# They who conduct the most surveys count the most arthropods!

# [[-0.25]] Code formatting: Code within a single code block should not be
# separated by blank lines unless it is separated by a comment