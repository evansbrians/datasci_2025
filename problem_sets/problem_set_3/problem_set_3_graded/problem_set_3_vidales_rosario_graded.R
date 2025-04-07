
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
    region == 'DC'|
      region == 'MD'|
      region == 'VA'
  )

# [[-0.50]] `|` is not among the functions that you may use for this
# assignment.

# [[-0.10]] Code parsimony: This could have been more simply completed using 
# the `%in%` function.
  
# question 5 --------------------------------------------------------------

# How many caterpillars has Caterpillars Count counted? Please provide your
# answer as a one-value numeric vector.

observations %>%
  filter(arthropod == "caterpillar") %>% 
  summarize(n = n())

# [[-0.50]] Incorrect: Because `n()` calculates the number of rows, and the count
# can be greater than one for each row, this did not return the total number of
# caterpillars.

# [[-0.10]] Incorrect: The answer was not provided as a one-value numeric
# vector.

# [[-0.10]] Code parsimony: The `summarize()` function was not necessary.

# question 6 --------------------------------------------------------------

# Create a summary table that displays the number of surveys by year and 
# arrange from the highest to lowest number of surveys per year.

surveys %>% 
  group_by(year(date)) %>% 
  summarize(n = n()) %>% 
  arrange(
    desc(n)
  ) 

# [[-0.50]] `group_by()` is not among the functions that you may use for this
# assignment.

# [[-0.10]] Code formatting: Include no more than one prefix function per line
# of code.

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

surveys %>% 
  left_join(
    observations,
    by = "survey_id"
  ) %>% 
  filter (
    arthropod == "caterpillar"|
      arthropod == "none"
  ) %>% 
  group_by(observation_method) %>% 
  summarize(
    mean_caterpillars_obs =
      mean(arthropod_quantity)
  )

# [[No points removed]] Incorrect: 
# * To maintain zeros, you would have needed to filter to "caterpillar" prior
#   to joining the data.
# * A `full_join()` was necessary here to maintain observations with no 
#   caterpillars.

# [[No points removed]] `group_by()` and `|` are not among the functions that
# you may use for this assignment.

# [[No points removed]] Code formatting:
# * Infix functions should be separated from surrounding code with a single
#   leading and trailing space.
# * Parentheses, curly braces, and square bracket operators should not be
#   preceded or followed by a space.

# question 9 --------------------------------------------------------------

# Please generate a bar plot that displays the number of surveys conducted
# in the District of Columbia, Maryland, and Virginia in 2024. Plot the data
# such that:

# * Your x-aesthetic is region and is labeled "Region"
# * Your y-axis is labeled “Count”
# * The y-axis ranges from 0 to 2500
# * The plot includes a descriptive title

surveys %>% 
  
  # Subset to observations in 2024
  
  filter(
    year(date) == 2024
  ) %>% 
  
  # Select survey_id and branch_id (foreign key to survey_locations)
  
  select(survey_id, branch_id) %>% 
  
  # Join to df with foreign key for sites_dmv (site_id)
  # Maintain matching rows only using inner_join
  
  inner_join(
    survey_locations %>% 
      
      # Select primary and foreign (site_id) key columns only
      
      select(branch_id, site_id),
    by = "branch_id"
  ) %>% 
  
  # Join to sites_dmv, maintain matching rows only
  
  inner_join(
    sites_dmv %>% 
      
      # Select site_id and region columns only
      
      select(site_id, region),
    by = "site_id"
  ) %>% 
  
  # Select only region and site_id column
  
  select(region, survey_id) %>% 
  
  # Count the amount of surveys per region, name value "survey_count"
  
  summarize(
    survey_count = n(),
    .by = region
  ) %>% 
  
  # Plot
  
  ggplot(
    aes(
      x = region,
      y = survey_count
    )
  ) +
  geom_bar(
    stat = "identity"
  ) +
  scale_y_continuous(
    limits = c(0, 2500)
  ) +
  labs(
    title = "Number of surveys by region",
    x = "Region",
    y = "Count"
  ) +
  theme_bw()

# [[-0.25]] Incorrect: Your y-axis goes below 0 and above 2500.

# [[-1.25]] `theme_bw()` is not among the functions that you may use for this
# assignment.
  
# question 10 --------------------------------------------------------------

# Please generate a summary table that provides the total number of arthropods
# counted per state in the District of Columbia, Maryland, and Virginia in 2024.

# observations has variable of interest arthropod_quantity 
# select arthropod_quantity and survey_id (foreign key to surveys)

observations %>% 
  select(arthropod_quantity, survey_id) %>%
  
  # Use date from surveys to subset to observations in 2024
  # Select survey_id and branch_id (foreign key to survey_locations)
  # Join to observations by survey_id
  
  inner_join(
    surveys %>%   
      filter(
        year(date) == 2024) %>% 
      select(survey_id, branch_id),
    by = "survey_id"
  ) %>%
  
  # Select branch_id and site_id (foreign key to sites_dmv)
  # Join to surveys by branch_id
  
  inner_join(
    survey_locations %>% 
      select(branch_id, site_id),
    by =  "branch_id"
  ) %>%
  
  # Select site_id and region (to summarize count data)
  # Join to survey_locations by site_id
  
  inner_join(
    sites_dmv %>% 
      select(site_id, region),
    by = "site_id"
  ) %>% 
  
  # Keep only region and arthropod quantity from the joins
  
  select(region, arthropod_quantity) %>% 
  
  # Summarize arthropod count by region
  
  summarize(
    arthropods_counted = n(),
    .by = region
  )

# [[-0.50]] Incorrect: `n()` is a row counter, thus this did not retrieve the
# sum of arthropods counted.

# [[-0.25]] Code formatting: Indentation and hanging parentheses: If a function
# spans more than one line of code, closing parentheses should be placed on
# their own line and indented to the same level as the start of the function.