
# Answer key for problem set 3: Counting caterpillars

# Score: 49.0% See comments below.

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
  filter(region %in%
           c("DC","MD","VA"
           )
  )

# [[-0.10]] Code formatting:
# * If a function spans more than one line of code, the opening parentheses 
#   should be followed by a line break.
# * If you provide three or more arguments to a function, place each argument
#   on its own line.
# * Commas should be followed by one trailing space.
# * Indentation: Closing parentheses should be indented to the same level as
#   the start of the function.

# question 5 --------------------------------------------------------------

# How many caterpillars has Caterpillars Count counted? Please provide your
# answer as a one-value numeric vector.

observations %>% 
  filter(arthropod == "caterpillar") %>% 
  pull(arthropod_quantity) %>% 
  sum()

# question 6 --------------------------------------------------------------

# Create a summary table that displays the number of surveys by year and 
# arrange from the highest to lowest number of surveys per year.

surveys %>% 
  mutate(year = year(date)) %>% 
  summarize(
    count = n(),
    .by = year
    ) %>% 
  arrange(
    desc(count)
    ) 

# [[-0.10]] Code formatting:
# * Include no more than one prefix function per line of code.
# * Indentation: Closing parentheses should be indented to the same level as
#   the start of the function.

# question 7 --------------------------------------------------------------

# Subset the survey_locations data frame to records that are located within the
# District of Columbia, Maryland, or Virginia. For full credit, please complete
# this such that:

# * No columns are added to, or removed from, `survey_locations`
# * The `filter` function is not used to subset rows
# * The `select` function is not used to subset columns

survey_locations %>% 
  semi_join(
    sites_dmv)

# [[-0.20]] Code formatting: If a function spans more than one line of code,
# closing parentheses should be placed on their own line.

# [[No points removed]]: It is best practice to specify the name of the
# variable that you are joining by.

# question 8 --------------------------------------------------------------

# Please generate a summary table that provides the average (mean) number of
# caterpillars observed in "Beat sheet" and "Visual" surveys
# (`observation_method`).

observations %>% 
  left_join(surveys) %>% 
  filter(arthropod == "caterpillar") %>% 
  mutate(
    arthropod_quantity =
      replace_na(arthropod_quantity, 0)
    ) %>% 
  summarize(
    count = n(),
    .by = observation_method) 

# I can't figure out how to find the mean of the two. The function is "mean()", but I messed up somewhere along the way.

# [[-1.50]] Incorrect:
# * Only a full join after filtering your observations table maintains surveys
#   where no caterpillars were counted. 
# * `n()` is a function for counting the number of rows. To calculate the mean,
#   you would have had to use `mean(arthropod_quantity)` instead of `n()`.

# [[-0.25]] Code parsimony: You conducted `replace_na()` on the
# `arthropod_quantity` variable, but there were no `NA` values present in the
# data at this point.

# [[-0.25]] Code formatting:
# * Indentation: Closing parentheses should be indented to the same level as
#   the start of the function.
# * If a function spans more than one line of code, closing parentheses 
#   should be placed on their own line.
# * Code and comments should not exceed 80 characters in width (if it is 
#   avoidable) – add a line break, if possible.

# question 9 --------------------------------------------------------------

# Please generate a bar plot that displays the number of surveys conducted
# in the District of Columbia, Maryland, and Virginia in 2024. Plot the data
# such that:

# * Your x-aesthetic is region and is labeled "Region"
# * Your y-axis is labeled “Count”
# * The y-axis ranges from 0 to 2500
# * The plot includes a descriptive title

sites_dmv %>% 
  semi_join(surveys)

# [[-2.50]] Incorrect: See key.

# question 10 --------------------------------------------------------------

# Please generate a summary table that provides the total number of arthropods
# counted per state in the District of Columbia, Maryland, and Virginia in 2024.

# [[-2.50]] Not answered: See key.
