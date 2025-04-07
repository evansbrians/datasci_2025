
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
        c("DC", "MD", "VA")
    )

# [[-0.10]] Code formatting: If you provide three or more arguments to a
# function, place each argument on its own line.

# question 5 --------------------------------------------------------------

# How many caterpillars has Caterpillars Count counted? Please provide your
# answer as a one-value numeric vector.

observations$arthropod_quantity %>% 
  sum()

# 310267

# [[-0.50]] `$` is not among the functions that you may use for this
# assignment.

# [[-0.40]] Incorrect: This is all arthropods, you needed to subset to
# caterpillars.

# question 6 --------------------------------------------------------------

# Create a summary table that displays the number of surveys by year and 
# arrange from the highest to lowest number of surveys per year.

surveys %>%
  mutate(
    year_surv = year(date)
    ) %>% 
    summarize(
    n = n(),
    .by = year_surv
    ) %>% 
  arrange(desc(n))

# [[-0.10]] Code formatting: 
# * Include no more than one prefix function per line of code.
# * Indentation: Closing parentheses should be indented to the same level as
#   the start of the function.
# * Indentation of summarize is incorrect.

# question 7 --------------------------------------------------------------

# Subset the survey_locations data frame to records that are located within the
# District of Columbia, Maryland, or Virginia. For full credit, please complete
# this such that:

# * No columns are added to, or removed from, `survey_locations`
# * The `filter` function is not used to subset rows
# * The `select` function is not used to subset columns

survey_locations %>% 
  semi_join(sites_dmv, by = "site_id")

# question 8 --------------------------------------------------------------

# Please generate a summary table that provides the average (mean) number of
# caterpillars observed in "Beat sheet" and "Visual" surveys
# (`observation_method`).

observations %>% 
  left_join(surveys, by = "survey_id") %>% 
  subset(arthropod_quantity, observation_method)
  mean_counts = mean(arthropod_quantity) %>% 
  summarize(
    n = n(),
    .by = "mean_counts"
  )
  
# [[No points removed]] Incorrect: 
# * You needed to filter to "caterpillar" prior to joining the data.
# * A `full_join()` was necessary here to maintain observations with no 
#   caterpillars.
# * Your code will not run because there was a break after `subset()` and
#   your `mean_counts = ...` would need to be placed within a summarize.

# [[No points removed]]: `subset()` is not among the functions that you may use
# for this assignment.
  
# [[No points removed]] Code parsimony: When using `.by = ...` it is not 
# necessary to place your variable name in quotes.
  
# question 9 --------------------------------------------------------------

# Please generate a bar plot that displays the number of surveys conducted
# in the District of Columbia, Maryland, and Virginia in 2024. Plot the data
# such that:

# * Your x-aesthetic is region and is labeled "Region"
# * Your y-axis is labeled “Count”
# * The y-axis ranges from 0 to 2500
# * The plot includes a descriptive title

  surveys %>%
    left_join(
      survey_locations, 
      by = "branch_id" 
      ) %>%
    inner_join(
      sites_dmv, 
      by = "site_id"
      ) %>% 
    mutate(
      year_surv = year(date)
      ) %>% 
    filter(year_surv == "2024") %>% 
    summarize(
      n = n(),
      .by = region
      ) %>% 
    ggplot() + 
    aes(
      x = region,
      y = n
    ) + 
    geom_bar(stat = "identity") +
    labs(
      title = "Surveys conducted in DC, Maryland, and Virginia in 2024",
      x = "Region",
      y = "Count"
    ) +
    scale_y_continuous(
      limits = c(0, 2500),
      expand = c(0, 0)
    )
   
# [[-0.25]] Code parsimony:
# * It was not necessary to place year in quotes
# * It was not necessary to assign the year variable `year_surv`, this could
#   have been completed within the `filter()`.
  
# [[-0,25]] Code formatting: Closing parentheses should be indented to the same
# level as the start of the function.
  
# question 10 --------------------------------------------------------------

# Please generate a summary table that provides the total number of arthropods
# counted per state in the District of Columbia, Maryland, and Virginia in 2024.

  surveys %>%
    left_join(
      survey_locations, 
      by = "branch_id" 
    ) %>%
    inner_join(
      sites_dmv, 
      by = "site_id"
    ) %>% 
    mutate(year_surv = year(date)
    ) %>% 
    left_join(
      observations, 
      by = "survey_id"
    )%>% 
    filter(year_surv == "2024") %>% 
    summarize(
      n = n(),
      .by = region
    )

# [[-0.50]] Incorrect: `n()` is a row counter, thus this did not retrieve the
# sum of arthropods counted.
  
# [[-0.25]] Code parsimony:
# * It was not necessary to place year in quotes
# * It was not necessary to assign the year variable `year_surv`, this could
#   have been completed within the `filter()`.
  
# [[-0.25]] Code formatting:
# * If a function spans more than one line of code, the opening parentheses 
#   should be followed by a line break.
# * Include no more than one prefix function per line of code.
# * Infix functions should be separated from surrounding code with a single
#   leading and trailing space.
  
