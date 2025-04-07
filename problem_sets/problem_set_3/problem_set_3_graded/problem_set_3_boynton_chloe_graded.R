
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
  filter(region == "DC" |
           region == "MD"|
           region == "VA")

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
  summarize(
    .by = date,
    year = year(date),
    n = n()
  ) %>%
  arrange(
    desc(year))

# [[-0.30]] Incorrect: Because `group_by()` was not an available function, you
# needed to mutate to calculate year.

# [[-0.20]] Incorrect: You needed to group by the year, not date.

# [[-0.10]] Incorrect: You were asked to arrange by descending number of
# surveys, not year.

# [[-0.10]] Code formatting: Indentation and hanging parentheses: If a function
# spans more than one line of code, closing parentheses should be placed on
# their own line and indented to the same level as the start of the function.

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
  select(survey_id, observation_method) %>%
  left_join(
    observations %>%
      select(arthropod, survey_id, arthropod_quantity),
    by = "survey_id"
  ) %>%
  filter(
    arthropod == "caterpillar",
    observation_method == "Beat sheet" |
      observation_method == "Visual") %>%
  summarize(
    mean =
      mean(arthropod_quantity, na.rm = TRUE),
    .by = observation_method,
  )

# [[No points removed]] Incorrect:
# * You needed to filter your observations to caterpillars prior to joining the
#   data frames;
# * You needed to use a `full_join()` to maintain surveys in which no
#   caterpillars were observed;

# [[No points removed]] `|` is not among the functions that you may use for this
# assignment.

# [[No points removed]] Code parsimony:
# * Because all surveys were beat sheet or visual, it was not necessary to 
#   subset to these values.
# * There were no `NA` values at the point of your `summarize()` function, thus
#   `na.rm = TRUE` did not modify the resultant object.

# [[No points removed]] Code formatting: 
# * If you provide three or more arguments to a function, place each argument 
#   on its own line.
# * If a function spans more than one line of code, closing parentheses should
#   be placed on their own line

# question 9 --------------------------------------------------------------

# Please generate a bar plot that displays the number of surveys conducted
# in the District of Columbia, Maryland, and Virginia in 2024. Plot the data
# such that:

# * Your x-aesthetic is region and is labeled "Region"
# * Your y-axis is labeled “Count”
# * The y-axis ranges from 0 to 2500
# * The plot includes a descriptive title

surveys2 <-
  surveys %>%
  left_join(
    survey_locations %>%
      select(branch_id, site_id),
    by = "branch_id"
  )

sites_dmv %>%
  inner_join(
    surveys2 %>%
      select(site_id,date),
    by = "site_id"
  ) %>%
  group_by(
    year = year(date),
    region
  ) %>%
  filter(
    year == "2024", na.rm = TRUE
  ) %>%
  summarize(
    n = n()
  ) %>%
  ggplot() +
  aes(x = region,
      y = n
  ) +
  theme_bw() +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    limits = c(0, 2500)
  ) +
  labs(
    title = "Number of surveys conducted in DC, Maryland & Virginia in 2024",
    x = "Region",
    y = "Count"
  )

# [[-0.25]] Incorrect: Your y-axis goes below 0 and above 2500.

# [[-1.25]] You were not asked to assign a name to the global environment in
# this question.

# [[-1.00]] `group_by()` and `theme_bw()` are not among the functions that you may use for this assignment.

# [[No points removed]] Code parsimony: 
# * It was not necessary necessary to quote 2024 in year
# * `na.rm = TRUE` did not modify the resultant object

# [[No points removed]] Code formatting: If you have more than one = in a
# function, place each argument on its own line.

# question 10 --------------------------------------------------------------

# Please generate a summary table that provides the total number of arthropods
# counted per state in the District of Columbia, Maryland, and Virginia in 2024.

surveys3 <-
  sites_dmv %>%
  left_join(
    surveys2 %>%
      select(survey_id, site_id),
    by = "site_id"
  )

observations %>%
  inner_join(
    surveys3 %>%
      select(survey_id,region),
    by = "survey_id"
  ) %>%
  summarize(
    .by = region,
    sum(arthropod_quantity)
  )

# [[-0.50]] Incorrect: Did not subset to 2024

# [[-1.25]] You were not asked to assign a name to the global environment in
# this question.
