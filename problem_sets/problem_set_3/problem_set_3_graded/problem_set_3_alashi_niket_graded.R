
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
    region == "DC" | 
      region == "MD" |
      region == "VA"
  )

# [[-0.50]] `|` is not among the functions that you may use for this
# assignment.

# [[-0.10]] Code parsimony: This could have been more simply completed using 
# the `%in%` function.

# question 5 --------------------------------------------------------------

# How many caterpillars has Caterpillars Count counted? Please provide your
# answer as a one-value numeric vector.

# [[-1.0]] Incorrect (not answered)

# question 6 --------------------------------------------------------------

# Create a summary table that displays the number of surveys by year and 
# arrange from the highest to lowest number of surveys per year.

surveys %>% 
  group_by(
    year = year(date)
  ) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n))

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
    by = 
      c("site_id")
  )

# [[-0.20]] Code parsimony: When supplying a single value, it is not necessary
# to wrap that value inside of `c()`. The `c` function combines multiple values
# with a vector.

# question 8 --------------------------------------------------------------

# Please generate a summary table that provides the average (mean) number of
# caterpillars observed in "Beat sheet" and "Visual" surveys
# (`observation_method`).

surveys %>% 
  filter(observation_method %in% c("Beat sheet", "Visual")) %>% 
  count(observation_method) %>%  
  group_by(observation_method) %>%
  summarize(mean = mean(n))

# [[No points removed] Incorrect:
# * Did not filter to caterpillars
# * A join was necessary here

# [[No points removed]] `count()` and `group_by()` are not among the functions
# that you may use for this assignment.

# [[No points removed]] Code parsimony:
# * Because all surveys were beat sheet or visual, it was not necessary to 
#   subset to these values.
# * It was not necessary to use `count()` 

# [[No points remove]] Code formatting: Include no more than one prefix function
# per line of code.

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
    by = 
      c("branch_id")
  ) %>% 
  inner_join(
    sites_dmv,
    by = "site_id") %>%
  
  #generate box plot
  
  mutate(date = as.Date(date)) %>%
  filter(year(date) == 2024) %>%
  ggplot(aes(x = region, fill = region)) +
  geom_bar() +
  labs(
    title = "Number of Surveys Conducted in DC, Maryland, and Virginia (2024)",
    x = "Region",
    y = "Count") +
  scale_y_continuous(limits = c(0, 2500))

# [[-0.25]] Incorrect: Your y-axis goes below 0 and above 2500.

# [[-1.25]] `as.Date()` is not among the functions that you may use for this
# assignment.

# [[-0.25]] Code parsimony:
# * `date` was already a date object, so `as.Date()` was not necessary.
# * When supplying a single value, it is not necessary to wrap that value 
#   inside of `c()`. The `c` function combines multiple values with a vector.

# [[-0.25]] Code formatting:
# * Include no more than one prefix function per line of code.
# * Add a single space between the hashtag (#) and comment.
# * Indentation and hanging parentheses: If a function spans more than one line
#   of code, closing parentheses should be placed on their own line and 
#   indented to the same level as the start of the function.
# * If you have more than one = in a function, place each argument on its own
#   line.

# question 10 --------------------------------------------------------------

# Please generate a summary table that provides the total number of arthropods
# counted per state in the District of Columbia, Maryland, and Virginia in 2024.

surveys %>% 
  left_join(
    survey_locations,
    by = 
      c("branch_id")
  ) %>% 
  inner_join(
    sites_dmv,
    by = "site_id") %>% 
  group_by(region,
           year = 2024
  ) %>% 
  summarise(n = n())

# [[-1.50]] Incorrect:
# * You needed to use the observations object to obtain the number of 
#   arthropods counted.
# * You did not subset to the year 2024, instead you created a new variable
#   called year and set the values of that variable to 2024.
# * `n()` is a row counter, thus this did not retrieve the sum of arthropods
#   counted.

# [[-1.00]] `group_by()` is not among the functions that you may use for this
# assignment.

# [[No points removed]] Code parsimony: When supplying a single value, it is not
# necessary to wrap that value inside of `c()`. The `c` function combines
# multiple values with a vector.

# [[No points removed]] Code formatting:
# * Indentation and hanging parentheses: If a function spans more than one line
#   of code, closing parentheses should be placed on their own line and 
#   indented to the same level as the start of the function.
# * If a function spans more than one line of code, the opening parentheses 
#   should be followed by a line break.
