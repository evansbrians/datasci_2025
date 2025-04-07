
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
# Columbia), `MD` (Maryland), or `VA` (Virginia) and globally assign the name
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

observations %>% 
  filter(arthropod == "caterpillar") %>% 
  summarise(
    n = n()
  ) %>% 
  sum()

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
  summarise(
    n = n()
  ) %>% 
  arrange(n)

# [[-0.50]] `group_by()` is not among the functions that you may use for this
# assignment.

# [[-0.10]] Incorrect: You were asked to arrange by descending number of
# surveys.

# [[-0.10]] Code formatting: Maintain one blank line between code blocks and
# comments. In your version there were additional spaces prior to the section
# header.

# question 7 --------------------------------------------------------------

# Subset the survey_locations data frame to records that are located within the
# District of Columbia, Maryland, or Virginia. For full credit, please complete
# this such that:

# * No columns are added to, or removed from, `survey_locations`
# * The `filter` function is not used to subset rows
# * The `select` function is not used to subset columns

sites_filter<-
  sites %>% 
  filter(
    region == "DC" |
      region == "MD" |
      region == "VA"  
  ) %>% 
  pull(site_id )


survey_locations %>% 
  .[.$site_id %in% sites_filter,]

# [[-1.0]] You were not asked to a assign a name to the global environment in
# this question.

# [[-1.0]] `[...]`, `$`, and `|` are not among the functions that you may use
# for this assignment.

# [[No points removed]] Code formatting:
# * Infix functions should be separated from surrounding code with a single
#   leading and trailing space.
# * Parentheses should not be preceded or followed by a space.
# * Maintain one blank line between code blocks and comments.
# * Commas should be followed by one trailing space.

# question 8 --------------------------------------------------------------

# Please generate a summary table that provides the average (mean) number of
# caterpillars observed in "Beat sheet" and "Visual" surveys
# (`observation_method`).

observations %>% 
  left_join(
    surveys %>% 
      select(survey_id , observation_method), 
    by = "survey_id"
  ) %>% 
  filter(
    arthropod == "caterpillar"
  ) %>%
  group_by(observation_method) %>%
  summarize(mean = mean(arthropod_quantity))

# [[No points removed]] Incorrect: 
# * To maintain zeros, you would have needed to filter to "caterpillar" prior
#   to joining the data.
# * A `full_join()` was necessary here to maintain observations with no 
#   caterpillars.

# [[No points removed]] `group_by()` is not among the functions that you may use
# for this assignment.

# [[No points removed]] Code formatting:
# * Commas should be followed by one trailing space, but not a leading space.
# * Include no more than one prefix function per line of code.
# * Maintain one blank line between code blocks and comments. In your version
#   there were additional spaces prior to the section header.

# question 9 --------------------------------------------------------------

# Please generate a bar plot that displays the number of surveys conducted
# in the District of Columbia, Maryland, and Virginia in 2024. Plot the data
# such that:

# * Your x-aesthetic is region and is labeled "Region"
# * Your y-axis is labeled “Count”
# * The y-axis ranges from 0 to 2500
# * The plot includes a descriptive title

surveys %>%
  
  # Filter surveys conducted in 2024
  
  filter(year(date) == 2024) %>%
  
  # Join to survey_locations (for site_id)
  
  inner_join(
    survey_locations, 
    by = "branch_id"
    ) %>%
  
  # Join to sites (for region/state information)
  
  inner_join(
    sites, 
    by = "site_id"
    ) %>%
  
  # Keep only relevant states/regions
  filter(
    region %in% c(
      "District of Columbia", 
      "Maryland", 
      "Virginia")) %>%
  
  # Count number of surveys per region
  group_by(region) %>%
  summarise(count = n()) %>%  
ggplot() +
  aes(
    x = region,
    y = count,
    fill = region
  ) +
  geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +
  scale_fill_manual(
    values =
      c(
        "#4A1D2C",
        "#682779",
        "#DC331B",
        "#F29121"
      )
  ) +
  scale_y_continuous(
    limits = c(0,2500),
    expand = c(0,0)
  ) +
  labs (
    title = "N surveys conducted in District of Columbia, 
    Maryland, and Virginia in 2024",
    x = "Region",
    y = "Count"
  ) +
  theme_light()

# [[-0.50]] Incorrect: Your filtering by region did not work, because your
# region variable did not contain the full name of the state.

# [[-1.25]] `group_by()`, `scale_fill_manual()`, and `theme_light()` are not
# among the functions that you may use for this assignment.

# [[-0.25]] Code formatting:
# * Include no more than one prefix function per line of code.
# * Indentation and hanging parentheses: If a function spans more than one line
#   of code, closing parentheses should be placed on their own line and 
#   indented to the same level as the start of the function.
# * Maintain one blank line between code blocks and comments.
# * Indentation is off between `summarize(...)` and `ggplot()`.
# * If you provide three or more arguments to a function, place each argument
#   on its own line.
# * Commas should be followed by one trailing space.
# * Parentheses, curly braces, and square bracket operators should not be
#   preceded or followed by a space.

# question 10 --------------------------------------------------------------

# Please generate a summary table that provides the total number of arthropods
# counted per state in the District of Columbia, Maryland, and Virginia in 2024.

observations %>%
  
  # Join surveys to get date and branch_id
  
  inner_join(
    surveys, 
    by = "survey_id"
  ) %>%
  
  # Filter for surveys conducted in 2024
  
  filter(year(date) == 2024) %>%
  
  # Join survey_locations to get site_id
  
  inner_join(
    survey_locations,
    by = "branch_id"
  ) %>%
  
  # Join sites to get region information (state)
  inner_join(
    sites, 
    by = "site_id"
  ) %>%
  
  # Filter for the states of interest
  filter(
    region %in% c("DC", "MD", "VA")
  ) %>%
  
  # Summarize the total number of arthropods per state
  group_by(region) %>%
  summarise(total_arthropods = sum(arthropod_quantity, na.rm = TRUE)) %>%
  arrange(desc(total_arthropods))

# [[-1.25]] `group_by()` is not among the functions that you may use for this
# assignment.

# [[-0.25]] Code parsimony: 
# * Because there were no `NA` values in the data at the point of your
#   `summarize()`, it was not necessary to use `na.rm = TRUE`.
# * You were not asked to arrange the resultant data.

# [[-0.25]] Code formatting: 
# * If you provide three or more arguments to a function, place each argument
#   on its own line.
# * Include no more than one prefix function per line of code.


