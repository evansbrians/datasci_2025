
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
  filter(region %in% 
           c(
             "DC",
             "MD",
             "VA"
           )
  )

# [[-0.10]] Code formatting: If the opening parentheses and the first argument
# of a function are on different lines, the first argument should be indented
# two spaces (one tab stop) relative to the start of the line above.

# question 5 --------------------------------------------------------------

# How many caterpillars has Caterpillars Count counted? Please provide your
# answer as a one-value numeric vector.

observations %>% 
  summarize(
    sum(arthropod_quantity)
  ) %>% 
  pull()

# [[-0.40]] Incorrect: This is all arthropods, you needed to subset to
# caterpillars.

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
      select(survey_id, arthropod_quantity),
    by = "survey_id"
  ) %>% 
  mutate(
    arthropod_quantity = replace_na(arthropod_quantity, 0)
  ) %>% 
  group_by(observation_method) %>% 
  summarize(
    mean = mean(arthropod_quantity),
    .groups = "drop"
  )

# [[No points removed]] Incorrect:
# * You needed to filter your observations to caterpillars prior to joining the
#   data frames;
# * You needed to use a `full_join()` to maintain surveys in which no
#   caterpillars were observed;

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

surveys %>% 
  
  # only keep surveys from 2024:
  
  filter(year(date) == 2024) %>% 
  
  # select columns needed for joins:
  
  select(survey_id, branch_id) %>% 
  
  # add site_id for the 2024 surveys:
  
  left_join(
    survey_locations %>% 
      select(branch_id, site_id),
    by = "branch_id"
  ) %>% 
  
  # add the region of surveys and only keep those in DC, MD or VA:
  
  inner_join(
    sites_dmv %>% 
      select(site_id, region),
    by = "site_id"
  ) %>% 
  
  # count the number of surveys per region
  
  summarize(
    count = n(),
    .by = region
  ) %>% 
  
  # plot the data
  
  ggplot() +
  aes(
    x = region,
    y = count
  ) +
  geom_bar(
    stat = "identity"
  ) +
  labs(
    x = "Region",
    y = "Count",
    title = "Survey effort by region (DC, MD, VA) in 2024"
  ) +
  scale_y_continuous(
    limits = c(0,2500),
    expand = c(0,0)
  )

# [[-0.25]] Code formatting:
# * Include no more than one prefix function per line of code.
# * Commas should be followed by one trailing space.

# question 10 --------------------------------------------------------------

# Please generate a summary table that provides the total number of arthropods
# counted per state in the District of Columbia, Maryland, and Virginia in 2024.

surveys %>% 
  
  # filter out surveys outside of 2024:
  
  filter(year(date) == 2024) %>% 
  
  # only keep columns needed for joins:
  
  select(survey_id, branch_id) %>% 
  
  # add site_id for the 2024 surveys:
  
  left_join(
    survey_locations %>% 
      select(branch_id, site_id),
    by = "branch_id"
  ) %>% 
  
  # add the region of surveys and only keep those in DC, MD or VA:
  
  inner_join(
    sites_dmv %>% 
      select(site_id, region),
    by = "site_id"
  ) %>% 
  
  # add the number of arthropods registered per site
  
  inner_join(
    observations %>% 
      select(survey_id, arthropod_quantity),
    by = "survey_id"
  ) %>% 
  
  # sum the number of arthropods counted per region
  
  summarize(
    n = sum(arthropod_quantity),
    .by = region
  ) 

# [[-0.25]] Code formatting: Include no more than one prefix function per line
# of code.
