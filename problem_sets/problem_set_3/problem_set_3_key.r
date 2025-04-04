
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
  
  # Subset sites to DC, MD, and VA:
  
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
  
  # Subset to caterpillar observations:
  
  filter(arthropod == "caterpillar") %>% 
  
  # Extract the variable of interest:
  
  pull(arthropod_quantity) %>% 
  
  # Calculate the total number of arthropods:
  
  sum()

# question 6 --------------------------------------------------------------

# Create a summary table that displays the number of surveys by year and 
# arrange from the highest to lowest number of surveys per year.

surveys %>% 
  
  # Create a `year` column:
  
  mutate(
    year = year(date)
  ) %>% 
  
  # Calculate the number of surveys by year:
  
  summarize(
    n = n(),
    .by = year
  ) %>% 
  
  # Arrange in descending order:
  
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
  
  # Subset survey locations to matching key values:
  
  semi_join(
    sites_dmv,
    by = "site_id"
  )

# question 8 --------------------------------------------------------------

# Please generate a summary table that provides the average (mean) number of
# caterpillars observed in "Beat sheet" and "Visual" surveys
# (`observation_method`).

observations %>% 
  
  # Subset observations to caterpillars:
  
  filter(arthropod == "caterpillar") %>%
  
  # Subset to variables of interest:
  
  select(survey_id, arthropod_quantity) %>% 
  
  # Join to surveys to observations, maintaining NA values:
  
  full_join(
    surveys,
    by = "survey_id"
  ) %>% 
  
  # Replace NA values with 0:
  
  mutate(
    arthropod_quantity = replace_na(arthropod_quantity, 0)
  ) %>% 
  
  # Calculate the average (mean) number of caterpillars by observation method:
  
  summarize(
    average_caterpillars = mean(arthropod_quantity),
    .by = observation_method
  )

# Or, without mutate (more parsimonious):

observations %>% 
  
  # Subset observations to caterpillars:
  
  filter(arthropod == "caterpillar") %>%
  
  # Subset to variables of interest:
  
  select(survey_id, arthropod_quantity) %>% 
  
  # Join to surveys to observations, maintaining NA values:
  
  full_join(
    surveys,
    by = "survey_id"
  ) %>% 
  
  # Calculate the average (mean) number of caterpillars by observation method:
  
  summarize(
    average_caterpillars = 
      arthropod_quantity %>% 
      
      # Replace NA values with 0:
      
      replace_na(0) %>% 
      mean(),
    .by = observation_method
  )

# question 9 --------------------------------------------------------------

# Please generate a bar plot that displays the number of surveys conducted 
# in the District of Columbia, Maryland, and Virginia in 2024. Plot the data
# such that:

# * Your x-aesthetic is region and is labeled "Region"
# * Your y-axis is labeled “Count”
# * The y-axis ranges from 0 to 2500
# * The plot includes a descriptive title

surveys %>% 
  
  # Subset to surveys in 2024:
  
  filter(
    year(date) == 2024
  ) %>% 
  
  # Subset to variables of interest:
  
  select(survey_id:branch_id) %>% 
  
  # Join source to target table, maintaining all of the rows in target:
  
  left_join(
    survey_locations %>% 
      
      # Reduce columns to just those that we want to maintain:
      
      select(!plant),
    by = "branch_id"
  ) %>% 
  
  # Join source to target table, maintaining only matching key values:
  
  inner_join(
    sites_dmv %>% 
      
      # Subset to variables of interest:
      
      select(site_id, region),
    by = "site_id"
  ) %>% 
  
  # Plot the number of surveys per region:
  
  ggplot() +
  aes(x = region) +
  geom_bar() +
  scale_y_continuous(
    limits = c(0, 2500),
    expand = c(0, 0)
  ) +
  labs(
    x = "Region",
    y = "Count",
    title = "Caterpillar Counts! surveys in D.C., Maryland, and Virginia in 2024"
  )

# question 10 --------------------------------------------------------------

# Please generate a summary table that provides the total number of arthropods
# counted per state in the District of Columbia, Maryland, and Virginia in 2024.

observations %>% 
  
  # Subset to variables of interest:
  
  select(survey_id, arthropod_quantity) %>% 
  
  # Join the source to the target table, maintaining only matching key values:
  
  inner_join(
    surveys %>% 
      
      # Subset to surveys in 2024:
      
      filter(
        year(date) == 2024
      ) %>% 
      
      # Subset to variables of interest:
      
      select(survey_id, branch_id),
    by = "survey_id"
  ) %>% 
  
  # Join the source to the target table, maintaining only matching key values:
  
  inner_join(
    survey_locations %>%
      
      # Subset to variables of interest:
      
      select(branch_id, site_id),
    by = "branch_id"
  ) %>% 
  
  # Join the source to the target table, maintaining only matching key values:
  
  inner_join(
    sites_dmv %>% 
      
      # Subset to variables of interest:
      
      select(site_id, region),
    by = "site_id"
  ) %>% 
  
  # Count the number of arthropods observed by region:
  
  summarize(
    n_dmv = sum(arthropod_quantity),
    .by = region
  )
