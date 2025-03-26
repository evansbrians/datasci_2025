
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

# Read in the `caterpillars_count.rds` and assign the names of the list to the
# global environment.

read_rds("data/raw/caterpillars_count.rds") %>% 
  list2env(.GlobalEnv)

# question 4 --------------------------------------------------------------

# How many caterpillars has Caterpillars Count counted? Please provide your
# answer as a one-value numeric vector.

observations %>% 
  
  # Subset to caterpillar observations:
  
  filter(arthropod == "caterpillar") %>% 
  
  # Extract the variable of interest:
  
  pull(arthropod_quantity) %>% 
  
  # Calculate the total number of arthropods:
  
  sum()

# question 5 --------------------------------------------------------------

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

# question 6 --------------------------------------------------------------

# Create a summary table that displays the number of surveys by year across
# regions and arrange from highest to lowest number of surveys.

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

# Subset the survey_locations data frame to those that are located within sites
# in the District of Columbia, Maryland, or Virginia. For full credit, please
# complete this such that:

# * No columns are added to, or removed from, `survey_locqtions`;
# * The `filter` function is not used to subset rows;
# * The `select` function is not used to subset columns.

survey_locations %>% 
  
  # Subset survey locations to matching key values:
  
  semi_join(
    sites_dmv,
    by = "site_name"
  )

# question 8 --------------------------------------------------------------

# Please generate a summary table that provides the average (mean) number of
# caterpillars observed in "Beat sheet" and "Visual" surveys across sampling
# regions. Note that to properly address this question you will have to do
# something about surveys in which no caterpillars were observed!

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

# Please generate a bar plot that displays the total number of surveys conducted
# in the District of Columbia, Maryland, and Virginia in 2024.

surveys %>% 
  
  # Subset to surveys in 2024:
  
  filter(
    year(date) == 2024
  ) %>% 
  
  # Subset to variables of interest:
  
  select(survey_id, survey_location) %>% 
  
  # Join source to target table, maintaining all of the rows in target:
  
  left_join(
    survey_locations %>% 
      
      # Reduce columns to just those that we want to maintain:
      
      select(!plant),
    by = "survey_location"
  ) %>% 
  
  # Join source to target table, maintaining only matching key values:
  
  inner_join(
    sites_dmv %>% 
      
      # Subset to variables of interest:
      
      select(site_name, region),
    by = "site_name"
  ) %>% 
  
  # Plot the number of surveys per region:
  
  ggplot() +
  aes(x = region) +
  geom_bar()

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
      
      select(survey_id, survey_location),
    by = "survey_id"
  ) %>% 
  
  # Join the source to the target table, maintaining only matching key values:
  
  inner_join(
    survey_locations %>%
      
      # Subset to variables of interest:
      
      select(survey_location, site_name),
    by = "survey_location"
  ) %>% 
  
  # Join the source to the target table, maintaining only matching key values:
  
  inner_join(
    sites_dmv %>% 
      
      # Subset to variables of interest:
      
      select(site_name, region),
    by = "site_name"
  ) %>% 
  
  # Count the number of arthropods observed by region:
  
  summarize(
    n_dmv = sum(arthropod_quantity),
    .by = region
  )
