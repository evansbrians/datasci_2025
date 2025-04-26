# Mark Peyton - problem set 7 

# 2 -----------------------------------------------------------------------

# Attach the tidyverse metapackage to your current R session.

library(tidyverse)

# 3 -----------------------------------------------------------------------

# Read in `"data/raw/problem_set_7_data.rds"` and assign each of the list
# item names to your global environment. 

read_rds("data/raw/problem_set_7_data.rds") %>% 
  list2env(.GlobalEnv)

# 4. backwards columns ----------------------------------------------------

# A common problem in data management is column values that have been
# reversed. In `backwards columns, column "a" is intended to contain only
# negative numbers and column "b" is intended to contain only positive numbers.
# If column a is negative and column b is positive, switch the values between
# the columns:

# original idea used case_when but QA video session shows if_else. Modified to video suggestion. 

backwards_columns %>% 
  mutate(
    tempa = if_else(
     a > 0,
     b,
     a
    ),
    b = if_else(
      b < 0,  
     a,
     b)) %>% 
  select(
    a = temp, b)

# Interesting video solution

backwards_columns %>% 
  mutate(
    a =
      if_else(
        a > 0,
        b,
        a
      ),
    b = 
      if_else(
        b < 0,
        backwards_columns %>% 
          pull(a),
        b))
  
# Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
  pluck("question_4")

# 5. regex-cellence -------------------------------------------------------

# The dataset `bad_birds` is a vector. Fix the spellings of the common names
# to "House finch", "House sparrow", and "House wren":

bad_birds %>% 
  str_replace("^[Hh][0Oo]?.?se? [Ff]i?nch", "House finch") %>%
  str_replace("^[Hh][0Oo]?.?se? [Ss]pa?rr?o?w", "House sparrow") %>%
  str_replace("^[Hh][0Oo]?.?se? [Ww]?r.?n", "House wren")

# Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
  pluck("question_5")

# 6-7. anti and semi join -------------------------------------------------

# The list object `matching_list_tables` contains two tables, `boy` and `howdy`.
# The column `heroes` in `boy` corresponds with the column `hello` in `howdy`.

# Without using filter, subset the table `boy` in `matching_list_tables` such
# that only rows in which the value of column `heroes` has a corresponding value
# in the column `hello` of table `howdy`:

# This appears to achieve results, however there is no use of a join. I've also included the join solution below. 

matching_list_tables %>%
  pluck("boy") %>%
  slice(1:5)

# Using semi_join

matching_list_tables %>% 
  pluck("boy") %>% 
  semi_join(
    pluck(
      matching_list_tables, 
      "howdy"),
    by = c(
      "heroes" = "hello")) 

# Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
  pluck("question_6")

# Without using filter, subset the table `boy` in `matching_list_tables` such
# that only rows in which the value of column `heroes` does not have a
# corresponding value in the column `hello` of table `howdy`:

# Using anti_join

matching_list_tables %>% 
  pluck("boy") %>% 
  anti_join(
    pluck(
      matching_list_tables, 
      "howdy"),
    by = c(
      "heroes" = "hello")) 

# Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
  pluck("question_7")

# 8. butterflies and moths ------------------------------------------------

# The dataset leps_dc contains all of the observations of Lepidoptera in
# Washington DC in 2021.

# Observe the dataset 

leps_dc

# * Please write a comment that describes whether these data are tidy and, 
#   if not, which tidy data rule the tibble violates.

# It appears the answer is already provide (see below). This appears to be a violation of the second normal rule. 

# These data are not tidy, as the `common_name` varies by the 
# `scientific_name`, not the observation.

# * Subset the data to observations of the three species with the most total
#   number of observations across time and assign the name common_leps to the 
#   resultant object.

# I also achieved the desired results using slice_max and a left_join function, but since left_join is not in the list, I have modified to a more streamlined code using available functions. 

common_leps <- 
  leps_dc %>%
  
  # organize data by scientific name and get total count of each (using the summarize tool with arranged data is also helpful to visualize total counts but doesn't retain all columns of data). This part of the code is unnecessary to achieving the final results, but is helpful in determining which species to filter for. 
  
  mutate(
    count = n(),
    .by = scientific_name) %>% 
  
  # filter by the 3 species with the highest total count
  
  filter(
    scientific_name %in% 
      c(
        "Atalopedes campestris", 
        "Danaus plexippus", 
        "Papilio glaucus")) %>% 
  
  # select all columns except count to match with desired results
  
  select(-count)
     
 
  # Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
  pluck("question_8")

# Extra credit 1. Using a map function, create three bar plots of the number of
# observations per month for each of the three most commonly observed
# Lepidoptera species. In doing so:

# * Label the x-axis with month abbreviations (e.g., Apr, May, Jun);
# * Include the scientific name of the species as the title of the plot.

common_leps %>%
  
# Create a month column with abbreviations
  
  mutate(month = month(
    date, 
    label = TRUE, 
    abbr = TRUE)) %>%
  
# Group the data by month and scientific name and summarize counts 
  
  group_by(
    month, 
    scientific_name) %>%
  summarize(
    count = n(), 
    .groups = 'drop') %>%
  
# Not an approved function - try to find an alternative 
  
  group_split() %>%
  
# Iterate a plotting function for each of the three species with counts by month using the purrr::map function
  
  map(
    ~ ggplot(
      data = .x, 
      aes(x = month, 
          y = count, 
          fill = scientific_name)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Month", 
           y = "Count") +
      facet_wrap(~ scientific_name, 
                 scales = "free") +
      theme(legend.position = "none")
  )

# Without using map function I can create the bar plots, but didn't figure out how to use map without the "illegal" function

common_leps %>%
  mutate(month = month(
    date, 
    label = TRUE, 
    abbr = TRUE)) %>%
  group_by(
    month, 
    scientific_name) %>%
  summarize(count = n(), 
            .groups = 'drop') %>%
  ggplot(aes(
    x = month, 
    y = count, 
    fill = scientific_name)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Month", y = "Count") +
  facet_wrap(~ scientific_name, 
             scales = "free") +
  theme(legend.position = "none")

# 9. seems like old times -------------------------------------------------

# The object `dates_and_times` contains a date and two times in which an
# event occurred. Please modify the object `dates_and_times`:

# * Convert date to an ISO-8601 date object.
# * Convert time1 and time2 to a ISO-8601 date-time object.
# * Subset the data to where time2 occurs after time1.
# * Replace time1 and time2 with the hour associated with the time.

# My original effort to solve this question - watched video and attempted the video answers below 

dates_and_times %>% 
  
# * Convert date to an ISO-8601 date object.
  
  mutate(
    date = mdy(date),

# * Convert time1 and time2 to a ISO-8601 date-time object.

    time1 = hms::as.hms(time1),
    time2 = hms::as.hms(time2)) %>% 

# * Subset the data to where time2 occurs after time1.
  
  filter(
    time2 > time1) %>% 
  
# * Replace time1 and time2 with the hour associated with the time.
  
  mutate(
    time1 = hour(
      time1),
    time2 = hour(
      time2))

# Video solution without across

dates_and_times %>% 
  filter(time2 > time1) %>% 
  mutate(
    date = mdy(date),
    time1 =
      str_c(date, 
            time1, sep = " ") %>% 
      ymd_hms() %>% 
      hour(),
    time2 =
      str_c(date,
            time2,
            sep = " ") %>% 
      ymd_hms() %>% 
      hour()) 

# Using across to make more parsimonious 

dates_and_times %>% 
  filter(time2 > time1) %>% 
  mutate(
    date = mdy(date),
    across(
      time1:time2,
      ~ str_c(date, .x, sep = " ") %>% 
        ymd_hms() %>% 
        hour() 
    )
  )

# Hint: Your resultant object should be equivalent to:

read_rds("data/raw/problem_set_7_lifelines.rds") %>% 
  pluck("question_9")

# casing the joint --------------------------------------------------------

# The dataset `size_and_volume` represents the observations of different
# individuals, their size, and how loud they were. As parsimoniously as
# possible create a column called `species` where:

# * Sizes less than 10 and volumes less than or equal to 30 are classified as
#   a "Deer mouse";
# * Sizes less than 10 and volumes greater than 30 are classified as a "House
#   wren";
# * Sizes great than or equal to 10 and volumes less than or equal to 30 are
#   classified as a "Three-toed sloth";
# * Sizes great than or equal to 10 and volumes greater than 30 are classified
#   as a "Howler monkey".

size_and_volume %>% 
  mutate(
    species = case_when(
      size < 10 & volume <= 30 ~ "Deer mouse",
      size < 10 & volume > 30 ~ "House wren",
      size >= 10 & volume <= 30 ~ "Three-toed sloth",
      size >= 10 & volume > 30 ~ "Howler monkey"))

# Hint: Your resultant object should be equivalent to:

read_rds("data/raw/problem_set_7_lifelines.rds") %>% 
  pluck("question_10")

# Extra credit 2. Create a bar plot of the number of observations of each
# species where:

# * Species are on the y-axis and the count is on the x-axis;
# * The bars are arranged by the number of observations per species.

# Identifying species based on size and volume

size_and_volume %>% 
  mutate(
    species = case_when(
      size < 10 & volume <= 30 ~ "Deer mouse",
      size < 10 & volume > 30 ~ "House wren",
      size >= 10 & volume <= 30 ~ "Three-toed sloth",
      size >= 10 & volume > 30 ~ "Howler monkey")) %>% 
  
# Count of each species and reorder in descending order
  
  count(species) %>% 
  mutate(
    species = 
      fct_reorder(
        species,
        n,
        .desc = TRUE)) %>%
  
# Plot the species count data in descending order with species on the y-axis and count on the x-axis
  
  ggplot(
    aes(
      x = species, 
      y = n)) +
  geom_bar(
    stat = "identity") +
  theme_minimal() +
  labs(
    x = "Species", 
    y = "Count") +
  coord_flip()
