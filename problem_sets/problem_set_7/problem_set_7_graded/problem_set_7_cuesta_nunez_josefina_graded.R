
# Script file for problem set 7: Final exam training session

# 1 -----------------------------------------------------------------------

# Before opening your script file for this problem set, change the name of
# `problem_set_7.R` to "problem_set_7_[last name]_[first name].R" using a snake
# case naming convention. *Note: You will submit this script file as your
# assignment*.

# 2 -----------------------------------------------------------------------

# Open the script file in RStudio and attach the core tidyverse packages to your
# current R session.

library(tidyverse)

# 3 -----------------------------------------------------------------------

# Read in `data/raw/problem_set_7_data.rds` and assign each of the list
# item names to your global environment. 

read_rds("data/raw/problem_set_7_data.rds") %>% 
  list2env(.GlobalEnv)

# 4 -----------------------------------------------------------------------

# A common problem in data management is column values that have been
# reversed. In `backwards columns`, column "a" is intended to contain only
# negative numbers and column "b" is intended to contain only positive numbers.
# If column `a` is positive, replace that value with the negative value in
# column `b`. If column `b` is negative, replace that value with the positive 
# value in column `a`.

backwards_columns %>% 
  mutate(
    a_fixed =
      case_when(
        a > 0 & b < 0 ~ b,
        TRUE ~ a
      ),
    b_fixed =
      case_when(
        b < 0  ~ a,
        TRUE ~ b
      )
  ) %>% 
  select(
    a = a_fixed, 
    b = b_fixed
  )

# 5 -----------------------------------------------------------------------

# The dataset `bad_birds` is a vector. Fix the spellings of the common names
# to "House finch", "House sparrow", and "House wren":

bad_birds %>%
  str_replace("[Hh].?u?se?", "House") %>% 
  str_replace("[Ff]i?nch", "finch") %>% 
  str_replace("([Ww])?re?n", "wren") %>% 
  str_replace("[Ss]pa?rr?o?w", "sparrow")

# 6 -----------------------------------------------------------------------

# The list object `matching_list_tables` contains two tables, `boy` and `howdy`.
# The column `heroes` in `boy` corresponds with the column `hello` in `howdy`.

# Without using filter, subset the table `boy` in `matching_list_tables` such
# that only rows in which the value of column `heroes` has a corresponding value
# in the column `hello` of table `howdy`:

matching_list_tables %>% 
  pluck("boy") %>% 
  semi_join(
    matching_list_tables %>% 
      pluck("howdy"),
    by = join_by(heroes == hello) 
  )

# Without using filter, subset the table `boy` in `matching_list_tables` such
# that only rows in which the value of column `heroes` does not have a
# corresponding value in the column `hello` of table `howdy`:

matching_list_tables %>% 
  pluck("boy") %>% 
  anti_join(
    matching_list_tables %>% 
      pluck("howdy"),
    by = join_by(heroes == hello) 
  )

# 7 -----------------------------------------------------------------------

# The dataset `leps_dc` contains all of the observations of Lepidoptera in
# Washington DC in 2021.

# * Please write a comment that describes whether these data are tidy and, 
#   if not, which tidy data rule the tibble violates:

# Not tidy – breaks rule 3: each level of observation forms a table  
# Scientific and common names are transitive columns 
# One should be a key, or create a new variable to act as species key  
# Then link names through a species table and use only the key in leps_dc  
# Could also add a visit key and store visit info (e.g. date) in another table

# * Subset the data to observations of the three species with the most total
#   number of observations across time and assign the name common_leps to the 
#   resultant object:

common_leps <-
  leps_dc  %>% 
  semi_join(
    leps_dc %>%
      summarise(
        total = n(), 
        .by = scientific_name
      ) %>%
      slice_max(
        order_by = total, 
        n = 3
      ),
    by = "scientific_name"
  ) 

# 8 -----------------------------------------------------------------------

# Using `common_leps` and iteration, create three bar plots of the number of
# observations per month for each of the three most commonly observed
# Lepidoptera species. In doing so:

# * Label the x-axis with month abbreviations (e.g., Apr, May, Jun);
# * Include the scientific name of the species as the title of the plot.

# get the scientific name of each species

common_leps %>%
  pull(scientific_name) %>% 
  unique() %>%
  map(
    ~ {
      
      # for each unique scientific name...
      
      common_leps %>%
        filter(scientific_name == .x) %>%  
        
        # ... generate a column with abbreviated month names
        
        mutate(
          month = month(
            date, 
            label = TRUE, 
            abbr = TRUE
          )
        ) %>%
        
        # plot the data  with "month" on the x-axis
        
        ggplot() + 
        aes(x = month) +
        
        # get counts per month on the y axis with stat = "count"
        
        geom_bar(stat = "count") +
        
        # add a unique title for each plot using the species name 
        
        labs(
          title = str_c("Species: ", .x),
          x = "Month",
          y = "Count"
        )
    })

# 9 -----------------------------------------------------------------------

# The object `dates_and_times` contains a date and two times in which an
# event occurred. Please modify the object `dates_and_times`:

# * Convert date to an ISO-8601 date object.
# * Convert time1 and time2 to a ISO-8601 date-time object.
# * Subset the data to where time2 occurs after time1.
# * Replace time1 and time2 with the hour associated with the time.

dates_and_times %>% 
  mutate(
    
    # Convert date to an ISO-8601 date object
    
    date = mdy(date),
    
    # Convert time1 and time2 to a ISO-8601 date-time object.
    
    across(
      time1:time2,
      ~ ymd_hms(
        str_c(date, .)
      )
    )
  ) %>%
  
  # Subset the data to where time2 occurs after time1.
  
  filter(time2 > time1) %>% 
  
  # Replace time1 and time2 with the hour associated with the time.
  
  mutate(
    across(
      time1:time2,
      ~ hour(.)
    )
  )

# [[No points removed]] I do not recommend using `.` to represent a variable
# in `across()`.

# 10 ----------------------------------------------------------------------

# The dataset `size_and_volume` represents the observations of different
# individuals, their size, and how loud they were. As parsimoniously as
# possible, create a column called `species` where:

# * Sizes less than 10 and volumes less than or equal to 30 are classified as
#   a "Deer mouse";
# * Sizes less than 10 and volumes greater than 30 are classified as a "House
#   wren";
# * Sizes greater than or equal to 10 and volumes less than or equal to 30 are
#   classified as a "Three-toed sloth";
# * Sizes great than or equal to 10 and volumes greater than 30 are classified
#   as a "Howler monkey".

size_and_volume %>% 
  mutate(
    species = 
      case_when(
        
        # "Deer mouse" if size < 10 and volume <= 30
        
        size < 10 & volume <= 30  ~ "Deer mouse",
        
        # "House wren" if size < 10 and volume > 30
        
        size < 10 & volume > 30 ~ "House wren",
        
        # "Three-toed sloth" if volume <= 30 (size must be >= 10 by now)
        
        volume <= 30 ~ "Three-toed sloth",
        
        # "Howler monkey" if size >= 10 and volume > 30 (last remaining case)
        
        TRUE ~ "Howler monkey"
      )
  )

# Extra credit: Create a bar plot of the number of observations of each
# species where:

# * Species are on the y-axis and the count is on the x-axis;
# * The bars are arranged by the number of observations per species.

size_and_volume %>% 
  mutate(
    species = 
      case_when(
        size < 10 & volume <= 30  ~ "Deer mouse",
        size < 10 & volume > 30 ~ "House wren",
        volume <= 30 ~ "Three-toed sloth",
        TRUE ~ "Howler monkey"
      )
  ) %>%
  
  # generate a column with species counts
  
  summarize(
    count = n(),
    .by = species
  ) %>% 
  
  # reorder the species based on counts 
  
  mutate(
    species = fct_reorder(species, count)
  ) %>% 
  
  # plot the data
  
  ggplot() +
  aes(y = species, x = count) +
  geom_bar(stat = "identity") +
  labs(y = "Species", x = "Count")

#  [[+0.9]] Points of for code formatting: If you have more than one = in a
#  function, place each argument on its own line.
