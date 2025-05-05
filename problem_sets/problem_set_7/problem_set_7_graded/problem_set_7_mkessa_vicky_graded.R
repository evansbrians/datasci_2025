
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
    A = 
      if_else(
        a > 0,
        b,
        a
      ),
    B = 
      if_else(
        b < 0,
        a, 
        b)
  ) %>% 
  select(
    a = A, 
    b = B)

# [[-0.10]] If a function spans more than one line of code, closing parentheses 
# should be placed on their own line.

# 5 -----------------------------------------------------------------------

# The dataset `bad_birds` is a vector. Fix the spellings of the common names
# to "House finch", "House sparrow", and "House wren":

bad_birds %>% 
  str_replace("(^[Hh]s?[0o]?(use)?).[Ss]pa?rr?o?w", "House sparrow") %>% 
  str_replace("(^[Hh]s?o?u?(se)?).[Ff]i?nch", "House finch") %>% 
  str_replace("(^[Hh]s?(ouse)?).[Ww]?re?n", "House wren")

# 6 -----------------------------------------------------------------------

# The list object `matching_list_tables` contains two tables, `boy` and `howdy`.
# The column `heroes` in `boy` corresponds with the column `hello` in `howdy`.

# Assign names to the two list objects in matching_list_tables

boy <- matching_list_tables %>% 
  pluck("boy") 

howdy <- matching_list_tables %>% 
  pluck("howdy") 

# Without using filter, subset the table `boy` in `matching_list_tables` such
# that only rows in which the value of column `heroes` has a corresponding value
# in the column `hello` of table `howdy`:

boy %>% 
  
  # use semi join to filter rows in heroes that has corressponsing values in howdy
  
  semi_join(
    howdy,
    by = c("heroes" = "hello")
  )

# Without using filter, subset the table `boy` in `matching_list_tables` such
# that only rows in which the value of column `heroes` does not have a
# corresponding value in the column `hello` of table `howdy`:

boy %>% 
  
  # use semi join to filter rows in heroes that has corressponsing values in howdy
  
  anti_join(
    howdy,
    by = c("heroes" = "hello")
  )

# [[-0.50]] You were not asked to globally assign for this question.

#  [[-0.10]] Code formatting: If a code block spans more than one line of code,
#  add a new line after the assignment operator.

# 7 -----------------------------------------------------------------------

# The dataset `leps_dc` contains all of the observations of Lepidoptera in
# Washington DC in 2021.

# * Please write a comment that describes whether these data are tidy and, 
#   if not, which tidy data rule the tibble violates:

# The data Violates the 3rd tidy data rule that all columns are non-transitively 
# dependent. The two columns scientific_name and common_name all describe the lepidoptera species observed in 2021, so dataset should have kept one column.

# * Subset the data to observations of the three species with the most total
#   number of observations across time and assign the name common_leps to the 
#   resultant object:

common_leps <- 
  leps_dc %>% 
  semi_join(
    leps_dc %>% 
      summarize( 
        n = n(),
        .by = scientific_name
      ) %>% 
      slice_max(
        n,
        n = 3
      )
  )

# [[-0.15]] Code formatting: Code and comments should not exceed 80 characters
# in width (if it is avoidable) – add a line break, if possible.

# 8 -----------------------------------------------------------------------

# Using `common_leps` and iteration, create three bar plots of the number of
# observations per month for each of the three most commonly observed
# Lepidoptera species. In doing so:

# * Label the x-axis with month abbreviations (e.g., Apr, May, Jun);
# * Include the scientific name of the species as the title of the plot.

# plotting number of observations for the three most commonly observed species

common_leps %>% 
  pull(scientific_name) %>% 
  unique() %>% 
  map(
    function(x) {
      common_leps %>% 
        mutate(
          month = 
            month(
              date,
              
              # will return the month abbreviations to be plotted on the x -axis
              
              label = TRUE
            )
        ) %>% 
        filter(
          scientific_name == x ) %>% 
        
        # Summarize rows by month:
        
        summarize(
          n = n(),
          .by = month
        ) %>% 
        
        # Plot the data: 
        
        ggplot() +
        aes(
          x = month,
          y = n
        ) +
        geom_bar(stat = "identity") +
        labs(
          
          # Includes the scientific name of the species as the title of the plot.
          
          title = x,
          x = "Month", 
          y = "Number of Observations"
        ) +
        theme_bw()
    }
  )

# [[-0.15]] Code formatting:
# * Parentheses and square bracket operators should not be preceded or followed
#   by a space.
# * If a function spans more than one line of code, closing parentheses 
#   should be placed on their own line.

# 9 -----------------------------------------------------------------------

# The object `dates_and_times` contains a date and two times in which an
# event occurred. Please modify the object `dates_and_times`:

# * Convert date to an ISO-8601 date object.
# * Convert time1 and time2 to a ISO-8601 date-time object.
# * Subset the data to where time2 occurs after time1.
# * Replace time1 and time2 with the hour associated with the time.

dates_and_times %>% 
  mutate(
    
    # convert date to ISO-8601 date object
    
    date = mdy(date),
    
    # convert time1 and time2 to ISO-8601 date-time object   
    
    time1 = 
      str_c(date, time1, sep=" "),
    time2 = 
      str_c(date, time2, sep=" ")
  ) %>% 
  
  # Subset the data to where time2 occurs after time1
  
  filter(
    time2 > time1
  ) %>% 
  
  # Replace time1 and time2 with the hour associated with the time
  
  mutate(
    time1 = hour(time1),
    time2 = hour(time2)
  )

# [[-0.15]] Code parsimony: Your code could have been made less redundant
# through the use of `across()`.

# [[-0.15]] Code formatting: 
# * Include no more than one prefix function per line of code.
# * Infix functions should be separated from surrounding code with a single
#   leading and trailing space.

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
        size < 10 & volume <= 30 ~ "Deer mouse",
        size < 10 & volume > 30 ~ "House wren",
        size >= 10 & volume <= 30 ~ "Three-toed sloth",
        size >= 10 & volume > 30 ~ "Howler monkey"
      )
  )

# [[-0.10]] Code parsimony: It is not necessary to specify size and volume for
# each of the combinations.

# Extra credit: Create a bar plot of the number of observations of each
# species where:

# * Species are on the y-axis and the count is on the x-axis;
# * The bars are arranged by the number of observations per species.

size_and_volume %>% 
  mutate(
    species = 
      case_when(
        size < 10 & volume <= 30 ~ "Deer mouse",
        size < 10 & volume > 30 ~ "House wren",
        size >= 10 & volume <= 30 ~ "Three-toed sloth",
        size >= 10 & volume > 30 ~ "Howler monkey"
      )
  ) %>% 
  
  # Count the number of observations per species
  
  summarize(
    n = n(),
    .by = species
  ) %>% 
  
  # Plot the observations arranged by the number of observations per species 
  
  ggplot() +
  aes(
    x = reorder(species, n),
    y = n
  ) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "Species",
    y = "Number of observations"
  ) +
  theme_bw()

# [[+0.50]] Half of the extra credit points were taken off because `reorder` is
# not among the functions that you may use in this assignment ... `fct_reorder`
# was.
