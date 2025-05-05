
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

for(i in 1:7){
  if (backwards_columns$a[i] > 0) {
    backwards_columns[i,] <- 
      list(
        backwards_columns$b[i],
        backwards_columns$a[i]
      )
  }
}

# [[-0.50]] This was a pretty cool base R solution! However, `for`, `if`.
# `[...]`, `$` and `list` are not among the functions that you may use for this
# assignment.

# [[-0.10]] Code formatting: Space before opening parenthesis.

# 5 -----------------------------------------------------------------------

# The dataset `bad_birds` is a vector. Fix the spellings of the common names
# to "House finch", "House sparrow", and "House wren":

bad_birds <- 
  bad_birds %>% 
  str_replace(
    ".*w$", 
    "House Sparrow"
  ) %>% 
  str_replace(
    ".*h$",
    "House Finch"
  ) %>% 
  str_replace(
    ".*n$",
    "House Finch"
  )

# [[-0.75]] You were not asked to globally assign in this question.

# [[No points removed]] Your answer works, and is very parsimonious, but could
# be dangerous if there were additions to the rows over time.

# 6 -----------------------------------------------------------------------

# The list object `matching_list_tables` contains two tables, `boy` and `howdy`.
# The column `heroes` in `boy` corresponds with the column `hello` in `howdy`.

# Without using filter, subset the table `boy` in `matching_list_tables` such
# that only rows in which the value of column `heroes` has a corresponding value
# in the column `hello` of table `howdy`:

semi_join(
  matching_list_tables$boy,
  matching_list_tables$howdy,
  by = join_by(heroes == hello)
)

# Without using filter, subset the table `boy` in `matching_list_tables` such
# that only rows in which the value of column `heroes` does not have a
# corresponding value in the column `hello` of table `howdy`:

anti_join(
  matching_list_tables$boy,
  matching_list_tables$howdy,
  by = join_by(heroes == hello)
)

# [[-0.50]] `$` is not among the functions that you may use for this
# assignment.

# 7 -----------------------------------------------------------------------

# The dataset `leps_dc` contains all of the observations of Lepidoptera in
# Washington DC in 2021.

# * Please write a comment that describes whether these data are tidy and, 
#   if not, which tidy data rule the tibble violates:

# No, it violates the first normal rule that all rows represent a unique record, 
# as there is no primary key. Observations of multiple individuals of the 
# same species on the same day are recorded in separate rows, while there 
# should be a column for the total number of each species observed during a day.
# In addition, it also violates the 

# * Subset the data to observations of the three species with the most total
#   number of observations across time and assign the name common_leps to the 
#   resultant object:

common_leps <- 
  leps_dc %>% 
  filter(
    scientific_name %in% 
      c(
        leps_dc %>% 
          summarize(
            count = n(),
            .by = scientific_name
          ) %>% 
          slice_max(
            order_by = count,
            n = 3
          ) %>% 
          pull(scientific_name)
      )
  )

# [[-0.50]] Incorrect: Even though there are duplicate rows, each row represents
# an observation. This was a violation of the third tidy data rule that every
# level of observation is stored in their own table.

# 8 -----------------------------------------------------------------------

# Using `common_leps` and iteration, create three bar plots of the number of
# observations per month for each of the three most commonly observed
# Lepidoptera species. In doing so:

# * Label the x-axis with month abbreviations (e.g., Apr, May, Jun);
# * Include the scientific name of the species as the title of the plot.

unique(common_leps$scientific_name) %>% 
  map(
    \(x){
      common_leps %>% 
        filter(
          scientific_name == x
        ) %>% 
        mutate(
          month = 
            month(
              date, 
              label = TRUE,
              abbr = TRUE
            )
        ) %>% 
        ggplot() +
        geom_bar(
          aes(month),
          fill = "darkorange"
        ) +
        labs(
          title = x
        )
    }
  )

# [[-0.75]]: `$` is not among the functions that you may use for this
# assignment.

# 9 -----------------------------------------------------------------------

# The object `dates_and_times` contains a date and two times in which an
# event occurred. Please modify the object `dates_and_times`:

# * Convert date to an ISO-8601 date object.
# * Convert time1 and time2 to a ISO-8601 date-time object.
# * Subset the data to where time2 occurs after time1.
# * Replace time1 and time2 with the hour associated with the time.

dates_and_times <- 
  dates_and_times %>% 
  mutate(
    date = mdy(date),
    across(
      time1:time2,
      ~ ymd_hms(str_c(date, .x))
    )
  ) %>% 
  filter(
    time1 < time2
  ) %>% 
  mutate(
    across(
      time1:time2,
      ~ hour(.x)
    )
  )

# [[-0.75]] You were not asked to globally assign in this question.

# [[-0.15]] Code formatting: Include no more than one prefix function per line
# of code.

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

size_and_volume <- 
  size_and_volume %>% 
  mutate(
    species = 
      case_when(
        size < 10 & volume <= 30 ~ "Deer mouse",
        size < 10 & volume > 30 ~ "House wren",
        volume <= 30 ~ "Three-toed sloth",
        TRUE ~ "Howler monkey"
      )
  )

# [[-0.50]] You were not asked to a assign a name to the global environment in
# this question.


# Extra credit: Create a bar plot of the number of observations of each
# species where:

# * Species are on the y-axis and the count is on the x-axis;
# * The bars are arranged by the number of observations per species.

size_and_volume %>%
  ggplot() +
  geom_bar(
    aes(
      fct_reorder(
        species, 
        species, 
        .fun = length
      )
    )
  ) +
  coord_flip()

# [[+1.0]]
