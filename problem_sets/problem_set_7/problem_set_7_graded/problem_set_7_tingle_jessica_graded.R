
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
    a_prime =
      if_else(
        a > 0,
        b,
        a
      ),
    b =
      if_else(
        b < 0,
        a,
        b
      )
  ) %>% 
  select(
    a = a_prime,
    b
  )

# 5 -----------------------------------------------------------------------

# The dataset `bad_birds` is a vector. Fix the spellings of the common names
# to "House finch", "House sparrow", and "House wren":

bad_birds %>% 
  str_replace(
    pattern = "^[Hh].* .*w$",
    replacement = "House sparrow"
  ) %>% 
  str_replace(
    pattern = "^[Hh].* .*n$",
    replacement = "House wren"
  ) %>% 
  str_replace(
    pattern = "^[Hh].* .*h$",
    replacement = "House finch"
  )

# [[No points removed]] Your answer works, and is very parsimonious (Tara
# approved!), but could be dangerous if there were additions to the rows over
# time.

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

# not tidy!
# the first and third rules are violated -
# rows do not all represent unique records;
# date is not functionally dependent on scientific name or column name;
# really, we need a column with another variable that is 
# functionally dependent on species + date (e.g. count);
# also, if we care about common names, those should go in a separate table with
# scientific name as the primary key (otherwise, we could remove common names)

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
      slice_max(n, n = 3),
    by = "scientific_name"
  )

# 8 -----------------------------------------------------------------------

# Using `common_leps` and iteration, create three bar plots of the number of
# observations per month for each of the three most commonly observed
# Lepidoptera species. In doing so:

# * Label the x-axis with month abbreviations (e.g., Apr, May, Jun);
# * Include the scientific name of the species as the title of the plot.

common_leps %>% 
  pull(scientific_name) %>% 
  unique() %>%
  map(
    ~ common_leps %>% 
      filter(scientific_name == .x) %>% 
      ggplot() +
      aes(
        x = month(date, label = TRUE)
      ) +
      geom_bar(stat = "count") +
      labs(
        title = .x,
        x = "Month",
        y = "Count"
      )
  )

# 9 -----------------------------------------------------------------------

# The object `dates_and_times` contains a date and two times in which an
# event occurred. Please modify the object `dates_and_times`:

# * Convert date to an ISO-8601 date object.
# * Convert time1 and time2 to a ISO-8601 date-time object.
# * Subset the data to where time2 occurs after time1.
# * Replace time1 and time2 with the hour associated with the time.

dates_and_times %>% 
  mutate(
    date = mdy(date),
    across(
      time1:time2,
      ~ str_c(date, .x) %>% 
        ymd_hms()
    )
  ) %>% 
  filter(time2 > time1) %>% 
  mutate(
    across(
      time1:time2,
      ~ hour(.x)
    )
  )

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
        size < 10 & volume >= 30 ~ "House wren",
        size >= 10 & volume <= 30 ~ "Three-toed sloth",
        size >= 10 & volume >= 30 ~ "Howler monkey"
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
        size < 10 & volume >= 30 ~ "House wren",
        size >= 10 & volume <= 30 ~ "Three-toed sloth",
        size >= 10 & volume >= 30 ~ "Howler monkey"
      )
  ) %>% 
  summarize(
    n = n(),
    .by = species
  ) %>% 
  mutate(
    label =
      str_c(
        species, 
        c(
          "Trying hard now",
          "Gonna fly now",
          "Getting strong now",
          "Oh we're ready"
        ),
        sep = "\n"
      ) %>% 
      fct_reorder(
        desc(n)
      )
  ) %>% 
  ggplot() +
  aes(
    x = label,
    y = n
  ) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Da da daaaaaa",
    subtitle = "Da da daaaaaa",
    x = NULL,
    y = "# observations"
  )

# :)!!!!!!!!! We LOVE your goofy plots!!!!!!

# [[+1.0]]
