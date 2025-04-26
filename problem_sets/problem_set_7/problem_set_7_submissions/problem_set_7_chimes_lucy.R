
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

backwards_columns %>% 
  mutate(
    negative = 
      if_else(
        a > 0,
        b,
        a),
    positive = if_else(
      b < 0,
      a,
      b)) %>% 
  select(
    a = negative,
    b = positive)

# hi Brian, please ignore the below, it is just so I can remember that I can also do
#   the above with transmute too (but obviously transmute is not included in the 
#   functions allowed for this question set)!

backwards_columns %>% 
  transmute(
    negative = 
      if_else(
        a > 0,
        b,
        a),
    positive = if_else(
      b < 0,
      a,
      b)) %>% 
  rename(a = negative,
         b = positive)

# Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
  pluck("question_4")

# 5. regex-cellence -------------------------------------------------------

# the dataset `bad_birds` is a vector. Fix the spellings of the common names
# to "House finch", "House sparrow", and "House wren":

bad_birds %>% 
  str_replace(".*h$", "House finch") %>% 
  str_replace(".*w$", "House sparrow") %>% 
  str_replace(".*n$", "House wren")

# Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
  pluck("question_5")

# 6-7. anti and semi join -------------------------------------------------

# The list object `matching_list_tables` contains two tables, `boy` and `howdy`.
# The column `heroes` in `boy` corresponds with the column `hello` in `howdy`.

# Without using filter, subset the table `boy` in `matching_list_tables` such
# that only rows in which the value of column `heroes` has a corresponding value
# in the column `hello` of table `howdy`:

matching_list_tables %>% 
  pluck("boy") %>% 
  semi_join(
    matching_list_tables %>% 
      pluck("howdy") %>% 
      select(heroes = hello),
    by = "heroes")

# Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
  pluck("question_6")

# Without using filter, subset the table `boy` in `matching_list_tables` such
# that only rows in which the value of column `heroes` does not have a
# corresponding value in the column `hello` of table `howdy`:

matching_list_tables %>% 
  pluck("boy") %>% 
  anti_join(
    matching_list_tables %>% 
      pluck("howdy") %>% 
      select(heroes = hello),
    by = "heroes")

# Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
  pluck("question_7")

# 8. butterflies and moths ------------------------------------------------

# The dataset leps_dc contains all of the observations of Lepidoptera in
# Washington DC in 2021.

# * Please write a comment that describes whether these data are tidy and, 
#   if not, which tidy data rule the tibble violates.

# Not tidy - violates the third rule that each level of observation forms a 
#   table. `common_name` is transitively dependent on `scientific_name` so should
#   be in a different table. There is also no primary key.

# * Subset the data to observations of the three species with the most total
#   number of observations across time and assign the name common_leps to the 
#   resultant object.

common_leps_top_spp <-
  leps_dc %>% 
  summarise(
    n = n(),
    .by = scientific_name) %>% 
  slice_max(order_by = n,
            n = 3) 

common_leps <-
  leps_dc %>% 
  semi_join(
    common_leps_top_spp,
    by = "scientific_name") 

rm(common_leps_top_spp)

# Hi Brian, sorry I know I probably wasn't meant to assign things but I wasn't sure
#   how to do it without assigning or using arrange and won't have time to
#   attend the live session unfortunately!

# Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
  pluck("question_8")

# Extra credit 1. Using a map function, create three bar plots of the number of
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
      mutate(
        month = month(date, 
                      label = TRUE)) %>% 
      summarise(
        n = n(),
        .by = month) %>% 
      ggplot(
        aes(x = month, 
            y = n)) +
      geom_bar(stat = "identity") +
      labs(
        title = .x,
        x = "Month",
        y = "Number of Observations"))

# 9. seems like old times -------------------------------------------------

# The object `dates_and_times` contains a date and two times in which an
# event occurred. Please modify the object `dates_and_times`:

# * Convert date to an ISO-8601 date object.
# * Convert time1 and time2 to a ISO-8601 date-time object.
# * Subset the data to where time2 occurs after time1.
# * Replace time1 and time2 with the hour associated with the time.

dates_and_times %>% 
  mutate(date = mdy(date),
         time1 = ymd_hms(str_c(date, time1)),
         time2 = ymd_hms(str_c(date, time2))) %>% 
  filter(time1 < time2) %>% 
  mutate(
    time1 = hour(time1),
    time2 = hour(time2))

# Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
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
    species = 
      case_when(
        size < 10 & volume <= 30 ~ "Deer mouse",
        size < 10 & volume > 30 ~ "House wren",
        size >= 10 & volume <= 30 ~ "Three-toed sloth",
        size >= 10 & volume > 30 ~ "Howler monkey"))

# Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
  pluck("question_10")

# Extra credit 2. Create a bar plot of the number of observations of each
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
        size >= 10 & volume > 30 ~ "Howler monkey")) %>% 
  summarise(
    observations = n(),
    .by = species) %>%
  mutate(
    species =
      fct_reorder(species, 
                  observations,
                  .desc = FALSE)) %>% 
  ggplot() +
  aes(
    x = species,
    y = observations) +
  geom_bar(stat = "identity") +
  coord_flip()
