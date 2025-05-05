
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
    A = a,
    B = b,
    a = 
      if_else(
        A > 0,
        -B,
        A
      ),
    b = 
      if_else(
        B < 0,
        -A,
        B
      )
  ) %>%
  select(-A, -B)

# 5 -----------------------------------------------------------------------

# The dataset `bad_birds` is a vector. Fix the spellings of the common names
# to "House finch", "House sparrow", and "House wren":

bad_birds <- 
  map(
    bad_birds,
    \(name) {
      case_when(
        str_detect(
          name,
          "^[Hh].*[Ff]in."
        ) ~ "House finch",
        str_detect(
          name,
          "^[Hh].*[Ss].\\w"
        ) ~ "House sparrow",
        str_detect(
          name,
          "^[Hh].*w?r."
        ) ~ "House wren",
        TRUE ~ name
      )
    }
  ) %>% unlist()

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
    by = 
      join_by(heroes == hello)
  )

# Without using filter, subset the table `boy` in `matching_list_tables` such
# that only rows in which the value of column `heroes` does not have a
# corresponding value in the column `hello` of table `howdy`:

matching_list_tables %>% 
  pluck("boy") %>% 
  anti_join(
    matching_list_tables %>% 
      pluck("howdy"),
    by = 
      join_by(heroes == hello)
  )

# 7 -----------------------------------------------------------------------

# The dataset `leps_dc` contains all of the observations of Lepidoptera in
# Washington DC in 2021.

# * Please write a comment that describes whether these data are tidy and, 
#   if not, which tidy data rule the tibble violates:

# * The dataset 'leps_dc' violates the third rule of tidy data - 'Every level
#   of observation forms a table'. The column 'common_name' is a quality of
#   'scientific_name' which are just 268 variables.

# * Subset the data to observations of the three species with the most total
#   number of observations across time and assign the name common_leps to the 
#   resultant object:

common_leps <- 
  leps_dc %>%
  semi_join(
    leps_dc %>%
      count(
        scientific_name,
        common_name
      ) %>%
      slice_max(
        n, 
        n = 3
      ) %>%
      select(
        scientific_name,
        common_name
      ),
    by = 
      join_by(
        scientific_name,
        common_name
      )
  )

# 8 -----------------------------------------------------------------------

# Using `common_leps` and iteration, create three bar plots of the number of
# observations per month for each of the three most commonly observed
# Lepidoptera species. In doing so:

# * Label the x-axis with month abbreviations (e.g., Apr, May, Jun);
# * Include the scientific name of the species as the title of the plot.

map(
  common_leps %>% 
    pluck("scientific_name") %>% 
    unique(),
  ~ common_leps %>%
    filter(scientific_name == .x) %>%
    mutate(
      month =
        month(
          date, 
          label = TRUE
        )
    ) %>%
    count(month) %>%
    ggplot() + 
    aes(
      x = month,
      y = n
    ) +
    geom_bar(
      stat = "identity",
      fill = "blue",
      color = "black"
    ) +
    scale_y_continuous(
      expand = c(0,0)
    ) +
    labs(
      x = "Month",
      y = "Observations",
      title = .x
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
    time1 = ymd_hms(
      str_c(
        date,
        time1,
        sep = " "
      )
    ),
    time2 = ymd_hms(
      str_c(
        date,
        time2,
        sep = " "
      )
    ) 
  ) %>%
  filter(time2 > time1) %>%
  mutate(
    time1 = 
      hour(time1),
    time2 = 
      hour(time2)
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

size_and_volume <- 
  size_and_volume %>%
  mutate(
    species = 
      case_when(
        size < 10 & volume <= 30 
        ~ "Deer mouse",
        size < 10 & volume > 30  
        ~ "House wren",
        size >= 10 & volume <= 30 
        ~ "Three-toed sloth",
        size >= 10 & volume > 30  
        ~ "Howler monkey"
      )
  )

# Extra credit: Create a bar plot of the number of observations of each
# species where:

# * Species are on the y-axis and the count is on the x-axis;
# * The bars are arranged by the number of observations per species.

size_and_volume %>%
  group_by(species) %>%
  summarize(
    n = n()
  ) %>% 
  
  # plot number of observations using ggplot
  
  ggplot() + 
  aes(
    x = fct_reorder(
      species,
      n
    ),
    y = n
  ) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "Species",
    y = "Number of Observations"
  )
