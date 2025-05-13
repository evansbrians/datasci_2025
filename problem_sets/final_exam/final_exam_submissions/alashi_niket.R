
# Script file for the final exam

# You may include a maximum of one global assignment per code block. If you
# choose to do so, please remove all previously assigned objects at the end of
# each question.

# For each question that includes a ggplot, you will receive up to 1 point
# extra credit for the styling of your visualizations (e.g., theme elements,
# fill color).

# 1 -----------------------------------------------------------------------

# Open the script file in RStudio and attach the core tidyverse packages to your
# current R session.

library(tidyverse)

# 2 -----------------------------------------------------------------------

# Read in `data/raw/va_wildlife_collisions.rds` and globally assign the
# resultant object to the name `collisions`:

collisions <-
  read_rds("data/raw/va_wildlife_collisions.rds") 

# 3 -----------------------------------------------------------------------

# Fix all variables that include date and/or time values:

# * Ensure that all times in the data frame are formatted as ISO-8601 datetime
#   values and stored as datetime objects. *Note: This includes the columns
#   `crash_time`, `county_sunrise`, and `county_sunset`*.
# * Remove any transitive columns.
# * Arrange the resultant table from earliest to most recent collisions. 
# * Globally assign the name `collisions_date_fix` to the resultant object.

collisions_date_fix <-
  bind_rows(
    
    # Detect European dates and transform 
    
    collisions %>% 
      filter(
        str_detect(date, "^[0-9]")
      ) %>% 
      mutate(
        date = dmy(date)
      ),
    
    # Detect American dates and transform
    
    collisions %>%
      filter(
        str_detect(date, "^[A-Za-z]")
      ) %>% 
      mutate(
        date = mdy(date)
      )
  ) %>%
  mutate(
    
    # combine date with crash_time, county_sunrise and county_sunset
    
    date = ymd_hms(
      str_c(
        as_date(date),
        crash_time,
        sep = " "
      )
    ),
    county_sunrise = ymd_hms(
      str_c(
        as_date(date),
        county_sunrise,
        sep = " "
      )
    ),
    county_sunset = ymd_hms(
      str_c(
        as_date(date),
        county_sunset,
        sep = " "
      )
    )
  ) %>%
  
  # remove transitive columns
  
  select(-crash_time)

# Remove the name `collisions` from your global environment:

rm(collisions)

# [[-1.0]] Incorrect: 
# * Did not remove transitive columns.
# * Did not arrange the table from earliest to most recent collisions.

# [[-0.30]] Code parsimony: Avoid repeating functions. Here, crash_time,
# county_sunrise, and county_sunset operations could have been combined inside
# of `across()`.

# [[-0.15]] Code formatting: If a code block spans more than one line of code,
# add a new line after the assignment operator.

# 4 -----------------------------------------------------------------------

# Repair the `species` column:

# * Ensure that the four species are recorded as (with the first letter
#   capitalized) "Black bear", "Opossum", "Raccoon", and "White-tailed deer".
# * Globally assign the resultant object to the name `collisions_spp_fix`.

collisions_spp_fix <-
  collisions_date_fix %>%
  mutate(
    species = str_replace(species, ".*[Dd]e[ea]r$", "White-tailed deer") , 
    species = str_replace(species, ".*b.*[re]$|bear$", "Black bear") , 
    species = str_replace(species, ".*[Rr]accoon|[Rr]acc?o?on", "Raccoon") , 
    species = str_replace(species, ".*[Oo0]pp?ossum$", "Opossum")
  )

# Remove the name collisions_date_fix from your global environment:

rm(collisions_date_fix)

# [[-0.30]] Code parsimony: You did not need to use the `|` operator in your
# regex for "Black bear" or "Raccoon". You could have searched for all
# variations of a given species within a single search string.

# 5 -----------------------------------------------------------------------

# Some of the geographic coordinates (longitudes and latitudes) were switched in
# the records! Please:

# * Modify the data such that these coordinates are provided in the correct
#   columns.
# * Globally assign the resultant object to the name `collisions_coord_fix`.

collisions_coord_fix <- 
  collisions_spp_fix %>%
  mutate(
    across(
      longitude:latitude,
      ~ if_else(
        .x < 0,
        longitude,
        latitude
      )
    )
  )

# Remove the name `collisions_spp_fix` from your global environment:

rm(collisions_spp_fix)

# 6 -----------------------------------------------------------------------

# Organize the data into a relational database:

# * Make these data database-ready by normalizing (i.e., tidying) the data.
#   Remember to follow all of Hadley’s principles of tidy data!
# * Store the resultant objects within a single list file globally assigned to
#   the name collisions_tidy.

collisions_tidy <- 
  list(
    
    # subsetting collision information
    
    collisions = 
      collisions_coord_fix %>%
      select(
        object_id:day, 
        crash_dsc:weather_condition
      ),
    
    # subsetting county information
    
    county = 
      collisions_coord_fix %>%
      select(
        county_id, 
        county_name:county_sunset,
        county_population
      ) %>%
      distinct()
  )

# Remove the name collisions_coord_fix from your global environment:

rm(collisions_coord_fix)

# [[-1.0]] Incorrect: Missed a level of observation (see key)!

# 7 -----------------------------------------------------------------------

# Generate a summary table of the total number of the total number of collisions
# by species and year, where the columns are `year` and the names of each
# species:

collisions_tidy %>% 
  pluck("collisions") %>% 
  group_by(year, species) %>%
  summarize(
    n = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = species,
    values_from = n
  )

# [[No points removed]] Code parsimony: Because you did not need to calculate
# year, it would have been more parsimonious to use `.by =` instead of
# `group_by`. This would have allowed you to skip the `.groups = "drop"`
# argument.

# 8 -----------------------------------------------------------------------

# Create a visualization of the total number of collisions by species and year:

# * Complete all data processing steps (correctly) prior to piping the data 
#   into `ggplot()`.
# * Create a scatterplot with the total annual collisions on the vertical axis
#   and year on the horizontal axis.
# * Use `geom_line()` to connect the points in the scatterplot.
# * Create a facet for each species.
# * Make sure that the y-axis is scaled so we can see annual variation in each
#   facet. You can do that by setting scales = "free" (check out ?facet_wrap).

collisions_tidy %>% 
  pluck("collisions") %>% 
  group_by(year, species) %>%
  summarize(
    n = n(),
    .groups = "drop"
  ) %>%
  ggplot() +
  aes(
    x = year,
    y = n
  ) +
  geom_point() +
  geom_line() +
  facet_wrap(
    ~ species,
    scales = "free_y"
  ) +
  labs(
    x = "Year",
    y = "Total annual collisions",
    title = "Annual wildlife-vehicle collisions by species"
  ) +
  theme_bw() +
  theme(
    element_text(
      family = "Serif", 
      size = 12
    ),
    element_rect(fill = "white"),
    panel.grid.major = element_line()
  )

# [[+1.0]] We like your plot!

# 9 -----------------------------------------------------------------------

# Prove your iteration skills to Chad:

# * Use iteration to generate four separate bar plots where each plot 
#   represents a single species.
# * Ensure that the horizontal axis of each plot represents years.
# * Ensure that the vertical axis represents the total number of collisions
#   with a given species on a given year.
# * Title each plot with the common name of the species.

collisions_tidy %>% 
  pluck("collisions") %>% 
  pluck("species") %>% 
  unique() %>% 
  map(
    ~ collisions_tidy[["collisions"]] %>% 
      filter(species == .x) %>% 
      group_by(year, species) %>%
      summarize(
        n = n(),
        .groups = "drop"
      ) %>%
      
      # plot bar plots of annual collisions for each species
      
      ggplot() +
      aes(
        x = year, 
        y = n
      ) +
      geom_bar(
        stat = "identity",
        fill = "#556B2F"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 14,
          hjust = 0.5
        ),
        axis.title = 
          element_text(size = 12),
        axis.text = 
          element_text(size = 12)
      ) +
      labs(
        title = .x,
        x = "Year",
        y = "Total annual collisions"
      )
  )

# [[-0.15]] Code formatting: If a code block spans more than one line of code,
# add a new line after the assignment operator.

# [[+0.50]] We like your plot, but when creating a bar plot with count data it
# is best to start your bars at zero.

# [[No points removed]]:
# * It is best to use `pull()` instead of `pluck()` when extracting a variable
#   from a data frame. That lets your reader know the type of object you are
#   extracting from.
# * It is confusing to use `pluck()` and `[[...]]` within the same code block
#   (an odd mix of language forms).

# 10 ----------------------------------------------------------------------

# Check small animal collision reports. In a single code block, please:

# * Verify whether every county reported at least one Opossum or Raccoon
#   collision in 2017.
# * If you find that some of these counties are missing, please provide me 
#   with a character vector of the offending county names.

collisions_tidy %>%
  pluck("collisions") %>% 
  filter(year == 2017) %>%
  left_join(
    collisions_tidy$county,
    by = "county_id") %>%
  distinct(
    county_id,
    county_name
  ) %>%
  filter(
    !county_id %in% (
      collisions_tidy$collisions %>%
        filter(
          year == 2017,
          species %in% c("Opossum", "Raccoon")
        ) %>%
        pull(county_id)
    )
  ) %>%
  pull(county_name)

# [[-1.50]] Incorrect: This didn't quite work (see key).

# [[-0.30]] Code parsimony: This could have been greatly simplified.

# [[-0.15]] Code formatting: If a function spans more than one line of code,
# closing parentheses should be placed on their own line.

# 11 ----------------------------------------------------------------------

# We would like to generate a data visualization that describes the distribution
# of the total number of crashes per year for the 10 counties with the highest
# number of total crashes across years. Please:

# * Complete all data processing steps (correctly) prior to piping the data
#   into ggplot().
# * Visualize the data with a boxplot geometry.
# * Ensure that the horizontal axis of the plot is the number of crashes in a
#   given year and the vertical axis is the names of the counties.
# * Arrange the plot from the county with the most crashes, across time, to
#   the county with the least amount of crashes across time.

# Note: You may use up to two global assignments to address this task!

collisions_tidy %>% 
  pluck("collisions") %>%
  
  # counting number of crashes in each county and selecting top 10 counties
  
  count(county_id) %>%
  slice_max(n, n = 10) %>%
  
  # Join with county names
  
  inner_join(
    collisions_tidy %>% 
      pluck("county"),
    by = "county_id"
  ) %>%
  select(county_id, county_name) %>%
  
  #filtering data for top 10 county names
  
  right_join(collisions_tidy$collisions, by = "county_id", relationship = "many-to-many") %>%
  group_by(county_name, year) %>%
  summarize(
    crashes = n(),
    .groups = "drop"
  ) %>%
  mutate(county_name = fct_reorder(county_name, crashes, .fun = sum)) %>%
  drop_na() %>% 
  
  # generating boxplots 
  
  ggplot(aes(x = crashes, y = county_name)) +
  geom_boxplot() +
  labs(
    x = "Number of crashes per year",
    y = "County",
    title = "Distribution of annual crashes for top 10 counties"
  )

# [[-1.20]] Incorrect: Your numbers ended up very high due to the `right_join()`
# ... never trust a many-to-many relationship unless you really want that!

# [[-1.50]] `right_join` is not among the functions that we have used in this
# course.

# [[-0.30]] Code formatting:
# * Add a single space between the hashtag (#) and comment.
# * Code and comments should not exceed 80 characters in width (if it is
#   avoidable) – add a line break, if possible.
# * If you provide three or more arguments to a function, place each argument
#   on its own line.
# * Include no more than one prefix function per line of code.
# * If you have more than one = in a function, place each argument on its own
#   line.

# 12 ----------------------------------------------------------------------

# Create a visualization of collisions by season (Sept-Nov = Fall; Dec-Feb =
# Winter; Mar-May = Spring; June-Aug = Summer). Please:

# * Complete all data processing steps (correctly) prior to piping the data 
#   into `ggplot()`.
# * Visualize the data with a stacked bar plot geometry.
# * Ensure that the horizontal axis is season, in the order Winter, Spring,
#   Summer, Fall.
# * Ensure that the vertical axis is the total number of collisions, across 
#   years.
# * Ensure that the fill color of the bars is determined by `species`.

# [[-3.0]] Not answered.

# extra credit 1 ----------------------------------------------------------

# Complete questions 2-5 in a single piped statement

read_rds("data/raw/va_wildlife_collisions.rds") %>% 
  {
    bind_rows(
      
      # Detect European dates and transform 
      
      filter(.,
             str_detect(date, "^[0-9]")
      ) %>% 
        mutate(
          date = dmy(date)
        ),
      
      # Detect American dates and transform
      
      filter(.,
             str_detect(date, "^[A-Za-z]")
      ) %>% 
        mutate(
          date = mdy(date)
        )
    )
  } %>%
  
  # combine date with crash_time, county_sunrise and county_sunset
  
  mutate(
    date = ymd_hms(
      str_c(
        as_date(date),
        crash_time,
        sep = " "
      )
    ),
    county_sunrise = ymd_hms(
      str_c(
        as_date(date),
        county_sunrise,
        sep = " "
      )
    ),
    county_sunset = ymd_hms(
      str_c(
        as_date(date),
        county_sunset,
        sep = " "
      )
    )
  ) %>%
  
  # remove transitive columns
  
  select(-crash_time) %>% 
  
  # correct species names
  
  mutate(
    species = str_replace(species, ".*[Dd]e[ea]r$", "White-tailed deer") , 
    species = str_replace(species, ".*b.*[re]$|bear$", "Black bear") , 
    species = str_replace(species, ".*[Rr]accoon|[Rr]acc?o?on", "Raccoon") , 
    species = str_replace(species, ".*[Oo0]pp?ossum$", "Opossum")
  ) %>% 
  
  # Fix swapped coordinates  
  
  mutate(
    across(
      longitude:latitude,
      ~ if_else(
        .x < 0,
        longitude,
        latitude
      )
    )
  )

# [[+2.54]] Nicely done! Note points off for this extra credit reflect the
# points taken off on the original questions 2-5. Therefore, this score
# represents the maximum points available.

# extra credit 2 ----------------------------------------------------------

# Complete memo 11 in a single piped statement:

collisions_tidy %>% 
  pluck("collisions") %>%
  
  # counting number of crashes in each county and selecting top 10 counties
  
  count(county_id) %>%
  slice_max(n, n = 10) %>%
  
  # Join with county names
  
  inner_join(
    collisions_tidy %>% 
      pluck("county"),
    by = "county_id"
  ) %>%
  select(county_id, county_name) %>%
  
  #filtering data for top 10 county names
  
  right_join(collisions_tidy$collisions, by = "county_id", relationship = "many-to-many") %>%
  group_by(county_name, year) %>%
  summarize(
    crashes = n(),
    .groups = "drop"
  ) %>%
  mutate(county_name = fct_reorder(county_name, crashes, .fun = sum)) %>%
  drop_na() %>% 
  
  # generating box plots 
  
  ggplot(aes(x = crashes, y = county_name)) +
  geom_boxplot() +
  labs(
    x = "Number of crashes per year",
    y = "County",
    title = "Distribution of annual crashes for top 10 counties"
  )

# [[+0.25]] We could not award you many points for this because you did not
# receive any points for Q11. However, we gave you some for doing this within
# a single code block without any global assignments.

# extra credit 3 ----------------------------------------------------------

# Do more accidents occur when a vehicle is traveling into or away from the
# sun?

# * Subset to only vehicles traveling East or West.
# * Subset the data to collisions that occurred within two hours after the 
#   sunrise time (Note: You may round the time to hour to address this 
#   question).
# * Classify the incidents that occurred for Eastbound drivers as "Into the 
#   sun" and those that occurred for Westbound drivers as "Away from the sun".
# * Generate a data visualization (of your choosing!) that illustrates whether
#   more incidents occur for vehicles traveling into, or away from, the sun.


