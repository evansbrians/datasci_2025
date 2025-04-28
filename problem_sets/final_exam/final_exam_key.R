
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

# * Ensure that all times in the data frame are formatted as ISO-8601 datetime values and stored as datetime objects. *Note: This includes the columns `crash_time`, `county_sunrise`, and `county_sunset`*.
# * Remove any transitive columns.
# * Arrange the resultant table from earliest to most recent collisions. 
# * Globally assign the name `collisions_date_fix` to the resultant object.
# * Remove the name `collisions` from your global environment.

collisions_date_fix <- 
  
  # Subset to US dates and convert to ISO-8601 dates:
  
  collisions %>%
  filter(
    str_detect(date, "^[A-Z]")
  ) %>% 
  mutate(
    date = mdy(date)
  ) %>% 
  
  # Bind with modified European dates:
  
  bind_rows(
    
    # Subset to US dates and convert to ISO-8601 dates:
    
    collisions %>%
      filter(
        !str_detect(date, "^[A-Z]")
      ) %>% 
      mutate(
        date = dmy(date)
      )
  ) %>%
  
  # Combine dates with times and make into ISO-8601 date-time objects:
  
  mutate(
    across(
      c(
        crash_time, 
        county_sunrise, 
        county_sunset
      ),
      ~ str_c(
        date, 
        .x, 
        sep = " "
      ) %>% 
        ymd_hms()
    )
  ) %>% 
  
  # Remove unnecessary columns and arrange:
  
  select(!date) %>% 
  arrange(crash_time)

# Another option that was sort of hidden in the regex content (in a 
# section that I said was for advanced users) is:

collisions_date_fix <- 
  collisions %>%
  mutate(
    date = 
      
      # Use regex to convert US dates to European dates:
      
      str_replace(
        date,
        "^([A-Za-z]*) ([0-9]{1,2}), ([0-9]{4})$",
        "\\2 \\1 \\3"
      ) %>% 
      
      # Convert European dates to ISO-8601 dates:
      
      dmy(),
    
    # Combine dates with times and make into ISO-8601 date-time objects:
    
    across(
      c(
        crash_time, 
        county_sunrise, 
        county_sunset
      ),
      ~ str_c(
        date, 
        .x, 
        sep = " "
      ) %>% 
        ymd_hms()
    )
  ) %>% 
  
  # Remove unnecessary columns and arrange:
  
  select(!date) %>% 
  arrange(crash_time)

# ... or:

collisions_date_fix <- 
  collisions %>%
  mutate(
    
    # Use regex to convert US and European dates to ISO-8601 dates:
    
    date = 
      date %>% 
      str_replace(
        "^([A-Za-z]*) ([0-9]{1,2}), ([0-9]{4})$",
        "\\3 \\1 \\2"
      ) %>% 
      str_replace(
        "^([0-9]{1,2}) ([A-Za-z]*) ([0-9]{4})$",
        "\\3 \\2 \\1"
      ),
    
    # Combine dates with times and make into ISO-8601 date-time objects:
    
    across(
      c(
        crash_time, 
        county_sunrise, 
        county_sunset
      ),
      ~ str_c(
        date, 
        .x, 
        sep = " "
      ) %>% 
        ymd_hms()
    )
  ) %>% 
  
  # Remove unnecessary columns and arrange:
  
  select(!date) %>% 
  arrange(crash_time)

# or even:

collisions_date_fix <- 
  collisions %>%
  mutate(
    across(
      c(
        crash_time, 
        county_sunrise, 
        county_sunset
      ),
      
      # Use regex to convert US and European dates to ISO-8601 dates:
      
      ~ date %>% 
        str_replace(
          "^([A-Za-z]*) ([0-9]{1,2}), ([0-9]{4})$",
          "\\3 \\1 \\2"
        ) %>% 
        str_replace(
          "^([0-9]{1,2}) ([A-Za-z]*) ([0-9]{4})$",
          "\\3 \\2 \\1"
        ) %>% 
        str_c(.x, sep = " ") %>% 
        
        # Combine dates with times and make into ISO-8601 date-time objects:
        
        ymd_hms()
    )
  ) %>% 
  
  # Remove unnecessary columns and arrange:
  
  select(!date) %>% 
  arrange(crash_time)

# A way that is faster and more parsimonious still (but includes a function
# that I did not show you, so you would get points off for this -- I am
# including it here only because it is worth knowing):

collisions_date_fix <- 
  collisions %>% 
  
  # Use parse_date_time to convert US and European dates, and all times, to
  # ISO-8601 datetime:
  
  mutate(
    across(
      c(
        crash_time, 
        county_sunrise, 
        county_sunset
      ),
      ~ str_c(
        date, 
        .x, 
        sep = " "
      ) %>% 
        parse_date_time(
          c("bdY HMS", "dbY HMS")
        )
    )
  )  %>% 
  
  # Remove unnecessary columns and arrange:
  
  select(!date) %>% 
  arrange(crash_time)

rm(collisions)

# 4 -----------------------------------------------------------------------

# Repair the `species` column:

# * Ensure that the four species are recorded as (with the first letter
#   capitalized) "Black bear", "Opossum", "Raccoon", and "White-tailed deer".
# * Globally assign the resultant object to the name `collisions_spp_fix`.

# Conservative regex method:

collisions_spp_fix <-
  collisions_date_fix %>% 
  mutate(
    species = 
      species %>% 
      str_replace("^(Virginia )?[Oo0]pp?ossum$", "Opossum") %>% 
      str_replace("^([Cc]ommon )?[Rr]acc?oon$", "Raccoon") %>% 
      str_replace("^([Bb]lack )?b(ear|are)$", "Black bear") %>%
      str_replace(
        "^([Ww]hite(-| )?tailed )?[Dd]e[ae]r$",
        "White-tailed deer"
      )
  )

# I will also accept something like (faster, easier, but dangerous):

collisions_spp_fix <- 
  collisions_date_fix %>% 
  mutate(
    species = 
      case_when(
        str_detect(species, "^[O0oV]") ~ "Opossum",
        str_detect(species, "^[CcRr]") ~ "Raccoon",
        str_detect(species, "^[Bb]") ~ "Black bear",
        .default = "White-tailed deer"
      )
  ) 

# Remove the name collisions_date_fix from your global environment:

rm(collisions_date_fix)

# 5 -----------------------------------------------------------------------

# Some of the geographic coordinates (longitudes and latitudes) were switched in
# the records! Please:

# * Modify the data such that these coordinates are provided in the correct
#   columns.
# * Globally assign the resultant object to the name `collisions_coord_fix`.

# Method 1: Create a temporary column:

collisions_coord_fix <- 
  collisions_spp_fix %>% 
  mutate(
    temp = 
      if_else(
        longitude > 0, 
        latitude, 
        longitude
      ),
    latitude = 
      if_else(
        latitude < 0,
        longitude, 
        latitude
      ),
    longitude = temp
  ) %>% 
  select(!temp)

# Or:

collisions_coord_fix <- 
  collisions_spp_fix %>% 
  mutate(
    longitude = 
      if_else(
        longitude > 0,
        latitude, 
        longitude
      ),
    latitude = 
      if_else(
        latitude < 0,
        pull(., longitude), 
        latitude
      )
  )

# Or better still:

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
    
    # County-level data:
    
    counties =
      collisions_coord_fix %>% 
      select(
        county_id, 
        county_name,
        county_area,
        county_population
      ) %>% 
      distinct(),
    
    # Data in which the level of observation is a county on a given day:
    
    county_sunrise_sunset = 
      collisions_coord_fix %>% 
      select(
        county_id, 
        county_sunrise,
        county_sunset
      ) %>% 
      distinct(),
    
    # Crash level data:
    
    crashes =
      collisions_coord_fix %>% 
      select(
        !c(
          year,
          day,
          county_population,
          county_name:county_sunset
        )
      )
  )

# Remove the name collisions_coord_fix from your global environment:

rm(collisions_coord_fix)

# 7 -----------------------------------------------------------------------

collisions_tidy %>%
  
  # Extract crashes list item:
  
  pluck("crashes") %>% 
  
  # Group and mutate the data in a single step:
  
  group_by(
    species, 
    year = year(crash_time)
  ) %>% 
  
  # Count the number of crashes per species and year:
  
  summarize(
    n = n()
  ) %>% 
  
  # Pivot the table:
  
  pivot_wider(
    names_from = species,
    values_from = n
  )

# Or, with a minor deduction for parsimony:

collisions_tidy %>%
  
  # Extract crashes list item:
  
  pluck("crashes") %>% 
  
  # Calculate year:
  
  mutate(
    year = year(crash_time)
  ) %>% 
  
  # Count the number of crashes per species and year:
  
  summarize(
    n = n(),
    .by = c(species, year)
  ) %>% 
  
  # Pivot the table:
  
  pivot_wider(
    names_from = species,
    values_from = n
  )

# 8 -----------------------------------------------------------------------

# Bare minimum (plot-wise):

collisions_tidy %>%
  
  # Extract crashes list item:
  
  pluck("crashes") %>% 
  
  # Group and mutate the data in a single step:
  
  group_by(
    species, 
    year = year(crash_time)
  ) %>% 
  
  # Count the number of crashes per species and year:
  
  summarize(
    n = n(),
    .groups = "drop"
  ) %>%
  
  # Plot the data:
  
  ggplot() +
  aes(
    x = year, 
    y = n
  ) +
  geom_point() +
  geom_line() +
  facet_wrap(
    ~ species, 
    scales = "free"
  )

# 9 -----------------------------------------------------------------------

# Bare minimum (plot-wise):

collisions_tidy %>%
  
  # Extract crashes list item:
  
  pluck("crashes") %>% 
  
  # Extract a unique character vector of species:
  
  pull(species) %>% 
  unique() %>% 
  
  # Iterate across species:
  
  map(
    ~ collisions_tidy %>%
      
      # Extract crashes list item:
      
      pluck("crashes") %>% 
      
      # Subset to species:
      
      filter(species == .x) %>% 
      
      # Group by year:
      
      group_by(
        year = year(crash_time)
      ) %>% 
      
      # Count the number of observations per year:
      
      summarize(
        n = n()
      ) %>% 
      
      # Plot the data:
      
      ggplot() +
      aes(
        x = year, 
        y = n
      ) +
      geom_bar(stat = "identity") +
      labs(title = .x)
  )

# 10 ----------------------------------------------------------------------

collisions_tidy %>%
  
  # Extract counties list item:
  
  pluck("counties") %>% 
  
  # Subset to counties NOT in the counties below:
  
  anti_join(
    collisions_tidy %>%
      
      # Extract crashes list item:
      
      pluck("crashes") %>%
      
      # Subset to reported crashes with Opossums and Raccoons in 2017:
      
      filter(
        year(crash_time) == 2017,
        species %in% c("Opossum", "Raccoon")
      ),
    by = "county_id"
  ) %>% 
  
  # Extract a character vector of county names:
  
  pull(county_name)

# 11 ----------------------------------------------------------------------

# Create and globally assign a data frame of crashes by year and county_id:

crashes_by_year_and_county <- 
  collisions_tidy %>%
  
  # Extract crashes list item:
  
  pluck("crashes") %>%
  
  # Group and mutate the data in a single step:
  
  group_by(
    year = year(crash_time),
    county_id
  ) %>% 
  
  # Count the number of crashes by year and county and drop the grouping:
  
  summarize(
    crashes = n(),
    .groups = "drop"
  )

# Create and globally assign

big_10_counties <- 
  collisions_tidy %>%
  
  # Extract crashes list item:
  
  pluck("crashes") %>% 
  
  # Count the number of crashes by county, across time:
  
  summarize(
    crashes = n(),
    .by = county_id
  ) %>% 
  
  # Subset to the 10 counties with the most crashes:
  
  slice_max(crashes, n = 10) %>% 
  
  # Join with county data (order could be reversed with an inner_join):
  
  left_join(
    collisions_tidy %>%
      pluck("counties") %>% 
      select(county_id, county_name),
    by = "county_id"
  ) %>% 
  
  # Order counties by the total number of crashes across time:
  
  mutate(
    county_name = fct_reorder(county_name, crashes)
  ) %>% 
  
  # Remove crashes:
  
  select(!crashes)

# Plot the data (bare minimum):

crashes_by_year_and_county %>% 
  
  # Subset to crashes in the top 10 counties:
  
  inner_join(
    big_10_counties,
    by = "county_id"
  ) %>% 
  ggplot() +
  aes(x = county_name, y = crashes) +
  geom_boxplot() +
  coord_flip()

# 12 ----------------------------------------------------------------------

# Bare minimum:

collisions_tidy %>%
  
  # Extract crashes list item:
  
  pluck("crashes") %>% 
  
  # Group and mutate the data in a single step:
  
  group_by(
    species,
    
    # Calculate seasons:
    
    season = 
      case_when(
        month(crash_time) %in% 3:5 ~ "Spring",
        month(crash_time) %in% 6:8 ~ "Summer",
        month(crash_time) %in% 9:11 ~ "Fall", 
        TRUE ~ "Winter"
      ) %>% 
      
      # Order time-of-year, from Winter through Fall:
      
      fct_relevel(
        c(
          "Winter",
          "Spring",
          "Summer",
          "Fall"
        )
      )
  ) %>% 
  
  # Count the number of crashes per species and season:
  
  summarize(
    n = n()
  ) %>% 
  
  # Plot the data:
  
  ggplot() +
  aes(
    x = season, 
    y = n, 
    fill = species) +
  geom_bar(stat = "identity") +
  theme(
    axis.text = element_text(size = 20)
  )

# extra credit 1 ----------------------------------------------------------

# Complete questions 2-5 in a single piped statement

collisions_clean <- 
  
  # Generate separate data frames of US dates and convert to ISO-8601:
  
  read_rds("data/raw/va_wildlife_collisions.rds") %>% 
  filter(
    str_detect(date, "^[A-Z]")
  ) %>% 
  mutate(
    date = mdy(date)
  ) %>% 
  
  # Generate separate data frames of European dates, convert to ISO-8601, and
  # bind with the above:
  
  bind_rows(
    read_rds("data/raw/va_wildlife_collisions.rds") %>% 
      filter(
        !str_detect(date, "^[A-Z]")
      ) %>% 
      mutate(
        date = dmy(date)
      )
  ) %>%
  
  # Transform variables
  
  mutate(
    
    # Repair dates and times
    
    across(
      c(
        crash_time, 
        county_sunrise, 
        county_sunset
      ),
      ~ str_c(
        date, 
        .x, 
        sep = " "
      ) %>% 
        ymd_hms()
    ),
    
    # Repair species names:
    
    species = 
      species %>% 
      str_replace("(Virginia )?[Oo0]pp?ossum", "Opossum") %>% 
      str_replace("([Cc]ommon )?[Rr]acc?oon", "Raccoon") %>% 
      str_replace("([Bb]lack )?b(ear|are)", "Black bear") %>%
      str_replace(
        "([Ww]hite(-| )?tailed )?[Dd]e[ae]r",
        "White-tailed deer"
      ),
    
    # Repair longitudes and latitudes:
    
    temp = 
      if_else(
        longitude > 0, 
        latitude, 
        longitude
      ),
    latitude = 
      if_else(
        latitude < 0, 
        longitude, 
        latitude
      ),
    longitude = temp
  ) %>% 
  
  # Remove the temporary columns:
  
  select(
    !c(date, temp)
  ) %>% 
  
  # Arrange by time of collision:
  
  arrange(crash_time)

# Or (full regex solution for date repair):

read_rds("data/raw/va_wildlife_collisions.rds") %>%
  
  # Transform variables
  
  mutate(
    
    # Repair dates and times
    
    date = 
      date %>% 
      str_replace(
        "^([A-Za-z]*) ([0-9]{1,2}), ([0-9]{4})$",
        "\\3 \\1 \\2"
      ) %>% 
      str_replace(
        "^([0-9]{1,2}) ([A-Za-z]*) ([0-9]{4})$",
        "\\3 \\2 \\1"
      ),
    across(
      c(
        crash_time, 
        county_sunrise, 
        county_sunset
      ),
      ~ str_c(
        date, 
        .x, 
        sep = " "
      ) %>% 
        ymd_hms()
    ),
    
    # Repair species names:
    
    species = 
      species %>% 
      str_replace("(Virginia )?[Oo0]pp?ossum", "Opossum") %>% 
      str_replace("([Cc]ommon )?[Rr]acc?oon", "Raccoon") %>% 
      str_replace("([Bb]lack )?b(ear|are)", "Black bear") %>%
      str_replace(
        "([Ww]hite(-| )?tailed )?[Dd]e[ae]r",
        "White-tailed deer"
      ),
    
    # Repair longitudes and latitudes:
    
    temp = 
      if_else(
        longitude > 0,
        latitude,
        longitude
      ),
    latitude = 
      if_else(
        latitude < 0, 
        longitude, 
        latitude
      ),
    longitude = temp
  ) %>% 
  
  # Remove the temporary columns:
  
  select(
    !c(date, temp)
  ) %>% 
  
  # Arrange by time of collision:
  
  arrange(crash_time)

# Another option:

read_rds("data/raw/va_wildlife_collisions.rds") %>%
  
  # Transform variables
  
  mutate(
    
    # Repair dates and times
    
    date = 
      date %>% 
      str_replace(
        "^([A-Za-z]*) ([0-9]{1,2}), ([0-9]{4})$",
        "\\3 \\1 \\2"
      ) %>% 
      str_replace(
        "^([0-9]{1,2}) ([A-Za-z]*) ([0-9]{4})$",
        "\\3 \\2 \\1"
      ),
    across(
      c(
        crash_time, 
        county_sunrise, 
        county_sunset
      ),
      ~ str_c(
        date, 
        .x, 
        sep = " "
      ) %>% 
        ymd_hms()
    ),
    
    # Repair species names:
    
    species = 
      species %>% 
      str_replace("(Virginia )?[Oo0]pp?ossum", "Opossum") %>% 
      str_replace("([Cc]ommon )?[Rr]acc?oon", "Raccoon") %>% 
      str_replace("([Bb]lack )?b(ear|are)", "Black bear") %>%
      str_replace(
        "([Ww]hite(-| )?tailed )?[Dd]e[ae]r",
        "White-tailed deer"
      ),
    
    # Repair longitudes and latitudes:
    
    longitude = 
      if_else(
        longitude > 0, 
        latitude, 
        longitude
      ),
    latitude = 
      if_else(
        latitude < 0, 
        pull(., longitude), 
        latitude
      )
  ) %>% 
  
  # Remove the temporary column:
  
  select(!date) %>% 
  
  # Arrange by time of collision:
  
  arrange(crash_time)

# Yet another option:

read_rds("data/raw/va_wildlife_collisions.rds") %>%
  
  # Transform variables
  
  mutate(
    
    # Repair dates and times
    
    date = 
      date %>% 
      str_replace(
        "^([A-Za-z]*) ([0-9]{1,2}), ([0-9]{4})$",
        "\\3 \\1 \\2"
      ) %>% 
      str_replace(
        "^([0-9]{1,2}) ([A-Za-z]*) ([0-9]{4})$",
        "\\3 \\2 \\1"
      ),
    across(
      c(
        crash_time, 
        county_sunrise, 
        county_sunset
      ),
      ~ str_c(
        date,
        .x, 
        sep = " ") %>% 
        ymd_hms()
    ),
    
    # Repair species names:
    
    species = 
      species %>% 
      str_replace("(Virginia )?[Oo0]pp?ossum", "Opossum") %>% 
      str_replace("([Cc]ommon )?[Rr]acc?oon", "Raccoon") %>% 
      str_replace("([Bb]lack )?b(ear|are)", "Black bear") %>%
      str_replace(
        "([Ww]hite(-| )?tailed )?[Dd]e[ae]r",
        "White-tailed deer"
      ),
    
    # Repair longitudes and latitudes:
    
    across(
      longitude:latitude,
      \(x) {
        if_else(
          x < 0,
          longitude,
          latitude
        )
      }
    )
  ) %>% 
  
  # Remove the temporary column:
  
  select(!date) %>% 
  
  # Arrange by time of collision:
  
  arrange(crash_time)


# extra credit 2 ----------------------------------------------------------

# Complete memo 11 in a single piped statement:

collisions_tidy %>%
  
  # Extract crashes list item:
  
  pluck("crashes") %>%
  
  # Group and mutate the data in a single step:
  
  group_by(
    year = year(crash_time),
    county_id
  ) %>% 
  
  # Count the number of crashes by year and county and drop the grouping:
  
  summarize(
    crashes = n(),
    .groups = "drop"
  ) %>% 
  
  # Join summary information from crashes (with only matching records):
  
  inner_join(
    collisions_tidy %>%
      
      # Extract crashes list item:
      
      pluck("crashes") %>% 
      
      # Count the number of crashes by county, across time:
      
      summarize(
        crashes = n(),
        .by = county_id
      ) %>% 
      
      # Subset to the 10 counties with the most crashes:
      
      slice_max(crashes, n = 10) %>% 
      
      # Join with county data (order could be reversed with an inner_join):
      
      left_join(
        collisions_tidy %>%
          pluck("counties"),
        by = "county_id"
      ) %>% 
      
      # Order counties by the total number of crashes across time:
      
      mutate(
        county_name = fct_reorder(county_name, crashes)
      ) %>% 
      
      # Remove crashes:
      
      select(!crashes),
    by = "county_id") %>% 
  
  # Plot the data:
  
  ggplot() +
  aes(x = county_name, y = crashes) +
  geom_boxplot() +
  coord_flip()

# extra credit 3 ----------------------------------------------------------

# Do more accidents occur when a vehicle is traveling into or away from the
# sun?

collisions_tidy %>%
  
  # Extract crashes list item:
  
  pluck("crashes") %>%
  
  # Add a date column (for join) and subset to the variables of interest:
  
  mutate(
    county_id,
    crash_time,
    road,
    date = as_date(crash_time),
    .keep = "none"
  ) %>% 
  
  # Join with county sunrise and sunset times by county and date:
  
  left_join(
    collisions_tidy %>%
      
      # Extract county_sunrise_sunset list item:
      
      pluck("county_sunrise_sunset") %>% 
      
      # Add a date column (for join):
      
      mutate(
        date = as_date(county_sunrise)
      ),
    by = join_by(county_id, date)
  ) %>% 
  
  # Remove county_id (no longer necessary):
  
  select(!county_id) %>% 
  
  # Subset the data to ...
  
  filter(
    
    # ... Eastbound or Westbound crashes:
    
    str_detect(road, "[EW]B$"),
    
    # ... crashes that occurred after sunrise:
    
    crash_time > county_sunrise,
    
    # ... crashes that occurred within two hours of sunrise:
    
    crash_time - county_sunrise < 2*3600
  ) %>% 
  
  # Classify to describe whether a crash was into or away from the sun:
  
  mutate(
    sun_or_no = 
      if_else(
        str_detect(road, "EB$"),
        "Into the sun",
        "Away from the sun")
  ) %>% 
  
  # Count the number of crashes into or away from the sun:
  
  summarize(
    n = n(),
    .by = sun_or_no
  ) %>% 
  
  # Plot the data:
  
  ggplot() +
  aes(
    x = sun_or_no, 
    y = n
  ) + 
  geom_bar(stat = "identity")

# Or (just by hour is okay, but a little different):

collisions_tidy %>%
  
  # Extract crashes list item:
  
  pluck("crashes") %>%
  
  # Add a date column (for join) and subset to the variables of interest:
  
  mutate(
    county_id,
    crash_time,
    road,
    date = as_date(crash_time),
    .keep = "none"
  ) %>% 
  
  # Join with county sunrise and sunset times by county and date:
  
  left_join(
    collisions_tidy %>%
      
      # Extract county_sunrise_sunset list item:
      
      pluck("county_sunrise_sunset") %>% 
      
      # Add a date column (for join):
      
      mutate(
        date = as_date(county_sunrise)
      ),
    by = join_by(county_id, date)
  ) %>% 
  
  # Remove county_id (no longer necessary):
  
  select(!county_id) %>% 
  
  # Subset the data to ...
  
  filter(
    
    # ... Eastbound or Westbound crashes:
    
    str_detect(road, "[EW]B$"),
    
    # ... crashes that occurred after sunrise:
    
    crash_time > county_sunrise,
    
    # ... crashes that occurred within two hours of sunrise:
    
    hour(crash_time) - hour(county_sunrise) <= 2) %>% 
  
  # Classify to describe whether a crash was into or away from the sun:
  
  mutate(
    sun_or_no = 
      if_else(
        str_detect(road, "EB$"),
        "Into the sun",
        "Away from the sun")
  ) %>% 
  
  # Count the number of crashes into or away from the sun:
  
  summarize(
    n = n(),
    .by = sun_or_no
  ) %>% 
  
  # Plot the data:
  
  ggplot() +
  aes(
    x = sun_or_no, 
    y = n
  ) + 
  geom_bar(stat = "identity")
