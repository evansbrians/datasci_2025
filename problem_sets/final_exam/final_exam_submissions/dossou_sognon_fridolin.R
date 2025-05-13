
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
  
  # Combine rows from two different date formats (European-style and US-style)
  
  bind_rows(  
    
    # Process European-style dates 
    
    collisions %>% 
      
      # Keep rows where the date starts with a digit (European date style)
      
      filter(
        str_detect(date, "^[0-9]")
      ) %>% 
      
      # Convert European-style string dates to Date objects
      
      mutate(
        date = dmy(date)
      ), 
    
    # Process US-style dates 
    
    collisions %>%
      
      # Keep rows where the date starts with a letter ( US date style)
      
      filter(
        str_detect(date, "^[A-Za-z]")
      ) %>% 
      
      # Convert US-style string dates to Date objects
      
      mutate(
        date = mdy(date)  
      )
  ) %>%
  
  # Convert all time-related columns to full datetime (ISO-8601) using the date
  
  mutate(
    across(
      c(
        crash_time, 
        county_sunrise, 
        county_sunset
      ), 
      
      # Combine date and time into a single datetime string, then parse as 
      # datetime
      
      ~ ymd_hms(
        str_c(date, .x)
      )
    )
  ) %>% 
  
  # Drop intermediate columns like year, day, and the original date
  
  select(
    !c(year:day, date)
  ) %>% 
  
  # Sort the data in ascending order based on crash time
  
  arrange(crash_time)

# Remove the name `collisions` from your global environment:

rm(collisions)

# 4 -----------------------------------------------------------------------

# Repair the `species` column:

# * Ensure that the four species are recorded as (with the first letter
#   capitalized) "Black bear", "Opossum", "Raccoon", and "White-tailed deer".
# * Globally assign the resultant object to the name `collisions_spp_fix`.

# Standardize the species column with proper capitalization and consistent 
# naming

collisions_spp_fix <-
  collisions_date_fix %>% 
  
  # Use `mutate` and `case_when` to correct inconsistent species labels
  
  mutate(
    species = 
      case_when(
        
        # Match variations of "opossum", accounting for typos and change 
        # to "Opossum"
        
        str_detect(species, "[O0o]p?possum") ~ "Opossum", 
        
        # Match variations of "deer", accounting for typos and change 
        # to "White-tailed deer"
        
        str_detect(species, "[Dd]e[ea]r") ~ "White-tailed deer", 
        
        # Match spelling variations of "black bear", including "bare", "bear"
        # and change to "Black bear"
        
        str_detect(species, "b[ae]a?re?") ~ "Black bear", 
        
        # Match variations of "Raccoon",and change to "Raccoon"
        
        str_detect(species, "[Rr]ac?coon") ~ "Raccoon"
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

collisions_coord_fix <- 
  
  # Create corrected 'long' and 'lat' columns based on coordinate logic
  
  collisions_spp_fix %>% 
  mutate(
    
    # If longitude is mistakenly positive, assume it's a latitude value
    
    long = 
      if_else(
        longitude > 0, 
        latitude, 
        longitude), 
    
    # If latitude is mistakenly negative, assume it's a longitude value
    
    lat = 
      if_else(
        latitude < 0, 
        longitude, 
        latitude
      )
  ) %>% 
  
  # Rename long' as 'longitude', lat' as 'latitude' and select relevant columns
  
  select(object_id:species, 
         longitude = long, 
         latitude = lat, 
         county_population:county_sunset
  )

# Remove the name `collisions_spp_fix` from your global environment:

rm(collisions_spp_fix)

# [[-0.15]] Code formatting:
# * If a function spans more than one line of code, the opening parentheses 
#   should be followed by a line break.
# * If a function spans more than one line of code, closing parentheses 
#   should be placed on their own line.

# 6 -----------------------------------------------------------------------

# Organize the data into a relational database:

# * Make these data database-ready by normalizing (i.e., tidying) the data.
#   Remember to follow all of Hadley’s principles of tidy data!
# * Store the resultant objects within a single list file globally assigned to
#   the name collisions_tidy.

collisions_tidy <-
  
  # create a list
  
  list(
    
    # Generate a county-level data frame and assign to the name `county`:
    
    county = 
      collisions_coord_fix %>% 
      select(
        county_id, 
        county_population, 
        county_name:county_area
      ) %>% 
      distinct(), 
    
    # Generate an collision_observation-level data frame and assign to the name
    # `collision_observation`:
    
    collision_observation = 
      collisions_coord_fix %>% 
      select(
        object_id:latitude, 
        crash_dsc, 
        weather_condition
      ), 
    
    # Generate an road-level data frame and assign to the name
    # `road`:
    
    road = 
      collisions_coord_fix %>% 
      distinct(
        county_id, 
        road
      ), 
    
    # Generate an sun_move-level data frame and assign to the name
    # `sun_move`:
    
    sunrise_sunset = 
      collisions_coord_fix %>% 
      select(
        county_id, 
        county_sunset:county_sunrise
      ) %>% 
      distinct()
  )

# Remove the name collisions_coord_fix from your global environment:

rm(collisions_coord_fix)

# [[-0.50]] Incorrect levels of observation for `road` (see key). Note that the
# road and county variables do not need to be treated as their own level of
# observation because a road can span multiple counties.

# 7 -----------------------------------------------------------------------

# Generate a summary table of the total number of the total number of collisions
# by species and year, where the columns are `year` and the names of each
# species:

# Generate a summary table showing total collisions by species and year

collisions_tidy %>% 
  
  # Extract the 'collision_observation' data frame from the collisions_tidy list
  
  pluck("collision_observation") %>% 
  
  # Create a new column 'year' by extracting the year from 'crash_time'
  
  mutate(
    year = year(crash_time)
  ) %>% 
  
  # Summarize the number of collisions grouped by year and species
  
  summarize(
    total_number = n(), 
    .by = c(year, species)
  ) %>% 
  
  # Reshape the data to a wide format, making species names the column headers
  # and using total_number as the cell values
  
  pivot_wider(
    names_from = species, 
    values_from = total_number
  )

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

# Generate a plot showing total number of collisions by species and year

collisions_tidy %>% 
  pluck("collision_observation") %>% 
  mutate(
    
    # Extract year from crash_time
    
    year = year(crash_time)
  ) %>% 
  
  # Count total collisions by year and species
  
  summarize(
    total_number = n(), 
    .by = c(year, species)
  ) %>% 
  
  # Plot 
  
  ggplot() + 
  
  # Set x-axis as year, y-axis as total number of collisions
  
  aes(
    x = year, 
    y = total_number
  ) + 
  
  # Plot points for each data point
  
  geom_point() + 
  
  # Connect the points with lines
  
  geom_line() + 
  
  # Facet the plot by species, allowing different y-axis scales for each
  
  facet_wrap(
    ~ species, 
    nrow = 2, 
    scales = "free_y" 
  ) + 
  
  # Customize labels for axes and title
  
  labs(
    
    x = "Year", 
    y = "Total annual collisions", 
    title = "Total number of collisions by species and year"
  ) + 
  
  # Apply theme adjustments to the plot
  
  theme(
    
    # Set the font to Times New Roman for all text elements
    
    text = element_text(family = "Times New Roman"), 
    
    # Adjust font size for title, axis titles, axis labels, and facet labels
    
    plot.title = element_text(size = 18), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12), 
    strip.text = element_text(size = 14)
  )

# [[-0.15]] Code formatting: Code within a single code block should not be
# separated by blank lines unless it is separated by a comment

# [[+1.0]] We like your plot!

# 9 -----------------------------------------------------------------------

# Prove your iteration skills to Chad:

# * Use iteration to generate four separate bar plots where each plot 
#   represents a single species.
# * Ensure that the horizontal axis of each plot represents years.
# * Ensure that the vertical axis represents the total number of collisions
#   with a given species on a given year.
# * Title each plot with the common name of the species.

# Iterate over each unique species to generate individual bar plots

collisions_tidy %>% 
  pluck("collision_observation") %>% 
  pull(species) %>% 
  
  # get unique species name
  
  unique() %>% 
  
  # Use map() to generate a separate plot for each species
  
  map(
    ~ collisions_tidy %>% 
      pluck("collision_observation") %>%  
      
      # Filter data for the current species
      
      filter(species == .x) %>% 
      
      # Group by year and count the collisions
      
      group_by(
        year = 
          year(crash_time)
      ) %>% 
      summarize(
        number = n()
      ) %>% 
      
      # Create a plot for the current species with "year" on x-axis and "number"
      # on y-axis
      
      ggplot() +
      aes(
        x = year, 
        y = number
      ) +
      
      # Plot bars with a specific fill and border color
      
      geom_bar(stat = "identity", 
               fill = "#aaddff", 
               color = "black") +
      
      # Add title of the plot and the axis
      
      labs(
        x = "Year", 
        y = "Total number of collisions", 
        title = .x) +
      
      # Apply theme adjustments to the plot 
      
      theme(
        
        # change the font associated with all text components of the plot
        
        text = element_text(family = "Times New Roman"), 
        
        # change the font size of each text element 
        
        plot.title = element_text(size = 18), 
        axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12)
      )
  )

# [[-0.15]] Code formatting:
# * If a function spans more than one line of code, closing parentheses 
#   should be placed on their own line.
# * If a function spans more than one line of code, the opening parentheses 
#   should be followed by a line break.

# [[+0.50]] We like your plot, but when creating a bar plot with count data it
# is best to start your bars at zero.

# 10 ----------------------------------------------------------------------

# Check small animal collision reports. In a single code block, please:

# * Verify whether every county reported at least one Opossum or Raccoon
#   collision in 2017.
# * If you find that some of these counties are missing, please provide me 
#   with a character vector of the offending county names.

# Start with the full list of counties

collisions_tidy %>% 
  pluck("county") %>% 
  
  # Anti-join to exclude counties that reported at least one Opossum or Raccoon collision in 2017
  
  anti_join(
    
    # counties that reported at least one Opossum or Raccoon collision in 2017
    
    collisions_tidy %>% 
      pluck("collision_observation") %>% 
      filter(
        year(crash_time) == 2017, 
        str_detect(species, "Opossum|Raccoon")
      ) %>% 
      
      # Get distinct counties with reported collisions
      
      distinct(county_id), 
    
    # join by "county_id"
    
    by = "county_id"
  ) %>% 
  
  # Return the names of counties that are missing such reports
  
  pull(county_name)

# [[-0.30]] Code parsimony: `distinct(county_id)` did not alter the output.

# 11 ----------------------------------------------------------------------

# Generate a visualization of the total number of crashes per year for the 10
# counties with the highest number of total crashes across years. Please:

# * Complete all data processing steps (correctly) prior to piping the data
#   into ggplot().
# * Visualize the data with a boxplot geometry.
# * Ensure that the horizontal axis of the plot is the number of crashes in a
#   given year and the vertical axis is the names of the counties.
# * Arrange the plot from the county with the most crashes, across time, to
#   the county with the least amount of crashes across time.

# Note: You may use up to two global assignments to address this task!

# Add year and merge data for observations with county information and asssign 
# to "observation_per_county"

observation_per_county <- 
  collisions_tidy %>% 
  pluck("collision_observation") %>% 
  
  # Extract the year from crash_time
  
  mutate(
    year = 
      year(crash_time)
  ) %>%   
  
  # Join the county information based on the county_id column
  
  left_join(
    collisions_tidy %>% 
      pluck("county"), 
    by = "county_id"
  )

# Identify the top 10 counties with the most crashes and assign to
# "top_ten_counties"

top_ten_counties <- 
  observation_per_county %>% 
  
  # Count the number of crashes per county
  
  summarize(
    crashes = n(), 
    .by = "county_name"
  ) %>% 
  
  # Extract the top 10 counties with the most crashes
  
  slice_max(
    crashes, 
    n = 10
  ) %>% 
  
  # Pull the names of the top 10 counties
  
  pull(county_name)

# Filter the data to only include the top 10 counties and prepare for plotting

observation_per_county %>% 
  filter(
    county_name %in% top_ten_counties
  ) %>% 
  
  # Count the number of crashes per year per county
  
  summarize(
    number = n(), 
    .by = c(county_name, year)
  ) %>% 
  
  # Reorder the counties based on the number of crashes for better visualization
  
  mutate(
    county_name = 
      fct_reorder(county_name, number)
  ) %>% 
  ggplot() + 
  
  # Set the x-axis as the number of crashes and y-axis as the county names
  
  aes(
    x = number, 
    y = county_name
  ) + 
  
  # Create a boxplot to visualize the distribution of crashes by county and year
  
  geom_boxplot(
    fill = "#FE872A", 
    outliers = FALSE
  ) + 
  
  # Add title of the plot and the axis
  
  labs(
    x = "Number of crashes in a given year", 
    y = "Counties", 
    title = "Total number of crashes per year for the 10 top counties"
  ) + 
  
  # Apply theme adjustments
  
  theme(
    
    # change the font associated with all text components 
    
    text = element_text(family = "Times New Roman"), 
    
    # change the font size of each text element 
    
    plot.title = element_text(size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12), 
    strip.text = element_text(size = 14)
  )

rm(top_ten_counties, observation_per_county)

# [[No points removed]]: You were not asked to remove the outliers ... they are
# often important in a boxplot!

# [[+1.0]] We like your plot!

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

collisions_tidy %>% 
  pluck("collision_observation") %>% 
  mutate(
    
    # Extract month from crash_time
    
    month = 
      month(crash_time, 
            label = TRUE), 
    season = 
      case_when(
        str_detect(month, "Sep|Oct|Nov") ~ "Fall", 
        str_detect(month, "Dec|Jan|Feb") ~ "Winter", 
        str_detect(month, "Mar|Apr|May") ~ "Spring", 
        str_detect(month, "Jun|Jul|Aug") ~ "Summer", 
      ), 
    
    # Ensure correct order of season
    
    season = fct_relevel(season, 
                         "Winter", 
                         "Spring", 
                         "Summer", 
                         "Fall"
    ), 
    year = year(crash_time)
  ) %>% 
  
  # Count total number of collisions per season and year
  
  summarize(
    number = n(), 
    .by = c("season", "year")
  ) %>% 
  
  # Fill by season to create a stacked bar plot
  
  ggplot() + 
  aes(
    x = season, 
    y = number, 
    fill = as.factor(year)
  ) +
  geom_bar(stat = "identity") + 
  theme(
    
    # change the font associated with all text components 
    
    text = element_text(family = "Times New Roman"), 
    
    # change the font size of each text element 
    
    plot.title = element_text(size = 16), 
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12), 
    strip.text = element_text(size = 14)
    
  )

# [[-1.50]] `as.factor()` is not a function that we used in this course.

# [[-0.30]] Code parsimony: Quotes are not necessary when referring to a 
# variable name or names with `.by = ...`.

# [[-0.30]] Code formatting:
# * If a function spans more than one line of code, the opening parentheses 
#   should be followed by a line break.
# * If a code block spans more than one line of code, add a new line after the
#   assignment operator.
# * Code within a single code block should not be separated by blank lines
#   unless it is separated by a comment.

# extra credit 1 ----------------------------------------------------------

# Complete questions 2-5 in a single piped statement

# Fix date-time columns 

# Standardize the date column based on format 

bind_rows(  
  
  # European-style dates
  
  read_rds("data/raw/va_wildlife_collisions.rds") %>% 
    filter(
      str_detect(date, "^[0-9]")
    ) %>% 
    mutate(
      date = dmy(date)
    ), 
  
  # US-style dates
  
  read_rds("data/raw/va_wildlife_collisions.rds") %>%
    filter(
      str_detect(date, "^[A-Za-z]")
    ) %>% 
    mutate(
      date = mdy(date)
    )
) %>%
  
  # Convert all time-related columns to proper datetime format using the cleaned
  # date
  
  mutate(
    across(
      c(
        crash_time, 
        county_sunrise, 
        county_sunset
      ), 
      ~ ymd_hms(
        str_c(date, .x)
      )
    ), 
    
    # Standardize species names to four expected categories
    
    species = 
      case_when(
        str_detect(species, "[O0o]p?possum") ~ "Opossum", 
        str_detect(species, "[Dd]e[ea]r") ~ "White-tailed deer", 
        str_detect(species, "b[ae]a?re?") ~ "Black bear", 
        str_detect(species, "[Rr]ac?coon") ~ "Raccoon"
      ), 
    
    # Fix coordinate issues
    
    long = 
      if_else(
        longitude > 0, 
        latitude, 
        longitude), 
    lat = 
      if_else(
        latitude < 0, 
        longitude, 
        latitude
      )
  ) %>% 
  
  # Arrange rows chronologically by crash time
  
  arrange(crash_time) %>% 
  
  # Select and rename relevant columns
  
  select(
    object_id:county_id, 
    crash_time:species, 
    longitude = long, 
    latitude = lat, 
    county_population:county_sunset
  )

# extra credit 2 ----------------------------------------------------------

# Complete memo 11 in a single piped statement:

# extra credit 3 ----------------------------------------------------------

# Do more accidents occur when a vehicle is traveling into or away from the
# sun?

# * Subset to only vehicles traveling East or West.
# * Subset the data to collisions that occurred within two hours of the sunrise
#   time (Note: You may round the time to hour to address this question).
# * Classify the incidents that occurred for Eastbound drivers as "Into the 
#   sun" and those that occurred for Westbound drivers as "Away from the sun".
# * Generate a data visualization (of your choosing!) that illustrates whether
#   more incidents occur for vehicles traveling into, or away from, the sun.


