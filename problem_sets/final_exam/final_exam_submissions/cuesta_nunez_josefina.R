
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
    
    # Detect European dates and transform to date class objects:
    
    collisions %>% 
      filter(
        str_detect(date, "^[0-9]")
      ) %>% 
      mutate(
        date = dmy(date)
      ),
    
    # Detect US dates and transform to date class objects:
    
    collisions %>% 
      filter(
        str_detect(date, "^[A-Za-z]")
      ) %>% 
      mutate(
        date = mdy(date)
      )
  ) %>%
  
  # Transform time values to ISO-8601 format and store them as datetime objects
  
  mutate(
    across(
      c(
        crash_time, 
        county_sunrise:county_sunset
      ),
      ~ str_c(
        date, 
        .x
      ) %>% 
        ymd_hms()
    )
  ) %>% 
  
  # Remove transitive columns 
  # date, day and year give the same information as crash_time 
  
  select(
    !c(
      date, 
      day, 
      year
    )
  ) %>% 
  
  # Arrange from earliest to most recent collision
  
  arrange(crash_time)

# Remove the name `collisions` from your global environment:

rm(collisions)

# 4 -----------------------------------------------------------------------

# Repair the `species` column:

# * Ensure that the four species are recorded as (with the first letter
#   capitalized) "Black bear", "Opossum", "Raccoon", and "White-tailed deer".
# * Globally assign the resultant object to the name `collisions_spp_fix`.

collisions_spp_fix <- 
  collisions_date_fix %>% 
  
  # Fix the spelling of species 
  
  mutate(
    species = species %>% 
      str_replace("([Bb]lack )?b[ea]a?re?", "Black bear") %>% 
      str_replace(".*?[Oo0]pp?ossum", "Opossum") %>% 
      str_replace("([Cc]ommon )?[Rr]acc?oon", "Raccoon") %>% 
      str_replace("([Ww]hite[- ]tailed )?[Dd]e[ea]r", "White-tailed deer")
  ) 

# Remove collisions_date_fix from global environment

rm(collisions_date_fix)

# 5 -----------------------------------------------------------------------

# Some of the geographic coordinates (longitudes and latitudes) were switched in
# the records! Please:

# * Modify the data such that these coordinates are provided in the correct
#   columns.
# * Globally assign the resultant object to the name `collisions_coord_fix`.

collisions_coord_fix <- 
  collisions_spp_fix %>% 
  mutate(
    
    # Keep negative longitudes; replace others with corresponding latitude
    
    longitude = 
      if_else(
        longitude < 0, 
        longitude, 
        latitude
      ),
    
    # Keep positive latitudes; replace others with original longitude values
    
    latitude = 
      if_else(
        latitude > 0,
        latitude, 
        pull(., longitude)
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

# The data has three observation levels: collisions, roads, and counties
# The code below tidies the data to follow Hadley's 3rd rule 
# (each table = one level of observation):

collisions_tidy <- 
  
  # Store the resulting tables in a single list
  
  list(
    
    # Collision-level data with a road key for linking to road table
    
    collisions_data = collisions_coord_fix %>% 
      select(
        object_id,
        crash_time:latitude,
        crash_dsc:weather_condition,
        county_sunrise:county_sunset
      ),
    
    # Road-level data: unique road–county combinations
    
    road_data = collisions_coord_fix %>% 
      distinct(road, county_id),
    
    # County-level data: unique county attributes
    
    county_data =  collisions_coord_fix %>% 
      select(
        county_id,
        county_population,
        county_name:county_area
      ) %>%
      distinct() 
    
  )

# Remove the name collisions_coord_fix from your global environment:

rm(collisions_coord_fix)

# [[-1.0]] Incorrect levels of observation for collisions_data and road_data
# (see key)! Note that:
# * You need to have a foreign key in your collision data -- county_id. Without
#   that, you cannot join the tables.
# * The road and county variables do not need to be treated as their own level
#   of observation because a road can span multiple counties.
# * Sunrise and sunset times vary at the level of county and day.

# [[-0.15]] Code formatting:
# * If a code block spans more than one line of code, add a new line after the
#   assignment operator.
# * Code within a single code block should not be separated by blank lines 
#   unless it is separated by a comment.

# 7 -----------------------------------------------------------------------

# Generate a summary table of the total number of the total number of collisions
# by species and year, where the columns are `year` and the names of each
# species:

collisions_tidy %>% 
  
  # Extract the collision-level table
  
  pluck("collisions_data") %>% 
  
  # Keep only date and species information
  
  select(
    crash_time, 
    species
  ) %>% 
  
  # Extract the year from each crash time
  
  mutate(
    year = year(crash_time)
  ) %>% 
  
  # Count collisions per species per year
  
  summarize(
    N = n(), 
    .by = c(species, year)
  ) %>% 
  
  # Make species names columns
  
  pivot_wider(
    names_from = species,
    values_from = N
  )

# [[-0.15]] Code formatting: 
# * Assigned names should be written in snake_case.
# * Maintain one blank line between code blocks and comments. In your version
#   there were additional spaces prior to the section header.

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
  
  # Extract the collision-level table
  
  pluck("collisions_data") %>% 
  
  # Keep only date and species information
  
  select(
    crash_time, 
    species
  ) %>% 
  
  # Extract the year from each crash time
  
  mutate(
    year = year(crash_time)
  ) %>% 
  
  # Count collisions per species per year
  
  summarize(
    N = n(), 
    .by = c(species, year)
  ) %>%
  
  # Start the plot
  
  ggplot()+
  
  # Map year to x axis, collisions to y, color by species
  
  aes(
    y = N,
    x = year,
    colour = species
  ) +
  
  # Generate scatterplot
  
  geom_point(size = 1.7) +
  
  # Connect points with lines
  
  geom_line(linewidth = 1) +
  
  # Facet by species, free y-scale to show trends more clearly
  
  facet_wrap(
    ~ species,
    scales = "free_y",
    ncol = 1
  ) +
  
  # Set axis labels
  
  labs(
    x = "Year",
    y = "Number of collisions"
  ) +
  
  # Use custom species colors
  
  scale_color_manual(
    values = c(
      "Black bear" = "#1C1C1C", 
      "Opossum" = "#7E6A9F", 
      "Raccoon" = "#4682B4", 
      "White-tailed deer" = "#8B4513"
    )
  ) +
  
  # Show all years on x-axis
  
  scale_x_continuous(
    breaks = collisions_tidy %>%
      pluck("collisions_data") %>%
      pull(crash_time) %>%
      year() %>%
      unique()
  ) +
  
  # Clean up background and borders and hide legend (redundant with facets)
  
  theme(
    panel.background = element_blank(),
    strip.background = element_rect(
      colour = "black", 
      linewidth = .7
    ),
    panel.border = element_rect(
      colour = "black", 
      fill = NA, 
      linewidth = .7
    ),
    legend.position = "none"
  )

# [[-0.30]] Code formatting:
# * Infix functions should be separated from surrounding code with a single
#   leading and trailing space.
# * Assigned names should be written in snake_case.
# * If a code block spans more than one line of code, add a new line after the
#   assignment operator.

# [[+1.0]] We like your plot!

# 9 -----------------------------------------------------------------------

# Prove your iteration skills to Chad:

# * Use iteration to generate four separate bar plots where each plot 
#   represents a single species.
# * Ensure that the horizontal axis of each plot represents years.
# * Ensure that the vertical axis represents the total number of collisions
#   with a given species on a given year.
# * Title each plot with the common name of the species.

# Extract the unique species from the dataset

collisions_tidy %>% 
  pluck("collisions_data") %>% 
  pull(species) %>% 
  unique() %>% 
  map(
    
    # For each species, prep the data and plot
    
    ~ collisions_tidy %>% 
      pluck("collisions_data") %>% 
      select(crash_time, species) %>% 
      mutate(
        year = year(crash_time)
      ) %>% 
      summarize(
        N = n(), 
        .by = c(species, year)
      ) %>%
      
      # Filter data for the current species in iteration
      
      filter(species == .x) %>% 
      
      # Plot data - same customization as previous plot, adding:
      # Custom title for each species
      # Removed strip.background since there are no facets anymore
      # Set y-axis limits to avoid displaying negative counts
      
      ggplot() +
      aes(
        x = year, 
        y = N,
        fill = .x
      ) +
      geom_bar(stat = "identity") +
      labs(
        x = "Year",
        y = "Number of collisions",
        
        # Species-specific title
        
        title = .x
      ) +
      scale_fill_manual(
        values = c(
          "Black bear" = "#1C1C1C", 
          "Opossum" = "#7E6A9F", 
          "Raccoon" = "#4682B4", 
          "White-tailed deer" = "#8B4513"
        )
      ) +
      scale_x_continuous(
        breaks = collisions_tidy %>% 
          pluck("collisions_data") %>% 
          pull(crash_time) %>% 
          year() %>% 
          unique()
      ) +
      
      # Start plot at 0 - cant have negative number of collisions
      
      scale_y_continuous(
        expand = c(0, 0)
      ) +
      theme(
        panel.background = element_blank(),
        panel.border = element_rect(
          colour = "black", 
          fill = NA, 
          linewidth = .7
        ),
        legend.position = "none"
      )
  )

# [[-0.15]] Code formatting:
# * Assigned names should be written in snake_case.
# * If a code block spans more than one line of code, add a new line after the
#   assignment operator.

# [[+0.75]] We like your plot, but it is best to have some space at the top
# of your bars (especially when the axis scale does not display those upper
# values).

# 10 ----------------------------------------------------------------------

# Check small animal collision reports. In a single code block, please:

# * Verify whether every county reported at least one Opossum or Raccoon
#   collision in 2017.
# * If you find that some of these counties are missing, please provide me 
#   with a character vector of the offending county names.

collisions_tidy %>% 
  pluck("collisions_data") %>% 
  
  # Filter to include only Opossum and Raccoon collisions from 2017
  
  filter(
    species %in% c("Opossum", "Raccoon"),
    year(crash_time) == 2017 
  ) %>% 
  
  # Identify roads that did NOT report any Opossum or Raccoon collisions.
  # If the resulting tibble is empty, all counties reported a collision.
  
  anti_join(
    collisions_tidy %>% 
      pluck("road_data"),
    .,
    by = "road"
  ) %>% 
  
  # Join with county data to get the county names for said roads
  
  semi_join(
    collisions_tidy %>% 
      pluck("county_data"),
    .,
    by = "county_id"
  ) %>% 
  
  # Extract just the county names as a vector
  
  pull(county_name)

# [[-1.50]] Incorrect: This didn't quite work (see key).

# [[-0.30]] Code parsimony: This could have been greatly simplified.

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

# get the county ID for every road where there was a collision

yearly_collisions <- 
  collisions_tidy %>% 
  pluck("collisions_data") %>% 
  inner_join(
    collisions_tidy %>% 
      pluck("road_data") %>% 
      
      # since a road has always the same county_id, avoid duplicates
      
      distinct(road, .keep_all = TRUE),
    by = "road"
  ) %>% 
  
  # get the name the counties
  
  inner_join(
    collisions_tidy %>% 
      pluck("county_data"),
    by = "county_id"
  ) %>% 
  
  # Extract the year from each crash time
  
  mutate(
    year = year(crash_time),
    county_name = county_name,
    .keep = "none"
  ) %>% 
  
  # Count total collisions per county 
  
  summarize(
    N = n(), 
    .by = c(county_name, year)
  ) 

# get vector of top 10 counties

top_counties <- 
  yearly_collisions %>%
  
  # count the number of crashes per county and sort from most to least
  
  count(
    county_name, 
    wt = N, 
    sort = TRUE
  ) %>% 
  
  # keep only top 10 counties
  
  slice_head(n = 10) %>% 
  pull(county_name)

yearly_collisions %>% 
  filter(county_name %in% top_counties) %>% 
  
  # arrange the data so in the plot it shows from most to least collisions
  
  mutate(
    county_name = fct_reorder(
      county_name, 
      N, 
      .fun = sum, 
      .desc = FALSE
    )
  ) %>% 
  
  # plot the data
  
  ggplot()+
  aes(
    x = N,
    y = county_name,
    fill = county_name
  ) +
  geom_boxplot(alpha = .7) +
  labs(
    x = "Number of collisions",
    y = element_blank(),
    title = "Yearly Distribution of  Collisions in the 10 Most Affected Counties"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1),
    legend.position = "none",
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 11)
  )

# [[-0.30]] Code formatting:
# * Assigned names should be written in snake_case.
# * If a code block spans more than one line of code, add a new line after the
#   assignment operator.
# * Infix functions should be separated from surrounding code with a single
#   leading and trailing space.
# * If you have more than one = in a function, place each argument on its own
#   line.

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
# * Ensure that the fill color of the bars is determined by `species`.


collisions_tidy %>% 
  pluck("collisions_data") %>% 
  
  # Add season information
  
  mutate(
    
    # Extract abbreviated month name in English
    
    month = month(
      crash_time,
      label = TRUE,
      abbr = TRUE,
      locale = "en_US"
    ),
    
    # Assign season based on month
    
    season = 
      case_when(
        month %in% c("Dec", "Jan", "Feb") ~ "Winter",
        month%in% c("Mar", "Apr", "May") ~ "Spring",
        month  %in% c("Jun", "Jul", "Aug") ~ "Summer",
        TRUE ~ "Fall"
      ),
    
    # Keep species column for grouping
    
    species = species,
    
    # Drop all other columns
    
    .keep = "none"
  ) %>% 
  
  # Count collisions per season and species
  
  summarize(
    N = n(),
    .by = c(season, species)
  ) %>% 
  
  # Create plot
  
  ggplot() +
  
  # Map aesthetics
  
  aes(
    x = season,
    y = N,
    fill = species
  ) +
  
  # Draw stacked bars
  
  geom_bar(
    stat = "identity", 
    position = "stack",
    alpha = .8
  ) +
  
  # Add labels and scales
  
  labs(
    x = "Season",
    y = "Number of collisions",
    fill = "Species",
    title = "Seasonal Distribution of Vehicle-Wildlife Collisions"
  ) +
  scale_y_continuous(
    limits = c(0, 20000),
    expand = c(0,0)
  ) +
  scale_fill_manual(
    values = c(
      "Black bear" = "#1C1C1C", 
      "Opossum" = "#7E6A9F", 
      "Raccoon" = "#4682B4", 
      "White-tailed deer" = "#8B4513"
    )
  ) +
  
  # Adjust theme
  
  theme(
    panel.background = element_blank(),
    axis.line = element_line(
      color = "black", 
      linewidth = .7
    ),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(color = "black")
  )

# [[-0.30]] Code formatting:
# * Maintain one blank line between code blocks and comments.
# * Assigned names should be written in snake_case.
# * If a code block spans more than one line of code, add a new line after the
#   assignment operator.
# * If you provide three or more arguments to a function, place each argument
#   on its own line.
# * Infix functions should be separated from surrounding code with a single
#   leading and trailing space.

# [[+1.0]] We like your plot!

# extra credit 1 ----------------------------------------------------------

# Complete questions 2-5 in a single piped statement

read_rds("data/raw/va_wildlife_collisions.rds") %>% 
  {
    process_dates <- function(.data, .pattern, .process) {
      .data %>%
        filter(str_detect(date, .pattern)) %>%
        mutate(date = .process(date))
    }
    
    bind_rows(
      process_dates(., "^[0-9]", dmy),
      process_dates(., "^[A-Za-z]", mdy)
    )
  } %>% 
  mutate(
    across(
      c(
        crash_time, 
        county_sunrise:county_sunset
      ),
      ~ str_c(
        date, 
        .x
      ) %>% 
        ymd_hms()
    )
  ) %>% 
  select(
    !c(date, day, year)
  ) %>% 
  arrange(crash_time) %>% 
  mutate(
    species = species %>% 
      str_replace("([Bb]lack )?b[ea]a?re?", "Black bear") %>% 
      str_replace(".*?[Oo0]pp?ossum", "Opossum") %>% 
      str_replace("([Cc]ommon )?[Rr]acc?oon", "Raccoon") %>% 
      str_replace("([Ww]hite[- ]tailed )?[Dd]e[ea]r", "White-tailed deer")
  ) %>% 
  mutate(
    longitude = 
      if_else(
        longitude < 0, 
        longitude, 
        latitude
      ),
    latitude = 
      if_else(
        latitude > 0,
        latitude, 
        pull(., longitude)
      )
  )

# extra credit 2 ----------------------------------------------------------

# Complete memo 11 in a single piped statement:

# Compute yearly collisions

collisions_tidy %>% 
  pluck("collisions_data") %>% 
  inner_join(
    collisions_tidy %>% 
      pluck("road_data") %>%
      distinct(road, .keep_all = TRUE),
    by = "road"
  ) %>% 
  inner_join(
    collisions_tidy %>% 
      pluck("county_data"),
    by = "county_id"
  ) %>% 
  mutate(
    year = year(crash_time),
    county_name = county_name,
    .keep = "none"
  ) %>% 
  summarize(
    N = n(), 
    .by = c(county_name, year)
  ) %>% 
  
  #  Filter top 10 counties 
  
  semi_join(
    count(
      ., 
      county_name, 
      wt = N, 
      sort = TRUE
    ) %>% 
      slice_head(n = 10),
    by = "county_name"
  ) %>% 
  mutate(
    county_name = fct_reorder(
      county_name, 
      N, 
      .fun = sum, 
      .desc = FALSE
    )
  ) %>% 
  ggplot()+
  aes(
    x = N,
    y = county_name,
    fill = county_name
  ) +
  geom_boxplot(alpha = .7) +
  labs(
    x = "Number of collisions",
    y = element_blank(),
    title = "Yearly Distribution of  Collisions in the 10 Most Affected Counties"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 1),
    legend.position = "none",
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 11)
  )

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

collisions_tidy %>%
  pluck("collisions_data") %>%
  
  # Keep only vehicles traveling Eastbound or Westbound
  
  filter(
    str_detect(road, "EB$|WB$")
  ) %>%
  mutate(
    
    # Extract the hour from crash and sunrise times
    
    crash_hour = hour(crash_time),
    sunrise_hour = hour(county_sunrise),
    
    # Label direction based on road: EB = Into the sun, WB = Away
    
    direction = 
      if_else(
        str_detect(road, "EB$"),
        "Into the sun",
        "Away from the sun"
      )
  )%>%
  
  # Keep crashes that occurred within 0–2 hours after sunrise
  
  filter(
    crash_hour >= sunrise_hour, 
    crash_hour <= sunrise_hour + 2
  ) %>% 
  
  # Calculate time since sunrise (in hours)
  
  mutate(
    hours_after_sunrise = crash_hour - sunrise_hour
  ) %>% 
  
  # Count crashes by direction and hours after sunrise
  
  summarize(
    N = n(),
    .by = c(direction, hours_after_sunrise)
  ) %>% 
  
  # Plot number of collisions by time since sunrise
  
  ggplot() +
  aes(
    x = hours_after_sunrise, 
    y = N, 
    color = direction
  ) +
  geom_line(
    linewidth = 1.5,
    alpha = .4
  ) +
  geom_point(
    size = 7, 
    shape = "🚗"
  ) +
  scale_color_manual(
    values = c(
      "Into the sun" = "#FDB863",
      "Away from the sun" = "#8B9CFF"
    )
  ) +
  labs(
    x = "Time Since Sunrise (hours)",
    y = "Number of Collisions",
    title = "Do More Crashes Occur Driving Into the Sun?",
    color = "Direction of Travel"
  ) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(
      colour = "black", 
      linewidth = 1
    ),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(colour = "black"),
    axis.title = element_text(size = 12)
  )

