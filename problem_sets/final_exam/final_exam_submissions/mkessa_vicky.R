
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

collisions <- read_rds("data/raw/va_wildlife_collisions.rds")

# 3 -----------------------------------------------------------------------

# Fix all variables that include date and/or time values:

# * Ensure that all times in the data frame are formatted as ISO-8601 datetime
#   values and stored as datetime objects. *Note: This includes the columns
#   `crash_time`, `county_sunrise`, and `county_sunset`*.
# * Remove any transitive columns.
# * Arrange the resultant table from earliest to most recent collisions. 
# * Globally assign the name `collisions_date_fix` to the resultant object.

collisions_date_fix <- 

  # detect, filter and bind US and European dates 
  
  bind_rows(
    
    # detect European dates and transform to date class object
    
    collisions %>% 
      filter(
        str_detect(date, "^[0-9]")
      ) %>% 
      mutate(
        date = dmy(date)
      ),
    
    # detect US dates and transform to date class object
    
    collisions %>% 
      filter(
        str_detect(date, "^[A-Za-z]")
      ) %>% 
      mutate(
        date = mdy(date)
      )
  ) %>% 
  
  # combine crash_time, sunrise and sunset with date and convert to ISO datetime
  # format
  
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
  
  # use negation to exclude transitive columns 
  
  select(
    !c(date, year, day)
  ) %>% 
  
  # arrange rows starting with the earliest incidents
  
  arrange(crash_time)

# Remove the name `collisions` from your global environment:

rm(collisions)

#  [[-0.15]] Code formatting: If you provide three or more arguments to a
#  function, place each argument on its own line.

# 4 -----------------------------------------------------------------------

# Repair the `species` column:

# * Ensure that the four species are recorded as (with the first letter
#   capitalized) "Black bear", "Opossum", "Raccoon", and "White-tailed deer".
# * Globally assign the resultant object to the name `collisions_spp_fix`.

collisions_spp_fix <- 
  collisions_date_fix %>%
  
  # using mutate and regex to replace the different strings in species to 
  # capitalized first letters and correct spellings
  
  mutate(
    species = 
      species %>% 
      str_replace(".*be?are?$", "Black bear") %>% 
      str_replace(".*m$", "Opossum") %>% 
      str_replace(".*n", "Raccoon") %>%
      str_replace(".*[Dd]e[ea]r", "White-tailed deer")
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
  collisions_spp_fix %>% 
  
  # modifying the coordinate columns to change longitude and latitude values to
  # negative and positive values respectively
  
  mutate(
    long = longitude,
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
        long
      )
  ) %>% 
  select(!long)

# Remove the name `collisions_spp_fix` from your global environment:

rm(collisions_spp_fix)

# 6 -----------------------------------------------------------------------

# Organize the data into a relational database:

# * Make these data database-ready by normalizing (i.e., tidying) the data.
#   Remember to follow all of Hadley’s principles of tidy data!
# * Store the resultant objects within a single list file globally assigned to
#   the name collisions_tidy.

# list objects of tidy data

collisions_tidy <- 
  list(
    
    # generating a data frame associated with info about the counties  
    
    county_info = 
      collisions_coord_fix %>%
      distinct(        
        county_id,
        county_name,
        county_population,
        county_area
      ),
    
    # generating a data frame associated with info about the collision incidence 
    # and assign the name incidents. county_id and county_name are the same 
    # variable so only keep county_name which is more informative
    
    incidents = 
      collisions_coord_fix %>%
      select(
        object_id,
        county_name,
        crash_time:latitude,
        crash_dsc,
        weather_condition
      ),
    
    # the county sunrise and sunset are the same in each county where a 
    # collision occurred and are day by day observations. We give these 
    # observations their own data frame assigning the name sunrise_sunset
    
    sunrise_sunset =
      collisions_coord_fix %>%
      distinct(
        county_name,
        county_sunset,
        county_sunrise
      )
  )

# Remove the name collisions_coord_fix from your global environment:

rm(collisions_coord_fix)

# 7 -----------------------------------------------------------------------

# Generate a summary table of the total number of the total number of collisions
# by species and year, where the columns are `year` and the names of each
# species:

collisions_tidy %>% 
  
  # get the incidents list with collision incidents
  
  pluck("incidents") %>% 
  
  # group observations by year from the crash_time variable
  
  group_by(
    year = year(crash_time)
  ) %>% 
  
  # get the count of each species in each year 
  
  count(species) %>% 
  
  # transpose the dataframe from long to wide form to provide species in columns 
  # and the species counts for each year in rows
  
  pivot_wider(
    names_from = species,
    values_from = n
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

collisions_tidy %>% 
  
  # calculate total number of collisions by species and year 
  
  pluck("incidents") %>% 
  
  # Group the data
  
  group_by(
    year = year(crash_time),
    species
  ) %>% 
  
  # Count the number of observations per group
  
  summarize(
    n = n(),
    
    # remove the grouping structure from the data
    
    .groups = "drop"
  )  %>% 
  
  # pipe the data into ggplot
  
  ggplot() +
  aes(
    x = year,
    y = n,
    color = species
  ) +
  
  # adding a layer of points to the plot
  
  geom_point(
    size = 2, 
    shape = 15
  ) +
  
  # connect the points in the scatterplot
  
  geom_line(
    linetype = 3, 
    colour = "black", 
    size = 0.5
  ) +
  
  # create a facet for each species and allow the y-axis to vary by facet using 
  # free_y
  
  facet_wrap(
    ~ species,
    ncol = 1,
    scales = "free_y"
  ) +
  
  # expand the y-axis by an additive expansion factor of 50
  
  scale_y_continuous(
    expand =
      expansion(
        add = c(50, 50)
      )
  ) +
  scale_color_brewer(palette="Spectral") +
  
  # label the axis and plot titles
  
  labs(
    title = "Annual variation of wildlife-vehicle collisions in the state of Virginia",
    x = "Year", 
    y = "Number of collisions"
  ) +
  
  # modify theme elements of our plot
  
  theme(
    panel.background = 
      element_rect(
        fill = "white",
        color = "black"
      ),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    panel.grid.minor.y = 
      element_line(
        color = "#dcdcdc",
        linetype = "dashed"
      ),
    strip.background = element_rect(color = "black"),
    axis.line = element_line(linewidth = .7),
    panel.spacing = unit(0.75, "lines"),
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 10)
  )

# 9 -----------------------------------------------------------------------

# Prove your iteration skills to Chad:

# * Use iteration to generate four separate bar plots where each plot 
#   represents a single species.
# * Ensure that the horizontal axis of each plot represents years.
# * Ensure that the vertical axis represents the total number of collisions
#   with a given species on a given year.
# * Title each plot with the common name of the species.

collisions_tidy %>% 
  pluck("incidents") %>% 
  pull(species) %>% 
  unique() %>% 
  
  # create bar plots of collisions for each species with iteration
  
  map(
    ~ collisions_tidy %>% 
      pluck("incidents") %>% 
      
      # Group the data:
      
      group_by(
        year = year(crash_time)
      ) %>% 
      
      # visualize the number of collisions for each species
      
      filter(
        species == .x ) %>% 
      
      # Count the number of observations
      
      summarize(
        n = n()
      ) %>% 
      
      # Plot the data: 
      
      ggplot() +
      aes(
        x = year,
        y = n
      ) +
      
      # add bar graph to the plot
      
      geom_bar(
        stat = "identity",
        fill = "#BCD2EE",
        color = "#000"
      ) +
      
      # define the upper and lower limits of the y-axis
      
      scale_y_continuous(
        expand =
          expansion(
            mult = c(0, 0.1)
          )
      ) +
        
      # Include the species as the title of the plot and add axis titles
      
      labs(
        title = .x,
        x = "Year", 
        y = "Number of Collisions"
      ) +
      theme(
        panel.background = 
          element_rect(
            fill = "white",
            color = "black"
          ),
        panel.grid = element_blank(),
        axis.line = element_line(linewidth = .7),
        panel.spacing = unit(0.75, "lines"),
        text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
  )

# 10 ----------------------------------------------------------------------

# Check small animal collision reports. In a single code block, please:

# * Verify whether every county reported at least one Opossum or Raccoon
#   collision in 2017.
# * If you find that some of these counties are missing, please provide me 
#   with a character vector of the offending county names.

collisions_tidy %>% 
  pluck("county_info") %>% 
  
  # perform a filtering join with anti_join to show counties that haven't 
  # reported opossum and raccoon collision in 2017 
  
  anti_join(
    collisions_tidy %>% 
      pluck("incidents") %>% 
      
      # filter collisions of Opossum and Raccoon species observed in 2017
      
      filter(
        species == "Opossum" | species == "Raccoon",
        year(crash_time) == 2017
      ) %>% 
      group_by(
        county_name
      ),
    by = "county_name"
  ) %>% 
  
  # get character vecor of counties that have no reports of raccoon and 
  # opossum collision
  
  .[["county_name"]]
  
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

most_crashes <- 
  
  # # summarize and identify the 10 counties with the highest number of collisions 
  # and assign the name most_crashes 
  
  collisions_tidy %>% 
  pluck("incidents") %>% 
  group_by(
    county_name
  ) %>% 
  summarize(
    crashes = n()
  ) %>% 
  slice_max(
    crashes,
    n = 10
  )

filtered_counties <- 
  collisions_tidy %>% 
  pluck("incidents") %>% 
  
  # subset data from the original dataset to only include counties with the 
  # most crashes and assign name filtered_counties
  
  filter(
    county_name %in% most_crashes$county_name
  ) 

# summarize the the total number of crashes across time for the 10 counties

filtered_counties %>% 
  group_by(
    year = year(crash_time),
    county_name
  ) %>% 
  summarize(
    n = n(),
    .groups = "drop"
  ) %>% 
  
  # use fct_reorder to arrange the county boxplot from highest to lowest 
  # number of crashes
  
  mutate(
    county_name = 
      fct_reorder(county_name, n)
  ) %>% 
  
  # Plot the data: 
  
  ggplot() +
  aes(
    x = n,
    y = county_name
  ) +
  geom_boxplot(
    fill = "#b64d38",
    color="black",
    width = 0.8, 
    size = 0.3,
    outliers = FALSE
  ) +
  
  # label the axis and plot titles
  
  labs(
    title = "10 Counties with highest wildlife-vehicle collision incidents",
    x = "Number of collisions", 
    y = "County"
  ) +
  
  # modify theme elements of our plot
  
  theme(
    panel.background = 
      element_rect(
        fill = "white",
        color = "black"
      ),
    panel.grid = element_blank(),
    axis.line = element_line(linewidth = .7),
    panel.spacing = unit(0.75, "lines"),
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

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
  pluck("incidents") %>% 
  mutate(
    month = 
      month(crash_time, label = TRUE),
    season = case_when(
      month %in% c("Sept", "Oct", "Nov") ~ "Fall",
      month %in% c("Dec", "Jan", "Feb") ~ "Winter",
      month %in% c("Mar", "Apr", "May") ~ "Spring",
      TRUE ~ "Summer",
    ) %>% 
      fct_relevel(
        "Winter",
        "Spring",
        "Summer",
        "Fall"
      )
  ) %>% 
  
  # group the data by season
  
  group_by(
    season,
    species
  ) %>% 
  summarize(
    crashes = n(),
    .groups = "drop"
  ) %>% 
  ggplot() + 
  aes(
    x = season,
    y = crashes,
    fill = species
  ) +
  geom_bar(
    stat = "identity",
    color = "black"
    ) +
  scale_fill_manual(
    values=c("#CD3333", "#E69F00", "#56B4E9", "#2E8B57")
    ) +
  
  # define the upper and lower limits of the y-axis
  
  scale_y_continuous(
    expand =
      expansion(
        mult = c(0, 0.1)
      )
  ) +
  
  # label the axis and plot titles
  
  labs(
    title = "Seasonal occurrence of wildlife-vehicle collisions",
    x = "Seasons", 
    y = "Number of collisions"
  ) +
  
  # modify theme elements of our plot
  
  theme(
    panel.background = 
      element_rect(
        fill = "white",
        color = "black"
      ),
    panel.grid = element_blank(),
    axis.line = element_line(linewidth = .7),
    panel.spacing = unit(0.75, "lines"),
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# extra credit 1 ----------------------------------------------------------

# Complete questions 2-5 in a single piped statement

collisions <- read_rds("data/raw/va_wildlife_collisions.rds") %>% 
  arrange(crash_time) %>% 
  mutate(
    date = 
      date %>% 
      map_vec(
        ~ if(
          str_detect(.x, "^[0-9]")
        ) {
          dmy(.x)
        } else {
          mdy(.x)
        }
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
    species = 
      species %>% 
      str_replace(".*be?are?$", "Black bear") %>% 
      str_replace(".*m$", "Opossum") %>% 
      str_replace(".*n", "Raccoon") %>%
      str_replace(".*[Dd]e[ea]r", "White-tailed deer"),
    long = longitude,
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
        long
      )
  ) %>% 
  select(
    !c(
      date, 
      year, 
      day, 
      long)
  )

# extra credit 2 ----------------------------------------------------------

# Complete memo 11 in a single piped statement:


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


