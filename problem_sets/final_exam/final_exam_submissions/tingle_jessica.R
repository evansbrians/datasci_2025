
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

# Attach the tidyverse, 
# and continue to wonder if Brian's office really does 
# feature a life-sized poster/cutout/wax figure of Hadley

library(tidyverse)

# Load fonts for plots:

extrafont::loadfonts(device = "win")

#  [[-0.15]] Code formatting: Tingle! Maintain one blank line between code
#  blocks and comments. In your version there were additional spaces prior to
#  the section header.

# 2 -----------------------------------------------------------------------

# Read in `data/raw/va_wildlife_collisions.rds` and globally assign the
# resultant object to the name `collisions`:

# Read in the data

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
  collisions %>% 
  mutate(
    
    # Convert date from character to date format:
    
    date =
      
      # if_else() to properly handle European vs. US date formats:
      
      if_else(
        str_detect(date, "^[1-9]"),
        dmy(date, quiet = TRUE),
        mdy(date, quiet = TRUE)
      ),
    
    # Convert all three time columns into SO-8601 datetime values:
    
    across(
      c(
        crash_time, 
        county_sunrise, 
        county_sunset
      ),
      ~ str_c(date, .x) %>% 
        ymd_hms()
    )
  ) %>% 
  
  # Rename crash_time to clarify that it now also contains date:
  
  rename(crash_date_time = crash_time) %>% 
  
  # Remove transitive columns:
  
  select(
    !c(
      date,
      year,
      day
    )
  ) %>% 
  
  # Sort the table from earliest to most recent collision:
  
  arrange(
    crash_date_time
  )

# Remove the name `collisions` from your global environment:

rm(collisions)

# [[No points removed]] I'm not crazy about the if_else solution because (if you
# didn't include quiet = TRUE) it spits out parsing failure messages. This is
# because the functions `dmy` and `mdy` are run regardless of whether the
# statement evaluates to TRUE or FALSE. This is because `if_else` is not a
# control flow operation. If the data were big, such duplicated runs can
# increase memory usage and processing time.

# 4 -----------------------------------------------------------------------

# Repair the `species` column:

# * Ensure that the four species are recorded as (with the first letter
#   capitalized) "Black bear", "Opossum", "Raccoon", and "White-tailed deer".
# * Globally assign the resultant object to the name `collisions_spp_fix`.

collisions_spp_fix <- 
  collisions_date_fix %>%
  mutate(
    species =
      
      # Bear repair:
      
      species %>% 
      str_replace(
        "^[Bb].*[re]$",
        "Black bear"
      ) %>% 
      
      # Make opossum awesome:
      
      str_replace(
        "^[oO0].*|Virginia.*",
        "Opossum"
      ) %>%
      
      # Racoon, get well soon:
      
      str_replace(
        ".*[rR]ac.*",
        "Raccoon"
      ) %>% 
      
      # Put deer in the clear:
      
      str_replace(
        ".*[dD]e[ea]r",
        "White-tailed deer"
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
  collisions_spp_fix %>% 
  mutate(
    
    # Swap long and lat where necessary, using across() because we're fancy:
    
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
  
  # Create a list of tidy data frames:
  
  list(
    
    # Individual collision events:
    
    collision_events =
      collisions_coord_fix %>%
      select(
        object_id,
        crash_date_time,
        county_id,
        road,
        longitude,
        latitude,
        species,
        weather_condition,
        crash_dsc
      ),
    
    # Fixed data for the counties:
    
    county_data =
      collisions_coord_fix %>%
      select(
        county_id,
        county_name,
        county_area,
        county_population
      ) %>% 
      unique(),
    
    # Sunrise and sunset times across counties and days of the study period:
    
    county_daylight =
      collisions_coord_fix %>%
      select(
        county_id,
        county_sunrise,
        county_sunset
      ) %>% 
      unique()
  )

# Remove the name collisions_coord_fix from your global environment:

rm(collisions_coord_fix)

# [[No points removed]] Not crazy about the use of `unique()` in this context.

# 7 -----------------------------------------------------------------------

# Generate a summary table of the total number of the total number of collisions
# by species and year, where the columns are `year` and the names of each
# species:

collisions_tidy %>% 
  pluck("collision_events") %>% 
  
  # Prove that group_by() still holds a special place in our nerdy hearts:
  
  group_by(
    Year = year(crash_date_time),
    species
  ) %>% 
  
  # Tally up the collisions by year and species:
  
  summarize(
    n = n()
  ) %>% 
  ungroup() %>% 
  
  # Reshape the data frame to impress our stakeholders:
  
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

# Prepare data for plotting:

collisions_tidy %>% 
  pluck("collision_events") %>% 
  group_by(
    year = year(crash_date_time),
    species
  ) %>% 
  summarize(
    n = n()
  ) %>% 
  ungroup() %>% 
  
  # Make plot:
  
  ggplot() +
  aes(
    x = year,
    y = n
  ) +
  
  # Add points:
  
  geom_point() +
  
  # Connect the points:
  
  geom_line() +
  
  # Create a facet for each species, while playing some Skynyrd:
  
  facet_wrap(
    ~ species,
    ncol = 1,
    scales = "free"
  ) +
  
  # Adjust labels:
  
  labs(
    title = "Annual collisions by species",
    x = "",
    y = "Number of collisions"
  ) +
  
  # Beautify:
  
  theme(
    panel.background = 
      element_rect(
        fill = "white",
        color = "black"
      ),
    plot.background =
      element_rect(
        fill = "white"
      ),
    strip.background = 
      element_rect(
        fill = "#D3E8F7",
        color = "black",
        linewidth = 0.5
      ),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#F5F5F5"),
    plot.title = 
      element_text(
        family = "Segoe UI",
        size = 20
      ),
    axis.title = 
      element_text(
        family = "Calibri",
        size = 18
      ),
    axis.text =
      element_text(
        family = "Calibri",
        size = 12
      ),
    strip.text = 
      element_text(
        family = "Myriad Web",
        size = 16,
        hjust = 0.01
      )
  )

# [[+1.0]] We like your plot, but it was missing an epic guitar solo!

# 9 -----------------------------------------------------------------------

# Prove your iteration skills to Chad:

# * Use iteration to generate four separate bar plots where each plot 
#   represents a single species.
# * Ensure that the horizontal axis of each plot represents years.
# * Ensure that the vertical axis represents the total number of collisions
#   with a given species on a given year.
# * Title each plot with the common name of the species.

# Create a vector of species for iteration:

collisions_tidy %>% 
  pluck("collision_events") %>%
  pull(species) %>% 
  unique() %>% 
  
  # Use map to iterate over species:
  
  map(
    
    # Prepare data:
    
    ~ collisions_tidy %>% 
      pluck("collision_events") %>% 
      filter(species == .x) %>% 
      group_by(
        year = year(crash_date_time)
      ) %>% 
      summarize(
        n = n()
      ) %>% 
      ungroup() %>% 
      
      
      # Make plot:
      
      ggplot() +
      aes(
        x = year,
        y = n
      ) +
      geom_bar(
        stat = "identity",
        fill = "#153C29",
        alpha = 0.7
      ) +
      scale_y_continuous(
        expand = 
          expansion(
            mult = c(0, .2)
          )
      ) +
      
      # Adjust labels:
      
      labs(
        title = 
          str_c(
            .x,
            " collisions 2011-2017"
          ),
        subtitle = "Happy now, Chad?",
        x = "",
        y = "# collisions"
      ) +
      
      # Beautify:
      
      theme(
        panel.background = 
          element_rect(
            fill = "white",
            color = "black"
          ),
        plot.background =
          element_rect(
            fill = "white"
          ),
        panel.grid = element_blank(),
        plot.title = 
          element_text(
            family = "Leelawadee UI",
            size = 20
          ),
        plot.subtitle = 
          element_text(
            family = "Ink Free",
            size = 12
          ),
        axis.title = 
          element_text(
            family = "Leelawadee UI Semilight",
            size = 18
          ),
        axis.text.x =
          element_text(
            family = "Calibri",
            size = 14
          ),
        axis.text.y =
          element_text(
            family = "Calibri",
            size = 12
          )
      ) +
      
      # More cowbell:
      
      rphylopic::add_phylopic(
        uuid = 
          case_when(
            .x == "Opossum" ~ "f9f6ae2b-ed15-4e25-976c-ab4e15e0fe41",
            .x == "White-tailed deer" ~ "8569838c-c725-4772-b0a3-b5eb04baaada",
            .x == "Black bear" ~ "369a7880-4798-41bf-851a-ec5da17fafa3",
            .x == "Raccoon" ~ "fb1daf6a-0db6-4b3f-ab84-289760c9c5b2"
          ),
        x = 
          case_when(
            .x == "Opossum" ~ 2017,
            .x == "White-tailed deer" ~ 2013,
            .x == "Black bear" ~ 2011,
            .x == "Raccoon" ~ 2013
          ),
        y = 
          case_when(
            .x == "Opossum" ~ 797 + 797/24,
            .x == "White-tailed deer" ~ 4352 + 4352/16,
            .x == "Black bear" ~ 284 + 284/18,
            .x == "Raccoon" ~ 658 + 658/20
          ),
        height =
          case_when(
            .x == "Opossum" ~ 797/12,
            .x == "White-tailed deer" ~ 4352/8,
            .x == "Black bear" ~ 284/9,
            .x == "Raccoon" ~ 658/10
          )
      )
  )

# [[+1.0]] Whoa!!!! You taught a cool new trick and really explored the space
# Gene!

# 10 ----------------------------------------------------------------------

# Check small animal collision reports. In a single code block, please:

# * Verify whether every county reported at least one Opossum or Raccoon
#   collision in 2017.
# * If you find that some of these counties are missing, please provide me 
#   with a character vector of the offending county names.

collisions_tidy %>% 
  pluck("collision_events") %>% 
  
  # Subset to 2017:
  
  filter(
    year(crash_date_time) == 2017
  ) %>% 
  
  # Tally roadkills by county and species:
  
  summarize(
    n = n(),
    .by = c(county_id, species)
  ) %>% 
  
  # Subset to counties with at least one Opossum and/or Raccoon report:
  
  filter(
    species %in% c("Opossum", "Raccoon"),
    n > 0
  ) %>% 
  
  # Use and anti-join with the county data to get counties with none reported:
  
  anti_join(
    collisions_tidy %>% 
      pluck("county_data"),
    .,
    by = "county_id"
  ) %>% 
  
  # Shame the underreporting counties:
  
  pull(county_name)

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

# Prepare data for plotting:

collisions_tidy %>% 
  pluck("collision_events") %>%
  
  # Filter collision data to include only the 10 most grisly counties:
  
  semi_join(
    collisions_tidy %>% 
      pluck("collision_events") %>% 
      summarize(
        collisions = n(),
        .by = county_id
      ) %>% 
      slice_max(
        collisions, 
        n = 10
      ),
    by = "county_id"
  ) %>% 
  
  # Tally collisions by county and year:
  
  group_by(
    county_id,
    year = year(crash_date_time)
  ) %>% 
  summarize(
    collisions = n()
  ) %>% 
  ungroup() %>% 
  
  # Add county names, as a factor in order of deadliness:
  
  left_join(
    collisions_tidy %>% 
      pluck("county_data") %>% 
      select(
        county_id, 
        county_name
      ),
    by = "county_id"
  ) %>% 
  mutate(
    county_name =
      county_name %>% 
      fct_reorder(collisions)
  ) %>% 
  
  # Make plot:
  
  ggplot() +
  aes(
    x = county_name,
    y = collisions
  ) +
  geom_boxplot(
    fill = "#7D6075",
    alpha = 0.8
  ) +
  coord_flip() +
  
  # Adjust labels:
  
  labs(
    title = "Wildlife collisions per year",
    subtitle = "Not much peace or long life in these counties",
    x = "",
    y = "# collisions"
  ) +
  
  # Beautify:
  
  theme(
    panel.background = 
      element_rect(
        fill = "white",
        color = "black"
      ),
    plot.background =
      element_rect(
        fill = "white"
      ),
    panel.grid = element_blank(),
    plot.title = 
      element_text(
        family = "Segoe UI",
        size = 20
      ),
    plot.subtitle = 
      element_text(
        family = "Candara Light",
        size = 12
      ),
    axis.title = 
      element_text(
        family = "Malgun Gothic",
        size = 12
      ),
    axis.text =
      element_text(
        family = "Malgun Gothic",
        size = 10
      )
  ) +
  
  # More cowbell:
  
  rphylopic::add_phylopic(
    img =
      rphylopic::get_phylopic(uuid  = "5dca4757-56a5-4321-a3ed-08731046c4e9") %>%
      rphylopic::rotate_phylopic(angle = 155),
    x = 1.5,
    y = 310,
    height = 3
  )

# [[+1.0]] Love Roadkill Rocky (or maybe Rocky Road Raccoon?), you are
# brilliant!!!

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

# Prepare data for plotting:

collisions_tidy %>% 
  pluck("collision_events") %>% 
  
  # Create a new season variable, in the picky boss' preferred order:
  
  mutate(
    season =
      case_when(
        month(crash_date_time) %in% 3:5 ~ "Spring",
        month(crash_date_time) %in% 6:8 ~ "Summer",
        month(crash_date_time) %in% 9:11 ~ "Fall",
        TRUE ~ "Winter"
      ) %>% 
      fct_relevel(
        c(
          "Winter",
          "Spring",
          "Summer",
          "Fall"
        )
      )
  ) %>% 
  
  # Tally:
  
  summarize(
    n = n(),
    .by = c(season, species)
  ) %>% 
  
  # Plot:
  
  ggplot() +
  aes(
    x = season,
    y = n,
    fill = species
  ) +
  geom_bar(
    stat = "identity",
    color = "black"
  ) +
  scale_y_continuous(
    expand = 
      expansion(
        mult = c(0, .2)
      )
  ) +
  
  # Not an allowed function, but the default gives me a conniption:
  
  scale_fill_viridis_d(
    option = "cividis",
    direction = -1,
    alpha = 0.9
  ) +
  
  # Adjust labels:
  
  labs(
    title = "Wildlife collisions by season",
    x = "",
    y = "# collisions",
    fill = "Species"
  ) +
  
  # Beautify:
  
  theme(
    panel.background = 
      element_rect(
        fill = "white",
        color = "#7f7f7f"
      ),
    plot.background =
      element_rect(
        fill = "white"
      ),
    aspect.ratio = 1,
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color = "#bebebe"),
    panel.grid.minor.x = element_line(color = "#ededed"),
    legend.position = "inside",
    legend.position.inside =
      c(0.17, 0.84),
    legend.background =
      element_rect(
        fill = "#fbfbfb",
        color = "#7f7f7f"
      ),
    plot.title = 
      element_text(
        family = "Bauhaus 93",
        size = 24
      ),
    legend.title =
      element_text(
        family = "Bahnschrift",
        size = 12
      ),
    legend.text =
      element_text(
        family = "Arial",
        size = 10
      ),
    axis.title.y = 
      element_text(
        family = "Bahnschrift",
        size = 14,
        vjust = 3,
        hjust = 0.57
      ),
    axis.text.y =
      element_text(
        family = "Arial",
        size = 10,
        color = "#6a6a6a"
      ),
    axis.text.x =
      element_text(
        family = "Bahnschrift",
        size = 14
      )
  ) 

# [[+1.0]] Your picky boss (Tara) says "whoa!!!!"

# extra credit 1 ----------------------------------------------------------

# Complete questions 2-5 in a single piped statement

collisions_pipe_flex <- 
  
  # Read in the data:
  
  read_rds("data/raw/va_wildlife_collisions.rds") %>% 
  
  # Fix the dates and times:
  
  mutate(
    date =
      if_else(
        str_detect(date, "^[1-9]"),
        dmy(date, quiet = TRUE),
        mdy(date, quiet = TRUE)
      ),
    across(
      c(
        crash_time, 
        county_sunrise, 
        county_sunset
      ),
      ~ str_c(date, .x) %>% 
        ymd_hms()
    )
  ) %>%
  rename(crash_date_time = crash_time) %>% 
  select(
    !c(
      date,
      year,
      day
    )
  ) %>% 
  arrange(
    crash_date_time
  ) %>% 
  
  # Fix the species names:
  
  mutate(
    species =
      species %>% 
      str_replace(
        "^[Bb].*[re]$",
        "Black bear"
      ) %>% 
      str_replace(
        "^[oO0].*|Virginia.*",
        "Opossum"
      ) %>%
      str_replace(
        ".*[rR]ac.*",
        "Raccoon"
      ) %>% 
      str_replace(
        ".*[dD]e[ea]r",
        "White-tailed deer"
      )   
  ) %>% 
  
  # Fix the coordinates:
  
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

# Bask in our glory:

imager::load.image("https://tobacco-img.stanford.edu/wp-content/uploads/cigars/pipes/pipe_12.jpg") %>% 
  plot()

# [[+3.0]] You make grading so fun!

# extra credit 2 ----------------------------------------------------------

# Complete memo 11 in a single piped statement:

# [done in original answer]

# [[+3.0]]

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
  pluck("collision_events") %>% 
  
  # Keep only the columns we'll need:
  
  select(
    county_id,
    crash_date_time,
    road
  ) %>% 
  
  mutate(
    
    # Create a new column for sun impairment:
    
    blinded_by_the_light =
      case_when(
        str_detect(road, "EB$") ~ "Into the sun",
        str_detect(road, "WB$") ~ "Away from the sun",
        TRUE ~ NA
      ),
    
    # Create a date column to facilitate joining with the sunrise time data:
    
    date = date(crash_date_time)
  ) %>%
  
  # Keep only East- and West-bound incidents:
  
  filter(
    !is.na(blinded_by_the_light),
    
  ) %>%
  
  # Join with county daylight data:
  
  left_join(
    collisions_tidy %>% 
      pluck("county_daylight") %>% 
      mutate(
        date = date(county_sunrise)
      ),
    by = 
      c("county_id", "date")
  ) %>% 
  
  # Remove columns we no longer need:
  
  select(
    !c(
      county_id,
      road,
      date,
      county_sunset
    )
  ) %>% 
  
  # Subset to crashes within two hours after sunrise:
  
  filter(
    hour(crash_date_time) - hour(county_sunrise) >= 0 & 
      hour(crash_date_time) - hour(county_sunrise) <= 2
  ) %>% 
  
  # Prepare data for plotting:
  
  group_by(
    year(crash_date_time),
    blinded_by_the_light
  ) %>% 
  summarize(
    collisions = n(),
  ) %>% 
  ungroup() %>% 
  
  # Plot:
  
  ggplot() +
  aes(
    x = blinded_by_the_light,
    y = collisions
  ) +
  geom_boxplot(fill = "#EF9C3A") +
  
  # Adjust labels:
  
  labs(
    title = "Wildlife collisions within two hours of sunrise:",
    subtitle = "Drivers blinded by the light hit more wildlife",
    x = "",
    y = "# collisions per year"
  ) +
  
  # Beautify:
  
  theme(
    panel.background = 
      element_rect(
        fill = "white",
        color = "#7f7f7f"
      ),
    plot.background =
      element_rect(
        fill = "white"
      ),
    aspect.ratio = 0.75,
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "#F5F5F5"),
    plot.title = 
      element_text(
        family = "Bookman Old Style",
        size = 18
      ),
    plot.subtitle = 
      element_text(
        family = "Segoe UI Light",
        size = 15
      ),
    axis.title.y = 
      element_text(
        family = "Segoe UI",
        size = 14,
        vjust = 3
      ),
    axis.text.x =
      element_text(
        family = "Segoe UI",
        size = 14
      ),
    axis.text.y =
      element_text(
        family = "Segoe UI Light",
        size = 10
      )
  )

# [[+3.0]] Mama always told me not to look into the eyes of the sun ... unless
# you're driving, in which case you should always look into the eyes of the sun.
