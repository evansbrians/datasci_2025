
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

# first we fix dates by checking USA and European style and binding them
  
bind_rows(
  
  # We start with European style dates
  
  collisions %>%
    filter(
      str_detect(date, "^[0-9]")
    ) %>%
    mutate(
      date = dmy(date)
    ),
  
  # now USA style dates 
  
  collisions %>%
    filter(
      str_detect(date, "^[A-Za-z]")
    ) %>%
    mutate(
      date = mdy(date)
    )
) %>% 
  
  # converting crash_time, sunrise and sunset into ISO
  
  mutate(
    across(
      c(
        crash_time,
        county_sunrise:county_sunset),
      ~ str_c(
        date, 
        .x, 
        sep = " "
      ) %>% 
        ymd_hms()
    )
  ) %>% 
  
  # now we select non transitive columns 
  
  select(
    !c(
      date,
      year:day)
  ) %>% 
  
  # Arrange from earliest to most recent collisions. That means 
  # in ascendant order

  arrange(crash_time)

# Remove the name `collisions` from your global environment:

rm(collisions)

# [[-0.15]] Code formatting:  
# * If a function spans more than one line of code, closing parentheses should 
#   be placed on their own line.
# * Indentation is off

# 4 -----------------------------------------------------------------------

# Repair the `species` column:

# * Ensure that the four species are recorded as (with the first letter
#   capitalized) "Black bear", "Opossum", "Raccoon", and "White-tailed deer".
# * Globally assign the resultant object to the name `collisions_spp_fix`.

collisions_spp_fix <-
  collisions_date_fix %>% 
  
  # we mutate and use str_replace to search data using regex and all
  # possibilities of different names for each specie
  
  mutate(
    species = 
      species %>%
      str_replace("[Bb]lack (bear|bare)|^bear", "Black bear") %>% 
      str_replace("(Virginia )?[0Oo]p?pos?sum", "Opossum") %>% 
      str_replace("(^[Cc]ommon )?[Rr]acc?oo?n","Raccoon") %>% 
      str_replace("(^[Ww]hite[- ]?tailed )?[Dd]e[ea]r", "White-tailed deer") 
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
  
  # we first modify lon and lat creating two new dummy columns
  
  mutate(
    
    # first with x that is longitude 
    
    x  = 
      if_else(
        longitude > 0,
        latitude, 
        longitude),
    
    # then with y that is latitude 
    
    y = 
      if_else(
        latitude < 0,
        longitude,
        latitude),
    
    # after that, we replace content of columns based on 
    # modification
    
    longitude = x,
    latitude = y
  ) %>%
  
  # finally we un-select the x and y columns.
  
  select(
    !x:y
  )

# Remove the name `collisions_spp_fix` from your global environment:

rm(collisions_spp_fix)

# [[-0.15]] Code formatting: If a function spans more than one line of code, 
# closing parentheses should be placed on their own line.

# 6 -----------------------------------------------------------------------

# Organize the data into a relational database:

# * Make these data database-ready by normalizing (i.e., tidying) the data.
#   Remember to follow all of Hadley’s principles of tidy data!
# * Store the resultant objects within a single list file globally assigned to
#   the name collisions_tidy.


collisions_tidy <- list(
  
  # we create the tibble data_crash to contain variables about
  # crash and species.
  # note: if there is interest about taxonomy, we can create a 
  # tibble containing species and their taxonomic classification
  
  data_crash = collisions_coord_fix %>%
    select(
      object_id,
      county_id,
      crash_time,
      species,
      crash_dsc,
      road,
      weather_condition
      ),
  
  # we also need a tibble for county data
  
  data_county  = collisions_coord_fix %>%
    distinct(
      county_id,
      county_name,
      county_area,
      county_population
      ),
  
  # finally we need a tibble consider time variables where .
  
  data_time    = collisions_coord_fix %>%
    distinct(
      crash_time,
      county_id,
      county_sunrise,
      county_sunset
      )
)

# Remove the name collisions_coord_fix from your global environment:

rm(collisions_coord_fix)

# [[-0.25]] Incorrect: It is not necessary to include `crash_time` within your
# `data_time` table.

# [[-0.30]] Code formatting:
# * If a code block spans more than one line of code, add a new line after the
#   assignment operator.
# * Indentation: Closing parentheses should be indented to the same level as
#   the start of the function.
# * Infix functions should be separated from surrounding code with a single
#   leading and trailing space.
# * Maintain one blank line between code blocks and comments. In your version
#   there were additional spaces prior to the section header.

# 7 -----------------------------------------------------------------------

# Generate a summary table of the total number of the total number of collisions
# by species and year, where the columns are `year` and the names of each
# species:

collisions_tidy$data_crash %>%
  
  # we first mutate the year by itself
  
  mutate(year = year(crash_time)) %>%
  
  # as each row is an observation, we count n-rows per specie per year
  
  count(
    year,
    species
    ) %>%
  
  # then we transform the tibble to get year and each specie in a column
  
  pivot_wider(
    names_from = species,
    values_from = n,
    values_fill = 0
    )

# [[-0.30]] Code parsimony: Because none of the groups had a count of `0`,
# `values_fill = 0` did not affect the output.

# [[-0.15]] Code formatting:
# * Indentation: Closing parentheses should be indented to the same level as
#   the start of the function.
# * Include no more than one prefix function per line of code.

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

collisions_tidy$data_crash %>%
  
  # we first mutate year from crash_time and make it a string of characters
  
  mutate(year = year(crash_time)) %>%
  count(
    year,
    species
    ) %>%
  mutate(year = str_c(year)) %>% 
  
  # then we plot, ensuring year is being read as a group to connect line
  #in geom_line()
  
  ggplot(
    aes(
      x = year,
      y = n,
      group = 1
      )
  ) +
  geom_point() +
  geom_line() +
  facet_wrap(
    ~ species,
    scales = "free",
    ) +
  labs(
    x = "Year",
    y = "Number of collisions",
    title = "Annual Collisions by Species",
    subtitle = "Data 2011–2017 | Scales free per facet"
  ) +
  theme(
    axis.text.x = 
      element_text(
        angle = 45, 
        hjust = 1
        )
  ) + 
  theme_bw()

# [[-0.30]] Code parsimony: Your `str_c(year)` did not alter the output.

# [[-0.30]] Code formatting: 
# * Include no more than one prefix function per line of code.
# * Add a single space between the hashtag (#) and comment.
# * Indentation: Closing parentheses should be indented to the same level as
#   the start of the function.

# [[+1.0]] We like your plot!

# 9 -----------------------------------------------------------------------

# Prove your iteration skills to Chad:

# * Use iteration to generate four separate bar plots where each plot 
#   represents a single species.
# * Ensure that the horizontal axis of each plot represents years.
# * Ensure that the vertical axis represents the total number of collisions
#   with a given species on a given year.
# * Title each plot with the common name of the species.

collisions_tidy$data_crash %>%
  
  # we first mutate year and create a tibble per specie 
  
  mutate(year = year(crash_time)) %>%
  count(
    year,
    species
    ) %>%
  split(.$species) %>%
  
  # in this section give us the iteration over ggplot per specie
  
  map(
    ~ ggplot(
      .x, 
      aes(
        x = year,
        y = n
      )
    ) +
      geom_bar(stat = "identity") +
      labs(
        title = unique(.x$species),
        x     = "Year",
        y     = "Total number of collisions"
      ) +
      theme(
        axis.text.x = 
          element_text(
            angle = 45,
            hjust = 1
          )
      )
  )

# 10 ----------------------------------------------------------------------

# Check small animal collision reports. In a single code block, please:

# * Verify whether every county reported at least one Opossum or Raccoon
#   collision in 2017.
# * If you find that some of these counties are missing, please provide me 
#   with a character vector of the offending county names.

# this allow us to explore all counties in the data_county tibble

collisions_tidy$data_county %>% 
  distinct(county_name)

# now we filter small mammals (Opossun and Raccoon) in 2017 from each county

collisions_tidy$data_crash %>%
  filter(
    year(crash_time) == 2017,
    species %in% c(
      "Opossum",
      "Raccoon"
      )
    ) %>%
  
  # then we join with the county data
  
  inner_join(
    collisions_tidy$data_county,
    by = "county_id"
    ) %>%
  
  # then we obtain which counties has reports in 2017 only 
  
  distinct(county_name)

# now, if we want to know which one are not, we use an antijoin and create
# a vector of missing offended counties 

collisions_tidy$data_county %>%
  anti_join(
    collisions_tidy$data_crash %>%
      filter(
        year(crash_time) == 2017,
        species %in% c(
          "Opossum",
          "Raccoon"
          )
        ) %>%
      distinct(county_id),
    by = "county_id"
  ) %>%
  pull(county_name)

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

# first global assignment: obtaining the county code_id that correspond
# to the top 10 with collisions

top10_counties <- 
  collisions_tidy$data_crash %>%
  count(county_id) %>%
  slice_max(
    n,
    n = 10
  ) %>%
  pull(county_id)

# second global assignment: count top 10 county per year and we use
# left_join to identify the county by code generated previously

counts_by_county_year <- 
  collisions_tidy$data_crash %>%
  filter(county_id %in% top10_counties) %>%
  mutate(year = year(crash_time)) %>%
  count(
    county_id,
    year
  ) %>% 
  left_join(
    collisions_tidy$data_county,
    by = "county_id"
    )

# then we plot

ggplot(
  counts_by_county_year,
  aes(
    x = n,
    y = fct_reorder(
      county_name,
      n,
      .fun = median
      )
    )
  ) +
  geom_boxplot() +
  labs(
    x = "Crashes per year",
    y = "County",
    title = "Distribution of Annual Collisions for Top 10 Counties"
  ) +
  theme(
    axis.text.y = 
      element_text(
        size = 8
        )
    ) +
  theme_bw()

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

# first we mutate the data  and get wich month correspont to wich season

collisions_tidy$data_crash %>%
  mutate(
    month  = month(crash_time),
    season = 
      case_when(
      month %in% c(12, 1, 2)  ~ "Winter",
      month %in% 3:5         ~ "Spring",
      month %in% 6:8         ~ "Summer",
      month %in% 9:11        ~ "Fall"
    ),
    
    # then we make then a factor, presetting the order 
    
    season = 
      fct_relevel(
      season,
      "Winter",
      "Spring", 
      "Summer", 
      "Fall"
      )
  ) %>%
  count(
    season,
    species
    ) %>%
  
  # then we plot, using fill to set color per species
  
  ggplot(
    aes(
      x = season, 
      y = n, 
      fill = species
      )
    ) +
  geom_bar(stat = "identity") +
  labs(
    x = "Season",
    y = "Total collisions",
    title = "Collisions by Season and Species"
  ) +
  theme(
    axis.text.x = 
      element_text(
      angle = 45,
      hjust = 1
      )
    ) +
  theme_bw()

# extra credit 1 ----------------------------------------------------------

# Complete questions 2-5 in a single piped statement

# we create collisions_coord_fix

collisions_coord_fix <- 
    read_rds("data/raw/va_wildlife_collisions.rds") %>% 
  
  # we mutate and check using regex and applying functions dmy() when
  # a condition is given (start number from 0 to 9), otherwise, 
  # mdy() is used. There is a warning but the code works 
  
    mutate(
      date = 
        case_when(
        str_detect(date, "^[0-9]") ~ dmy(date),  
        TRUE  ~ mdy(date)
      )
    ) %>% 
    mutate(
      across(
        c(
          crash_time,
          county_sunrise:county_sunset
          ),
          ~ ymd_hms(
            str_c(
              date,
              .x,
              sep = " "
              )
            )
        )
    ) %>% 
    select(
      !c(
        date,
        year:day)) %>% 
    arrange(crash_time) %>% 
    mutate(
      species = species %>% 
        str_replace("[Bb]lack (bear|bare)|^bear", "Black bear") %>% 
        str_replace("(Virginia )?[0Oo]p?pos?sum", "Opossum") %>% 
        str_replace("(^[Cc]ommon )?[Rr]acc?oo?n", "Raccoon") %>% 
        str_replace("(^[Ww]hite[- ]?tailed )?[Dd]e[ea]r", "White-tailed deer")
    ) %>% 
    mutate(
      x = 
        if_else(
          longitude > 0,
          latitude,
          longitude),
      y = 
        if_else(
          latitude  < 0,
          longitude,
          latitude),
      longitude = x,
      latitude  = y
    ) %>% 
    select(!x:y)

# extra credit 2 ----------------------------------------------------------

# Complete memo 11 in a single piped statement:

collisions_tidy$data_crash %>%
  
  # we do a inner join to the the data of the top 10 counties with crashes
  
  inner_join(
    collisions_tidy$data_crash %>%
      count(county_id) %>%
      slice_max(
        n,
        n = 10
        ),
    by = "county_id"
    
    # then we look for county id and county name using inner join and
    # get the counts per year 
    
  ) %>%
  inner_join(
    collisions_tidy$data_county,
    by = "county_id"
    ) %>%
  mutate(year = year(crash_time)) %>%
  count(
    county_name, 
    year
    ) %>%
  
  # then we plot 
  
  ggplot(
    aes(
    x = n,
    y = fct_reorder(
      county_name,
      n,
      .fun = median
      )
    )
    ) +
  geom_boxplot() +
  labs(
    x     = "Crashes per year",
    y     = "County",
    title = "Distribution of Annual Collisions for Top 10 Counties"
  ) +
  theme(
    axis.text.y = 
      element_text(
        size = 8
        )
    ) +
  theme_bw()

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

collisions_tidy$data_crash %>%
  
  # we first connect crash data with time data using inner joins 
  
  inner_join(
    collisions_tidy$data_time,
    by = 
      c(
        "crash_time",
        "county_id"
        )
    ) %>% 
  
  # then we filter data according to what is asked
  
  filter(
    
    # only E/W travel
    
    str_detect(road, "(EB|WB)"), 
    
    # after sunrise
    
    crash_time >= county_sunrise,
    
    # within 2 hours
    
    (crash_time - county_sunrise) <= 7200
    
  ) %>%
  
  # we change the names of road from EB or WB to into the sun or
  # away from the sun and group them all together
  
  mutate(
    sun_dir = 
      case_when(
      str_detect(road, "EB") ~ "Into the sun",
      str_detect(road, "WB") ~ "Away from the sun"
    )
  ) %>%
  count(sun_dir)%>%
  ggplot(
    aes(
      x = sun_dir, 
      y = n, 
      fill = sun_dir
      )
    ) +
  geom_bar(stat = "identity") +
  labs(
    x     = "Travel direction relative to sun",
    y     = "Number of collisions\n(≤2 h after sunrise)",
    title = "Collisions: Into vs. Away from the Sun"
  ) +
  theme(
    axis.text.x = 
      element_text(
        angle = 25, 
        hjust = 1
        )
    )

# thank you!