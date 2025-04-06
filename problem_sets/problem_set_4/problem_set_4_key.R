
# Script file for problem set 4: Cicada emergence

# question 1 --------------------------------------------------------------

# Before opening your script file for this problem set, change the name of
# `problem_set_4.R` to "problem_set_4_[last name]_[first name].R" using a snake
# case naming convention. *Note: You will submit this script file as your
# assignment*.

# question 2 --------------------------------------------------------------

# Open the script file in RStudio and attach the core tidyverse packages to your
# current R session.

library(tidyverse)

# 3 -----------------------------------------------------------------------

# Read in `data/raw/cicadas_2021.rds` and globally assign the list to the
# name cicada_list:

cicada_list <- 
  read_rds("data/raw/cicadas_2021.rds")

# 4 -----------------------------------------------------------------------

# In a single, chained analysis with no intermediate assignments:

# * Subset `cicada_list` to `cicada_observations_2021`
# * Without using `c()` or `%in%`, subset to observations in Maryland, 
#   Virginia, and the District of Columbia ("MD", "VA", and "DC");
# * Subset to observations where data values are not `NA`;
# * Subset to research grade observations (variable = `quality_grade`);
# * Change the variable name `scientific_name` to `species`
# * Remove the columns `city`, `state`, and `quality_grade`;
# * Globally assign the resultant object to the name `cicada_research_quality`.

cicada_research_quality <- 
  cicada_list %>% 
  
  # Extract the list item of interest:
  
  pluck("cicada_observations_2021") %>% 
  
  # Subset the data frame as described above:
  
  filter(
    str_detect(state, "MD|VA|DC"),
    !is.na(date),
    quality_grade == "research"
  ) %>% 
  
  # Subset and rename variables:
  
  select(
    date,
    species = scientific_name,
    address
  )

# 5 -----------------------------------------------------------------------

# This study is focused on the three species of Brood X cicada. Without using
# `filter()`, please:

# * Subset `cicada_research_quality` to Brood X species;
# * Globally assign the resultant object to the name `brood_x_observations`.

brood_x_observations <- 
  cicada_research_quality %>% 
  
  # Subset the data frame to species in `cicada_list$brood_x`:
  
  semi_join(
    cicada_list %>%
      
      # Extract the list item of interest:
      
      pluck("brood_x"),
    by = "species"
  )

# 6 -----------------------------------------------------------------------

# Unfortunately for us, "Park" is a common street name and there is a type of
# road in the area called a "parkway" (which may be abbreviated as "pkwy").
# Let's explore values in the `address` variable. Without using `filter()`,
# extract a vector of addresses from the data frame `brood_x_observations` 
# such that the extracted values:

# * Starts with the word "Park" or "park", *or*
# * Contain the strings "Parkway", "parkway", "Pkwy", or "pkwy";
# * Are globally assigned to the name `not_parks`.

not_parks <-
  brood_x_observations %>% 
  
  # Extract the `address` variable as a character vector:
  
  pull(address) %>% 
  
  # Subset the character vector ...
  
  keep(
    
    # ... using regex to test the vector for patterns of interest:
    
    ~ str_detect(.x, "^[Pp]ark|[Pp](ar)?kwa?y")
  )

# 7 -----------------------------------------------------------------------

# Subset `brood_x_observations` such that:

# * The `address` variable contains "park", "Park", or "Zoo";
# * The `address` is *not* found in the vector assigned to the name
#   `not_parks`;
# * The resultant object is a tibble data frame that is globally assigned to 
#   the name `brood_x_parks`.

brood_x_parks <- 
  brood_x_observations %>% 
  
  # Subset the data frame ...
  
  filter(
    
    # ... using regex to test for the patterns of interest:
    
    str_detect(address, "[Pp]ark|Zoo"),
    
    # ... and to where the address is not in the `not_parks` vector:
    
    !address %in% not_parks
  )

# 8 -----------------------------------------------------------------------

# Using `brood_x_parks`, generate a summary table that:

# * Provides the number of observations of brood X cicada within parks where 
#   more than 40 cicadas have been observed;
# * Is arranged from the highest to lowest number of cicada observations;
# * Is globally assigned to the name `brood_x_park_summary`.

brood_x_park_summary <- 
  brood_x_parks %>% 
  
  # Subset the data frame to parks with more than 40 observations:
  
  filter(
    n() > 40,
    .by = address
  ) %>% 
  
  # Generate a summary data frame of the number of observations per park:
  
  summarize(
    n = n(),
    .by = address
  ) %>% 
  
  # Sort the data frame from highest to lowest number of observations: 
  
  arrange(
    desc(n)
  )

# 9 -----------------------------------------------------------------------

# We want to start placing our ARUs at the four locations with the greatest
# number of cicada observations and figure out which of those locations had the
# earliest emergence of brood X cicadas. We will use a density plot to visualize
# the time of emergence. Within a single piped code block, use
# `brood_x_park_summary` and `brood_x_parks` to:

# * Subset `brood_x_parks` to the four parks with the greatest number of
# cicada observations;
# * Subset the resultant object to the observations in April of 2021;
# * Generate a density plot, in which:
#   * The `date` variable is mapped to the x-axis;
#   * The `species` variable is mapped to fill color;
#   * The geometry of the plot represents the statistical density of
#     observations of each species, with the transparency altered such that all
#     species densities are visible;
#   * Each park is placed within its own facet and all of the facets are
#     positioned in a single column;
#   * The position of values on the y-axis for each facet is determined by the
#     range of density values within a given park;
#   * The position of values on the x-axis does *not* vary by park;
#   * The scale of the y-axis for a given park ranges from 0 to a value of 0.20 
#     times above the maximum brood X observation density for that park;
#   * The color scale associated with `species` is determined using
#     `scale_fill_brewer()`;
#   * The plot is titled "Density distribution of Brood X cicada observations
#     by date"
#   * The x-axis is labeled "Date";
#   * The y-axis is labeled "Density";
#   * The legend is labeled "Species";
#   * The panel background of the plot is white and has a black border;
#   * No grid lines are displayed;
#   * All text is in Times New Roman;
#   * The plot title is in 16 pt font;
#   * The facet strip text and axis titles are in 14 pt font.

brood_x_parks %>% 
  
  # Subset the data frame to parks in `brood_x_park_summary` to the four parks
  # with the most observations:
  
  semi_join(
    brood_x_park_summary %>% 
      slice_max(n, n = 4),
    by = "address"
  ) %>% 
  
  # Use regex to subset the data frame to observations in April 2021:
  
  filter(
    str_detect(date, "2021-04")
  ) %>% 
  
  # Initiate the plot:
  
  ggplot() +
  
  # Define the aesthetic mappings:
  
  aes(
    x = date,
    fill = species,
  ) + 
  
  # Add geometry:
  
  geom_density(alpha = 0.7) +

  # Divide the plot into facets:
  
  facet_wrap(
    ~ address,
    ncol = 1,
    scales = "free_y"
  ) +
  
  # Define the scales of the plot:
  
  scale_y_continuous(
    expand =
      expansion(
        mult = c(0, 0.2)
      )
  ) +
  scale_fill_brewer(palette = "Set1") +
  
  # Add labels:
  
  labs(
    title = "Density distribution of Brood X cicada observations by date",
    x = "Date",
    y = "Density",
    fill = "Species"
  ) +
  
  # Define the plot theme elements:
  
  theme(
    panel.background = 
      element_rect(
        color = "#000000",
        fill = "#ffffff"
      ),
    panel.grid = element_blank(),
    strip.background = element_rect(color = "black"),
    text = element_text(family = "Times"),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 14)
  )

