
# Problem set 2

# 1 -----------------------------------------------------------------------

# Before opening your script file for this problem set, change the name of
# the `problem_set_2.R` to "problem_set_2_[last name]_[first name].R" using
# a snake case naming convention. *Note: You will submit this script file 
# as your assignment*.

# 2 -----------------------------------------------------------------------

# Open the script file in RStudio and attach the tidyverse metapackage to 
# your current R session:

library(tidyverse)

# 3 -----------------------------------------------------------------------

# Using the *relative file path*, read in the file "coqui_counts.rds" 
# and globally assign the object to the name `coqui`. 

coqui <- 
  read_rds("data/raw/coqui_counts.rds")

# 4 -----------------------------------------------------------------------

# The location of a point on the Earth's surface is represented by two
# variables on different axes -- a point's longitudinal and latitudinal
# position. Because of this, the column `coordinates` violates Codd's 
# first normal rule that all values are atomic and Hadley Wickham's tidy 
# data rule that each variable forms a column.

# Split the column `coordinates` into the columns `longitude` and
# `latitude` and globally assign the resultant object to the name
# `coqui_coord_fix`:

coqui_coord_fix <- 
  coqui %>% 
    separate(
      col = "coordinates",
      into =
      c(
        "longitude",
        "latitude"
      ),
    sep = ","
    )

# Remove the name `coqui` from your global environment:

rm(coqui)

# 5 -----------------------------------------------------------------------

# The column `habitat_class` is derived from, and therefore transitively
# dependent on, the column `habitat`. 

# Remove the `habitat_class` column and globally assign the resultant 
# object to the name`coqui_no_class`:

coqui_no_class <- 
  coqui_coord_fix %>% 
    select(!habitat_class)

# Remove the name `coqui_coord_fix` from your global environment:

rm(coqui_coord_fix)

# 6 -----------------------------------------------------------------------

# As described in the metadata, this data frame represents counts of coqui 
# frogs. Each observation is a count at a given distance on a given 
# transect. Currently, `coqui_no_class` violates two aspects of Codd's 
# first normal rule and the tidy data rules that each row represents an
# observation and every column represents a variable. 

# Fix this such that the resultant object contains the variables
# `distance_class` and `count`, then globally assign the object to the 
# name `coqui_long`.

coqui_long <- 
  coqui_no_class %>% 
    pivot_longer(
      cols = "count_0-10m":"count_20-30m",
      names_to = "distance_class",
      values_to = "count"
    )

# Remove the name `coqui_no_class` from your global environment:

rm(coqui_no_class)

# 7 -----------------------------------------------------------------------

# Write a code block that produces a data frame that displays the number
# of transects that were sampled within each habitat type:

coqui_long %>% 
  select(
    habitat,
    transect_id
  ) %>% 
  distinct() %>% 
  count(habitat)

# [[No points removed]] Code parsimony: Distinct can be used to select columns
# while removing duplicate rows. As such, the `select` function was not
# necessary.
  
# 8 -----------------------------------------------------------------------

# Generate a plot that displays the number of observations per habitat type
# and site, with habitat on the x-axis and the fill color of the bars
# determined by site:

coqui_long %>% 
  select(
    site,
    habitat,
    transect_id
  ) %>% 
  distinct() %>% 
  count(habitat, site) %>% 
  ggplot() +
  aes(
    x = habitat,
    fill = site
  ) +
  geom_bar() +
  scale_y_continuous(
    limits = c(0, 170),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    "Site",
    values = c(
      nora = "#e19f19",
      treefall = "#815631"
    )
  ) +
  labs(
    title = "Coqui observations by habitat type and site",
    x = "Habitat",
    y = "Count"
  ) +
  theme_bw()

# [[-0.50]] Incorrect: This plotted the habitats per site, not the number of
# observations.

# 9 -----------------------------------------------------------------------

# Exploring duplicate rows among subsets of variables ...

# * Write a code block that subsets the data to site, longitude, and latitude
#   and removes duplicate rows:

coqui_long %>% 
  select(
    site,
    longitude,
    latitude
  ) %>% 
  distinct()

# * Write a code block that subsets the data to site, longitude, latitude, and
#   transect_id and removes duplicate rows:

coqui_long %>% 
  select(
    site,
    longitude,
    latitude,
    transect_id
  ) %>% 
  distinct()

# * Write a code block that subsets the data to transect_id and habitat and
#   removes duplicate rows:

coqui_long %>% 
  select(
    transect_id,
    habitat,
  ) %>% 
  distinct()

# * Write a code block that subsets the data to transect_id, habitat, and date
#   and removes duplicate rows:

coqui_long %>% 
  select(
    transect_id,
    habitat,
    date
  ) %>% 
  distinct()

# 10 ----------------------------------------------------------------------

# Please write a brief comment that describes which tidy data rule is being 
# violated in `coqui_long` and how you were able to determine that.

# Based on the list of unique sites, latitudes, and longitudes, coordinates vary by # site rather than by observation in 'coqui_long'. 
# This violates the second normal that all columns should be
# functionally dependent on the primary key. Dates may also be a separate
# visitation table, although I'm honestly not too sure how that would work         # with repeated visits to the same habitat and transect!

# [[-0.50]]: More specificity is needed here -- this is a violation of the rule
# that every level of observation is placed in its own table (typically the
# third tidy data rule).
