
# Problem set 2

# 1 -----------------------------------------------------------------------

# Before opening your script file for this problem set, change the name of
# the `problem_set_2.R` to "problem_set_2_[last name]_[first name].R" using
# a snake case naming convention. *Note: You will submit this script file 
# as your assignment*.

# done

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
  separate(
    coqui,
    col = coordinates,
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
  select(
    coqui_coord_fix,
    !habitat_class,
  )

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
    cols = `count_0-10m`:`count_20-30m`,
    names_to = "distance_class",
    values_to = "count"
  ) 

# Remove the name `coqui_no_class` from your global environment:

rm(coqui_no_class)

# 7 -----------------------------------------------------------------------

# Write a code block that produces a data frame that displays the number
# of transects that were sampled within each habitat type:

table(coqui_long$habitat)

# 8 -----------------------------------------------------------------------

# Generate a plot that displays the number of observations per habitat type
# and site, with habitat on the x-axis and the fill color of the bars
# determined by site:

coqui_long %>% 
  ggplot() +
  aes(
    x = habitat,
    fill = habitat
  ) +
  geom_bar( color = "black"
  ) +
  scale_fill_manual(
    values =
    c(
      "#4A1D2C",
      "#682779",
      "#DC331B",
      "#F29121"
    )
  ) +
  scale_y_continuous(
    limits = c(0,170),
    expand = c(0,0)
  ) +
  labs (
    title = "Coqui observations by habitat type and site",
    x = "Habitat",
    y = "Count"
  ) +
  theme_light()
  
  
# 9 -----------------------------------------------------------------------

# Exploring duplicate rows among subsets of variables ...

# * Write a code block that subsets the data to site, longitude, and latitude
#   and removes duplicate rows:

coqui_long %>% 
  select(
    site:latitude
  ) %>% 
  distinct()

# * Write a code block that subsets the data to site, longitude, latitude, and
#   transect_id and removes duplicate rows:

coqui_long %>% 
  select(site:latitude) %>% 
  distinct()

# * Write a code block that subsets the data to transect_id and habitat and
#   removes duplicate rows:

coqui_long %>% 
  select(transect_id:habitat) %>% 
  distinct() 

# * Write a code block that subsets the data to transect_id, habitat, and date
#   and removes duplicate rows:

coqui_long %>% 
  select(transect_id:date) %>% 
  distinct() %>% 

# 10 ----------------------------------------------------------------------

# Please write a brief comment that describes which tidy data rule is being 
# violated in `coqui_long` and how you were able to determine that.

# There we have multiple levels of observations:

# Level 1 - counting level: count and distance_class are attributes of 
# counting the frogs during a visit.

# Level 2 - Visit level: date, transect_id and habitat are attributes of
# visiting  site on an specific date.

# Level 3 - Site level: longitude and latitude are attributes of site.

# we conclude it by inspecting the results of distinct() function above
# where attributes of each level are observed. 
