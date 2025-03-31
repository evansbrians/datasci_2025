
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

coqui <- read_rds("data/raw/coqui_counts.rds")

# 4 -----------------------------------------------------------------------

# The location of a point on the Earth's surface is represented by two
# variables on different axes -- a point's longitudinal and latitudinal
# position. Because of this, the column `coordinates` violates Codd's 
# first normal rule that all values are atomic and Hadley Wickham's tidy 
# data rule that each variable forms a column.

# Split the column `coordinates` into the columns `longitude` and
# `latitude` and globally assign the resultant object to the name
# `coqui_coord_fix`:

coqui_coord_fix<- coqui %>% 
  separate(
    coordinates,
    into = c("longitude", "latitude"),
    sep = ","
    )

# Remove the name `coqui` from your global environment:

rm(coqui)

# [[-0.10]] Code formatting:

# * Infix functions should be separated from surrounding code with a single
#   leading and trailing space.

#  * If a code block spans more than one line of code, add a new line after the
#   assignment operator.

# 5 -----------------------------------------------------------------------

# The column `habitat_class` is derived from, and therefore transitively
# dependent on, the column `habitat`. 

# Remove the `habitat_class` column and globally assign the resultant 
# object to the name`coqui_no_class`:

coqui_no_class <-
  coqui_coord_fix %>% 
  select (-habitat_class)

# Remove the name `coqui_coord_fix` from your global environment:

rm(coqui_coord_fix)

# [[-0.50]] `-` is not among the functions that you may use for this
# assignment.

# [[-0.10]] Code formatting: Parentheses, curly braces, and square bracket
# operators should not be preceded or followed by a space.

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
  pivot_longer(coqui_no_class,
               cols = "count_0-10m":"count_20-30m",
               names_to = "distance_class",
               values_to = "count",
               names_prefix = "count_"
               )

# Remove the name `coqui_no_class` from your global environment:

rm(coqui_no_class)

# [[-0.15]] Code formatting: If the opening parentheses and the first argument
# of a function are on different lines, the first argument should be indented
# two spaces (one tab stop) relative to the start of the line above.

# 7 -----------------------------------------------------------------------

# Write a code block that produces a data frame that displays the number
# of transects that were sampled within each habitat type:

coqui_long %>% 
  distinct(
    transect_id,
    habitat
    ) %>% 
  count(habitat)

# 8 -----------------------------------------------------------------------

# Generate a plot that displays the number of observations per habitat type
# and site, with habitat on the x-axis and the fill color of the bars
# determined by site:

coqui_long %>% 
  count(habitat, site) %>% 
  ggplot() +
  aes(x = habitat, 
      y = n, 
      fill = site) +
  geom_bar(stat = "identity") +
  labs(
    title = "Coqui observations by habitat type and site",
    x = "Habitat",
    y = "Count",
  ) + 
  scale_fill_manual(values = c(
    nora = "blue", 
    treefall = "orange")
    ) + 
  scale_y_continuous(limits = c(0, 170)
                     )

# [[-0.15]] Code formatting:

# * Indentation: If the opening parentheses and the first argument of a 
#   function are on different lines, the first argument should be indented two
#   spaces (one tab stop) relative to the start of the line above.

# * Indentation and hanging parentheses: If a function spans more than one line
#   of code, closing parentheses should be placed on their own line.

# * Indentation: Closing parentheses should be indented to the same level as
#   the start of the function.

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
  select(
    site:transect_id
  ) %>% 
  distinct()

# * Write a code block that subsets the data to transect_id and habitat and
#   removes duplicate rows:

select(
  coqui_long,
  transect_id,
  habitat
) %>% 
  distinct()

# * Write a code block that subsets the data to transect_id, habitat, and date
#   and removes duplicate rows:

select(
  coqui_long,
  transect_id:date
  ) %>% 
  distinct()

# [[-0.025]] Code formatting (bullet point 4): Closing parentheses should be
# indented to the same level as the start of the function.

# 10 ----------------------------------------------------------------------

# Please write a brief comment that describes which tidy data rule is being 
# violated in `coqui_long` and how you were able to determine that.

# In 'coqui_long', the third tidy data rule (every level of observation forms 
# a table is violated.
# The variables 'longitude' and 'latitude' are qualities of the 'site'.
# This was determined during data exploration with the following code:
# coqui_long %>% 
# select(
#  site:latitude
# ) %>% 
#   distinct()

# Similarly, 'habitat' and 'date' are qualities of the 'transect_id'.
# This was determined during data exploration with the following code:
# select(
#   coqui_long,
#   transect_id:date
# ) %>% 
#    distinct()
