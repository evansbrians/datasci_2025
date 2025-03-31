
# Problem set 2

# 1 -----------------------------------------------------------------------

# Before opening your script file for this problem set, change the name of
# the `problem_set_2.R` to "problem_set_2_[last name]_[first name].R" using
# a snake case naming convention. *Note: You will submit this script file 
# as your assignment*.

# 2 -----------------------------------------------------------------------

# Open the script file in RStudio and attach the tidyverse metapackage to 
# your current R session:



# 3 -----------------------------------------------------------------------

# Using the *relative file path*, read in the file "coqui_counts.rds" 
# and globally assign the object to the name `coqui`. 



# 4 -----------------------------------------------------------------------

# The location of a point on the Earth's surface is represented by two
# variables on different axes -- a point's longitudinal and latitudinal
# position. Because of this, the column `coordinates` violates Codd's 
# first normal rule that all values are atomic and Hadley Wickham's tidy 
# data rule that each variable forms a column.

# Split the column `coordinates` into the columns `longitude` and
# `latitude` and globally assign the resultant object to the name
# `coqui_coord_fix`:



# Remove the name `coqui` from your global environment:



# 5 -----------------------------------------------------------------------

# The column `habitat_class` is derived from, and therefore transitively
# dependent on, the column `habitat`. 

# Remove the `habitat_class` column and globally assign the resultant 
# object to the name`coqui_no_class`:



# Remove the name `coqui_coord_fix` from your global environment:



# 6 -----------------------------------------------------------------------

# As described in the metadata, this data frame represents counts of coqui 
# frogs. Each observation is a count at a given distance on a given 
# transect. Currently, `coqui_no_class` violates two aspects of Codd's 
# first normal rule and the tidy data rules that each row represents an
# observation and every column represents a variable. 

# Fix this such that the resultant object contains the variables
# `distance_class` and `count`, then globally assign the object to the 
# name `coqui_long`.



# Remove the name `coqui_no_class` from your global environment:



# 7 -----------------------------------------------------------------------

# Write a code block that produces a data frame that displays the number
# of transects that were sampled within each habitat type:



# 8 -----------------------------------------------------------------------

# Generate a plot that displays the number of observations per habitat type
# and site, with habitat on the x-axis and the fill color of the bars
# determined by site:



# 9 -----------------------------------------------------------------------

# Exploring duplicate rows among subsets of variables ...

# * Write a code block that subsets the data to site, longitude, and latitude
#   and removes duplicate rows:



# * Write a code block that subsets the data to site, longitude, latitude, and
#   transect_id and removes duplicate rows:



# * Write a code block that subsets the data to transect_id and habitat and
#   removes duplicate rows:



# * Write a code block that subsets the data to transect_id, habitat, and date
#   and removes duplicate rows:



# 10 ----------------------------------------------------------------------

# Please write a brief comment that describes which tidy data rule is being 
# violated in `coqui_long` and how you were able to determine that.


