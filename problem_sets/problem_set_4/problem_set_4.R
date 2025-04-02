
# Script file for problem set 4: Cicada emergence

# question 1 --------------------------------------------------------------

# Before opening your script file for this problem set, change the name of
# the `problem_set_4.R` to "problem_set_4_[last name]_[first name].R" using
# a snake case naming convention. *Note: You will submit this script file 
# as your assignment*.

# question 2 --------------------------------------------------------------

# Open the script file in RStudio and attach the tidyverse metapackage to 
# your current R session.



# 3 -----------------------------------------------------------------------

# Read in `data/raw/cicadas_2021.rds` and globally assign the list to the
# name cicada_list:



# 4 -----------------------------------------------------------------------

# In a single, chained analysis with no intermediate assignments:

# * Subset `cicada_list` to `cicada_observations_2021`
# * Subset to observations in Maryland, Virginia, and the District of 
#   Columbia ("MD", "VA", and "DC");
# * Subset to research grade observations (variable = `quality_grade`);
# * Change the variable name `scientific_name` to `species`
# * Remove the columns `city`, `state`, and `quality_grade`;
# * Globally assign the name `cicada_research_quality` to the resultant 
#   object.



# 5 -----------------------------------------------------------------------

# This study is focused on Brood X cicada. Without using `filter()` please:
  
# * Subset `cicada_research_quality` to Brood X species (*Magicicada cassini*, *Magicicada
#   septendecim*, and *Magicicada septendecula*);
# * Assign the name `brood_x_observations` to the resultant object.



# 6 -----------------------------------------------------------------------

# Unfortunately for us, "Park" is a common street name and there is a type of
# road in the area called a "parkway" (which may be abbreviated as "pkwy").
# Let's explore values in the `address` variable. Without using `filter()`,
# extract a vector of addresses from the data frame `brood_x_observations` 
# such that the extracted values:
  
# * Starts with the word "Park" or "park", *or*
# * Contain the strings "Parkway", "parkway", "Pkwy", or "pkwy";
# * Are globally assigned to the name `not_parks`.



# 7 -----------------------------------------------------------------------

# Subset `brood_x_observations` to such that the resultant object:

# * The `address` variable contains "park" or "Park";
# * The `address` is *not* found in the vector assigned to the name
#   `not_parks`;
# * Is globally assigned to the name `brood_x_parks`.



# 8 -----------------------------------------------------------------------

# Using `brood_x_parks`, provide a unique vector of locations (variable =
# `address`) in which more than 40 cicadas have been observed:


