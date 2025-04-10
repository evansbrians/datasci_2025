
# Script file for problem set 4: Cicada emergence

# question 1 --------------------------------------------------------------

# Before opening your script file for this problem set, change the name of
# `problem_set_4.R` to "problem_set_4_[last name]_[first name].R" using a snake
# case naming convention. *Note: You will submit this script file as your
# assignment*.

# question 2 --------------------------------------------------------------

# Open the script file in RStudio and attach the core tidyverse packages to your
# current R session.



# 3 -----------------------------------------------------------------------

# Read in `data/raw/cicadas_2021.rds` and globally assign the list to the
# name cicada_list:



# 4 -----------------------------------------------------------------------

# In a single, chained analysis with no intermediate assignments:

# * Subset `cicada_list` to `cicada_observations_2021`
# * Without using `c()` or `%in%`, subset to observations in Maryland, 
#   Virginia, and the District of Columbia ("MD", "VA", and "DC");
# * Subset to observations where date values are not `NA`;
# * Subset to research grade observations (variable = `quality_grade`);
# * Change the variable name `scientific_name` to `species`
# * Remove the columns `city`, `state`, and `quality_grade`;
# * Globally assign the resultant object to the name `cicada_research_quality`.



# 5 -----------------------------------------------------------------------

# This study is focused on the three species of Brood X cicada. Without using
# `filter()`, please:

# * Subset `cicada_research_quality` to Brood X species;
# * Globally assign the resultant object to the name `brood_x_observations`.



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

# Subset `brood_x_observations` such that:

# * The `address` variable contains "park", "Park", or "Zoo";
# * The `address` is *not* found in the vector assigned to the name
#   `not_parks`;
# * The resultant object is a tibble data frame that is globally assigned to 
#   the name `brood_x_parks`.



# 8 -----------------------------------------------------------------------

# Using `brood_x_parks`, generate a summary table that:

# * Provides the number of observations of brood X cicada within parks where 
#   more than 40 cicadas have been observed;
# * Is arranged from the highest to lowest number of cicada observations;
# * Is globally assigned to the name `brood_x_park_summary`.



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



