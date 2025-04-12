# Problem set 5

# 2 -----------------------------------------------------------------------

# Attach the tidyverse metapackage to your current R session.



# 3 -----------------------------------------------------------------------

# Read in `district_cats.rds` as a list file and globally assign the list
# object to the name `dc_cats`.



# 4 -----------------------------------------------------------------------

# The table represented by the list item visits is unfortunately not tidy.
# Normalize this list item:

# * Extract visits from `dc_cats.`
# * Combine the date columns into a single ISO 8601 date column.
# * Assign to the global environment with the name `visits_tidy.`



# 5 -----------------------------------------------------------------------

# Using `visits_tidy`, `detections` (a list item in dc_cats), and a join, 
# verify that each visit had at least one recorded animal detection.



# 6 -----------------------------------------------------------------------

# The data frame represented by the list item named sites has a primary key
# and a few columns of interest that should be numeric but are currently
# character vectors.

# * Extract sites from `dc_cats`.
# * As parsimoniously as possible, convert `percent_impervious` and 
#   `population_density` to numeric.
# * Maintain only the primary key and the two columns that you converted 
#   to numeric.
# * Assign to your global environment with the name `site_characteristics`,



# 7 -----------------------------------------------------------------------

# The list item `detections` contains misspellings and inconsistencies
# in the species column and more information than we're currently interested
# in.

# * As parsimoniously as possible, and without using `if_else()` or
#   `case_when()`, repair the spelling of the species such that the species
#   names are provided as "cat", "dog" and "squirrel".
# * Generate a summary table that displays the total count of animals
#   observed per species and `visit_id`.
# * Assign to your global environment with the name `observations`.



# 8 -----------------------------------------------------------------------

tribble(
  ~"Impervious surface", ~"Classified land-use intensity",
  "0 - 10%",             "Low",
  "> 10 - 30%",          "Medium",
  "> 30%",               "High")

# Classify percent_impervious in the data frame `site_characteristics` as
# described in the table above. In doing so:

# * Assign the name urban_intensity to the derived column.
# * Maintain only fields site_id and urban_intensity.
# * Assign to your global environment with the name land_use.



# 9 -----------------------------------------------------------------------

# Using `observations` and `visits_tidy` (or their lifelines):

# * Create a summary table that shows the number of cat observations 
#   for each site_id and visit_id.
# * Assign the name "n_per_visit" to the derived variable.
# * Maintain the variables visit_id, site_id, and n_per_visit in the
#   resultant object.
# * Assign the summary table to your global environment with the name
#   `cats_per_visit`.



# 10 ----------------------------------------------------------------------

# Generate a boxplot where the x-axis is the classified urban intensity and
# the y-axis is the number of observations. In doing so, arrange the levels 
# of `urban_intensity` from "Low" to "High".


