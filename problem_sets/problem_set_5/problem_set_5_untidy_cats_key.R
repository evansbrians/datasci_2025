# Problem set 5

# 2 -----------------------------------------------------------------------

# Attach the tidyverse metapackage to your current R session.

library(tidyverse)

# 3 -----------------------------------------------------------------------

# Read in `district_cats.rds` as a list file and globally assign the list
# object to the name `dc_cats`.

dc_cats <- 
  read_rds("data/raw/district_cats.rds")

# 4 -----------------------------------------------------------------------

# The table represented by the list item visits is unfortunately not tidy.
# Normalize this list item:

# * Extract visits from `dc_cats.`
# * Combine the date columns into a single ISO 8601 date column.
# * Assign to the global environment with the name `visits_tidy.`

visits_tidy <- 
  dc_cats %>% 
  pluck("visits") %>% 
  unite(
    "date",
    year:day,
    sep = "-"
  ) %>% 
  mutate(
    date = as_date(date)
  )

# 5 -----------------------------------------------------------------------

# Using `visits_tidy`, `detections` (a list item in dc_cats), and a join, 
# verify that each visit had at least one recorded animal detection.

visits_tidy %>% 
  anti_join(
    dc_cats %>% 
      pluck("detections"),
    by = "visit_id"
  )

# This is also fine:

visits_tidy %>% 
  anti_join(
    pluck(dc_cats, "detections"),
    by = "visit_id"
  )

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

site_characteristics <- 
  dc_cats %>% 
  pluck("sites") %>% 
  mutate(
    site_id,
    across(
      c(percent_impervious, population_density),
      ~ as.numeric(.x)
    ),
    .keep = "none"
  )

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

observations <- 
  dc_cats %>% 
  pluck("detections") %>% 
  mutate(
    species = 
      species %>% 
      str_replace("(Domestic )?[Cc]at", "cat") %>% 
      str_replace("(Puppy )?[Dd]og", "dog") %>% 
      str_replace("[Ss]qui?rr?ell?", "squirrel")
  ) %>% 
  summarize(
    count = sum(count),
    .by = visit_id:species
  )

# 8 -----------------------------------------------------------------------

tribble(
  ~"Impervious surface", ~"Classified land-use intensity",
  "0 - 10%",             "Low",
  "> 10 - 30%",          "Medium",
  "> 30%",               "High"
)

# Classify percent_impervious in the data frame `site_characteristics` as
# described in the table above. In doing so:

# * Assign the name urban_intensity to the derived column.
# * Maintain only fields site_id and urban_intensity.
# * Assign to your global environment with the name land_use.

land_use <- 
  site_characteristics %>% 
  mutate(
    site_id,
    urban_intensity = 
      case_when(
        percent_impervious < 10 ~ "Low",
        percent_impervious < 30 ~ "Medium", 
        TRUE ~ "High"),
    .keep = "none"
  )

# 9 -----------------------------------------------------------------------

# Using `observations` and `visits_tidy` (or their lifelines):

# * Create a summary table that shows the number of cat observations 
#   for each site_id and visit_id.
# * Assign the name "n_per_visit" to the derived variable.
# * Maintain the variables visit_id, site_id, and n_per_visit in the
#   resultant object.
# * Assign the summary table to your global environment with the name
#   `cats_per_visit`.

cats_per_visit <- 
  observations %>% 
  filter(species == "cat") %>% 
  full_join(
    visits_tidy,
    by = "visit_id"
  ) %>% 
  mutate(
    count = replace_na(count, 0)) %>% 
  summarize(
    n_per_visit = sum(count, na.rm = TRUE),
    .by = c(visit_id, site_id)
  )

# Or (even better)

cats_per_visit <- 
  observations %>% 
  filter(species == "cat") %>% 
  full_join(
    visits_tidy,
    by = "visit_id"
  ) %>% 
  mutate(
    site_id,
    visit_id,
    n_per_visit = 
      replace_na(count, 0),
    .keep = "none"
  )

# 10 ----------------------------------------------------------------------

# Generate a boxplot where the x-axis is the classified urban intensity and
# the y-axis is the number of observations. In doing so, arrange the levels
# of `urban_intensity` from "Low" to "High".

cats_per_visit %>% 
  left_join(land_use, by = "site_id") %>% 
  mutate(
    urban_intensity = 
      urban_intensity %>% 
      fct_relevel(
        "Low",
        "Medium",
        "High"
      )
  ) %>% 
  ggplot() +
  aes(
    x = urban_intensity,
    y = n_per_visit
  ) +
  geom_boxplot()

