
# Script file for problem set 5: District cats

# 1 -----------------------------------------------------------------------

# Before opening your script file for this problem set, change the name of
# `problem_set_4.R` to "problem_set_4_[last name]_[first name].R" using a snake
# case naming convention. *Note: You will submit this script file as your
# assignment*.

# done

# 2 -----------------------------------------------------------------------

# Open the script file in RStudio and attach the core tidyverse packages to your
# current R session.

library(tidyverse)

# 3 -----------------------------------------------------------------------

# Read in `district_cats.rds` and globally assign the list object to the name
# `dc_cats`.

dc_cats <- 
  read_rds("data/raw/district_cats.rds")

# 4 -----------------------------------------------------------------------

# The table represented by the list item `visits` is unfortunately not tidy.
# Normalize this list item:

# * Extract `visits` from `dc_cats.`
# * Combine the columns `year`, `month`, and `day` into a single ISO 8601
#   formatted date class column.
# * Globally assign the resultant object to the name `visits_tidy`.

visits_tidy <-
  dc_cats %>% 
  pluck("visits") %>% 
  unite(
    col = date,
    year:day,
    sep = "-"
  ) %>% 
  mutate(
    date = as_date(date)
  )

# 5 -----------------------------------------------------------------------

# The data frame represented by the list item named sites has a primary key
# and a few columns of interest that should be numeric but are currently
# character vectors.

# * Extract sites from `dc_cats`;
# * As parsimoniously as possible, convert `percent_impervious` and 
#   `population_density` to numeric;
# * Maintain only the primary key and the two columns that you converted 
#   to numeric;
# * Globally assign the resultant object to the name `site_characteristics`.

site_characteristics <- 
  dc_cats %>% 
  pluck("sites") %>% 
  mutate(
    site_id,
    across(
      c(
        percent_impervious,
        population_density
      ),
      ~ as.numeric(.x)
    ),
    .keep = "none"
  )

# 6 -----------------------------------------------------------------------

# The list item `detections` in the list `district_cats` contains misspellings,
# inconsistencies in the species column, and more information than we are
# currently interested in.

# * As parsimoniously as possible, and without using `if_else()` or 
#   `case_when()`, repair the spelling within the `species` variable such 
#   that the values are provided as "cat", "dog" and "squirrel";
# * Generate a summary table where the variable `count` displays the total
#   count of animals observed per `species` and `visit_id`;
# * Globally assign the resultant object to the name `observations`.

observations <-
  dc_cats %>% 
  pluck("detections") %>% 
  mutate(
    species = 
      species %>% 
      str_replace("(Domestic )?[Cc]at", "cat") %>% 
      str_replace("(Puppy )?[Dd]og$", "dog") %>% 
      str_replace("[Ss]qui?r?rel?l", "squirrel")
  ) %>% 
  summarize(
    count = n(),
    .by = 
      c(
        visit_id, 
        species
      )
  ) 

# [[-0.50]] Incorrect: To count the number of animals observed, it was 
# necessary to use `sum()` ... `n()` is just a row counter.

# 7 -----------------------------------------------------------------------

# | Impervious surface | Classified land-use intensity |
# |    0 - 15%         |   Rural                       |  
# |    > 15 - 25%      |   Low-intensity suburb        |
# |    > 25 - 40%      |   High-intensity suburb       |
# |    > 40%           |   Urban                       |

# Classify `percent_impervious` in the data frame `site_characteristics` as
# described in the table above. In doing so:

# * Assign the name `urban_intensity` to the derived column;
# * Maintain only the fields `site_id` and `urban_intensity`;
# * Globally assign the resultant object to the name `land_use`.

land_use <-
  site_characteristics %>% 
  mutate(
    site_id,
    urban_intensity = 
      case_when(
        percent_impervious  <= 15 ~ "Rural",
        percent_impervious  <= 25 ~ "Low-intensity suburb",
        percent_impervious  <= 40 ~ "High-intensity suburb",
        TRUE ~ "Urban"
      ),
    .keep = "none"
  )

# 8 -----------------------------------------------------------------------

# Using `observations` and `visits_tidy` (or their lifelines):

# * Create a column that contains the value 1 for visits where at least  
#   one cat was detected and the value 0 for visits where no cats were 
#   detected;
# * Assign the name `presence_absence` to the derived variable;
# * Maintain only the variables `visit_id`, `site_id`, and `presence_absence`  
#   in the resultant object;
# * Globally assign the resultant object to the name `cat_occurrence`.

cat_occurrence <-
  visits_tidy %>% 
  full_join(
    observations %>% 
      filter(species == "cat"),
    by = "visit_id"
  ) %>% 
  mutate(
    visit_id,
    site_id,
    presence_absence = 
      case_when(
        count >= 1 ~ 1,
        TRUE ~ 0),
    .keep = "none"
  )

# [[-1.0]] `>=` is not among the functions that you may use for this assignment.

# 9 -----------------------------------------------------------------------

# Let's visualize the proportion of visits with cat detections for each urban
# intensity class. Using `cat_occurrence` and `land_use` (or their lifelines), 
# generate a bar plot where:

# * The x-axis represents the classified urban intensity;
# * The y-axis is the proportion of visits in which a cat was observed (number
#   of visits where cats were present / total number of visits);
# * The levels of `urban_intensity` are arranged in the order "Rural",
#   "Low-intensity suburb", "High-intensity suburb" and "Urban";
# * The x-axis is titled "Urban intensity", the y-axis is titled "Proportion of
#   visits with cats present", and the plot is titled "Proportion of cat
#   detections across urban intensity classes";
# * The y-axis ranges from 0 to 0.8;
# * You use three or more arguments of `theme()` to modify the theme elements
#   of the plot however you like!

# first we join cat_occurrence and land_use by site_id

cat_occurrence %>% 
  left_join(
    land_use,
    by = "site_id"
  ) %>%
  
  # converting the urban_intensity as factor and reorder levels
  
  mutate(
    urban_intensity =
      urban_intensity %>% 
      fct_relevel(
        c(
          "Rural",
          "Low-intensity suburb",
          "High-intensity suburb",
          "Urban"
        )
      )
  ) %>% 
  
  # summarizing and getting the proportion by urban_intensity
  
  summarize(
    cat_proportion = 
      sum(presence_absence == 1) / n(),
    .by = urban_intensity
  ) %>%
  
  # now we plot 
  
  ggplot() +
  aes(
    x = urban_intensity,
    y = cat_proportion
  ) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    limits = c(0,0.80)
  ) +
  labs (
    title = "Proportion of cat detections across urban intensity classes",
    x = "Urban intensity",
    y = "Proportion of visits with cats present"
  ) +
  theme(
    panel.background = 
      element_rect(
        fill = "white",
        color = "black"
      ),
    panel.grid = element_blank(),
    strip.background = element_rect(color = "black"),
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 14)
  )    

# [[-0.20]] Incorrect: The y-axis does not range from 0 to 0.8.

# [[-0.15]] Code parsimony: Since the values of `presence_absence` are currently
# one or zero, adding a logical test and then summing the integer values
# associated with the logical vector did not modify the code.

# [[-0.15]] Code formatting:
# * Parentheses, curly braces, and square bracket operators should not be
#   preceded or followed by a space.
# * Commas should be followed by one trailing space.
