
# Script file for problem set 5: District cats

# 1 -----------------------------------------------------------------------

# Before opening your script file for this problem set, change the name of
# `problem_set_4.R` to "problem_set_4_[last name]_[first name].R" using a snake
# case naming convention. *Note: You will submit this script file as your
# assignment*.

# 2 -----------------------------------------------------------------------

# Open the script file in RStudio and attach the core tidyverse packages to your
# current R session.

library(tidyverse)

# 3 -----------------------------------------------------------------------

# Read in `district_cats.rds` and globally assign the list object to the name
# `dc_cats`.

dc_cats <- 
  read_rds("data/raw/district_cats.rds")

# [[-0.025]] Code formatting: Maintain one blank line between code blocks and
# comments. In your version there were additional spaces prior to the section
# header.

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
    col = "date", 
    c(
      year, 
      month, 
      day
    ),
    sep = "-"
  ) %>% 
  mutate(
    date = 
      date %>% 
      str_replace("June", "06") %>% 
      str_replace("July", "07") %>% 
      str_replace("August", "08"), 
    date = as_date(date)
  ) 

# [[-0.10]] Code parsimony: 
# * It was not necessary to replace month names with numbers. The `as_date`
#   function works with month names, name abbreviations, or numbers.
# * It is not necessary to specify adjacent variables with `c()` when selecting
#   columns. Use `:` instead.

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
      c(percent_impervious,
        population_density
      ),
      as.numeric
    ),
    .keep = "none"
  ) 

# [[-0.15]] Code formatting: If a function spans more than one line of code, the
# opening parentheses should be followed by a line break.

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
      str_replace("[Cc]at|Domestic cat", "cat") %>% 
      str_replace("[Dd]og|Puppy dog", "dog") %>% 
      str_replace("[Ss]quirrel|squrell", "squirrel")
  ) %>% 
  summarize(
    count = n(), 
    .by = c(visit_id, species)
  )

# [[-0.50]] Incorrect: To count the number of animals observed, it was 
# necessary to use `sum()` ... `n()` is just a row counter.

# [[-0.15]] Code parsimony: Regex could have been simplified.

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
        percent_impervious <= 15 ~ "Rural",
        percent_impervious <= 25 ~ "Low-intensity suburb",
        percent_impervious <= 40 ~ "High-intensity suburb", 
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
  observations %>% 
  full_join(
    visits_tidy,
    by = "visit_id"
  ) %>% 
  mutate(
    presence_absence = 
      if_else( 
        species == "cat",
        if_else(count < 1,
                "0",
                "1"
        ), "0"
      ),  
    presence_absence = as.numeric(presence_absence)
  ) %>% 
  summarize(
    presence_absence = sum(presence_absence),
    .by = c(visit_id, site_id)
  )

# [[-1.0]] `<` is not among the functions that you may use for this
# assignment.

# [[-0.20]] Code parsimony:
# * Your nested `if_else()` and your `summarize()` would not have been
#   necessary if observations were filtered to cats.
# * If you are generating numeric values, it is not necessary to specify them
#   in quotes.

# [[-0.20]] Code formatting: 
# * If a function spans more than one line of code, the opening parentheses 
#   should be followed by a line break.
# * If you provide three or more arguments to a function, place each argument
#   on its own line.

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

cat_occurrence %>% 
  left_join(
    land_use,
    by = "site_id"
  ) %>% 
  summarize(
    n = sum(presence_absence), 
    .by = urban_intensity
  ) %>% 
  mutate(
    urban_intensity = fct_relevel(
      urban_intensity, 
      "Rural",
      "Low-intensity suburb", 
      "High-intensity suburb", 
      "Urban"
    ),
    proportion = n / 81 
  ) %>% 
  ggplot() + 
  aes(
    x = urban_intensity, 
    y = proportion
  ) + 
  geom_bar(stat = "identity") + 
  labs(
    x = "Urban intensity", 
    y = "Proportion of visits", 
    title = "Proportion of cat detections across urban intensity classes"
  ) + 
  scale_y_continuous(
    limits = c(0, 0.8)
  ) + 
  theme(
    panel.background = 
      element_rect(
        fill = "white", 
        color = "black"
      ), 
    panel.grid = element_blank(), 
    text = element_text(family = "Times New Roman"), 
    plot.title = element_text(size = 16), 
    axis.title = element_text(size = 14)
  )

# [[-0.45]] Incorrect:
# * Did not correctly calculate the proportion of visits with cat detections
#   for each urban intensity class.
# * The y-axis does not range from 0 to 0.8.
# * The label for the y-axis was not correct

# [[-0.15]] Code formatting: If a code block spans more than one line of code,
# add a new line after the assignment operator.
