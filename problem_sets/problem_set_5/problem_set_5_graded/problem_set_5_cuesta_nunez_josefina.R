
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
  unite(col = "date", year:day) %>% 
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
      c(percent_impervious, population_density),
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
      str_replace(
        "[Dd]og|Puppy dog",
        "dog"
      ) %>% 
      str_replace(
        "^[Ss]quirrel|squrell",
        "squirrel"
      ) %>% 
      str_replace(
        "[Cc]at|Domestic cat",
        "cat"
      )
  ) %>% 
  summarize(
    count = sum(count),
    .by = c(species, visit_id)
  )

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
  filter(species == "cat") %>% 
  full_join(
    .,
    visits_tidy,
    by = "visit_id"
  ) %>% 
  mutate(
    visit_id,
    site_id,
    presence_absence =  
      if_else(
        is.na(count),
        0,
        1
      ),
    .keep = "none"
  )

# [[-0.20]] Code parsimony: In chained code, when the resultant object from the
# previous step is passed to the first argument of the next step, a placeholder
# is not necessary.

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
  full_join(
    .,
    land_use,
    by = "site_id"
  ) %>%
  mutate(
    presence_absence =
      presence_absence %>%
      if_else(
        is.na(.),
        0,
        .
      ),
    urban_intensity = urban_intensity %>% 
      fct_relevel(
        c(
          "Rural",
          "Low-intensity suburb",
          "High-intensity suburb",
          "Urban"
        )
      )
  ) %>% 
  summarize(
    proportion = sum(presence_absence)/n(),
    .by = urban_intensity
  ) %>% 
  ggplot() + 
  aes(
    x = urban_intensity,
    y = proportion
  ) + 
  geom_bar(
    stat = "identity"
  ) +
  labs(
    x = "Urban intensity",
    y = "Proportion of visits with cats present",
    title = "Proportion of cat detections across urban intensity classes"
  ) +
  scale_y_continuous(
    limits = c(0,0.8),
    expand = c(0,0)
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey",linetype = "dashed"),
    axis.line = element_line (colour = "black", linewidth = .6),
    title = element_text(size = 14),
    axis.title = element_text(size = 13)
  )


# [[-0.15]] Code parsimony: 
# * You had to run the presence_absence code again due to the `full_join`. A 
#   `left_join` would have save you from repeating that step.
# * In chained code, when the resultant object from the previous step is passed
#   to the first argument of the next step, a placeholder is not necessary.

# [[-0.15]] Code formatting:
# * Infix functions should be separated from surrounding code with a single
#   leading and trailing space.
# * Commas should be followed by one trailing space.
# * Parentheses, curly braces, and square bracket operators should not be
#   preceded or followed by a space.
# * If you have more than one = in a function, place each argument on its own
#   line.
