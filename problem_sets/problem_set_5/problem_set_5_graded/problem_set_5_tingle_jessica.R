
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
  unite(
    col = date,
    year,
    month,
    day
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
      c(percent_impervious, population_density),
      as.numeric
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
      str_replace("Domestic |Puppy ", "") %>%
      
      # the next three lines seem unparsimonious, 
      # but I don't know what else to do since 
      # str_to_lower() isn't an option for this assignment,
      # and I can't find the right magic in the regex documentation
      
      str_replace("C", "c") %>% 
      str_replace("S", "s") %>% 
      str_replace("D", "d") %>% 
      str_replace("squrell", "squirrel")
  ) %>% 
  summarize(
    count = sum(count),
    .by = c(visit_id, species)
  )

# [[-0.15]] Code parsimony: Regex could have been simplified. Here, you ran
# more `str_replace` functions than was necessary. As a consequence, in 
# benchmarking your answer vs. the answer in the key, your answer ended up
# requiring 25% more memory and 20% more processing time. Metacharacters are
# your awkward friends (and sometimes enemies).

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
  visits_tidy %>% 
  left_join(
    observations %>% 
      filter(species == "cat"),
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

# Fun is worth point loss almost every time!

extrafont::loadfonts(device = "win")

# Now get to work

# Join the tibbles:

cat_occurrence %>% 
  left_join(
    land_use, 
    by = "site_id"
  ) %>%
  
  # Calculate the proportion of visits where a cat was observed:
  
  summarize(
    cat_ratio =
      sum(presence_absence) /
      n(),
    .by = urban_intensity
  ) %>% 
  
  # Re-order urban_intensity for sensible plotting:
  
  mutate(
    urban_intensity =
      urban_intensity %>% 
      fct_relevel(
        "Rural",
        "Low-intensity suburb",
        "High-intensity suburb",
        "Urban"
      )
  ) %>% 
  
  # Make a bar plot:
  
  ggplot() +
  aes(
    x = urban_intensity,
    y = cat_ratio
  ) +
  geom_bar(
    stat = "identity",
    fill = "#7D44A5"
    ) +
  scale_y_continuous(
    limits = c(0, 0.8),
    expand = c(0, 0)
  ) +
  labs(
    title = "Proportion of cat detections across urban intensity classes",
    subtitle = "An exercise in slightly reckless aesthetic choices",
    x = "Urban intensity",
    y = "Proportion of visits with cats present"
  ) +
  
  # Time to get frivolous:
  
  theme(
    panel.background = 
      element_rect(
        fill = "whitesmoke",
        color = "black"
      ),
    plot.background =
      element_rect(
        fill = "whitesmoke"
      ),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "peachpuff3"),
    panel.grid.minor.y = element_line(color = "lemonchiffon3"),
    
    # This monster can't be contained:
    
    plot.title = 
      element_text(
        family = "Broadway",
        size = 18
        ),
    plot.subtitle = 
      element_text(
        family = "Brush Script MT",
        size = 14,
        color = "#A25090"
      ),
    axis.title = 
      element_text(
        family = "Segoe UI",
        size = 16
        ),
    axis.text.y =
      element_text(
        family = "Segoe UI",
        size = 12
      ),
    axis.text.x =
      element_text(
        family = "Segoe UI Light",
        size = 12
      )
  )

# You are awesomely ridiculous!!! You have outdone the Papyrus in a big way!
