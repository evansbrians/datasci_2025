
# Script file for problem set 4: Cicada emergence

# question 1 --------------------------------------------------------------

# Before opening your script file for this problem set, change the name of
# `problem_set_4.R` to "problem_set_4_[last name]_[first name].R" using a snake
# case naming convention. *Note: You will submit this script file as your
# assignment*.

# question 2 --------------------------------------------------------------

# Open the script file in RStudio and attach the core tidyverse packages to your
# current R session.

library(tidyverse)

# 3 -----------------------------------------------------------------------

# Read in `data/raw/cicadas_2021.rds` and globally assign the list to the
# name cicada_list:

cicada_list <-
  read_rds("data/raw/cicadas_2021.rds")

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

cicada_research_quality <-
  cicada_list %>% 
  pluck("cicada_observations_2021") %>%
  filter(
    str_detect(state, "MD|VA|DC"),
    quality_grade == "research"
  ) %>% 
  drop_na(date) %>% 
  select(
    date,
    species = "scientific_name",
    address)

# [[-0.75]] `drop_na` is not among the functions that you may use for this
# assignment.

# [[-0.15]] Code parsimony: With `select()`, it is not necessary to place
# variable names inside of quotes.

#  [[-0.15]] Code formatting: If a function spans more than one line of code,
#  closing parentheses should be placed on their own line and indented to the
#  same level as the start of the function.

# 5 -----------------------------------------------------------------------

# This study is focused on the three species of Brood X cicada. Without using
# `filter()`, please:

# * Subset `cicada_research_quality` to Brood X species;
# * Globally assign the resultant object to the name `brood_x_observations`.

brood_x_observations <-
  cicada_research_quality %>%
  .[.$species %in% c("Magicicada septendecim", "Magicicada cassini", "Magicicada septendecula"), ]

# [[-0.50]] `[...]` is not among the functions that you may use for this
# assignment.

# [[-0.10]] Code formatting:
# * If you provide three or more arguments to a function, place each argument
#   on its own line.
# * Code and comments should not exceed 80 characters in width (if it is 
#   avoidable) – add a line break, if possible.

# 6 -----------------------------------------------------------------------

# Unfortunately for us, "Park" is a common street name and there is a type of
# road in the area called a "parkway" (which may be abbreviated as "pkwy").
# Let's explore values in the `address` variable. Without using `filter()`,
# extract a vector of addresses from the data frame `brood_x_observations` 
# such that the extracted values:
# * Starts with the word "Park" or "park", *or*
# * Contain the strings "Parkway", "parkway", "Pkwy", or "pkwy";
# * Are globally assigned to the name `not_parks`.

not_parks <-
  brood_x_observations$address %>% 
  keep(
    str_detect(.,"^[Pp]ark|[Pp]arkway|[Pp]kwy")
  )

# [[-0.75]] `$` is not among the functions that you may use for this
# assignment.

# [[-0.15]] Code parsimony: Regex could have been simplified.

# 7 -----------------------------------------------------------------------

# Subset `brood_x_observations` such that:

# * The `address` variable contains "park", "Park", or "Zoo";
# * The `address` is *not* found in the vector assigned to the name
#   `not_parks`;
# * The resultant object is a tibble data frame that is globally assigned to 
#   the name `brood_x_parks`.

brood_x_parks <-
  brood_x_observations %>%
  filter(str_detect(brood_x_observations$address, "[Pp]ark|Zoo"), 
         !(brood_x_observations$address %in% not_parks))

# [[-0.50]] `$` is not among the functions that you may use for this
# assignment.

# [[-0.10]] Code parsimony:
# * It was not necessary to wrap your `%in%` test within parentheses
# * With `filter()`, it is not necessary to specify the name of the data frame.

# [[-0.10]] Code formatting:
# * Include no more than one prefix function per line of code.
# * Indentation and hanging parentheses: If a function spans more than one line
#   of code, closing parentheses should be placed on their own line and 
#   indented to the same level as the start of the function.
# * The first argument in a function should be indented two spaces (one tab
#   stop) relative to the start of the line above.
# * Maintain one blank line between code blocks and comments. In your version
#   there were additional spaces prior to the section header.

# 8 -----------------------------------------------------------------------

# Using `brood_x_parks`, generate a summary table that:

# * Provides the number of observations of brood X cicada within parks where 
#   more than 40 cicadas have been observed;
# * Is arranged from the highest to lowest number of cicada observations;
# * Is globally assigned to the name `brood_x_park_summary`.

brood_x_park_summary <-
  brood_x_parks %>%
  group_by(address) %>%
  summarize(n = n()) %>% 
  filter(n > 40) %>%
  arrange(desc(n))

# [[-0.50]] `group_by` is not among the functions that you may use for this
# assignment.

# [[-0.10]] Code formatting: Include no more than one prefix function per line
# of code.

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

brood_x_parks %>% 
  semi_join(brood_x_park_summary %>%
              slice_max(n, n = 4),
            by = "address") %>%
  filter(
    str_detect(date, "2021-04")
  ) %>%
  ggplot() +
  aes(x = date,
      fill = species
  ) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ address,
             ncol = 1
  ) +
  scale_y_continuous(
    expand = 
      expansion(
        mult = c(0, 0.2)
      )
  ) +
  scale_fill_brewer("species") +
  labs(
    title = "Density distribution of Brood X cicada observations by date",
    x = "Date",
    y = "Density",
    fill = "Species"
  ) +
  theme(
    panel.background = element_rect(
      fill = "white",
      color = "black"),
    panel.grid = element_blank(),
    strip.background = element_rect(color = "black"),
    text = element_text(
      family = "serif"),
    plot.title = element_text(size = 16),
    strip.text = element_text(size = 14),
    axis.title = element_text(size = 14)
  )

# [[-0.10]] Incorrect: Inside of `facet_wrap()`, you needed to set `scales =
# "free_y"`.

# [[-0.25]] Code formatting:
# * If a function spans more than one line of code, the opening parentheses 
#   should be followed by a line break.
# * Indentation: If the opening parentheses and the first argument of a 
#   function are on different lines, the first argument should be indented two
#   spaces (one tab stop) relative to the start of the line above.
# * Indentation and hanging parentheses: If a function spans more than one line
#   of code, closing parentheses should be placed on their own line and 
#   indented to the same level as the start of the function.
