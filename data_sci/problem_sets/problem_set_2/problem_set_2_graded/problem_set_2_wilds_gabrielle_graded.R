
# Problem set 2

# 1 -----------------------------------------------------------------------

# Before opening your script file for this problem set, change the name of
# the `problem_set_2.R` to "problem_set_2_[last name]_[first name].R" using
# a snake case naming convention. *Note: You will submit this script file 
# as your assignment*.

# 2 -----------------------------------------------------------------------

# Open the script file in RStudio and attach the tidyverse metapackage to 
# your current R session:

library(tidyverse)

# 3 -----------------------------------------------------------------------

# Using the *relative file path*, read in the file "coqui_counts.rds" 
# and globally assign the object to the name `coqui`. 

coqui <- readr::read_rds("data/raw/coqui_counts.rds")

# [[No points removed]] Code parsimony: Because readr is a member of the core
# tidyverse, it is not necessary to specify `readr::` when running a function
# from that package.

# 4 -----------------------------------------------------------------------

# The location of a point on the Earth's surface is represented by two
# variables on different axes -- a point's longitudinal and latitudinal
# position. Because of this, the column `coordinates` violates Codd's 
# first normal rule that all values are atomic and Hadley Wickham's tidy 
# data rule that each variable forms a column.

# Split the column `coordinates` into the columns `longitude` and
# `latitude` and globally assign the resultant object to the name
# `coqui_coord_fix`:

coqui_coord_fix <- 
  coqui %>%
  separate(
    col = "coordinates",
    into = 
      c(
        "longitude",
        "latitude"
        ),
    sep = ","
  )

# Remove the name `coqui` from your global environment:

rm(coqui)

# 5 -----------------------------------------------------------------------

# The column `habitat_class` is derived from, and therefore transitively
# dependent on, the column `habitat`. 

# Remove the `habitat_class` column and globally assign the resultant 
# object to the name`coqui_no_class`:

coqui_no_class <-
  coqui_coord_fix %>%
    select(!habitat_class)

# Remove the name `coqui_coord_fix` from your global environment:

rm(coqui_coord_fix)

# 6 -----------------------------------------------------------------------

# As described in the metadata, this data frame represents counts of coqui 
# frogs. Each observation is a count at a given distance on a given 
# transect. Currently, `coqui_no_class` violates two aspects of Codd's 
# first normal rule and the tidy data rules that each row represents an
# observation and every column represents a variable. 

# Fix this such that the resultant object contains the variables
# `distance_class` and `count`, then globally assign the object to the 
# name `coqui_long`.

coqui_long <- 
  coqui_no_class %>%
    pivot_longer(
      cols = "count_0-10m":"count_20-30m",
      names_to = "distance_class",
      values_to = "count"
    )

# Remove the name `coqui_no_class` from your global environment:

rm(coqui_no_class)

# 7 -----------------------------------------------------------------------

# Write a code block that produces a data frame that displays the number
# of transects that were sampled within each habitat type:

transect_counts_by_habitat <- 
  coqui_long %>%
  count(habitat)

# [[-0.25]] Incorrect: This counted the number of observations, not transects.

# [[-0.25]] You were not asked to a assign a name to the global environment in
# this question.

# [[No points removed]] Code formatting: Maintain one blank line between code
# blocks and comments. In your version there were additional spaces prior to the
# section header.

# 8 -----------------------------------------------------------------------

# Generate a plot that displays the number of observations per habitat type
# and site, with habitat on the x-axis and the fill color of the bars
# determined by site:

coqui_long %>%
  ggplot() +
  aes(
    x = habitat,
    fill = site
  ) + 
  geom_bar() + 
  scale_fill_manual(
    values =
      c(
        nora = "#EB9BB6",
        treefall = "#BBD9F1"
      )
  ) +
  labs(
    title = "Coqui observations by habitat type and site",
    x = "Habitat",
    y = "Count"
  ) + 
  scale_y_continuous(
    limits = c(0, 170),
    expand = c(0, 0)
  )
 
# [[-0.15]] Code formatting: Maintain one blank line between code blocks and
# comments. In your version there were additional spaces prior to the section
# header.

# 9 -----------------------------------------------------------------------

# Exploring duplicate rows among subsets of variables ...

# * Write a code block that subsets the data to site, longitude, and latitude
#   and removes duplicate rows:

site_info <- 
  coqui_long %>%
  select(site:latitude) %>%
  distinct()
  

# * Write a code block that subsets the data to site, longitude, latitude, and
#   transect_id and removes duplicate rows:

site_info_ids <- 
  coqui_long %>%
  select(site:transect_id) %>%
  distinct()

# * Write a code block that subsets the data to transect_id and habitat and
#   removes duplicate rows:

habitat_trans_ids <- 
  coqui_long %>%
  select(transect_id:habitat) %>%
  distinct()

# * Write a code block that subsets the data to transect_id, habitat, and date
#   and removes duplicate rows:

date_habitat_ids <- 
  coqui_long %>%
  select(transect_id:date) %>%
  distinct()

# [[-0.05]] Code formatting: Maintain one blank line between code blocks and
# comments.

# 10 ----------------------------------------------------------------------

# Please write a brief comment that describes which tidy data rule is being 
# violated in `coqui_long` and how you were able to determine that.

# coqui_long violated the third tidy rule, which is that each level of
# observation formed a table. coqui long included observations of the site 
# itself (latitude and longitude), and then observations of the sampling at the 
# site (date and coqui counts). I was able to determine this by breaking down
# the qualities of each column, and determining what other column it was 
# referring to directly. The first column that tipped me off to this
# non-compliance was the habitat column, which describes the site, but is not
# an observation related to the count of frogs at the site. 

