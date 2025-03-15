# Script file for Problem Set 1.

# question 2 --------------------------------------------------------------

# Attach the tidyverse metapackage to your current R session:

library(tidyverse)

# question 3 --------------------------------------------------------------

# Read in the worksheet "point_count_observations" from the Microsoft Excel file
# "data/raw/bird_counts.xlsx" as a data frame and assign the object to the name
# `bird_counts`:

bird_counts <- 
  readxl::read_excel(
    "data/raw/bird_counts.xlsx", 
    sheet = "point_count_observations",
    skip = 2)

# question 4 --------------------------------------------------------------

# Explore the structure of `bird_counts` ...
  
# Write a line of code that generates a one-value character vector that
# describes how the object assigned to the name`bird_counts` is stored in your
# computer's memory:

typeof(bird_counts)

# Write a line of code that generates a one-value character vector that
# describes the class of the object assigned to the name`bird_counts`:

class(bird_counts)

# Write a line of code that prints the structure of the object assigned to the
# name `bird_counts`:

str(bird_counts)

# Write a line of code that prints a reference tree for all objects in your
# global environment:

lobstr::ref(.GlobalEnv)

# question 5 --------------------------------------------------------------

# Subset and arrange `bird_counts` ...

# Subset `bird_counts` to the first five rows:

slice_head(bird_counts, n = 5)

# Subset `bird_counts` to the last three rows:

slice_tail(bird_counts, n = 3)

# Arrange the rows of `bird_counts` by the variable `count` in descending order:

bird_counts %>% 
  arrange(
    desc(count)
  )

# Or:

arrange(
  bird_counts,
  desc(count)
)

# Using only the "Functions that you may use in this lesson" subset
# `bird_counts` to the species "AMRO" (American robin), "GRCA" (Gray catbird),
# and "NOCA" (Northern cardinal; see Preliminary lesson 4: Indexing):

bird_counts[bird_counts$species %in% c("AMRO", "GRCA", "NOCA"), ]

# question 6 --------------------------------------------------------------

# Describe and modify the variables assigned to `bird_counts` ...

# The variable `diet` represents a character vector. Subset the data to a
# character vector of unique character values:

unique(bird_counts$diet)

# In a single line of code, generate a statistical summary for all of the
# variables in `bird_counts`:

summary(bird_counts)

# Without the use of the `<-` function, change of the name of the variable
# `species` to `species_code`:

bird_counts %>% 
  rename(species_code = species)

# Or:

rename(bird_counts, species_code = species)

# question 7 --------------------------------------------------------------

# The code below represents a chained analysis — each step in the process is
# dependent on the output of a previous step. This is accomplished here be
# assigning the object generated at each step of the chain to a name.

# Assignment version (given):

diets <- bird_counts$diet

diets_factor <- factor(diets)

typeof(diets_factor)

# Modify this operation such that the output is produced with a single, nested
# code block with no global assignments (except `bird_counts`):

typeof(
  factor(bird_counts$diet)
)

# Modify this operation such that the output is produced with a piped block that
# contains no global assignments (except `bird_counts`):

bird_counts$diet %>% 
  factor() %>% 
  typeof()

# Or:

bird_counts %>% 
  .$diet %>% 
  factor() %>% 
  typeof()

# Or:

factor(bird_counts$diet) %>% 
  typeof()

# question 8 --------------------------------------------------------------

# The code below represents a chained analysis that uses nested functions to
# connect steps in the process.

# Nested version (given):

levels(
  factor(bird_counts$foraging)
)

# Modify this operation such that there are no nested functions and the object
# produced at each step in the chain is assigned to a name:

foraging_factor <- factor(bird_counts$diet)

levels(foraging_factor)

# Modify this operation such that the output is produced with a piped block that
# contains no global assignments (except `bird_counts`):

bird_counts$foraging %>% 
  factor() %>% 
  levels()

# Or:

bird_counts %>% 
  .$foraging %>% 
  factor() %>% 
  levels()

# Or:

factor(bird_counts$foraging) %>% 
  levels()

# question 9 --------------------------------------------------------------

# The code below represents a chained analysis where the object produced at each
# step in the chain is assigned to a name. Modify this operation such that no
# new names are assigned to the global environment and each step in the chain is
# connected by a pipe.

# Assignment version (given):

bird_counts_chickadee <-
  bird_counts[bird_counts$species == "CACH", ]

chickadee_counts <- bird_counts_chickadee$count

mean(chickadee_counts)

# Piped version:

bird_counts[bird_counts$species == "CACH", ] %>% 
  .$count %>% 
  mean() 

# Or:

bird_counts %>% 
  .[.$species == "CACH", ] %>% 
  .$count %>% 
  mean() 


