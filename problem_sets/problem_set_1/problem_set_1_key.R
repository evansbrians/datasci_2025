# Script file for Problem Set 1.

# question 1 --------------------------------------------------------------

# Before opening your script file for this problem set, change the name of
# the `problem_set_1.R` to "problem_set_1_[last name]_[first name].R" using
# a snake case naming convention. *Note: You will submit this script file 
# as your assignment*.

# question 2 --------------------------------------------------------------

# Open the script file in RStudio and attach the tidyverse metapackage to 
# your current R session:

library(tidyverse)

# question 3 --------------------------------------------------------------

# Using the *relative file path*, read in the worksheet
# "point_count_observations" from the Microsoft Excel file
# "data/raw/bird_counts.xlsx" as a data frame and assign the object to the name
# `bird_counts`. Make sure that your column names match the variables described
# in About the data!

bird_counts <- 
  readxl::read_excel(
    "data/raw/bird_counts.xlsx", 
    sheet = "point_count_observations",
    skip = 2
  )

# question 4 --------------------------------------------------------------

# Explore the structure of `bird_counts` ...
  
# Write a line of code that generates a one-value character vector that
# describes how the object assigned to the name `bird_counts` is stored in
# your computer's memory:

typeof(bird_counts)

# Write a line of code that generates a character vector that describes the
# class of the object assigned to the name `bird_counts`:

class(bird_counts)

# Write a line of code that prints the structure of the object assigned to the
# name `bird_counts`:

str(bird_counts)

# Write a line of code that prints a reference tree for all objects in your
# global environment (i.e., how the data are stored in your computer's
# random access memory; *Hint: See **Lesson 1.5: Assignments***).

lobstr::ref(.GlobalEnv)

# question 5 --------------------------------------------------------------

# Subset and arrange `bird_counts` ...

# Subset `bird_counts` to the first five rows:

slice_head(bird_counts, n = 5)

# Subset `bird_counts` to the last three rows:

slice_tail(bird_counts, n = 3)

# Arrange the rows of `bird_counts` by the variable `count` in descending
# order:

bird_counts %>% 
  arrange(
    desc(count)
  )

# Or:

arrange(
  bird_counts,
  desc(count)
)

# Subset `bird_counts` to the species "CACH" (Carolina chickadee).

bird_counts[bird_counts[["species"]] == "CACH", ]

# Subset `bird_counts` to the species "AMRO" (American robin), "GRCA" (Gray
# catbird), and "NOCA" (Northern cardinal; see Preliminary lesson 4: 
# Indexing):

bird_counts[
  bird_counts[["species"]] %in% 
    c(
      "AMRO", 
      "GRCA", 
      "NOCA"
    ), 
]

# question 6 --------------------------------------------------------------

# Describe and modify the variables assigned to `bird_counts` ...

# The variable `diet` represents a character vector. Subset the data to a
# character vector of unique character values:

unique(bird_counts[["diet"]])

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
# dependent on the output of a previous step. This is accomplished here by
# assigning the object generated at each step of the chain to a name.

# Assignment version (given):

diets <- bird_counts$diet

diets_factor <- 
  factor(diets)

typeof(diets_factor)

# Modify this operation such that only "Functions that you may use in this
# assignment" are used and the output is produced with a single, *nested*
# code block without assigning any names to your global environment:

typeof(
  factor(bird_counts[["diet"]])
)

# Modify this operation such that only "Functions that you may use in this
# assignment" are used and the output is produced with a single, *piped* 
# code block without assigning any names to your global environment.:

bird_counts[["diet"]] %>% 
  factor() %>% 
  typeof()

# Or:

bird_counts %>% 
  .[["diet"]] %>% 
  factor() %>% 
  typeof()

# Or:

factor(bird_counts[["diet"]]) %>% 
  typeof()

# question 8 --------------------------------------------------------------

# The code below represents a chained analysis that uses nested functions 
# to connect steps in the process.

# Nested version (given):

levels(
  factor(bird_counts$foraging)
)

# Modify this operation such that only "Functions that you may use in this
# assignment" are used, there are no nested functions, and the
# output is produced with a single, piped code block with no global
# assignments:

bird_counts[["foraging"]] %>% 
  factor() %>% 
  levels()

# Or:

bird_counts %>% 
  .[["foraging"]] %>% 
  factor() %>% 
  levels()

# Or:

factor(bird_counts[["foraging"]]) %>% 
  levels()

# question 9 --------------------------------------------------------------

# The code below represents a chained analysis where the object produced at
# each step in the chain is assigned to a name. Modify this operation such 
# that no new names are assigned to the global environment and each step in
# the chain is connected by a pipe.

# Assignment version (given):

bird_counts_chickadee <-
  bird_counts[bird_counts$species == "CACH", ]

chickadee_counts <- bird_counts_chickadee$count

mean(chickadee_counts)

# Piped version:

bird_counts[bird_counts[["species"]] == "CACH", ] %>% 
  .[["count"]] %>% 
  mean() 

# Or:

bird_counts %>% 
  .[.[["species"]] == "CACH", ] %>% 
  .[["count"]] %>% 
  mean() 

# question 10 -------------------------------------------------------------

# The code below contains ten violations of the "Course Style Guide". 
# Without removing comments or changing the code output, modify the code 
# block such that it follows the conventions of the style guide. 

# Poorly formatted version:

#Arrange by site, diet, and species, alphabetically:
birdCountsSorted= bird_counts%>% 
  arrange(site, diet,species) # Sort rows

# Answer:

# Arrange by site, diet, and species, alphabetically:

bird_counts_sorted <-
  bird_counts %>% 
  
  # Sort rows:
  
  arrange(
    site, 
    diet, 
    species
  ) 
  

