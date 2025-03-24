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
  skip = 1
  )

# [[-0.10]] Code formatting: Indentation is off -- if the opening parentheses
# and the first argument of a function are on different lines, the first
# argument should be indented two spaces (one tab stop) relative to the start of
# the line above.

# question 4 --------------------------------------------------------------

# Explore the structure of `bird_counts` ...

# Write a line of code that generates a one-value character vector that
# describes how the object assigned to the name `bird_counts` is stored in
# your computer's memory:

lobstr::ref(bird_counts)

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

# [[-0.025]] Incorrect: For the first bullet point, you were asked to produce
# a one-value character vector.

# question 5 --------------------------------------------------------------

# Subset and arrange `bird_counts` ...

# Subset `bird_counts` to the first five rows:

dplyr::slice_head(
  bird_counts,
  n = 5
)

# Subset `bird_counts` to the last three rows:

dplyr::slice_tail(
  bird_counts,
  n = 3
)

# Arrange the rows of `bird_counts` by the variable `count` in descending
# order:

arrange(bird_counts, 
  desc(count),
)

# Subset `bird_counts` to the species "CACH" (Carolina chickadee).

bird_counts[bird_counts[ ,"species"] == "CACH", ]

# Subset `bird_counts` to the species "AMRO" (American robin), "GRCA" (Gray
# catbird), and "NOCA" (Northern cardinal; see Preliminary lesson 4: 
# Indexing):

bird_counts[
  bird_counts[ ,"species"] == 
    c(
      "AMRO", 
      "GRCA", 
      "NOCA"), 
]

# [[-0.50]] Incorrect (bullet point 5): When comparing between sets of values,
# use `%in%`.

# [[-0.10]] Code formatting:
  
# * For functions that span more than one line, opening parentheses should be
#   followed by a line break.

# * Commas should be followed by one trailing space.

# * If a function spans more than one line of code, closing parentheses should
#   be placed on their own line and indented to the same level as the start of
#   the function.

# [[No points removed]] Code parsimony: Because dplyr is a member of the core
# tidyverse, it is not necessary to specify `dplyr::` when running a function
# from that package.

# question 6 --------------------------------------------------------------

# Describe and modify the variables assigned to `bird_counts` ...

# The variable `diet` represents a character vector. Subset the data to a
# character vector of unique character values:

unique(
  bird_counts[ ,"diet"]
)

# In a single line of code, generate a statistical summary for all of the
# variables in `bird_counts`:

summary(bird_counts)

# Without the use of the `<-` function, change of the name of the variable
# `species` to `species_code`:

rename(
  bird_counts,
  species_code = "species"
)

# [[-0.05]] Code formatting: Commas should be followed by one trailing space.

# [[No points removed]] Code parsimony: In the tidyverse, quotes are not
# necessary when referring to a column name.

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

#Error in xtrfrm.data.frame(c) : cannot xtfrm data frames

typeof(
  factor(
  bird_counts[ ,"diet"]
  )
)


# Modify this operation such that only "Functions that you may use in this
# assignment" are used and the output is produced with a single, *piped* 
# code  block without assigning any names to your global environment.:

bird_counts[ ,"diet"] %>% 
  factor() %>% 
  typeof()

# [[-0.10]] Code formatting:

# * Commas should be followed by one trailing space.

# * Indentation is off -- if the opening parentheses and the first argument of
#   a function are on different lines, the first argument should be indented 
#   two spaces (one tab stop) relative to the start of the line above.

# * Maintain one blank line between code blocks and comments.

# * Add a single space between the hashtag (#) and comment.

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

bird_counts[ ,"foraging"] %>% 
  factor() %>% 
  levels()

# [[-0.05]] Code formatting: Commas should be followed by one trailing space.

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

# Answer:

bird_counts[
  bird_counts[ ,"species"] == "CACH", ][,"count"] %>% 
  mean()

# [[-0.15]] Incorrect: Failed to connect each step in the process with a pipe.

# [[-0.10]] Code formatting:

# * Commas should be followed by one trailing space.

# * When subsetting or extracting a multidimensional object inside of square
#   brackets, include a single space for rows or columns in which all data
#   are returned.
  
# question 10 -------------------------------------------------------------

# The code below contains ten violations of the "Course Style Guide". 
# Without removing comments or changing the code output, modify the code 
# block such that it follows the conventions of the style guide. 

# Poorly formatted version (given):

#Arrange by site, diet, and species, alphabetically:
birdCountsSorted= bird_counts%>% 
  arrange(site, diet,species) # Sort rows

# Answer:

# Arrange by site, diet, and species, alphabetically:

bird_counts_sorted <-  
  bird_counts %>% 
  
  # Sort rows
  
  arrange(site, 
          diet, 
          species
  ) 

# [[-0.10]] Code formatting: If a function spans more than one line of code,
# opening parentheses should be followed by a line break.
