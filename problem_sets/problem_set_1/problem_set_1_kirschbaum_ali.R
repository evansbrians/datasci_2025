# Script file for Problem Set 1.

# Score: 78.75% See comments below.

# question 1 --------------------------------------------------------------

# Before opening your script file for this problem set, change the name of
# the `problem_set_1.R` to "problem_set_1_[last name]_[first name].R" using
# a snake case naming convention. *Note: You will submit this script file 
# as your assignment*.

# question 2 --------------------------------------------------------------

# Open the script file in RStudio and attach the tidyverse metapackage to 
# your current R session:

library(tidyverse)

# [[-0.05]] Code formatting: Maintain one blank line between code blocks and
# comments. In your version there were additional spaces prior to the section
# header.

# question 3 --------------------------------------------------------------

# Using the *relative file path*, read in the worksheet
# "point_count_observations" from the Microsoft Excel file
# "data/raw/bird_counts.xlsx" as a data frame and assign the object to the name
# `bird_counts`. Make sure that your column names match the variables described
# in About the data!


bird_counts <- 
  read_excel(
    "data/raw/bird_counts.xlsx",
    sheet = "point_count_observations",
    skip = 2)

bird_counts

# [[-0.50]] Incorrect: For this to work you would have to call `read_excel()`
# from the package `readxl` with `readxl::read_excel()`.

# [[-0.10]] Code formatting: Maintain one blank line between code blocks and
# comments.

# [[No points removed]] Code parsimony: There is no need to type the name of the
# object. With global assignments, just double-click the name to highlight it
# and run it with the keyboard shortcut command + enter.

# question 4 --------------------------------------------------------------

# Explore the structure of `bird_counts` ...

summary(bird_counts)


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

lobstr::ref(bird_counts)

# [[-0.10]] The last bullet was incorrect. To view a reference tree for your
# global environment, use `lobstr::ref(.GlobalEnv)`

# [[-0.10]] Code formatting: Maintain one blank line between code blocks and
# comments.

# question 5 --------------------------------------------------------------

# Subset and arrange `bird_counts` ...

# Subset `bird_counts` to the first five rows:

slice_head(
  bird_counts,
  n = 5
  )

# Subset `bird_counts` to the last three rows:

slice_tail(
  bird_counts,
  n=3
  )

# Arrange the rows of `bird_counts` by the variable `count` in descending
# order:

arrange(
  bird_counts,
  desc(count)
  )

# Subset `bird_counts` to the species "CACH" (Carolina chickadee).

bird_counts[
  bird_counts[["species"]] == "CACH", ]

# Subset `bird_counts` to the species "AMRO" (American robin), "GRCA" (Gray
# catbird), and "NOCA" (Northern cardinal; see Preliminary lesson 4: 
# Indexing):

bird_counts[
  bird_counts[["species"]] %in%
    c("AMRO", "GRCA", "NOCA"), ]

# [[-0.075]] Code formatting: 
# * Infix functions should be separated from surrounding code with a single
#   leading and trailing space
# * If you provide three or more arguments to a function, place each argument
#   on its own line.

# [[No points removed]] Code formatting (new style guide): 
# * Indentation: Closing parentheses should be indented to the same level as
#   the start of the function.

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

rename(
  bird_counts,
  species_code = species
  )

# [[No points removed]] Code formatting (new style guide): 
# * Indentation: Closing parentheses should be indented to the same level as
#   the start of the function.

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
  factor(
    bird_counts[["diet"]]
    )
  )

# Modify this operation such that only "Functions that you may use in this
# assignment" are used and the output is produced with a single, *piped* 
# code  block without assigning any names to your global environment.:


bird_counts %>%
  factor("diet") %>% 
  typeof
  
# [[-0.50]] Incorrect: This actually converted the column names to a factor
# variable. You need to extract the `diet` vector prior to converting it to a
# factor.

# [[-0.10]] Code formatting: Maintain one blank line between code blocks and
# comments. In your version there were additional spaces prior to the section
# header.

# [[No points removed]]: Whenever calling a named function, include parentheses
# (here, with `typeof`).

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
  levels

# [[-0.05]] Code formatting: Maintain one blank line between code blocks and
# comments. In your version there were additional spaces prior to the section
# header.

# [[No points removed]]: Whenever calling a named function, include parentheses
# (here, with `levels`).

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
  bird_counts[["species"]] == "CACH", ] %>% 
  '[['("count") %>% 
  mean

# [[-0.05]] Code formatting: Maintain one blank line between code blocks and
# comments. In your version there were additional spaces prior to the section
# header.

# [[No points removed]]: Whenever calling a named function, include parentheses
# (here, with `mean`).

# [[No points removed]]: Your solution works but is a bit wacky. See key.

# question 10 -------------------------------------------------------------

# The code below contains ten violations of the "Course Style Guide". 
# Without removing comments or changing the code output, modify the code 
# block such that it follows the conventions of the style guide. 

# Poorly formatted version (given):

#Arrange by site, diet, and species, alphabetically:
birdCountsSorted= bird_counts%>% 
  arrange(site, diet,species) # Sort rows

# Answer:

#Arrange by site, diet, and species, alphabetically:
birdCountsSorted = bird_counts %>% 
  arrange(
    site, 
    diet,
    species
    ) 
# Sort rows

# Incorrect [[-0.50]]: Missed five style errors (see key).
