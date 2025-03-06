# Lesson 1.3: R Objects

# This script will help you code along with the content in lesson 1.3. I have 
# not included *all* of the code in the lesson, as I suggest writing some code
# on your own (predomoninantly in the console pane).

# functions ---------------------------------------------------------------

# The combine function `c` is a prefix function:

c(1, 1, 2, 3)

# Some operators are prefix functions (but not many!)

?TRUE

-TRUE

!TRUE

# Most operators are infix functions

1 + 1

c(1, 1, 2, 3)[[3]]

# environments: the global environment ------------------------------------

# View all names assign to the global environment with `ls()`:

ls()

# Assign `greetings` to the global environment with the `<-` function:

greetings <- "hello world"

# Assign several names to the global environment:

ordered_numbers <- 1:20

fibo_numbers <- c(0, 1, 1, 2)

musicians <-
  c("roger",
    "pete",
    "john",
    "keith")

four_instruments <-
  c("vocals",
    "guitar",
    "bass",
    "drums")

# Remove an assigned name with `rm()`:

rm(greetings)

# Remove multiple assignments with `rm()` and separating each with a comma:

rm(ordered_numbers, fibo_numbers)

# Remove all assignments by nesting `ls()` inside of `rm()`:

rm(
  list = ls()
)

# environments: package environments --------------------------------------

# You can use `search()` to view the environments in R's search path:

search()

# To view the environment a give function is in, use `environment()`:

environment(mean)

# The following will not work in our current session:

tribble(
  ~a, ~b,
  1,  1,
  2,  3)

# We can use the `::` function to pull the instructions from the package's
# environment:

tibble::tribble(
  ~a, ~b,
  1,  1,
  2,  3)

# We can use library() to attach a package environment:

library(tibble)

# ... and then directly call the function from the environment:

tibble::tribble(
  ~a, ~b,
  1,  1,
  2,  3)

# Core packages, like tibble, are loaded with:

library(tidyverse)

# To view all of the tidyverse packages use:

tidyverse_packages()

# data objects ------------------------------------------------------------

# Because data objects are ordered, we can extract a value by position using
# the extraction function `[[]]`:

c(1, 1, 2, 3)[[3]]

# data objects: atomic vectors --------------------------------------------

# Determine the class of an object with the class function:

class(
  c(1, 2, 3, 4)
)

# Determine how the object is stored:

typeof(
  c(1, 2, 3, 4)
)

# Generate an integer of adjacent numbers with the `:` function:

typeof(1:4)

# Convert a vector from double numeric value to integer:

as.integer(
  c(1, 2, 3, 4)
)

# Evaluate the type of values in the object:

typeof(
  as.integer(
    c(1, 2, 3, 4)
  )
)

# Convert an integer vector to a double numeric and evaluate how the object is
# stored:

typeof(
  as.numeric(1:4)
)

# Generate an atomic vector of character values, and assign the object to the
# name `musicians`

musicians <-
  c("roger",
    "pete",
    "john",
    "keith")

# Things are not always straightforward when transforming between classes:

as.numeric(musicians)

# However, what if `musicians` were a factor?

factor(musicians)

# Recall that factors are stored as integers:

typeof(
  factor(musicians)
)

# ... as such:

as.numeric(
  factor(musicians)
)

# `str` provides a handy way to see how an object is structured and stored:
  
str(
  factor(musicians)
)


# It is super important to remember that all atomic vectors must share the
# same class and type:

c(musicians, 1:4)

class(
  c(musicians, 1:4)
)

typeof(
  c(musicians, 1:4)
)

# data objects: matrices --------------------------------------------------

# Generate a matrix with the `matrix` function:

matrix(
  1:4,
  nrow = 2,
  ncol = 2,
  byrow = FALSE)

# Note that the `byrow = TRUE/FALSE` argument specifies the order in which the
# atomic vector populates the matrix:

matrix(
  1:4,
  nrow = 2,
  ncol = 2,
  byrow = TRUE)

# The class of this object is a matrix (note: An array is a multi-dimensional
# object where all values are of the same type):

class(
  matrix(
    1:4,
    nrow = 2,
    ncol = 2,
    byrow = TRUE)
)

# ... a matrix *is* an atomic vector:

typeof(
  matrix(
    1:4,
    nrow = 2,
    ncol = 2,
    byrow = TRUE)
)

# The object is an integer with some dimensionality:

str(
  matrix(
    1:4,
    nrow = 2,
    ncol = 2,
    byrow = TRUE)
)

# To produce the matrix, we added dimensional attributes to the atomic vector.
# We can view this with the `attributes` function:

attributes(
  matrix(
    1:4,
    nrow = 2,
    ncol = 2,
    byrow = TRUE)
)

# The arrangement of values was determined by byrow = TRUE. If we convert the
# matrix back to a true atomic vector with the `as.vector` function, we can see
# how/why this worked:

as.vector(
  matrix(
    1:4,
    nrow = 2,
    ncol = 2,
    byrow = TRUE)
)

as.vector(
  matrix(
    1:4,
    nrow = 2,
    ncol = 2,
    byrow = FALSE)
)

# Remember that a matrix *is* an atomic vector!

matrix(
  c(musicians, 1:4),
  nrow = 4,
  ncol = 2,
  byrow = TRUE)

# data objects: lists -----------------------------------------------------

# When we combine atomic vectors into a list, the object classes and types are
# not altered:

my_list <- list(musicians, 1:4)

# We can use the 

# Note the class and type of a list object:

class(my_list)

typeof(my_list)

# Although the class of the object is a list and it is stored as a list,
# `str()` shows us that the individual list items are stored as atomic
# vectors:

str(my_list)

# Atomic vectors are stored with a single reference point, thus all values must
# be for the same type:

lobstr::ref(my_list)

# Conversely, in a list, each list item has its own reference point and thus
# lists are incredible flexible (list items may be of different types):

lobstr::ref(my_list)

# data objects: data frames -----------------------------------------------

# A data frame is a restrictive list where each column is a named list item:

my_df <- 
  data.frame(
    musician = musicians,
    number = 1:4)

my_df

# Printing the structure shows that each list item is a variable (column), and
# each has four observations (rows):

str(my_df)

# The class is a data frame, but it is stored as a list!

class(my_df)

typeof(my_df)

# We will be using tibble data frames in this course:

my_tibble <- 
  tibble(
    musician = musicians,
    number = 1:4)

# The biggest utility of tibbles is in how they are printed:

my_tibble

# Extracting by indexing, with the function `[]`, alters the class of base R
# data frames:

my_df[, 2]

# The class is maintained when indexing tibbles with `[]`:

my_tibble[, 2]

# Extracting by name, with the function `$`, uses partial matching with base R
# data frames:

my_df$musician

my_df$m

# Tibbles do not allow partial matching:

my_tibble$musician

my_tibble$m

# Base R data frames will recycle values during construction (dangerous!):

data.frame(
  a = 1, 
  b = 1:4)

data.frame(
  a = 1:2, 
  b = 1:4)

# Tibbles will only recycle single values:

tibble(
  a = 1, 
  b = 1:4)

tibble(
  a = 1:2,
  b = 1:4)