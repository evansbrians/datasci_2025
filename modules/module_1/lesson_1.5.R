# Lesson 1.5: Assignment

# This script will help you code along with the content in lesson 1.5. I have 
# not included *all* of the code in the lesson, as I suggest writing some code
# on your own (predominantly in the console pane).

# setup -------------------------------------------------------------------

library(tidyverse)

# global assignments ------------------------------------------------------

# My nerdy greeting, a one-value character vector:

nerdy_greeting <- "hello world"

# List names in the global environment (but see note):

ls()

# View how an object is stored:

lobstr::ref(nerdy_greeting)

# View the reference tree of the global environment:

lobstr::ref(.GlobalEnv)

# My southern saying, another one-value character vector:

southern_saying <- "boy howdy!"

# str_c() can be used to concatenate strings:

str_c(
  nerdy_greeting,
  southern_saying, 
  sep = ", "
)

# Notice that the names can be used interchangeably with the objects that they
# are assigned to:

str_c(
  "hello world",
  "boy howdy!", 
  sep = ", "
)

str_c(
  nerdy_greeting,
  "boy howdy!", 
  sep = ", "
)

str_c(
  "hello world",
  southern_saying, 
  sep = ", "
)

# Likewise:

c(0, 1, 1, 2)[[3]]

fibo_numbers <- 
  c(0, 1, 1, 2)

fibo_numbers[[3]]

# Notice the reference tree when multiple names are assigned to the same 
# object:

temp <- nerdy_greeting

lobstr::ref(.GlobalEnv)

# global assignments: removing names --------------------------------------

# Add some more junk to the global environment:

ordered_numbers <- 1:20

musicians <-
  c(
    "roger",
    "pete",
    "john",
    "keith"
  )

four_instruments <-
  c(
    "vocals",
    "guitar",
    "bass",
    "drums"
  )

# Remove a single name:

rm(temp)

# Remove multiple names:

rm(ordered_numbers, fibo_numbers)

# Generate a character vector of names in the global environment with ls():

ls()

# View the class returned by ls():

class(
  ls()
)

# Remove all objects from the global environment:

rm(
  list = ls()
)

# recursive assignments ---------------------------------------------------

# Generate a list and assign the name my_list to the global environment:

my_list <- 
  list("hello world", "boy howdy!")

# View the reference tree of my_list:

lobstr::ref(my_list)

# Generate a character vector and assign the name my_sayings to the global 
# environment:

my_sayings <- 
  c("hello world", "boy howdy!")

# View the reference tree of my_sayings:

lobstr::ref(my_sayings)

# Generate a list where each list item is named and view the reference tree:

my_list_named <- 
  list(
    nerdy_greeting = "hello world", 
    southern_saying = "boy howdy!"
  )

lobstr::ref(my_list_named)

# Return a character vector of names in my_list_named:

names(my_list_named)

# Extract a value by name using the function `[[x]]`:

my_list_named[["southern_saying"]]

# Extract a value by name using the function `$`:

my_list_named$southern_saying

# Remove a "recursive name" and view the reference tree:

my_list_named$southern_saying <- NULL

lobstr::ref(my_list_named)

# Why can't you use `$` with atomic vectors?

my_sayings_named <- 
  c(
    nerdy_greeting = "hello world", 
    southern_saying = "boy howdy!"
  )

lobstr::ref(my_sayings_named)

# naming conventions ------------------------------------------------------

# Read in a file with *terrible* names!

bad_df <-
  read_csv("data/raw/badNameDataFrame.csv")

# Non-syntactic variable names pose special problems:

unique(bad_df$plant/mushroom)

unique(bad_df$`plant/mushroom`)

# Rename one variable with `rename()`:

rename(
  bad_df, 
  bed = BED
)

# Rename multiple variables with `rename()`:

rename(
  bad_df, 
  bed = BED,
  light_access = LightAccess
)

# Rename non-syntactic variables with `rename()`:

rename(
  bad_df, 
  bed = BED,
  light_access = LightAccess,
  plant_mushroom = `plant/mushroom`,
  event_date = `Event date`
)

# No camelCase either!

rename(
  bad_df, 
  bed = BED,
  light_access = LightAccess,
  plant_mushroom = `plant/mushroom`,
  event_date = `Event date`,
  garden_event = gardenEvent
)

# Rename with set_names() ... but with bad names:

my_list_add_names <- 
  set_names(
    my_list, 
    c("Nerdy", "Southern")
  )

# Nested function to convert to lowercase names:

set_names(
  my_list_add_names, 
  tolower(
    names(my_list_add_names)
  )
)

# This works because `names()` generates a character vector of names:

names(my_list_add_names)

# ... and tolower() can convert a character vector of names to lowercase:

tolower(
  names(my_list_add_names)
)

# nested and non-nested objects -------------------------------------------

# Assignment version of the name-changing operation for my_list_add_names:

original_names <- 
  names(my_list_add_names)

lowercase_names <- 
  tolower(original_names)

set_names(my_list_add_names, lowercase_names)

# Remove unnecessary names:

rm(original_names, lowercase_names)

# the pipe ----------------------------------------------------------------

# Pass my_list_add_names(LHS) to names() (RHS) using magrittr's pipe:

my_list_add_names %>% 
  names()

# Pass the character vector generated with names() (LHS) to tolower() (RHS):

my_list_add_names %>% 
  names() %>% 
  tolower()

# The above is equivalent to the nested version of the code (but in the correct
# order of operations):

tolower(
  names(my_list_add_names)
)

# For `set_names()`, the character vector of names is the second argument. We
# *could* use a nested version of the code:

set_names(
  my_list_add_names, 
  my_list_add_names %>% 
    names() %>% 
    tolower()
)

# Or alternatively the LHS of the piped operation into the second argument of
# set_names() using the `.` placeholder:

my_list_add_names %>% 
  names() %>% 
  tolower() %>% 
  set_names(my_list_add_names, .)

# Pipes are great for avoiding unnecessary assignments:

read_csv("data/raw/badNameDataFrame.csv") %>% 
  rename(
    bed = BED,
    light_access = LightAccess,
    plant_mushroom = `plant/mushroom`,
    event_date = `Event date`,
    garden_event = gardenEvent
  )

# The options for doing so with nested code can be bleak:

set_names(
  read_csv("data/raw/badNameDataFrame.csv"), 
  c(
    "bed",
    "light_access",
    "plant_mushroom",
    "event_date",
    "garden_event"
  )
)

# Much nicer with pipes!

read_csv("data/raw/badNameDataFrame.csv") %>% 
  set_names(
    c(
      "bed",
      "light_access",
      "plant_mushroom",
      "event_date",
      "garden_event"
    )
  )

