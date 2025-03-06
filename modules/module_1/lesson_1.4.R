# Lesson 1.4: Importing, exploring, and writing data

# This script will help you code along with the content in lesson 1.4. I have 
# not included *all* of the code in the lesson, as I suggest writing some code
# on your own (predomoninantly in the console pane).

# video code --------------------------------------------------------------

library(tidyverse)

# reading data ------------------------------------------------------------

# Read in "my_covid_garden.csv" and assign the data frame to the name
# `my_garden`:

my_garden <-
  read_csv("data/raw/my_covid_garden.csv")

# You can read the data in as a base R data frame with read.csv (but really
# shouldn't):

my_garden_df <-
  read.csv("data/raw/my_covid_garden.csv")

# Some csv files are not formatted in a coder-frielndly way!

read_csv("data/raw/API_EN.ATM.CO2E.PC_DS2_en_csv_v2_2764620.csv")

# ... but additional arguments can help (see `?read_csv`):

read_csv(
  "data/raw/API_EN.ATM.CO2E.PC_DS2_en_csv_v2_2764620.csv", 
  skip = 4, 
  skip_empty_rows = FALSE)

# Read in an RDS file:

read_rds("data/raw/my_covid_garden.rds")

# For reading Microsoft Excel files, we'll use the package readxl. We can
# view the names of Excel worksheets with:

readxl::excel_sheets("data/raw/my_covid_garden.xlsx")

# We can read in the first worksheet with: 

readxl::read_excel("data/raw/my_covid_garden.xlsx")

# ... or add arguments to specify which sheet to read in:

readxl::read_excel(
  "data/raw/my_covid_garden.xlsx",
  sheet = "garden_life")

# Like csv files, we can skip rows:

readxl::read_excel(
  "data/raw/my_covid_garden.xlsx", 
  sheet = "garden_life",
  skip = 2)

# data exploration: structure  --------------------------------------------

# We can explore the structure of a file with `str()`:

str(my_garden_df)

# That is not as necessary with a tibble (metadata are printed by default):

my_garden

# Generate and print a one-value integer that describes the number of rows with
# nrow():

nrow(my_garden)

# Generate and print a one-value integer that describes the number of columns
# with ncol():

ncol(my_garden)

# Generate a two-value integer vector with `dim()`:

dim(my_garden)

# Generate a one-value integer vector describing the length of an atomic
# (numeric) vector:

length(
  c(0, 1, 1, 2)
)

# The length of a list is the number of list items (variables):

length(
  list(
    nerdy = "hello world",
    southern = "boy howdy",
    fibo_numbers = c(0, 1, 1, 2))
)

# Because a data frame is a type of list, the length of a data frame is the
# number of columns (variables):

length(my_garden)

# data exploration: heads and tails ---------------------------------------

# Subset a data frame to the first record (i.e., row):

slice_head(my_garden)

# Subset a data frame to the last record:

slice_tail(my_garden)

# Subset a data frame to the first two records with `n = ...`:

slice_head(my_garden, n = 2)

# Subset a data frame to the last two records with `n = ...`:

slice_tail(my_garden, n = 2)

# data exploration: ordering ----------------------------------------------

# Order a data frame by a variable using arrange (ascending order by default):

arrange(my_garden, bed)

# To order a data frame by a variable in descending order, nest the function
# `desc()` inside of `arrange()`:

arrange(
  my_garden, 
  desc(bed))

# data exploration: characters & factors ----------------------------------

# Generate and print the unique values in a vector (Note: this works with *any*
# type of vector!):

unique(my_garden$light_access)

# Generate and print the levels of a factor vector (returns a character vector):

levels(
  factor(my_garden$light_access)
)

# We can use `table` to count the number of observations for each factor level
# or unique character value:

table(my_garden$light_access)

# data exploration: statistical summaries ---------------------------------

# The base R `summary` function can be used to provide a load of summary
# statistics all at once!

summary(my_garden)

# It is often necessary or usefuly to calculate summary statics individually.
# Each of the functions below generates a one-value numeric vector representing
# a given summary statistic:

min(my_garden$date)

max(my_garden$date)

mean(my_garden$date)

median(my_garden$date)

var(my_garden$date)

sd(my_garden$date)

# Note on the above "var" stands for "variance" and "sd" for "standard
# deviation".

# write data: atomic vectors ----------------------------------------------

# Write an atomic vector to a file using write_lines:

write_lines(
  unique(my_garden$light_access),
  file = "data/raw/temp.txt")

# To see if we were successful, use `list.files()` to return a character vector
# of file names (or paths) in a specified folder:?

list.files("data/raw", pattern = "txt")

# Try to read the file back in to make sure that the writing process worked:

read_lines("data/raw/temp.txt")

# Remove a file with `file.remove()`:

file.remove("data/raw/temp.txt")

# Write four instruments to file:

write_rds(
  factor(my_garden$light_access),
  file = "data/raw/temp.rds")

# Did it work?

read_rds("data/raw/temp.rds")

# Yup, remove it!

file.remove("data/raw/temp.rds")

# write data: lists -------------------------------------------------------

# Make an expample list object:

my_list <-
  list(
    light_access = factor(my_garden$light_access),
    my_garden = my_garden)

# Save as an RDS file:

write_rds(my_list, "data/raw/temp.rds")

# Check to see if if worked:

read_rds("data/raw/temp.rds")

# Remove the file:

file.remove("data/raw/temp.rds")

# write data: data frames -------------------------------------------------

# DO NOT USE the base R's write.csv() to write data frames:

write.csv(my_garden, file = "data/raw/temp.csv")

# Notice that it adds a row number column by default:

read_csv("data/raw/temp.csv")

# That is dumb (and there is other stuff too) ... let's remove the file:

file.remove("data/raw/temp.csv")

# DO USE write_csv():

write_csv(my_garden, file = "data/raw/temp.csv")

read_csv("data/raw/temp.csv")

# Remove the file:

file.remove("data/raw/temp.rds")

# Writing a data frame as an RDS is also an option:

write_rds(my_garden, "data/raw/temp.rds")

# Check to see if it worked:

read_rds("data/raw/temp.rds")

# Remove the file:

file.remove("data/raw/temp.rds")

