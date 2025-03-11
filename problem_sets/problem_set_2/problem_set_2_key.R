# Script file for Problem Set 2.

# question 1 --------------------------------------------------------------

# Before opening your script file for this problem set, change the name of
# the `problem_set_2.R` to "problem_set_2_[last name]_[first name].R" using
# a snake case naming convention. *Note: You will submit this script file
# as your assignment*.

# question 2 --------------------------------------------------------------

# Open the script file in RStudio and attach the tidyverse metapackage to
# your current R session:

library(tidyverse)

# question 3 --------------------------------------------------------------

# The data file `district_birds.rds` is structured as a list object. The
# following code:

# * Reads in the list object
# * Globally assigns the list object to the name `my_list`
# * Globally assigns each list item to a name
# * Ensures that the name `my_list` is not stored in the global environment

my_list <-
  read_rds("data/raw/district_birds.rds")

sites <- my_list$sites

visits <- my_list$visits

counts <- my_list$counts

captures <- my_list$captures

birds <- my_list$birds

rm(my_list)

# As parsimoniously as possible, modify the above by reading in the data
# and assigning the names of each of the individual list items to the
# global environment in a single code block (*Note: `my_list` should not be
# added to the global environment*):

read_rds("data/raw/district_birds.rds") %>% 
  list2env(.GlobalEnv)

# Or:

list2env(
  read_rds("data/raw/district_birds.rds"),
  .GlobalEnv)

# question 4 --------------------------------------------------------------

# The code block below uses a nested coding structure to calculate the
# average mass of American robins ("AMRO"):

mean(
  captures[captures$spp == "AMRO", ]$mass,
  na.rm = TRUE)

# Please replace the above with a code block in which each step in the
# process is connected by a pipe and only "Functions that you may use in
# this assignment" are used (of course, this is required for each question
# in this problem set!):

captures %>% 
  filter(spp == "AMRO") %>% 
  pull(mass) %>% 
  mean(na.rm = TRUE)

# question 5 --------------------------------------------------------------

# The code block below generates a box plot to visualize the distribution
# of bird mass by species. The code works as expected, but is inconsistent
# and sloppy. Without removing comments or changing the output, modify the
# code block such that it follows the conventions our course style guide.

captures %>% 
  filter(spp%in% c("CACH", "CARW", "GRCA", "NOCA", "SOSP")) %>%
  drop_na(mass) %>% 
  # Initiate plot:
  ggplot() +
  
  # Define aesthetics and geometry:
  
  aes(x=spp, y = mass) + geom_boxplot(fill = "#88ccee") +
  
  # Define scale:
  
  scale_y_continuous(
    limits = c(0,75), 
    expand = c(0, 0))+
  
  # Flip axes:
  
  coord_flip() +
  
  # Define theme:
  
  theme(panel.background = element_blank(), axis.line = element_line(color = "black"), 
        panel.grid.major.x = element_line(color = "#dcdcdc"))

# Answer:

captures %>% 
  filter(
    spp %in% 
      c("CACH",
        "CARW",
        "GRCA",
        "NOCA",
        "SOSP")
  ) %>% 
  drop_na(mass) %>% 
  
  # Initiate plot:
  
  ggplot() +
  
  # Define aesthetics and geometry:
  
  aes(x = spp, y = mass) + 
  geom_boxplot(fill = "#88ccee") +
  
  # Define scale:
  
  scale_y_continuous(
    limits = c(0, 75), 
    expand = c(0, 0)) +
  
  # Flip axes:
  
  coord_flip() +
  
  # Define theme:
  
  theme(
    panel.background = element_blank(),
    axis.line = element_line(color = "black"), 
    panel.grid.major.x = element_line(color = "#dcdcdc"))

# question 6 --------------------------------------------------------------

# The code block below uses some pretty dated coding methods to:
# 
# * Subset the data to Carolina chickadees
# * Convert the wing length from millimeters to inches (1 inch = 25.4 mm)
# * Filter the data frame to wing lengths that are greater than or equal to
#   2.3 inches and less than or equal to 2.5 inches
# * Subset the variables to `capture_id`, `sex`, `age`, and `wing`
# * Ensure that the name `cach` is not stored in the global environment

# Subset data to Carolina chickadees:

cach <-
  captures[captures$spp == "CACH", ]

# Extract the mass variable from the data:

cach$wing <- cach$wing/25.4

# Subset the data:

cach[
  cach$wing >= 2.3 & 
    cach$wing <= 2.5 & 
    !is.na(cach$wing),
  c("capture_id",
    "sex",
    "age",
    "wing")]

rm(cach)

# Using `captures` as your starting point, please complete all of the steps
# in the operation above in a single code block:

captures %>% 
  filter(spp == "CACH") %>% 
  mutate(wing = wing / 25.4) %>% 
  filter(
    wing >= 2.3,
    wing <= 2.5
  ) %>% 
  select(capture_id, sex:wing)

# question 7 --------------------------------------------------------------

# The code block below attempts to provide a summary data frame that
# describes the number of captures and average mass of three bird species
# (Gray catbird: "GRCA", Northern Cardinal: "NOCA", Song sparrow: "SOSP").
# Unfortunately, there are four operational errors (i.e., not styling
# errors) in the code. Without adding any additional functions, please
# fix the errors in the code!

# Given:

captures %>% 
  filter(
    spp == 
      c("GRCA", 
        "NOCA", 
        "SOSP")) %>% 
  summarize(
    captures,
    n_birds = n(),
    mass = mean(mass)
  )

# Answer:

captures %>% 
  filter(
    spp %in% 
      c("GRCA", 
        "NOCA", 
        "SOSP"))  %>% 
  summarize(
    n_birds = n(),
    mass = mean(mass, na.rm = TRUE),
    .by = spp
  )

# question 8 --------------------------------------------------------------

# Without assigning any names to the global environment, generate a box plot
# (`geom_boxplot()`) that describes the distribution of mass *and* tail
# measurements of American robins (spp: "AMRO"). For full credit:

# * Remove the gray plot background 
# * Add gray axis lines
# * Ensure that there are no warning messages when plotting the data
# * Label the measured variables `Mass` and `Tail`

# Answer (at a minimum, but nicer plots are better!):

# Subset captures to American robin (AMRO):

captures %>% 
  filter(spp == "AMRO") %>% 
  
  # Remove NA values:
  
  drop_na(mass, tl) %>% 
  
  # Change names:
  
  rename(
    Mass = mass,
    Tail = tl
  ) %>% 
  
  # Reshape the data such that wing and mass values are in the same column:
  
  pivot_longer(
    c(Mass, Tail)) %>% 
  
  # Plot the data:
  
  ggplot() +
  aes(x = name, y = value) +
  geom_boxplot() +
  theme(
    axis.line = element_line(color = "#dcdcdc"),
    panel.background = element_blank()
  )

# It is also acceptable to nest `aes()` within `ggplot()`.
