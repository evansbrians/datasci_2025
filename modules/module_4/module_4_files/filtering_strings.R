# Filtering strings
# An instructional script on using regex for filtering data frames

# setup -------------------------------------------------------------------

library(tidyverse)

# Read in my silly covid garden dataset:

my_covid_garden <- 
  read_csv("data/raw/my_covid_garden.csv")

# before we begin ---------------------------------------------------------

# Now you! Without adding any new variables, normalize the my_covid_garden
# dataset:

my_covid_garden

# Note: 
# - Beds may contain multiple species
# - Each species was only planted in one bed
# - Each species has more than one event (e.g., planted, harvested)
# - Some treatments (e.g., adding compost) occurred at the bed-level, when this
#   occurred, the value in the species column is NA
# - Some treatments (e.g., plantinig or harvesting) occurred at the organism 
#   level (plant or fungi species) 

# the need for stringr ----------------------------------------------------

# Detecting weird strings (str_detect):

c("Georgia",
  "Savannah, Georgia",
  "Atlanta, georgia",
  "GA") == "Georgia"

# Now you ... detect "Squirrel" in the character vector below:

c("Gray squirrel",
  "Eastern gray Squirrel",
  "grey Squirrel",
  "Giant Squid")

# Now you! Detect "sparrow" in the character vector below:

c("House sparrow",
  "Grasshopper sparrow",
  "Eastern towhee",
  "arrowroot",
  "spadefoot toad")

# filtering with partial string matching ----------------------------------

# Look at unique garden beds that were in the shade:

garden_beds %>%
  filter(
    str_detect(light_access, "shade"))
  
# For larger datasets, I suggest looking at an ordered string of unique values:

food_events %>% 
  pull(event) %>% 
  unique() %>% 
  sort()

# Now you! Filter food_events based on a partial string match to just events
# that contain the string "harvest":

food_events %>% 
  filter(
    event %>% 
    str_detect("."))

# Now you (take 2)! Subset plants_and_shrooms to just species that contain the
# string "squash":

plants_and_shrooms

# anchors -----------------------------------------------------------------

# Anchor metacharacter, start of string (metacharacter ^):

c("gray wolf",
  "gray", 
  "stingray") %>% 
  str_detect("gray")

# Anchor metacharacter, end of string (metacharacter $):

c("gray wolf",
  "gray", 
  "stingray") %>% 
  str_detect("gray")

# Now you! Use anchor metacharacters to subset food_events to plants eaten
# by someone other than me:

food_events %>% 
  filter(
    str_detect(event, "eaten"))

# character class ---------------------------------------------------------

# Character class (metacharacter []):

c("Gray squirrel",
  "Eastern gray Squirrel",
  "grey Squirrel") %>% 
  str_detect("gray")

# Now you! Detect "Georgia" or "georgia" in the character string below:

c("Georgia",
  "Savannah, Georgia",
  "Atlanta, georgia",
  "GA") %>% 
  str_detect("Georgia")

# Match a range of letters (metacharacter [-]):

c("June-1",
  "june 9",
  "JUNE") %>% 
  str_detect("a-z")

# Match a range of numbers (metacharacter [-]):

c("June-1", "june 9", "JUNE") %>% 
  str_detect("[0-9]")

# Use with care!

c("12", "15", "29") %>% 
  str_detect("[10-19]")

# Now you! Use the character class metacharacter to subset food_events to
# those that occurred between April 1st (2020-04-01) and 19th:

food_events %>% 
  filter(
    date %>% 
      str_detect("2020-04-"))

# or ----------------------------------------------------------------------

# The or metacharacter (metacharacter |):

c("gray wolf",
  "grey squirrel", 
  "stingray",
  "graying") %>% 
  str_detect("^gre") 

# But avoid overusing this tool!

c("gray wolf",
  "grey squirrel", 
  "stingray",
  "graying") %>% 
  str_detect("gray wolf|grey|graying") 

# Now you! Use the "or" metacharacter to subset food_events to those that
# include the string "bugs" or "deer":

food_events %>% 
  pull(event) %>% 
  unique() %>% 
  sort()

food_events %>% 
  filter(
    event %>% 
      str_detect("bugs"))

# alternation constraint --------------------------------------------------

# Alternation constraint, groups of characters (metacharacter ()):

c("gray wolf",
  "green iguana",
  "black-throated green warbler",
  "grape",
  "bentgrass",
  "honeysuckle") %>% 
  str_detect("gr[ae]y|en")

# This can be very useful for the endings of words!

c("graying",
  "greyer",
  "gray treefrog",
  "stingray",
  "grayed") %>% 
  str_detect("^gr[ae]y$")

# Now you! Use alternation constraints to subset food_events to those in 
# which a plant was eaten by squirrels or deer:

food_events %>% 
  filter(
    event %>% 
      str_detect("eaten by"))

# you've got options? -----------------------------------------------------

# We can set an individual pattern as optional with the metacharacter `?`:

c("boy howdy!",
  "Boy howdy",
  "Boy-howdy!",
  "boy-howdy",
  "He sure does say 'boy howdy' a lot") %>% 
  str_detect("^[Bb]oy[ -]howdy!$")

# ... or set a character class as optional ...

c("boy howdy!",
  "Boyhowdy!",
  "boy-howdy!",
  "He sure does say 'boy howdy' a lot") %>% 
  str_detect("^[Bb]oy[ -]howdy!$")

# ... or set an alternation constraint as optional ...

c("boy howdy!",
  "howdy!",
  "He sure does say 'boy howdy' a lot") %>% 
  str_detect("^(boy )howdy!$")

# Now you! Modify the code below such that the first four values evaluate to
# TRUE, but "George" evaluates to FALSE:

c("Georgia",
  "Savannah, Georgia",
  "Atlanta, georgia",
  "GA",
  "George") %>% 
  str_detect("[Gg]eorgi[Aa]")

# you've got options. -----------------------------------------------------

# The wild card character (metacharacter .)!

c("gray wolf",
  "groy wolf",
  "grey wolf",
  "gray whale",
  "blue whale",
  "gravy") %>% 
  str_detect("gr[ae]y")

# Now you! Modify the code below such that the first six values evaluate to 
# TRUE:

c("graay wolf",
  "groy wolf",
  "graey wolf",
  "greoy wolf",
  "gry wolf",
  "gray whale",
  "blue whale",
  "gravy") %>% 
  str_detect("groy")

# But use wildcard characters sparingly!

list.files(
  "data/raw",
  pattern = ".csv")

# repetition --------------------------------------------------------------

# Repeat no times, or an unlimited number of times (metacharacter *):

c("gray whale",
  "gray wolf",
  "gratefully",
  "blue whale") %>% 
  str_detect("gra.y")

# Repeat at least once (metacharacter +):

c("gray whale",
  "gray wolf",
  "gratefully",
  "blue whale") %>% 
  str_detect("gra.y")

# A specified number of repetitions:

c("1-212-736-5000",
  "49-21-736-5000",
  "1-97-867-5309",
  "1-970-867-500",
  "1-970-867-5309") %>% 
  str_detect("[0-9]{1}-[0-9]{1}-[0-9]{1}-[0-9]{1}")

# A range of repetitions:

c("3/15/2021",
  "03/1/21",
  "03/15/2021",
  "2021/03/5") %>% 
  str_detect("[0-9]{2}/[0-9]{2}/[0-9]{4}")

# Now you! Detect US (mm/dd/yyyy) and European (dd/mm/yyyy) in the character
# vector below (15 March, 2021):

c("03/15/2021",
  "03/15/21",
  "2021/03/05") %>% 
  str_detect("[0-9]{2}/[0-9]{2}/[0-9]{4}")
