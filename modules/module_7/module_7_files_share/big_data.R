# Code for lesson on big data

# setup -------------------------------------------------------------------

# Load libraries:

library(arrow)
library(bench)
library(DBI)
library(RSQLite)
library(stringi)
library(tidyverse)

# *Note: The package bench is currently my go-to package for benchmarking!*

# Read in messy birds and remove the primary key, capture_id (we will make our
# own primary key):

messy_birds <-
  read_rds("data/raw/messy_birds.rds") %>% 
  select(!capture_id)

# Read in detections from our automated radio telemetry system (this is a
# fairly big file and may take a while to read in):

amke_memory <-
  read_csv("data/raw/amke_detections.csv")

# custom functions --------------------------------------------------------

# Now you! Write a custom function that will test whether any atomic vector
# contains duplicates:



# Now you! Write a custom function that will filter a nested data frame by a
# variable in a list column and a predicate:



# Now you! Write a custom function that will add a primary key to any data
# frame. An argument of the function should the number of symbols to use:



# Now you! Modify the function `add_key_to_frame` such that:

# * If there *are* duplicate values, return "Error: duplicate key values
#   detected!"
# * If there are *no* duplicate key values, the data frame is returned (with 
#   the primary key column)



# tidy data, nested objects -----------------------------------------------

# `messy_birds` is at two levels of observation, one associated with the capture
# records and the other the life history of the species.

# Split the data into two tables and combine the tables into a single list
# object:

tidy_birds <-
  list(
    captures = 
      messy_birds %>% 
      select(
        band_number:spp, 
        wing:mass
      ),
    life_history =
      messy_birds %>% 
      select(spp:diet) %>% 
      distinct()
  )

# tidy data, nested lists -------------------------------------------------

# Create a list, nested by level of observation and species:

nested_list <- 
  messy_birds %>% 
  pull(spp) %>% 
  unique() %>% 
  sort() %>% 
  set_names() %>% 
  map(
    \(x) {
      list(
        life_history = 
          messy_birds %>% 
          filter(spp == x) %>% 
          select(scientific_name:diet) %>% 
          distinct(),
        captures =
          messy_birds %>% 
          filter(spp == x) %>% 
          select(
            band_number, 
            wing:mass
          )
      )
    }
  )

# Change the name of a list item:

nested_list %>% 
  tidyselect:::rename(boy_howdy = SOSP) %>% 
  str()

# tidy data, nested data frames -------------------------------------------

# The trouble with lists is that they can be difficult to work with! We can
# instead produce a nested data frame:

nested_birds <-
  messy_birds %>% 
  nest(
    capture_records = c(band_number, wing:mass)
  )

# Use `unnest()` to return all of the columns from our nested list column:

nested_birds %>% 
  filter(spp == "GRCA") %>% 
  select(
    spp,
    common_name,
    capture_records
  ) %>% 
  unnest(cols = capture_records)

# Subset each data frame in the list column to those in which the mass is less
# than 20:

nested_birds %>% 
  select(
    spp, 
    common_name, 
    capture_records
  ) %>% 
  mutate(
    capture_records = 
      map(
        capture_records,
        \(x) {
          filter(x, mass < 20)
        }
      )
  )

# Use unnest to return only the subsetted records as a regular data frame
# (removing 0 length data frames):

nested_birds %>% 
  select(
    spp, 
    common_name, 
    capture_records
  ) %>% 
  mutate(
    capture_records = 
      map(
        capture_records, 
        \(x) {
          filter(x, mass < 20)
        }
      )
  ) %>% 
  unnest(capture_records)

# Subset the data without un-nesting it by adding a column that describes the
# number of matching records and filter to just matching rows:

nested_birds %>% 
  select(
    spp, 
    common_name, 
    capture_records
  ) %>% 
  mutate(
    n_records = 
      map_dbl(
        capture_records,
        \(x) {
          filter(x, mass < 20) %>% 
            nrow()
        }
      )
  ) %>% 
  filter(n_records > 0)

# It was, of course, not necessary to assign n_records:

nested_birds %>% 
  select(
    spp, 
    common_name, 
    capture_records
  ) %>% 
  filter(
    map_dbl(
      capture_records, 
      \(x) {
        filter(x, mass < 20) %>% 
          nrow()
      }
    ) > 0
  )

# Remove assigned names that we will no longer need:

rm(nested_list, nested_birds)

# primary and foreign keys ------------------------------------------------

# Create a key for `messy_birds` where each value contains 4 symbols:

messy_birds %>% 
  mutate(
    id = 
      stri_rand_strings(
        nrow(.),
        4
      ),
    .before = 1
  )

# Create a primary key using our custom function:

messy_birds_keyed <-
  messy_birds %>% 
  add_key_to_frame(n_symbols = 4) %>% 
  rename(observation_id = id)

# Add a foreign key by conducting a grouped operation:

messy_birds_primary_and_foreign_keys <- 
  messy_birds_keyed %>% 
  mutate(
    spp_id =
      stri_rand_strings(
        n = 1,
        length = 5
      ),
    .by = spp,
    .before = spp
  )

# Tidy the table by splitting based on the level of observation (the third tidy
# data rule):

my_tidy_list <-
  list(
    captures = 
      messy_birds_primary_and_foreign_keys %>% 
      select(
        observation_id,
        band_number:spp_id,
        wing:mass
      ),
    life_history = 
      messy_birds_primary_and_foreign_keys %>% 
      select(spp_id:diet) %>% 
      distinct()
  )

rm(
  messy_birds,
  messy_birds_keyed,
  messy_birds_primary_and_foreign_keys
)

# generate and work with sqlite databases ---------------------------------

# Generate a blank database (*Note: `dbConnect()` can be used to create a new
# database or connect to an existing database. Here, it does both tasks
# simultaneously.*):

captures_database <-
  dbConnect(
    SQLite(),
    "data/processed/captures_and_birds.sqlite"
  )

# Write a table to our database:

dbWriteTable(
  conn = captures_database, 
  name = "captures",
  value = 
    pluck(
      my_tidy_list, 
      "captures"
    )
)

# Verify that the table has been written with `dbListTables()`:

dbListTables(conn = captures_database)

# Overwrite an existing table:

dbWriteTable(
  conn = captures_database, 
  name = "captures",
  value = captures,
  overwrite = TRUE
)

# Now you! Add both tables in `my_tidy_list` at once using `map()`:



# Read in a database table with `tbl()`:

tbl(captures_database, "life_history")

# Under-the-hood, *dplyr* uses *dbplyr* functions when working with databases:

tbl(captures_database, "life_history") %>% 
  filter(diet == "insectivore") %>% 
  show_query()

# Subset `captures` to those in which the diet is "insectivore" (from the
# `life_history` table) with our sqlite database:

tbl(captures_database, "captures") %>% 
  semi_join(
    tbl(captures_database, "life_history") %>% 
      filter(diet == "insectivore"),
    by = "spp_id"
  )

# sql database with big data ----------------------------------------------

# Now you! Create a blank SQLite database in data/processed called `amke.sqlite`
# and globally assign the database connection to the name `amke_db`:



# Add `amke_memory` as a table in the database:

dbWriteTable(
  conn = amke_db, 
  name = "detections",
  value = amke_memory
)

# Transform the time column into a character vector before writing the table:

amke %>% 
  mutate(
    time = as.character(time)
  ) %>% 
  dbWriteTable(
    conn = amke_db, 
    name = "detections",
    value = .,
    overwrite = TRUE
  )

# Move the database to our memory using the function `collect()`:

detections %>% 
  filter(
    month(time) == 7,
    rssi > -90,
    node_id == "377c42"
  ) %>% 
  collect()

# Delete all records for a given tag_id:

dbExecute(
  amke_db,
  "DELETE FROM detections WHERE (tag_id = '78195233');"
)

# Write the subset data to a new csv file:

tbl(amke_db, "detections") %>% 
  collect() %>%
  write_csv("data/processed/append_example.csv")

# Use the tibble stored in our memory to create a data frame of just
# observations of `tag_id` "78195233":

amke_78195233 <- 
  amke %>% 
  filter(tag_id == "78195233")

# Append an SQL table with the function `dbAppendTable()`:

dbAppendTable(
  conn = amke_db,
  name = "detections",
  value = amke_78195233
)

# Close connections:

dbDisconnect(captures_database)

dbDisconnect(amke_db)

# Remove global assignments:

rm(
  my_tidy_list,
  captures_database,
  amke_78195233
)

# data.table and dtplyr ---------------------------------------------------

# Read in a data table object with the function `fread()` and globally assign it
# to the name `amke_dt`:

amke_dt <-
  fread("data/raw/amke_detections.csv")

# Create a "lazy data table":

amke_lazy <-
  fread("data/raw/amke_detections.csv") %>% 
  dtplyr::lazy_dt()

# The performance of the above is much faster and more memory efficient than
# even the native dt version. This is because you are not actually generating
# the resultant dataset, just previewing the operation. If you print the
# results, you can see the a preview of the results and the *data.table* code
# used:

amke_lazy %>% 
  filter(tag_id == "78195233")

# Access the results with `as_tibble()`:

amke_lazy %>% 
  filter(tag_id == "78195233") %>% 
  as_tibble()

# Consider the following operation:

amke_lazy %>% 
  summarize(
    n_detections = n(),
    .by = tag_id
  )

# Summarizing this dataset would require a whole new syntax:

amke_dt[ , .(n_detections = .N), keyby = .(tag_id)]

# arrow -------------------------------------------------------------------

# Establish a connection with a csv dataset and globally assign a name to the
# resultant arrow object:

amke_arrow <- 
  open_csv_dataset("data/raw/amke_detections.csv")

# The output is perhaps not very helpful for viewing the results:

amke_arrow %>% 
  filter(tag_id == "78195233")

# Use the pillar function `glimpse` to get *some* information on the object:

amke_arrow %>% 
  filter(tag_id == "78195233") %>% 
  glimpse()

# View the resultant object by passing it into memory with `collect()`:

amke_arrow %>% 
  filter(tag_id == "78195233") %>% 
  collect()

# Pull out a record for a single day and tag_id:

open_csv_dataset("data/raw/amke_detections.csv") %>% 
  filter(
    tag_id == "19332D2D",
    as_date(time) == "2023-06-30"
  ) %>% 
  collect()

# Return the three closest nodes to where a bird was the night before:

open_csv_dataset("data/raw/amke_detections.csv") %>% 
  filter(
    tag_id == "19332D2D",
    as_date(time) == "2023-06-30",
    hour(time) > 20
  ) %>% 
  
  # Calculate the average rssi by node_id:
  
  summarize(
    rssi = mean(rssi),
    .by = node_id
  ) %>% 
  
  # Subset to the three nodes with the highest rssi:
  
  slice_max(
    rssi, 
    n = 3,
    with_ties = FALSE
  ) %>% 
  collect()


