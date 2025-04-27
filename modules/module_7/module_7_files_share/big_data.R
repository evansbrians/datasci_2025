# Code for lesson on big data

# setup -------------------------------------------------------------------

# Load libraries:

library(arrow)
library(bench)
library(DBI)
library(RSQLite)
library(tidyverse)

# *Note: The package bench is currently my go-to package for benchmarking!*

# Read in messy birds (but remove the primary key field, capture_id):

messy_birds <-
  read_rds("data/raw/messy_birds.rds") %>% 
  select(!capture_id)

# custom functions --------------------------------------------------------

my_mark <-
  function(.process) {
    mark({{ .process }}) %>% 
      select(median, mem_alloc)
  }

# tidy data ---------------------------------------------------------------

# `messy_birds` is at two levels of observation, one associated with the capture records and the other the life history of the species. An indication of this is that we have repeated information for Song Sparrow:

messy_birds

# As such, these data should be split into two different tables:

captures <-
  messy_birds %>% 
  select(band_number:spp, wing:mass)

life_history <- 
  messy_birds %>% 
  select(spp:diet) %>% 
  distinct()

# We can combine these data into a single list object:

lst(
  captures,
  life_history
)

# tidy data, nested data frames -------------------------------------------

# The trouble with lists is that they can be difficult to work with! We can instead produce a nested data frame:

nested_birds <-
  messy_birds %>% 
  nest(
    capture_records = c(band_number, wing:mass))

# This can give us a tool to quickly (in terms of processing time) subset the data to the records we are interested in:

nested_birds %>% 
  filter(spp == "GRCA")

# We can use unnest to return all of the columns:

nested_birds %>% 
  filter(spp == "GRCA") %>% 
  unnest(cols = capture_records)

# We can modify the individual list items. In the below, I subset each data frame in the nested to column to those in which the mass is less than 20:

nested_birds %>% 
  mutate(
    capture_records = 
      map(
        capture_records,
        ~ filter(.x, mass < 20))
  )

# We can use unnest to return only the subsetted records as a regular data frame (removing 0 length data frames):

nested_birds %>% 
  mutate(
    capture_records = 
      map(
        capture_records, ~ filter(.x, mass < 20))
  ) %>% 
  unnest(capture_records)

# If we want to subset the data without un-nesting it, we can add a column that describes the number of matching records:

nested_birds %>% 
  mutate(
    n_records = 
      map_dbl(
        capture_records, ~ filter(.x, mass < 20) %>% 
          nrow())
  )

# And filter to just matching rows:

nested_birds %>% 
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
  filter(
    map_dbl(
      capture_records, 
      \(x) {
        filter(x, mass < 20) %>% 
          nrow()
      }
    ) > 0)

# We can make our own custom function to filter the data frame (with the functions we already know!):

filter_nested_frame <-
  function(.data, .list_column, .predicate) {
    .data %>% 
      filter(
        map_dbl(
          {{ .list_column }}, 
          \(x) {
            filter(x, {{ .predicate }}) %>% 
              nrow()
          }
        ) > 0)
  }

nested_birds %>% 
  filter_nested_frame(
    .list_column = capture_records, 
    mass < 20)

# primary and foreign keys ------------------------------------------------

# I use functions like the below to generate primary keys:

random_alpha_numeric <-
  function(n_symbols) {
    sample(
      c(0:9, 
        letters, 
        LETTERS),
      n_symbols
    ) %>% 
      str_c(collapse = "")
  }

# Create a primary key for observations using map:

observation_key <-
  map_chr(
    1:nrow(messy_birds),
    ~ random_alpha_numeric(5)
  )

# Or, in Base R (a hair faster):

observation_key <-
  replicate(
    n = nrow(messy_birds),
    random_alpha_numeric(5)
  )

# Check to see if it worked:

identical(
  observation_key %>% 
    unique() %>% 
    length(),
  nrow(messy_birds)
)

# We can wrap the above into a handy function:

add_key_to_frame <-
  function(.data, n_symbols) {
    
    # Generate a primary key:
    
    primary_key <-
      replicate(
        n = nrow(messy_birds),
        random_alpha_numeric({{ n_symbols }})
      )
    
    # Test if the key values are unique to the observations:
    
    if(
      identical(
        primary_key %>% 
        unique() %>% 
        length(),
        nrow(messy_birds)
      )) {
      
      # If the test evaluates to TRUE, add the column:
      
      .data %>% 
        mutate(
          primary_key = primary_key,
          .before = everything()
        ) %>% 
        return()
      
    } else {
      
      # If the test evaluates to FALSE, just return a warning message:
      
      return("Please try again, key values are not unique!")
    }
  }

# Create a new data frame with primary key:

messy_birds_w_key <-
  messy_birds %>% 
  add_key_to_frame(n_symbols = 5)

# When we conduct a grouped operation, the operation is conducted on each group separately. As such, adding foreign keys is straightforward:

messy_birds_w_primary_and_foreign_keys <-
  messy_birds_w_key %>% 
  mutate(
    spp_id =
      random_alpha_numeric(5),
    .by = spp,
    .before = spp
  )

# It did, so tidy the table by splitting based on the level of observation (the third tidy data rule):

captures <-
  messy_birds_w_primary_and_foreign_keys %>% 
  select(
    capture_id = primary_key,
    band_number:spp_id,
    wing:mass)

life_history <- 
  messy_birds_w_primary_and_foreign_keys %>% 
  select(spp_id:diet) %>% 
  distinct()

# Bask in your tidy glory:

my_tidy_list <-
  lst(
    captures,
    life_history
  )

# generate and work with sqlite databases ---------------------------------

# If we wanted to store these data, we would previously have created a list and stored it on our hard drives as an rds file:

lst(captures, life_history) %>% 
  write_rds("data/processed/captures_and_birds.rds")

# The above has the limitation that an rds file can only be read by R. Additionally, when we read in an RDS file, we read the whole file at once into our memory. This is not a great solution for big data!

# We will make a sqlite database with two packages, "DBI" (`dbConnect()`)and "RSQLite" (`SQLite()`). 

# Run the following to generate a blank database:

captures_database <-
  dbConnect(
    SQLite(),
    "data/processed/captures_and_birds.sqlite"
  )

# *Note: `dbConnect()` can be used to create a new database or connect to an existing database. In the above, it does both tasks simultaneously.*

# To write a table, we use the *DBI* function `dbWriteTable`. We supply the connection to the database, the name of the table we would like to write, and the data frame that we would like to add:

dbWriteTable(
  conn = captures_database, 
  name = "captures",
  value = captures)

# We can verify that the table has been written with `dbListTables`:

dbListTables(conn = captures_database)

# If we want to overwrite an existing table, we can add the argument `overwrite = TRUE` to `dbWriteTable()` (but use carefully!):

dbWriteTable(
  conn = captures_database, 
  name = "captures",
  value = captures,
  overwrite = TRUE)

# Let's add both of our tidy tables at once using `map()`

my_tidy_list %>% 
  names() %>% 
  map(
    \(x) {
      dbWriteTable(
        conn = captures_database, 
        name = x,
        value = pluck(my_tidy_list, x),
        overwrite = TRUE)
    }
  )

# And verify that both tables have been added:

dbListTables(conn = captures_database)

# We can read in a database table with the tidyverse function `tbl` (from the dplyr package):

tbl(captures_database, "life_history")

# Notice that the above is a bit different than our typical tibble output. We see the `Source`, which describes the tibble itself, and `Database`, which describes the database where the table is located.

# Let's subset the database table `life_history` to birds in which the diet is "insectivore":

tbl(captures_database, "life_history") %>% 
  filter(diet == "insectivore")

# Under-the-hood, `dplyr` uses `dbplyr` functions when working with databases. Our operation above was actually transformed to a SQL query (and conducted with SQL, not R). We can observe this by passing the above into `show_query()`:

tbl(captures_database, "life_history") %>% 
  filter(diet == "insectivore") %>% 
  show_query()

# The real power in a database comes from joins. To subset `captures` to those in which the diet is "insectivore" (from the `life_history` table), we would use:

my_tidy_list %>% 
  pluck("captures") %>% 
  semi_join(
    my_tidy_list %>% 
      pluck("life_history") %>% 
      filter(diet == "insectivore"),
    by = "spp_id"
  )

# To do so with our sqlite database, we would use:

tbl(captures_database, "captures") %>% 
  semi_join(
    tbl(captures_database, "life_history") %>% 
      filter(diet == "insectivore"),
    by = "spp_id"
  )

# Let's look at the database operation that we completed above:

tbl(captures_database, "captures") %>% 
  semi_join(
    tbl(captures_database, "life_history") %>% 
      filter(diet == "insectivore"),
    by = "spp_id"
  ) %>% 
  show_query()

# We would not see any performance improvements with data that are this small (in fact, the performance is considerably worse!). Below, I use the function `mark` from the *bench* package to evaluate the operation with a tibble and with a database:

my_mark(
  list(
    my_tidy_list %>% 
      pluck("captures") %>% 
      semi_join(
        my_tidy_list %>% 
          pluck("life_history") %>% 
          filter(diet == "insectivore"),
        by = "spp_id"
      )
  )
  
)

my_mark(
  tbl(captures_database, "captures") %>% 
    semi_join(
      tbl(captures_database, "life_history") %>% 
        filter(diet == "insectivore"),
      by = "spp_id"
    )
)

# The database operation is *way* slower and requires much more memory than the operation in which the data are stored in our memory!

# This is, in part, because more memory is allocated to the connection with the database ...

obj_size(
  tbl(captures_database, "captures")
)

# ... than the memory allocated to the object itself:

obj_size(
  pluck(my_tidy_list, "captures")
)


# sql database with big data ----------------------------------------------

# Let's look at the performance with big data. `amke` is a pretty big file (there are more than 3 million records) and only represents a small subset of the data that we collected:

amke

# Let's make a database to store the file:

amke_db <-
  dbConnect(
    SQLite(),
    "data/processed/amke.sqlite"
  )

dbWriteTable(
  conn = amke_db, 
  name = "detections",
  value = amke)

# We can then connect to our tibble with:

tbl(amke_db, "detections")

# Notice that the time data got transformed into numbers! These numbers represent the number of seconds since (positive values) or before (negative values) 1970-01-01 00:00:00 UTC (the unix timestamp). To get around this, we can transform the time column into a character vector before writing the table:

amke %>% 
  mutate(time = as.character(time)) %>% 
  dbWriteTable(
    conn = amke_db, 
    name = "detections",
    value = .,
    overwrite = TRUE)

# Let's verify that this worked:

tbl(amke_db, "detections")

# You might have noticed that, once the data are written to the database, the read operation is much faster for the database:

my_mark(
  read_csv("data/raw/amke_detections.csv")
)

my_mark(
  tbl(amke_db, "detections")
)

# This is because the data are not transferred into our memory:

obj_size(
  amke
)

obj_size(
  tbl(amke_db, "detections")
)

# This is also true if we globally assign the database table to a name:

temp <- 
  tbl(amke_db, "detections")

obj_size(
  temp
)

# The performance improvements do not stop there either. In the below, I test what it takes to filter the data:

my_mark(
  amke %>% 
    filter(
      month(time) == 7,
      rssi > -90,
      node_id == "377c42")
)

my_mark(
  detections %>% 
    filter(
      month(time) == 7,
      rssi > -90,
      node_id == "377c42")
)

# This is a huge savings in both memory allocation and processing time!

# Admittedly, however, much of the performance improvement is generated by the fact that we are talking to the database, but not actually storing the data in our memory. To move the database to our memory, we need to use the function `collect`:

detections %>% 
  filter(
    month(time) == 7,
    rssi > -90,
    node_id == "377c42") %>% 
  collect()

# Passing the results of our query to memory *does* negate the performance improvements in terms of processing time:

my_mark(
  amke %>% 
    filter(
      month(time) == 7,
      rssi > -90,
      node_id == "377c42")
)

my_mark(
  detections %>% 
    filter(
      month(time) == 7,
      rssi > -90,
      node_id == "377c42") %>% 
    collect()
)

# But notice in the above that the memory required to complete the operation above is considerably lower!

# Also, if we include the initial reading process in our operation ...

my_mark({
  
  # Establish connection:
  
  temp <-
    dbConnect(
      SQLite(),
      "data/processed/amke.sqlite")
  
  # Filter and collect:
  
  tbl(temp, "detections") %>% 
    filter(
      month(time) == 7,
      rssi > -90,
      node_id == "377c42") %>% 
    collect()
})

my_mark({
  read_csv("data/raw/amke_detections.csv") %>% 
    filter(
      month(time) == 7,
      rssi > -90,
      node_id == "377c42")
})

# The advantage of the database method is that we only have to store data in our memory when it is necessary. 

# dblyr helps a ton once you have read in a file. Knowing a bit of the SQL language, however, is necessary if you wish to modify the database itself. Below, I delete all records for a given tag_id:

dbExecute(
  amke_db,
  "DELETE FROM detections WHERE (tag_id = '78195233');")

# Let's verify that the tag has been removed:

tbl(amke_db, "detections") %>% 
  summarize(
    n_detections = n(),
    .by = tag_id
  )

# It has!

# If SQL is new for you, I recommend learning by showing the query every time you use dbplyr:

tbl(amke_db, "detections") %>% 
  summarize(
    n_detections = n(),
    .by = tag_id
  ) %>% 
  show_query()

# A HUGE advantage of using a database is the ability to append rows without reading in and writing the whole dataset. As an example, let's write our data subset to a new csv file:

tbl(amke_db, "detections") %>% 
  collect() %>%
  write_csv("data/processed/append_example.csv")

# Now we will use the tibble stored in our memory to create a data frame of just observations of `tag_id` "78195233":

amke_78195233 <- 
  amke %>% 
  filter(tag_id == "78195233")

# If we wanted to add these data to the csv file (`append_example.csv`), we would have to read in the whole file, add the new observations (with `bind_rows()`), and then write the data again. 

my_mark(
  read_csv("data/processed/append_example.csv") %>% 
    bind_rows(amke_78195233) %>% 
    write_csv("data/processed/append_example.csv")
)

# This process took a very long time and required a huge amount of allocated memory!

# To append a SQL table, we use the DBI function `dbAppendTable`:

my_mark(
  dbAppendTable(
    conn = amke_db,
    name = "detections",
    value = amke_78195233
  )
)

# That is WAY faster ... a huge advantage with big data! 

# The above provides a simple solutions for managing your team's data. You can have your team enter their data on spreadsheets, use R to clean the data, and then add the data to your database when you have done so. For many of my projects, I create a data entry website (a Shiny app) that serves as a Graphical User Interface (GUI) for my field team and the website does all of the database operations. 

# Before we leave this portion of the lesson, I want to note that I have used SQLite because it is a very common database format. Similar methods can be applied to other database programs (e.g., PostgreSQL or Mongo databases).

# data.table and dtplyr ---------------------------------------------------

# The data.table package provides an option for working with big data. 

# We can read in a data table object with the *data.table* function `fread`:

data.table::fread("data/raw/amke_detections.csv")

# Reading in a data table with `data.table::fread()` is much faster than `read_csv()`:

my_mark(
  data.table::fread("data/raw/amke_detections.csv")
)

my_mark(
  read_csv("data/raw/amke_detections.csv")
)

# Though note that `read_csv()` is faster and memory efficient than `read.csv()` (which I never use)!

my_mark(
  read.csv("data/raw/amke_detections.csv")
)

# The object returned by `data.table::fread()` is a data.table class object:

data.table::fread("data/raw/amke_detections.csv") %>% 
  class()

# Let's globally assign the name `amke_dt` to the resultant object:

amke_dt <-
  data.table::fread("data/raw/amke_detections.csv")

# Notice that I avoided attaching the *data.table* package and instead used `::` to call `fread()`. The tidyverse package *dtplyr* allows us to work with data tables as though they were tibbles. Under-the-hood, tidyverse functions are transformed to *data.table* functions:

# Native dt version:

my_mark(
  amke_dt[tag_id == "78195233", ]
)

my_mark(
  amke_dt %>% 
    filter(tag_id == "78195233")
)

# The tidyverse version is considerably slower and less memory efficient than using native *data.table* functions, but I greatly prefer the syntax. We can speed up the processing time by creating an object called a "lazy data table" by passing the data table to the *dtplyr* function `lazy_dt`.

amke_lazy <-
  fread("data/raw/amke_detections.csv") %>% 
  dtplyr::lazy_dt()

my_mark(
  amke_lazy %>% 
    filter(tag_id == "78195233")
)

# The performance of the above is much faster and more memory efficient than even the native dt version. This is because you are not actually generating the resultant dataset, just previewing the operation. If you print the results, you can see the a preview of the results and the *data.table* code used:

amke_lazy %>% 
  filter(tag_id == "78195233")

# As the message in the above states, we can access the results with `as_tibble()`:

amke_lazy %>% 
  filter(tag_id == "78195233") %>% 
  as_tibble()

# Of course, in doing so, our performance is once again reduced:

my_mark(
  amke_lazy %>% 
    filter(tag_id == "78195233")
)

my_mark(
  amke_lazy %>% 
    filter(tag_id == "78195233") %>% 
    as_tibble()
)

# The "lazy data table" method is useful because you can test out your code while running tidyverse functions, and only pass the resultant object to a tibble once all processing steps have been completed. Given that most of our coding time is spent processing data objects, the cost of using tidyverse functions is minimized. Moreover, any costs are greatly offset by the enhanced power and readability that tidyverse functions provide us.

# Consider the following operation:

amke_lazy %>% 
  summarize(
    n_detections = n(),
    .by = tag_id
  )

# You can see in "Call", above that summarizing this dataset would require a whole new syntax:

amke_dt[, .(n_detections = .N), keyby = .(tag_id)]

# Although this is fast:

my_mark(
  amke_dt[, .(n_detections = .N), keyby = .(tag_id)]
)

# I will gladly exchange readability and familiar functions for what is often a very minor speed improvement:

my_mark(
  amke_lazy %>% 
    summarize(
      n_detections = n(),
      .by = tag_id
    ) %>% 
    as_tibble()
)

# arrow -------------------------------------------------------------------

# Our final topic of this lesson will be the simplest -- the *arrow* package. I use this package a lot for big data that are stored as csv files. Like the database operations above, the advantage of this package is that files are not stored in your memory until it is necessary to do so.

# With *arrow*, we establish a connection with a csv dataset with the function `open_csv_dataset`:

open_csv_dataset("data/raw/amke_detections.csv")

# You probably noticed that was super fast. It is *much* faster and more memory efficient than even `fread()`:

my_mark(
  open_csv_dataset("data/raw/amke_detections.csv")
)

my_mark(
  fread("data/raw/amke_detections.csv")
)

# Like our database operations with *dtplyr*, the improvement is due to the fact that the full object is not actually being read into R:

obj_size(
  open_csv_dataset("data/raw/amke_detections.csv")
)

obj_size(
  fread("data/raw/amke_detections.csv")
)

# Let's globally assign a name to the resultant arrow object:

amke_arrow <- 
  open_csv_dataset("data/raw/amke_detections.csv")

# Like working with data tables with *dtplyr* and database tables with *dbplyr*, *arrow* allows us to use tidyverse functions when processing data. Applying tidyverse functions to arrow objects is incredibly fast and memory efficient:

my_mark(
  amke_arrow %>% 
    filter(tag_id == "78195233")
)

my_mark(
  amke_dt %>% 
    filter(tag_id == "78195233")
)

# Like *dbplyr*, which transforms a query into SQL, and *dtplyr*, which transforms a query into *data.table* syntax, *arrow* is transforming our tidyverse code into a query that the Apache C++ library understands. 

# Unlike with *dbplyr* and *dtplyr*, however, the output is perhaps not very helpful for viewing the results:

amke_arrow %>% 
  filter(tag_id == "78195233")

# The above output can be challenging in that the printed information does not allow us to see the results.

# We can use the pillar function `glimpse` (which we are given access to with `library(tidyverse)`) to get *some* information on the object (similar to what we might retrieve with `utils::str()` for regular data frames):

amke_arrow %>% 
  filter(tag_id == "78195233") %>% 
  glimpse()

# We can view the resultant object by passing it into memory with `collect()`:

amke_arrow %>% 
  filter(tag_id == "78195233") %>% 
  collect()

# However, in doing so, we lose our speed and memory advantage:

my_mark(
  amke_arrow %>% 
    filter(tag_id == "78195233") %>% 
    collect()
)

my_mark(
  amke_dt %>% 
    filter(tag_id == "78195233")
)

# So it might seem that we lose any advantage when we use arrow, but this is not the case! Although we have to code a bit blind, if we know what we want and how to code a given operation (e.g., it does not require exploration during pre-processing), this is a very effective method.

# For example, the field team for this study would often track birds using a handheld directional antenna. As such, they often wanted to know where a tagged bird was observed on a previous day. Pulling out a record for a single day and a single tag_id would look something like:

open_csv_dataset("data/raw/amke_detections.csv") %>% 
  filter(
    tag_id == "19332D2D",
    as_date(time) == "2023-06-30") %>% 
  collect()

# This is way faster and more memory efficient than the data table version:

# arrow:

my_mark(
  open_csv_dataset("data/raw/amke_detections.csv") %>% 
    filter(
      tag_id == "19332D2D",
      as_date(time) == "2023-06-30") %>% 
    collect()
)

# data.table

my_mark(
  fread("data/raw/amke_detections.csv") %>% 
    dtplyr::lazy_dt() %>% 
    filter(
      tag_id == "19332D2D",
      as_date(time) == "2023-06-30") %>% 
    as_tibble()
)

# We can continue to fine-tune our search and gain further performance improvements. For example, perhaps the field team is going to get up early and wants to know where the bird slept on the previous night:

open_csv_dataset("data/raw/amke_detections.csv") %>% 
  filter(
    tag_id == "19332D2D",
    as_date(time) == "2023-06-30",
    hour(time) > 20) %>% 
  collect()

# The rssi value is a measure of signal strength -- higher values means that the bird was closer to the node. As such, we might want to know the three closest nodes to where the bird was the night before:

open_csv_dataset("data/raw/amke_detections.csv") %>% 
  filter(
    tag_id == "19332D2D",
    as_date(time) == "2023-06-30",
    hour(time) > 20) %>% 
  summarize(
    rssi = mean(rssi),
    .by = node_id
  ) %>% 
  slice_max(
    rssi, 
    n = 3,
    with_ties = FALSE) %>% 
  collect()

# *Note: Some operations available in dplyr and not yet available with arrow. Above, I could not use `slice_max()` on grouped data and needed to specify `with_ties = FALSE`. This will likely change in the future.*

# The above will give a solid start for our field team's search! Importantly, because the data did not have to be read into memory until the final step in the process, this operation was fast and memory efficient:

my_mark(
  open_csv_dataset("data/raw/amke_detections.csv") %>% 
    filter(
      tag_id == "19332D2D",
      as_date(time) == "2023-06-30",
      hour(time) > 20) %>% 
    summarize(
      rssi = mean(rssi),
      .by = node_id
    ) %>% 
    slice_max(
      rssi, 
      n = 3,
      with_ties = FALSE) %>% 
    collect()
)

# Here is the memory allocation and processing time for completing the operation with *data.table* and *dtplyr*:

my_mark(
  fread("data/raw/amke_detections.csv") %>% 
    dtplyr::lazy_dt() %>% 
    filter(
      tag_id == "19332D2D",
      as_date(time) == "2023-06-30",
      hour(time) > 20) %>% 
    summarize(
      rssi = mean(rssi),
      .by = node_id
    ) %>% 
    slice_max(
      rssi, 
      n = 3,
      with_ties = FALSE) %>% 
    as_tibble()
)

# And here is how long it would have taken with `read_csv()`:

my_mark(
  read_csv("data/raw/amke_detections.csv") %>% 
    filter(
      tag_id == "19332D2D",
      as_date(time) == "2023-06-30",
      hour(time) > 20) %>% 
    summarize(
      rssi = mean(rssi),
      .by = node_id
    ) %>% 
    slice_max(
      rssi, 
      n = 3,
      with_ties = FALSE) %>% 
    as_tibble()
)

# final notes -------------------------------------------------------------

# The choice of which big data solution to choose depends on your situation. The full dataset from which `amke_detection.csv` was derived was considerably larger and I had to conduct operations like the above often. As such, I chose the *arrow* option. If the data were smaller, I may have chosen *data.table*. If I needed to append the records often, I may have chosen *dbplyr*.

