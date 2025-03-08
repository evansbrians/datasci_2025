
# My code for the tutorial: reshaping data frames

# setup --------------------------------------------------------

library(tidyverse)

# Read rds file:

angola_ungulates_list <-
  read_rds("data/raw/angola_ungulates.rds")


# sending list item names to the global environment -----------------------

# Assign each list item name to the global environment (Not my preferred
# method!):

list2env(angola_ungulates_list, envir = .GlobalEnv)

# Remove the name angola_ungulates_list from the global environment:

rm(angola_ungulates_list)

# Read and assign names (better!):

list2env(
  read_rds("data/raw/angola_ungulates.rds"),
  envir = .GlobalEnv)

# Read in a list and assign each list item name to the global environment
# (preferred method):

read_rds("data/raw/angola_ungulates.rds") %>%
  list2env(envir = .GlobalEnv)

# row-wise combinations of list items -------------------------------------

# Read in the list, bind the list items, and then globally assign to a name
# (this nested version is not preferred!):

angola_ungulates <-
  bind_rows(
    read_rds("data/raw/angola_ungulates.rds"))

# Read in the list, bind the list items, and then globally assign to a name
# (the piped version *is* preferred!):

angola_ungulates <-
  read_rds("data/raw/angola_ungulates.rds") %>%
  bind_rows()

# Remove the names perissodactyla and artiodactyla from the global environment:

rm(perissodactyla, artiodactyla)

# unite -------------------------------------------------------------------

# Unite date columns and globally assign to the name
# angola_ungulates_date_fix:

angola_ungulates_date_fix <-
  angola_ungulates %>%
  unite(
    col = "date",
    year:day,
    sep = "-")

# Unite genus and species columns and globally assign to the name
# angola_ungulates_spp_fix:

angola_ungulates_spp_fix <-
  angola_ungulates_date_fix %>%
  unite(
    col = "sci_name",
    genus:species,
    sep = " ")

# Remove the names angola_ungulates_spp_fix and angola_ungulates_date_fix
# from the global environment:

rm(angola_ungulates_spp_fix, angola_ungulates_date_fix)

# Complete in a single chained analysis and globally assign a name:

angola_ungulates <-
  
  # Read in the data:
  
  read_rds("data/raw/angola_ungulates.rds") %>%
  
  # Combine the datasets by row:
  
  bind_rows() %>%
  
  # Combine the date columns:
  
  unite(
    col = "date",
    year:day,
    sep = "-") %>%
  
  # Combine the scientific name columns:
  
  unite(
    col = "sci_name",
    genus:species,
    sep = " ")

# separate ----------------------------------------------------------------

# Separate the taxonomy columns and globally assign a name:

angola_ungulates_taxonomy_fix <-
  angola_ungulates %>%
  separate(
    col = "taxonomy",
    into = c("class", "order", "family"),
    sep = "-")

# Complete in a single chained analysis and globally assign a name:

angola_ungulates <-
  
  # Read in the data:
  
  read_rds("data/raw/angola_ungulates.rds") %>%
  
  # Combine the datasets by row:
  
  bind_rows() %>%
  
  # Combine the date columns:
  
  unite(
    col = "date",
    year:day,
    sep = "-") %>%
  
  # Combine the scientific name columns:
  
  unite(
    col = "sci_name",
    genus:species,
    sep = " ") %>%
  
  # Separate the taxonomy into multiple columns:
  
  separate(
    col = "taxonomy",
    into = c("class", "order", "family"),
    sep = "-")

# Remove the name angola_ungulates_taxonomy_fix from the global environment:

rm(angola_ungulates_taxonomy_fix)

# select columns ----------------------------------------------------------

# Subset to observations of ungulates and globally assign the name
# observations:

observations <-
  select(
    angola_ungulates,
    date:user_login,
    sci_name)

# Subset to taxonomic information and globally assign the name taxonomy:

taxonomy <-
  select(
    angola_ungulates,
    sci_name,
    class:family,
    common_name)

# Subset to observations of undulates and globally assign the name
# observations (preferred):

observations <-
  angola_ungulates %>%
  select(
    date:user_login,
    sci_name)

# Subset to taxonomic information and globally assign the name taxonomy
# (preferred):

taxonomy <-
  angola_ungulates %>%
  select(
    sci_name,
    class:family,
    common_name)

# Subset to taxonomic information (but do not include class) and globally
# assign the name taxonomy (preferred):

taxonomy <-
  angola_ungulates %>%
  select(
    sci_name,
    order:family,
    common_name)


# remove duplicates -------------------------------------------------------

# Subset to taxonomy columns, make distinct, and assign a name to the
# the global environment:

taxonomy_distinct <-
  angola_ungulates %>%
  select(
    sci_name,
    order:family,
    common_name) %>%
  distinct()

# bind columns ------------------------------------------------------------

# The function bind_cols can be used to join if you are super sure that the
# orders of the variables in the tables are equivalent"

observations %>%
  select(!sci_name) %>%
  bind_cols(taxonomy)

# But this is often unsafe!

observations %>%
  select(!sci_name) %>%
  bind_cols(
    taxonomy %>%
      arrange(sci_name)
  )

# join tables -------------------------------------------------------------

# A join is much safer than bind_cols!

observations %>%
  left_join(
    taxonomy_distinct,
    by = "sci_name"
  )

# ... but can also be unsafe if you are not careful!

observations %>%
  left_join(
    taxonomy,
    by = "sci_name"
  )

# Remove the name taxonomy from the global environment:

rm(taxonomy)

# Join taxonomic observations and globally assign a name to the global
# environment:

families <-
  observations %>%
  select(sci_name) %>%
  left_join(
    taxonomy_distinct %>%
      select(sci_name, family),
    by = "sci_name"
  )

# summarize data frames ---------------------------------------------------

# `summarize()` is a great (and flexible) way to summarize the data by some
# data grouping:

families %>%
  group_by(family) %>%
  summarize(count = n())

# With a new(ish) update to the tidyverse, this can be accomplished without
# `group_by()`:

families %>%
  summarize(
    count = n(),
    .by = family)

# pivot data frames -------------------------------------------------------

# We often need to pivot from long-form data to wide-form data:

wide_families <-
  families %>%
  count(family) %>%
  pivot_wider(
    names_from = family,
    values_from = n)

# When working with data supplied by others, the necessity to pivot from
# long to wide form is more common:

wide_families %>%
  pivot_longer(
    cols = Bovidae:Suidae,
    names_to = "family",
    values_to = "n")
