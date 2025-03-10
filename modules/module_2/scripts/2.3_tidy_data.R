# Script for the tidy data lesson

# setup -------------------------------------------------------------------

library(tidyverse)

read_rds("data/raw/untidy_data.rds") %>% 
  list2env(envir = .GlobalEnv)

# each variable forms a column --------------------------------------------

# Separate a column into multiple columns:

separate(
  data = badDate,
  col = observationDate,
  into = c("date1", "date2"),
  sep = ", "
)

# Or ... better:

badDate %>% 
  separate(
    col = observationDate,
    into = c("date1", "date2"),
    sep = ", "
  )

# Combine multiple columns into a single column:

unite(
  data = really_bad_date,
  col = "date",
  c(
    year, 
    month, 
    day
  ),
  sep = "-"
)

# Or ... better:

really_bad_date %>% 
  unite(
    col = "date",
    c(year, month, day),
    sep = "-"
  )

# Remove transitively dependent columns:

select(badYear, id:mass)

# Or ... better:

badYear %>% 
  select(id:mass)

# Or ... better yet:

badYear %>% 
  select(!observationYear)

# each observation forms a row --------------------------------------------

# Pivot from wide to long frame:

pivot_longer(
  data = untidyFrame,
  cols = treatmentA:treatmentB,
  names_to = "treatment",
  names_prefix = "treatment",
  values_to = "value"
)

# Or ... better:

untidyFrame %>% 
  pivot_longer(
    cols = treatmentA:treatmentB,
    names_to = "treatment",
    names_prefix = "treatment",
    values_to = "value"
  )

# When there are multiple rows per observation (pivot_wider):

pivot_wider(
  data = dfTooLong,
  names_from = measurement,
  values_from = value
)

# Or ... better:

dfTooLong %>% 
  pivot_wider(
    names_from = measurement,
    values_from = value
  )

# each level of observation forms a table ---------------------------------

# Split badBandingRecord by level of observation:

list(
  birds = 
    select(
      badBandingRecord,
      birdID,
      observationDate,
      site,
      mass
    ),
  sites = 
    select(
      badBandingRecord,
      site,
      canopyCover
    )
)

# Or ... better:

list(
  birds = 
    badBandingRecord %>% 
    select(
      birdID,
      observationDate,
      site,
      mass
    ),
  sites = 
    badBandingRecord %>% 
    select(
      site,
      canopyCover
    )
)

# Split badBandingRecord by level of observation (and make sites distinct):

bird_list <-
  list(
    birds = 
      badBandingRecord %>% 
      select(
        birdID,
        observationDate,
        site,
        mass
      ),
    sites = 
      badBandingRecord %>% 
      select(
        site,
        canopyCover
      )
  )

bird_list[[2]] <-
  distinct(bird_list[[2]])

# Or ... better:

  list(
    birds = 
      badBandingRecord %>% 
      select(
        birdID,
        observationDate,
        site,
        mass
      ),
    sites = 
      badBandingRecord %>% 
      select(
        site,
        canopyCover
      ) %>% 
      distinct()
  )

