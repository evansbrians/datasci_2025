# Code-a-long with the Focus on Functions tutorial

# Note: I have only replicated code blocks that may take a long time to
# enter!

# setup -------------------------------------------------------------------

library(tidyverse)

# The iris dataset:

iris <-
  read_rds("data/raw/iris.rds")

# Population and CO2 data:

read_rds("data/processed/populations_co2.rds") %>% 
  list2env(.GlobalEnv)

# function basics ---------------------------------------------------------

# A simple custom function:

multiply_by_two <- 
  function (x) {
    x * 2
  }

# Basic function applied, calculate mean sepal length for I. setosa:

iris %>% 
  filter(species == "setosa") %>% 
  pull(sepal_length) %>% 
  mean()

# Basic function applied, calculate mean sepal length for I. virginica:

iris %>% 
  filter(species == "virginica") %>% 
  pull(sepal_length) %>% 
  mean()

# Write a custom function to calculate mean sepal length for any species:

subset_sepal_lengths <-
  function(spp) {
    iris %>% 
      filter(species == spp) %>% 
      pull(sepal_length) %>% 
      mean()
  }

# functions & the global environment --------------------------------------

# What happens to internal assignments within a functions' body?

fun_with_internal_assignments <-
  function(spp) {
    iris_spp <-
      iris %>% 
      filter(species == spp)
    
    iris_spp %>% 
      pull(sepal_length) %>% 
      mean()
  }

# multiple variables ------------------------------------------------------

# Another look at our filter and mean calculation:

iris %>% 
  filter(species == "virginica") %>% 
  pull(sepal_length) %>% 
  mean()

# A custom function with two variables:

calc_mean_spp_measure <-
  function(spp, measure) {
    iris %>% 
      filter(species == spp) %>% 
      pull(measure) %>% 
      mean()
  }

# default values ----------------------------------------------------------

# Calculate the total global CO2 for the year 2010:

populations_co2 %>% 
  filter(year == 2010) %>% 
  mutate(total_co2 = co2 * population) %>%
  pull() %>%
  sum()

# Write a custom function to calculate the total global CO2 for a given
# year:

calculate_total_co2_yr <-
  function(yr) {
    populations_co2 %>% 
      filter(year == yr) %>% 
      mutate(total_co2 = co2 * population) %>%
      pull() %>%
      sum()
  }

# scoping -----------------------------------------------------------------

# Scoping ambiguity:

subset_sepal_lengths <-
  function(spp) {
    iris %>% 
      filter(species == spp) %>% 
      pull(sepal_length) %>% 
      mean()
  }

# An ill-advised way of naming formals:

fun_with_bad_formals <-
  function(species) {
    iris %>% 
      filter(species == species) %>% 
      pull(sepal_length) %>% 
      mean()
  }

# An better way of using formals:

fun_with_inserted_formals <-
  function(species) {
    iris %>% 
      filter(
        species == {{ species }}
      ) %>% 
      pull(sepal_length) %>% 
      mean()
  }

# Scoping and quoted variables ...

calc_mean_spp_measure <-
  function(spp, measure) {
    iris %>% 
      filter(species == spp) %>% 
      pull(measure) %>% 
      mean()
  }

# modifying existing functions --------------------------------------------

# Arrange descending is annoying ...

iris %>% 
  arrange(
    desc(sepal_length)
  )

# How about a custom function?

arrange_desc <-
  function(x, sort_by_variable) {
    arrange(
      x,
      desc(sort_by_variable)
    )
  }
