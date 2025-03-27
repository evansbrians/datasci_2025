# This code-along video script introduces the concept of across

# setup -------------------------------------------------------------------

library(tidyverse)

weather <- 
  read_rds('data/processed/weather_tidy.rds')

iris <- 
  read_rds('data/raw/iris.rds')

# across ------------------------------------------------------------------

# A very repetitive method:

weather %>% 
  pluck("observations") %>% 
  filter(station == "USW00094728") %>% 
  select(date, temperature_min:temperature_max) %>% 
  mutate(
    temperature_min = as.numeric(temperature_min)) %>% 
  mutate(
    temperature_max = as.numeric(temperature_max))

# Now you! Without adding a function, simplify the code block by reducing
# the code to a single mutate function.

weather %>% 
  pluck("observations") %>% 
  filter(station == "USW00094728") %>% 
  select(date, temperature_min:temperature_max)
  
# Reduce repetitions with across:

weather %>% 
  pluck("observations") %>% 
  filter(station == "USW00094728") %>% 
  select(date, temperature_min:temperature_max) 

# In more complex cases, use across with a formula (convert degrees Celsius 
# to Fahrenheit ... x * 1.8 + 32):

weather %>% 
  pluck("observations") %>% 
  filter(station == "USW00094728") %>% 
  select(date, temperature_min:temperature_max) %>% 
  mutate(
    temperature_min = 
      temperature_min %>% 
      as.numeric() * 1.8 + 32,
    temperature_max = 
      temperature_max %>% 
      as.numeric() * 1.8 + 32)

# Maintain only columns of interest (like good ol' transmute):



# Summarize, old school:

weather %>% 
  pluck("observations") %>% 
  group_by(station) %>% 
  summarize(
    temperature_min = 
      as.numeric(temperature_min) %>% 
      mean(na.rm = TRUE),
    temperature_max =
      as.numeric(temperature_max) %>% 
      mean(na.rm = TRUE))

# Now you! Calculate the mean minimum and maximum temperatures at weather
# stations using summarize and across:

weather %>% 
  pluck("observations") %>% 
  group_by(station) %>% 
  summarize()

# Now you! Without using group_by(), calculate the mean sepal_length and
# petal_width of each species of iris:

iris %>% 
  group_by(species) %>% 
  summarize(
    sepal_length = mean(sepal_length),
    petal_width = mean(petal_width))

# across with tidy selection ----------------------------------------------

# Convert a subset of columns to numeric:

weather %>% 
  pluck("observations") %>% 
  filter(station == "USW00094728") %>% 
  mutate(
    date,
    across(
      precip:temperature_max, 
      as.numeric),
    .keep = "none")

# Convert temperature variables to numeric and maintain temperature and 
# station columns:

weather %>% 
  pluck("observations") %>% 
  filter(station == "USW00094728") %>% 
  mutate(
    date,
    across(
      temperature_min:temperature_max, 
      as.numeric),
    .keep = "none")

# Now you! Simplify the code below by using the across and matches
# functions to iterate the mean function across temperature variables.

weather %>% 
  pluck("observations") %>% 
  summarize(
    temperature_min = 
      as.numeric(temperature_min) %>% 
      mean(na.rm = TRUE),
    temperature_max =
      as.numeric(temperature_max) %>% 
      mean(na.rm = TRUE),
    .by = station)

# Now you! Calculate the average sepal length and width for each species 
# using only the named functions summarize, across, and matches:

iris %>% 
  group_by(species) %>% 
  summarize(
    sepal_length = mean(sepal_length),
    sepal_width = mean(sepal_width))
