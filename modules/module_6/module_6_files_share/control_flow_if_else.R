# This script demonstrates the control flow operation:

# if(logical) {
#   do this process
# } else {
#   do this other process
# }

# setup -------------------------------------------------------------------

library(tidyverse)

# Read in life history data:

birds <-
  read_rds("data/raw/district_birds.rds") %>% 
  pluck("birds") %>% 
  select(spp = species, diet:foraging)

# Read in counts and captures:

counts_captures <-
  read_rds("data/raw/district_birds.rds") %>% 
  tidyselect:::select(counts, captures) %>% 
  
  # Subset to just the columns spp, count, and mass:
  
  map(
    ~ .x %>% 
      select(spp, matches("count|mass")))

# Repetitive plotting -----------------------------------------------------

# Plot counts by diet (bar plot):

counts_captures %>% 
  pluck("counts") %>% 
  
  # Summarize by diet:
  
  summarize(
    count = sum(count),
    .by = diet) %>% 
  
  # Visualize: 
  
  ggplot(
    aes(x = diet, y = count)) +
  geom_bar(stat = "identity")

# Now you! Generate a bar plot of counts by foraging guild:



# Plot mass by diet (box plot):

counts_captures %>% 
  pluck("captures") %>% 
  ggplot(
    aes(x = diet, y = mass)) +
  geom_boxplot()

# Now you! Generate a box plot of mass by foraging guild:



# Iterative plotting with map ---------------------------------------------

# Total count by life history guild:

c("foraging", "diet") %>% 
  purrr::map(
    ~ counts_captures %>% 
      pluck("counts") %>% 
      
      # Summarize by life history guild:
      
      summarize(
        count = sum(count),
        .by = matches(.x)))

# Programmatically plot total counts by life history guild:

c("foraging", "diet") %>% 
  purrr::map(
    ~ counts_captures %>% 
      pluck("counts") %>% 
      
      # Summarize by life history guild:
      
      summarize(
        count = sum(count),
        .by = matches(.x)) %>% 
      
      # Plot the data: 
      
      ggplot(
        aes(x = .data[[.x]], y = count)) + 
      geom_bar(stat = "identity"))

# Now you! Programmatically generate a box plot of mass by life history
# guild:


      
# Introduction to control flow: if else -----------------------------------

user <- "Brian"

# Basic usage, just the if:

if(user == "Brian") "Boy howdy"

# Basic usage, if with else:

if(user == "Brian") {
  "Boy howdy" } else {
    "Hello world"
  }

# ... but:

if(c("Brian", "Jen") == "Brian") {
  "Boy howdy" } else {
    "Hello world"
  }

# So ...

c("Brian", "Jen") %>% 
  map_vec(
    ~ if(.x == "Brian") {
      "Boy howdy"
    } else {
      "Hello world"
    })

# Of course ...

if_else(
  c("Brian", "Jen") == "Brian",
  "Boy howdy",
  "Hello world")

# How are if ... else and if_else different?

dates <-
  tibble(date = c("2023-03-25", "25/3/2023"))

dates %>% 
  mutate(
    date = 
      if_else(
        str_detect(date, "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
        as_date(date),
        dmy(date)))

# ... and even:

if_else(
  str_detect(dates$date[[2]], "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
  as_date(dates$date[[2]]),
  dmy(dates$date[[2]]))

# Thus:

dates %>% 
  mutate(
    date = 
      date %>% 
      map_vec(
        ~ if(
          str_detect(.x, "[0-9]{4}-[0-9]{2}-[0-9]{2}")) {
          as_date(.x)
        } else {
          dmy(.x)
        }))

# Application of if else --------------------------------------------------

# Programmatically prepare count or capture data for plotting by diet:

names(counts_captures) %>% 
  purrr::map(
    ~ if(.x == "counts") {
        counts_captures %>% 
          pluck(.x) %>% 
          summarize(
            count = sum(count),
            .by = diet)
      } else {
        counts_captures %>% 
          pluck(.x) %>% 
          select(diet, mass)
      })

# Now you! Modify the code such that it generates a bar plot for counts ~ diet
# or a box plot for mass ~ diet:

names(counts_captures) %>% 
  purrr::map(
    ~ if(.x == "counts") {
      counts_captures %>% 
        pluck(.x) %>% 
        summarize(
          count = sum(count),
          .by = diet)
    } else {
      counts_captures %>% 
        pluck(.x) %>% 
        select(diet, mass)
    })





