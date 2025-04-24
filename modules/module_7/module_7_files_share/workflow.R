# setup -------------------------------------------------------------------

library(tidyverse)

dc_birds <- 
  read_rds('data/raw/district_birds.rds')

source("scripts/source_script.R")

# prep data ---------------------------------------------------------------

prep_data <- 
  function(dataset, var) {
    dc_birds %>% 
      pluck(dataset) %>% 
      select(spp, var) %>% 
      
      # Get common names and diet:
      
      join_life_history() %>% 
      
      # Remove unused columns and reorder:
      
      select(common_name:diet, var)
  }

# plot bird counts by diet guild ------------------------------------------

prep_data("counts", "count") %>% 

  # Summarize data for plotting:
  
  summarize(
    n_birds = sum(count),
    .by = diet) %>% 
  
  # Plot data:
  
  ggplot(
    aes(x = diet,
        y = n_birds)) +
  geom_bar(
    stat = "identity",
    fill = "#dcdcdc",
    color = "black") +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 4000)) +
  labs(
    title = "Bird counts by diet guild",
    x = "Diet guild",
    y = "Birds observed") +
  my_theme()

# plot bird counts by foraging guild --------------------------------------

prep_data("counts", "count") %>% 
  
  # Summarize data for plotting:
  
  summarize(
    n_birds = sum(count),
    .by = foraging) %>% 
  
  # Plot data:
  
  ggplot(
    aes(x = foraging,
        y = n_birds)) +
  geom_bar(
    stat = "identity",
    fill = "#dcdcdc",
    color = "black") +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 5000)) +
  labs(
    title = "Bird counts by foraging guild",
    x = "Foraging guild",
    y = "Birds observed") +
  my_theme()

# plot bird mass by diet guild --------------------------------------------

prep_data("captures", "mass") %>% 
  
  # Plot data:
  
  ggplot(
    aes(x = diet,
        y = mass)) +
  geom_boxplot(
    fill = "#dcdcdc",
    na.rm = TRUE) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 125)) +
  labs(
    title = "Bird mass by diet guild",
    x = "Diet guild",
    y = "Mass") +
  my_theme()

# plot bird mass by foraging guild ----------------------------------------

prep_data("captures", "mass") %>% 
  
  # Plot data:
  
  ggplot(
    aes(x = foraging,
        y = mass)) +
  geom_boxplot(
    fill = "#dcdcdc",
    na.rm = TRUE) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 125)) +
  labs(
    title = "Bird mass by foraging guild",
    x = "Foraging guild",
    y = "Mass") +
  my_theme()
  
  
