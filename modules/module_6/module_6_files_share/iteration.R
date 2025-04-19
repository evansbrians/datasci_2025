# Code-a-long with the Iteration tutorial

# Note: I have only replicated code blocks that may take a long time to
# enter and those for which entering your own code will not enhance your
# learning experience.

# setup -------------------------------------------------------------------

library(tidyverse)

# Read in the data:

portal <- 
  read_csv("data/raw/portal.csv")

# review of indexing ------------------------------------------------------

# Example vector:

my_vector <- 6:11

# Example matrix:

my_matrix <- 
  matrix(
    my_vector, 
    nrow = 3, 
    ncol = 2
  )

# Nested lists can be difficult to interpret if subset/extracted with 
# base R:

list(portal)[[1]][1:5, ][[6]]

# The tidyverse style is much easier to read!

list(portal) %>% 
  pluck(1) %>% 
  slice(1:5) %>% 
  pull(6)

# A named vector:

my_vector_named <-
  my_vector %>% 
  set_names(
    "six",
    "seven",
    "eight",
    "nine",
    "ten",
    "eleven"
  )

my_vector_named

# why iterate? ------------------------------------------------------------

# A bit of subsetting, extracting, and calculating in base R:

mean(
  portal[portal$species == "Neotoma albigula", ]$hindfoot_length,
  na.rm = TRUE)

# The much-more-lovely tidyverse version:

portal %>% 
  filter(species == "Neotoma albigula") %>% 
  pull(hindfoot_length) %>% 
  mean(na.rm = TRUE)

# Repetition is to be avoided (base R version)!

mean(
  portal[portal$species == "Neotoma albigula", ]$hindfoot_length,
  na.rm = TRUE
)

mean(
  portal[portal$species == "Dipodomys merriami", ]$hindfoot_length,
  na.rm = TRUE
)

# Repetition is to be avoided (tidyverse version)!

portal %>% 
  filter(species == "Neotoma albigula") %>% 
  pull(hindfoot_length) %>% 
  mean(na.rm = TRUE)

portal %>% 
  filter(species == "Dipodomys merriami") %>% 
  pull(hindfoot_length) %>% 
  mean(na.rm = TRUE)

# A custom function helps some ...

get_spp_mean <-
  function(x) {
    portal %>% 
      filter(species == x) %>% 
      pull(hindfoot_length) %>% 
      mean(na.rm = TRUE)
  }

get_spp_mean("Neotoma albigula")

# simple for loop ---------------------------------------------------------

# The simplest of for loops:

target_vector <- 6:10

output_container <-
  vector(
    "numeric", 
    length = length(target_vector) 
  )

for(i in seq_along(target_vector)) {
  output_container[[i]] <-
    target_vector[[i]] + 1
}

# for loops applied: population model -------------------------------------

# Create output container:

population <-
  vector("numeric", length = 10)

# Assign a value to the first position of the output container:

population[[1]] <- 10

# For loop sequence statement:

for(t in 2:length(population)) {
  
  # For loop body:
  
  population[[t]] <- 2 * population[[t - 1]]
}

# for loops applied: split-apply-combine basics ---------------------------

# Recall:

portal %>% 
  filter(species == "Neotoma albigula") %>% 
  pull(hindfoot_length) %>% 
  mean(na.rm = TRUE)

# Specify input variable:

portal_species <-
  portal %>% 
  pull(species) %>% 
  unique()

# Create output container:

hindfeet <-
  vector(
    "numeric", 
    length = length(portal_species)
  )

# For loop sequence statement:

for(i in seq_along(portal_species)) {
  
  # For loop body:
  
  hindfeet[[i]] <-
    portal %>% 
    
    # Split:
    
    filter(species == portal_species[[i]]) %>% 
    pull(hindfoot_length) %>% 
    
    # Apply:
    
    mean(na.rm = TRUE)
}

# for loops applied: split-apply-combine to data frames (index) -----------

# Specify input variable:

portal_species <-
  portal %>% 
  pull(species) %>% 
  unique()

# Create output container:

hindfeet_frame <-
  vector(
    "list", 
    length = length(portal_species)
  )

# For loop sequence statement:

for(
  i in seq_along(portal_species)
) {
  
  # For loop body:
  
  hindfeet_frame[[i]] <-
    tibble(
      species = portal_species[[i]],
      hindfeet =
        portal %>% 
        
        # Split:
        
        filter(species == portal_species[[i]]) %>% 
        pull(hindfoot_length) %>% 
        
        # Apply:
        
        mean(na.rm = TRUE)
    )
}

# Combine:

bind_rows(hindfeet_frame)

# for loops applied: split-apply-combine to data frames (values) ----------

# Specify input variable:

portal_species <-
  portal %>% 
  pull(species) %>% 
  unique()

# Create output container:

hindfeet <-
  vector(
    "list", 
    length = length(portal_species)
  ) %>% 
  set_names(portal_species)

# For loop sequence statement:

for(x in portal_species) {
  
  # For loop body:
  
  hindfeet_frame[[x]] <-
    tibble(
      species = x,
      hindfeet =
        portal %>% 
        
        # Split:
        
        filter(species == x) %>% 
        pull(hindfoot_length) %>% 
        
        # Apply:
        
        mean(na.rm = TRUE)
    )
}

# Combine:

bind_rows(hindfeet)

# map by index ------------------------------------------------------------

# Target vector:

portal_spp <-
  unique(portal$species)

map(
  
  # Sequence statement:
  
  1:length(portal_spp),
  
  # Body:
  
  function(i) {
    tibble(
      species = portal_spp[[i]],
      hindfeet =
        portal %>% 
        
        # Split the data:
        
        filter(species == portal$species[[i]]) %>% 
        pull(hindfoot_length) %>% 
        
        # Apply a function:
        
        mean(na.rm = TRUE)
    )
  }
) %>% 
  
  # Combine the output:
  
  bind_rows()

# map by values -----------------------------------------------------------

# Target vector: (and sequence statement!)

unique(portal$species) %>% 
  map(
    
    # Body:
    
    function(x) {
      tibble(
        species = x,
        hindfeet =
          portal %>% 
          
          # Split the data:
          
          filter(species == x) %>% 
          pull(hindfoot_length) %>% 
          
          # Apply a function:
          
          mean(na.rm = TRUE)
      )
    }
  ) %>% 
  
  # Combine the output:
  
  bind_rows()

# specifying your formula `\(x)` ------------------------------------------

# Target vector: (and sequence statement!)

unique(portal$species) %>% 
  map(
    
    # Body:
    
    \(x) {
      tibble(
        species = x,
        hindfeet =
          portal %>% 
          
          # Split the data:
          
          filter(species == x) %>% 
          pull(hindfoot_length) %>% 
          
          # Apply a function:
          
          mean(na.rm = TRUE)
      )
    }
  ) %>% 
  
  # Combine the output:
  
  bind_rows()

# specifying your formula `~` ---------------------------------------------

# Target vector: (and sequence statement!)

unique(portal$species) %>% 
  map(
    
    # Body:
    
    ~ tibble(
      species = .x,
      hindfeet =
        portal %>% 
        
        # Split the data:
        
        filter(species == .x) %>% 
        pull(hindfoot_length) %>% 
        
        # Apply a function:
        
        mean(na.rm = TRUE)
    )
  ) %>% 
  
  # Combine the output:
  
  bind_rows()
