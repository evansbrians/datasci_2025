# assignment 6
# Hector Zumbado-Ulate

# 2 ---------------------------------------------------------

# Attach the tidyverse core packages to your current R session:

library(tidyverse)

# 3 ---------------------------------------------------------

# * Read in the list;
# * Subset to the list items `visits`, `birds`, and `measures`;
# * Globally assign each list item name to your global environment.

read_rds('data/raw/dc_birds.rds') %>% 
  tidyselect:::select(
    visits, 
    birds, 
    measures) %>% 
  list2env(.GlobalEnv)

# 4 ----------------------------------------------------------------

# In a chained analysis that utilizes `birds`, `visits`, and `measures`:

# * Combine the data frames and subset the variables such that the resultant
#   variables are `bird_id`, `common_name`, `site_id`, `date`, `age`, `sex`,
#   `wing`, and `mass` (in that order).
# * Globally assign the name `banding` to the resultant object.


banding <- 
  visits %>% 
  left_join(
    measures,
    by = "visit_id") %>% 
  left_join(
    birds,
    by = c("spp" = "species")) %>% 
  select(
    bird_id, 
    common_name,
    site_id:date,
    age:mass)
  
# remove the names `birds`,measures`, and `visits`

rm(birds, measures, visits)

# 5 -------------------------------------------------------------

banding_subset <- 
  
  # Subset to species (`common_name`) with n > 100 obs
  
  banding %>% 
  filter(
    n() > 100,
    .by = common_name, 
    
    # if_all or if_any to subset where `sex` and `age` is known 
    
    if_all(
      age:sex,
    ~ .x != "U"),
    
    # Subset to `wing` > 40 mm, `mass` > greater 7 g;
    wing > 40,
    mass > 7) %>% 
  
  # Modify `age` such that age classes are a factor variable 
  # levels = `juvenile` and `adult` (in that order)
  
  mutate(
    age = 
      age %>% 
      fct_collapse(
        juvenile = "HY",
        adult = c("AHY", "ASY", "SY")))

rm(banding) 

# 6 -------------------------------------------------------------

# The code block below:

# * Subsets `banding_subset` by row to the species (`common_name`) "Northern 
#   Cardinal";
# * Extracts the numeric vector assigned to `mass`;
# * Calculates the mean of the vector
# * Prints a one-value numeric vector.

banding_subset %>% 
  filter(common_name == "Northern Cardinal") %>% 
  pull(mass) %>% 
  mean()

# Convert the above to a custom function that can be used to calculate the mean `mass` or `wing` of any species in `banding_subset`. 

mean_species_var <- 
  function(species, measure) {
    banding_subset %>% 
      filter(species == common_name) %>% 
          pull(measure) %>% 
                 mean()
    }

mean_species_var(
  "Northern Cardinal",
  "mass")

# Extra credit version (`mass` or `wing` does not have to be quoted):

mean_species_var <- 
  function(species, measure) {
    banding_subset %>% 
      filter(species == common_name) %>% 
      pull({{ measure }}) %>% 
      mean()
  }

mean_species_var(
  "Northern Cardinal",
  mass)


# 7 -------------------------------------------------------------------

# The following for loop calculates the average mass of each species in
# `banding_subset` and returns a tibble:

# Generate a unique vector of species, arranged alphabetically:

species <-
  banding_subset %>% 
  pull(common_name) %>% 
  unique() %>% 
  sort()

# Create an output container to store the results of the for loop:

out_container <-
  vector(
    "list",
    length = length(species))

# Iterate across species with a for loop:

for(i in seq_along(species)) {
  out_container[[i]] <-
    banding_subset %>% 
    filter(common_name == species[[i]]) %>% 
    mutate(mean_mass = mean(mass)) %>% 
    distinct(common_name, mean_mass)
}

# Combine list items, by row, into a tibble:

bind_rows(out_container)

# Without generating any new global assignments, nor using any of the
# global assignments in the for loop example above (except `banding_subset`),
# replicate the results above using `purrr::map()`:

unique(banding_subset$common_name) %>% 
  purrr:::map(
    function(x) {
      tibble(
        common_name = x,
        mean_mass = 
          banding_subset %>% 
          filter(common_name == x) %>% 
          pull(mass) %>% 
          mean())
      }) %>% 
  bind_rows() %>% 
  arrange(common_name)

# Note: This tests your understanding of purrr map functions ... in the
# real world I would obviously use `summarize()` for this!

# 8 -----------------------------------------------------------------------

# * As parsimoniously as possible, use regex to provide a character vector
#   of all files that contain “bird” and ends with “rds”;
# * Use a `map()` function to read in all of the files at once;
# * Assign a name (of your choosing) to each list item;
# * Assign each list item name to the global environment.

# Generate a character vector of the files that match the provided pattern:

list.files(
  "data/raw", 
  pattern = "bird(s)?.*\\.rds$",
  full.names = TRUE) %>% 
  purrr::map(
    ~ .x %>% 
    read_rds()) %>% 
  set_names(c(
    "bird_cicadas",
    "dc_birds",
    "district_birds",
    "messy_birds")) %>%
  list2env(envir = .GlobalEnv)
