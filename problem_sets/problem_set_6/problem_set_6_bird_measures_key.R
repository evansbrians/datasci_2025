
# 2 -----------------------------------------------------------------------

# Attach the tidyverse core packages to your current R session:

library(tidyverse)

# 3 -----------------------------------------------------------------------

# The data file for this problem set is `dc_birds.rds`. This is a list file
# comprised of five separate tibbles, assigned to the names: `birds`, 
# `counts`, `measures`, `sites`, and `visits.` In a single, chained analysis:

# * Read in the list;
# * Subset to the list items `visits`, `birds`, and `measures`;
# * Globally assign each list item name to your global environment.

read_rds("data/raw/dc_birds.rds") %>% 
  tidyselect:::select(
    visits, 
    birds, 
    measures
  ) %>% 
  list2env(.GlobalEnv)

# 4 -----------------------------------------------------------------------

# In a chained analysis that utilizes `birds`, `visits`, and `measures`:

# * Combine the data frames and subset the variables such that the resultant
#   variables are `bird_id`, `common_name`, `site_id`, `date`, `age`, `sex`,
#   `wing`, and `mass` (in that order).
# * Globally assign the name `banding` to the resultant object.

# Subset and modify the measures frame:

banding <- 
  measures %>% 
  
  # Join with birds to get `common_name` column:
  
  left_join(
    birds %>% 
      select(
        spp = species, 
        common_name
      ), 
    by = "spp"
  ) %>% 
  
  # Join visits:
  
  left_join(visits, by = "visit_id") %>% 
  
  # Subset variables:
  
  select(
    bird_id,
    common_name,
    site_id:date,
    age:mass
  )

# In a separate code block from the above, remove the names `birds`,
# `measures`, and `visits` from your global environment.

rm(
  birds, 
  measures, 
  visits)

# 5 -----------------------------------------------------------------------

# Modify `banding` as parsimoniously as possible:

# * Subset to species (`common_name`) with more than 100 observations;
# * Using `if_all()` or `if_any()`, subset to observations where the `sex` 
#   and `age` of the bird is known (see variable descriptions above)
# * Subset to observations in which `wing` measurements are greater 40 mm;
# * Subset to observations in which `mass` measurements are greater 7 g;
# * Modify the `age` variable, such that the age classes are a factor variable 
#   with the levels `juvenile` and `adult` (in that order; see variable 
#   descriptions above);
# * Globally assign the name `banding_subset` to the resultant object.

banding_subset <-
  banding %>% 
  
  # Subset to species captured more than 100 times
  
  filter(
    n() > 100, 
    .by = common_name
  ) %>% 
  
  # Subset sex, age, wing, and mass by provided criteria:
  
  filter(
    if_all(
      sex:age,
      ~ .x != "U"),
    wing > 40,
    mass > 7
  ) %>% 
  
  # Set age as a factor with the levels Juvenile and Adult (in that order):
  
  mutate(
    age = 
      age %>% 
      fct_collapse(
        Juvenile = "HY",
        other_level = "Adult"
      ) %>% 
      fct_relevel(
        c("Juvenile", "Adult")
      )
  )

# In a separate code block from the above, remove the name `banding` from 
# your global environment:

rm(banding)

# 6 -----------------------------------------------------------------------

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

# Convert the above to a custom function that can be used to calculate the
# mean `mass` or `wing` of any species in `banding_subset`. Globally assign
# your function to the name `mean_species_var`:

mean_species_var <-
  function(species, variable) {
    banding_subset %>% 
      filter(common_name == species) %>% 
      pull(variable) %>% 
      mean()
  }

# Extra credit version (`mass` or `wing` does not have to be quoted):

mean_species_var <-
  function(species, variable) {
    banding_subset %>% 
      filter(common_name == species) %>% 
      pull({{ variable}}) %>% 
      mean()
  }

# 7 -----------------------------------------------------------------------

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
    length = length(species)
  )

# Iterate across species with a for loop:

for(i in seq_along(species)) {
  out_container[[i]] <-
    banding_subset %>% 
    filter(common_name == species[[i]]) %>% 
    mutate(
      mean_mass = mean(mass)
    ) %>% 
    distinct(common_name, mean_mass)
}

# Combine list items, by row, into a tibble:

bind_rows(out_container)

# Without generating any new global assignments, nor using any of the
# global assignments in the for loop example above (except `banding_subset`),
# replicate the results above using `purrr::map()`:

banding_subset %>% 
  pull(common_name) %>% 
  unique() %>% 
  sort() %>% 
  purrr::map(
    ~ banding_subset %>% 
      filter(common_name == .x) %>% 
      mutate(
        mean_mass = mean(mass)
      ) %>% 
      distinct(common_name, mean_mass)
  ) %>% 
  bind_rows()

# Or:

banding_subset %>% 
  pull(common_name) %>% 
  unique() %>% 
  sort() %>% 
  purrr::map(
    function(x) {
      banding_subset %>% 
        filter(common_name == x) %>% 
        mutate(
          mean_mass = mean(mass)
        ) %>% 
        distinct(common_name, mean_mass)
    }
  ) %>% 
  bind_rows()

# Or:

banding_subset %>% 
  pull(common_name) %>% 
  unique() %>% 
  sort() %>% 
  purrr::map(
    \(x) {
      banding_subset %>% 
        filter(common_name == x) %>% 
        mutate(
          mean_mass = mean(mass)
        ) %>% 
        distinct(common_name, mean_mass)
    }
  ) %>% 
  bind_rows()

# Or:

banding_subset %>% 
  pull(common_name) %>% 
  unique() %>% 
  sort() %>% 
  purrr::map(
    \(x) {
      banding_subset %>% 
        filter(common_name == x) %>% 
        mutate(
          mean_mass = mean(mass)
        )
    }
  ) %>% 
  bind_rows() %>% 
  distinct(common_name, mean_mass)

# Note: This tests your understanding of purrr map functions ... in the
# real world I would obviously use `summarize()` for this!

# 8 -----------------------------------------------------------------------

# One of the hats that I wear is "ornithologist" so we end up working with 
# data about birds *a lot*. In a single chained analysis:

# * As parsimoniously as possible, use *conservative* regex to provide a
#   character vector of all files that starts with "bird" and ends with "rds"
#   *or* ends with "birds.rds" (*Note: Please use a character escape for the
#   "." symbol*);
# * Use a `map()` function to read in all of the files at once;
# * Assign a name (of your choosing) to each list item;
# * Assign each list item name to the global environment.

# Generate a character vector of the files that match the provided pattern:

list.files(
  "data/raw", 
  pattern = "bird.*rds$",
  full.names = TRUE
) %>% 
  
  # Read in the files:
  
  map(
    ~ read_rds(.x)
  ) %>% 
  
  # Assign names to each list item (names may vary depending on the files
  # you have):
  
  set_names(
    "birds_cicadas_lc",
    "dc_birds",
    "district_birds",
    "messy_birds"
  ) %>% 
  
  # Assign each list item to the global environment:
  
  list2env(.GlobalEnv)

