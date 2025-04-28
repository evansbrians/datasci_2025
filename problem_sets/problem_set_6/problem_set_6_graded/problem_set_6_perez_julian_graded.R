
# Script file for problem set 6: Iteration with district birds

# 1 -----------------------------------------------------------------------

# Before opening your script file for this problem set, change the name of
# `problem_set_6.R` to "problem_set_6_[last name]_[first name].R" using a snake
# case naming convention. *Note: You will submit this script file as your
# assignment*.

# done

# 2 -----------------------------------------------------------------------

# Open the script file in RStudio and attach the core tidyverse packages to your
# current R session.

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
    c(
      "visits",
      "birds",
      "measures"
    )
  ) %>% 
  list2env(envir = .GlobalEnv)

# [[-0.05]] Code parsimony: With `tidyselect:::select()`, neither the quotes
# nor `c()` and necessary.

# 4 -----------------------------------------------------------------------

# In a chained analysis that utilizes `birds`, `visits`, and `measures`:

# * Combine the data frames and subset the variables such that the resultant
#   variables are `bird_id`, `common_name`, `site_id`, `date`, `age`, `sex`,
#   `wing`, and `mass` (in that order).
# * Globally assign the name `banding` to the resultant object.

banding <- 
  measures %>%
  left_join(
    birds,
    by = c("spp" = "species")
  ) %>%
  left_join(
    visits,
    by = "visit_id"
  ) %>%
  select(
    bird_id,
    common_name,
    site_id,
    date,
    age:mass
  )

# In a separate code block from the above, remove the names `birds`,
# `measures`, and `visits` from your global environment.

rm(birds, visits, measures)

# [[-0.125]] Code formatting: If you provide three or more arguments to a
# function, place each argument on its own line.

# 5 -----------------------------------------------------------------------

# Modify `banding` as parsimoniously as possible:

# * Subset to species (`common_name`) with more than 100 observations;
# * Using `if_all()` or `if_any()`, subset to observations where the `sex` 
#   and `age` of the bird is known (see variable descriptions above);
# * Subset to observations in which `wing` measurements are greater 40 mm;
# * Subset to observations in which `mass` measurements are greater 7 g;
# * Modify the `age` variable, such that the age classes are a factor variable 
#   with the levels `juvenile` and `adult` (in that order; see variable 
#   descriptions above);
# * Globally assign the name `banding_subset` to the resultant object.

banding_subset <-
  banding %>% 
  
  # counting birds with more than 100 observations
  
  count(common_name) %>% 
  arrange(
    desc(n)
  ) %>%
  
  # filtering 
  
  filter(n > 100) %>% 
  pull(common_name) %>% 
  
  # now the other filters 
  
  map(
    ~ banding %>%
        filter(
          common_name == .x,
          if_all(
            c(age,
              sex
            ),
            ~ . != "U"
          ),
          wing > 40,
          mass > 7
        )
  ) %>%
  bind_rows() %>% 
  
  # modify age variable
  
  mutate(
    age =
      age %>% 
      fct_collapse(
        juvenile = "HY",
        adult = 
          c(
            "AHY",
            "SY",
            "ASY"
          )
      ) %>% 
      fct_relevel(
        "juvenile",
        "adult"
      )
  ) 

# In a separate code block from the above, remove the name `banding` from 
# your global environment:

rm(banding)

# [[-0.75]] `count` is not among the functions that you may use for this
# assignment.

# [[-0.15]] Code parsimony: This was an interesting answer that says you are
# really exploring these functions. That being said, it was *way* to complex
# and required double the memory and four times the processing of the answer
# in the key. We get why you did it this way though -- it is a nice 
# translation from a base R for loop solution to a tidyverse one. The trick
# is that working within the tibbles rather than explicitly looping is almost
# always faster and more memory efficient.

# [[-0.15]] Code formatting: If a function spans more than one line of code, the
# opening parentheses should be followed by a line break.

# 6 -----------------------------------------------------------------------

# The code block below:

# * Subsets `banding_subset` by row to the species (`common_name`) "Northern 
#   Cardinal";
# * Extracts the numeric vector assigned to `mass`;
# * Calculates the mean of the vector;
# * Prints a one-value numeric vector.

banding_subset %>% 
  filter(common_name == "Northern Cardinal") %>% 
  pull(mass) %>% 
  mean()

# Convert the above to a custom function that can be used to calculate the
# mean `mass` **or** `wing` of any species in `banding_subset`. Globally 
# assign your function to the name `mean_species_var`:

mean_species_var <- 
  function(species, variable){
    banding_subset %>%
      filter(common_name == species) %>%
      pull(variable) %>%
      mean()
  }

# test 

mean_species_var("Northern Cardinal", "mass")
mean_species_var("Northern Cardinal", "wing")

# Extra credit version (`mass` or `wing` does not have to be quoted):

mean_species_var <- 
  function(
    species,
    variable
    ){
  banding_subset %>%
    filter(common_name == species) %>%
    pull({{variable}}) %>%
    mean()
}

# test 

mean_species_var(
  "Northern Cardinal",
  mass
  )

mean_species_var(
  "Northern Cardinal",
  wing
  )

# [[-0.10]] Code formatting: Indentation: Closing parentheses should be indented
# to the same level as the start of the function.

# [[+0.25]] Extra credit: Fantastic!

# 7 -----------------------------------------------------------------------

# Create a custom function that arranges *any* data frame in ascending *or*
# descending order by *any* variable in that object.

# * The function should include the following arguments:
#   * The data being sorted
#   * The variable being sorted
#   * The direction of arrangement (i.e., ascending or descending) (Hint: See 
#     Lesson 6.3 Control flow if else)
# * The default behavior of the function should arrange the data in ascending 
#   order.
# * Assign the function to the name `my_arrange`

my_arrange <- 
  function(
    data,
    var,
    direction = "asc"
    ){
  if(direction == "asc"){
    data %>% 
      arrange({{var}})
  } else {
    data %>% 
      arrange(
        desc({{var}})
        )
  }
}

# Test the function in ascending order with the `common_name` variable in
# `banding_subset`:

my_arrange(
  banding_subset,
  common_name,
  "asc"
  )

# Test the function in descending order with the `common_name` variable in
# `banding_subset`:

my_arrange(
  banding_subset,
  ommon_name,
  "desc"
)

# Test the function in ascending order with the `wing` variable in
# `banding_subset`:

my_arrange(
  banding_subset,
  wing,
  "asc"
)

# Test the function in descending order with the `wing` variable in
# `banding_subset`:

my_arrange(banding_subset,
           wing,
           "desc"
)

# [[-0.15]] Code formatting:
# * Maintain one blank line between code blocks and comments. In your version
#   there were additional spaces prior to the section header.
# * If a function spans more than one line of code, the opening parentheses 
#   should be followed by a line break.

# 8 -----------------------------------------------------------------------

# The following for loop calculates the average mass of each species in
# `banding_subset` and returns a tibble:

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
    filter(
      common_name == species[[i]]
    ) %>% 
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
  map(~ banding_subset %>%
        filter(common_name == .x) %>%
        mutate(
          mean_mass =
            mean(mass)
        ) %>%
        distinct(
          common_name,
          mean_mass
        )
  ) %>%
  bind_rows()

# Note: This tests your understanding of purrr map functions ... in the
# real world I would obviously use `summarize()` for this!

# [[-0.10]] Code formatting: If a function spans more than one line of code, the
# opening parentheses should be followed by a line break.

# 9 -----------------------------------------------------------------------

# Any function that follows `group_by()` or includes the argument .`by = ...` 
# is fundamentally an iteration. For example, the following code block subsets
# `banding_subset` to records associated with the maximum wing length observed
# for each species:

banding_subset %>% 
  filter(
    wing == max(wing),
    .by = common_name
  )

# Use `purrr::map()` to repeat the operation above without the use of
# `group_by()` or `.by = ....`:

banding_subset %>%
  pull(common_name) %>%
  unique() %>%
  map(
    ~ banding_subset %>%
      filter(common_name == .x) %>%
      filter(
        wing == 
          max(
            wing,
            na.rm = TRUE
          )
      )
  ) %>%
  bind_rows()

# [[-0.125]] Code parsimony: `na.rm = TRUE` was not necessary because there
# were no NA values in the wing measurements of `banding_subset`. They were
# removed in Question 5.

# 10 ----------------------------------------------------------------------

# One of the hats that I wear is "ornithologist" so we end up working with 
# data about birds *a lot*. In a single chained analysis:

# * As parsimoniously as possible, use regex to provide a character vector 
#   of all files that starts with "bird" and ends with "rds" *or* ends with 
#   "birds.rds";
# * Use a `map()` function to read in all of the files at once;
# * Assign a name (of your choosing) to each list item;
# * Assign each list item name to the global environment.

list.files(pattern = "^bird.*rds$|birds\\.rds$") %>%
  set_names() %>%
  map(read_rds) %>%
  list2env(.GlobalEnv)

# [[-0.75]] Incorrect: This did not search in the `data/raw` folder and you
# need to include the argument `full.names = TRUE` to read in the data.

# [[-0.15]] Code parsimony: Regex could be simplified.
