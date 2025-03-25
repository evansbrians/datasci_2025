
# what would we have them do? ---------------------------------------------

# 1. Name file

# 2. Load tidyverse

library(tidyverse)

# 3. Read in the data and send to the global environment

read_rds("data/raw/caterpillars_count.rds") %>% 
  list2env(.GlobalEnv)

# 4. How many caterpillars has Caterpillars Count counted? Please provide your
# answer as a one-value numeric vector.

observations %>% 
  filter(arthropod == "caterpillar") %>% 
  pull(arthropod_quantity) %>% 
  sum()

# 5. Please make a summary data frame of the number of sites in the District of
# Columbia, Maryland, and Virginia and arrange the table alphabetically by 
# region:

sites %>% 
  filter(
    region %in%
      c(
        "DC",
        "MD", 
        "VA"
      )
  ) %>% 
  summarize(
    n = n(),
    .by = region
  ) %>% 
  arrange(region)



observations %>% 
  filter(arthropod == "caterpillar")

observations %>% 
  filter(arthropod == "caterpillar") %>% 
  distinct(survey_id)
summarize(
  n_caterpillar = n(),
  .by = survey_id
) %>% summary()
