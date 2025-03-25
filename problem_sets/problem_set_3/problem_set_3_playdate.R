
# what would we have them do? ---------------------------------------------

# 1. Name file

# 2. Load tidyverse

library(tidyverse)

# 3. Read in the data and send to the global environment

read_rds("caterpillars_count.rds") %>% 
  list2env(.GlobalEnv)

# 4. How many caterpillars has Caterpillars Count counted? Please provide your
# answer as a one-value numeric vector.

cc_observations %>% 
  filter(arthropod == "caterpillar") %>% 
  pull(arthropod_quantity) %>% 
  sum()

# 