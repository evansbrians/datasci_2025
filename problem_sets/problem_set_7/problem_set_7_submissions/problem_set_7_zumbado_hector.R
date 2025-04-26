# problem 7
# Hector Zumbado-Ulate

# 2 --------------------------------

library(tidyverse)

# 3 -----------------------------------

read_rds("data/raw/problem_set_7_data.rds") %>% 
  list2env(.GlobalEnv)

# 4. backwards columns -----------------------

backwards_columns %>% 
  mutate(
    negative =
      if_else(a > 0, b, a),
    positive = 
      if_else(b < 0, a, b)) %>%
  select(
    a = "negative", 
    b = "positive")

# Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
  pluck("question_4")

# 5. regex-cellence -------------------------------

# Fix to "House finch", "House sparrow", and "House wren"

bad_birds %>%  
  str_replace(
    "[Hh]((0|o)u)?se? [Ss]pa?rr?o?w", "House sparrow") %>% 
  str_replace(
    "[Hh](ou)?se? [Ww]?re?n", "House wren") %>%
  str_replace(
    "[Hh](ou?)?se? [Ff]i?nch", "House finch")

# Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
  pluck("question_5")

# 6-7. anti and semi join ------------------

matching_list_tables %>% 
  pluck("boy") %>% 
  semi_join(
    matching_list_tables %>% 
      pluck("howdy"),
    by = c("heroes" = "hello"))

# Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
  pluck("question_6")


matching_list_tables %>% 
  pluck("boy") %>% 
  anti_join(
    matching_list_tables %>% 
      pluck("howdy"),
    by = c("heroes" = "hello"))

# Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
  pluck("question_7")

# 8. butterflies and moths ------------

######### common name is not part of the observation #########
######### It violates the 2nd Normal Rule ######### 

# Subset to the 3 more observed species

leps_dc %>%
  summarize(
    n = n(),
    .by = scientific_name) %>% 
  slice_max(n = 3, order_by = n) %>%
  pull(scientific_name)

common_leps <- 
  leps_dc %>% 
  filter(
    scientific_name %in% c(
      "Atalopedes campestris", 
      "Danaus plexippus",      
      "Papilio glaucus"))

# Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
  pluck("question_8")

# Extra credit 1. 

  # first iteration creates 3 lists
  
c(
  "Atalopedes campestris", 
  "Danaus plexippus",      
  "Papilio glaucus") %>% 
  map(
    \(x) {
      common_leps %>%  
        mutate(
          month = month(date, label = TRUE)) %>% 
        summarize(
          count = n(),
          .by = month)}) %>% 
  
  # second iteration to graph and add title
  
  map2(
    c(
      "Atalopedes campestris", 
      "Danaus plexippus",      
      "Papilio glaucus"),
    ~ ggplot(
      data = .x,
      aes(
        x = month, 
        y = count)) + 
      geom_bar(
        fill = "#7889EF",
        color = "black",
        stat = "identity") +
      scale_y_continuous(
        expand = c(0,0)) +
      scale_x_discrete(
        expand = c(0,0)) +
      labs(
        title = .y,
        x = "Month",
        y = "Butterflies observed") +
      theme_classic())  

# 9. seems like old times -------------------

# Convert date to ISO-8601

dates_and_times %>% 
  mutate(
    date = mdy(date)) %>%
  
  # Convert time to ISO-8601
  
  mutate(
    across(
      time1:time2,
      ~ str_c(date, .x, sep = " "))) %>%
  
  mutate(
    across(
      time1:time2,
      ymd_hms)) %>% 
  
  # Subset where t2 > t1
  
  filter(time2 > time1) %>% 
  
  # Replace t1 & t2 with the hour assoc with t
  
  mutate(
    across(
      time1:time2, 
      hour))
    
# Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
  pluck("question_9")

# casing the joint -------------------------------------

size_and_volume %>% 
  mutate(
    species = 
      case_when(
        size < 10 & volume <= 30 ~ "Deer mouse",
        size < 10 & volume > 30 ~ "House wren",
        size >= 10 & volume <= 30 ~ "Three-toed sloth",
        TRUE ~ "Howler monkey"))

# Hint: Your resultant object should be equivalent to:

read_rds("data/processed/problem_set_7_lifelines.rds") %>% 
  pluck("question_10")

# Extra credit 2. bar plot # of observations of each spp

# create new column

size_and_volume %>% 
  mutate(
    species = 
      case_when(
        size < 10 & volume <= 30 ~ "Deer mouse",
        size < 10 & volume > 30 ~ "House wren",
        size >= 10 & volume <= 30 ~ "Three-toed sloth",
        TRUE ~ "Howler monkey")) %>%
  
  # summarize counts per species
  
  summarize(
    count = n(),
    .by = species) %>% 
  
  # initiate ggplot
  
  ggplot() +
  
  # add geometry and settings
  
  aes(
    x = fct_reorder(species, count), 
    y = count) +
  geom_bar(
    fill = "#788238",
    color = "black",
    stat = "identity") +
  coord_flip() +
  labs(
    x = "Species",
    y = "Total of observations") +
  scale_y_continuous(
    limits = c(0, 20),
    expand = c(0,0)) +
  scale_x_discrete(
    expand = c(0,0)) +
  theme_classic()
