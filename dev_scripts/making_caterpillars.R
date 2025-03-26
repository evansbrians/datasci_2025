
# Tidying Caterpillars Count for problem set 3

# setup -------------------------------------------------------------------

library(tidyverse)

# Read in and pre-process the data:

arthropods <-
  read_csv("CaterpillarsCountData_timestamp-1742904029_uniqueid-b2CNa/CaterpillarsCountData.csv") %>% 
  janitor::clean_names() %>% 
  select(
    date = local_date,
    site_name:site_description,
    latitude:longitude,
    region,
    survey_location = survey_location_code,
    plant = official_plant_species,
    observation_method,
    herbivory_score,
    arthropod = updated_arthropod_group,
    arthropod_quantity
  ) %>% 
  mutate(
    arthropod_quantity = replace_na(arthropod_quantity, 0)
  ) %>% 
  distinct()

# tidy levels of observation ----------------------------------------------

# Site-level table:

cc_sites <- 
  arthropods %>% 
  select(
    site_name:site_description,
    latitude:region
  ) %>% 
  distinct() %>% 
  mutate(
    site_id =
      stringi::stri_rand_strings(
        n = nrow(.),
        length = 7
      ),
    .before = site_name
  )

# Survey location table:

cc_survey_locations <-
  arthropods %>% 
  distinct(
    survey_location,
    site_name,
    plant
  ) %>% 
  left_join(
    cc_sites %>% 
      select(site_id, site_name),
    by = "site_name"
  ) %>% 
  select(
    branch_id = survey_location,
    site_id,
    plant
  )

# Surveys table:

cc_surveys <-
  arthropods %>% 
  mutate(
    survey_id_temp = 
      str_c(
        survey_location,
        year(date),
        yday(date),
        sep = "-"
      ),
    survey_id_temp =
      if_else(
        str_detect(observation_method, "Visual"),
        str_c(survey_id_temp, "v"),
        str_c(survey_id_temp, "b")
      )
  ) %>%
  distinct(
    survey_id_temp,
    branch_id = survey_location,
    date,
    observation_method
  ) %>% 
  mutate(
    survey_id = 
      stringi::stri_rand_strings(
        n = nrow(.),
        length = 7
      ),
    .before = survey_id_temp
  )

# Counts table:

observations <-
  arthropods %>% 
  mutate(
    survey_id_temp = 
      str_c(
        survey_location,
        year(date),
        yday(date),
        sep = "-"
      ),
    survey_id_temp =
      if_else(
        str_detect(observation_method, "Visual"),
        str_c(survey_id_temp, "v"),
        str_c(survey_id_temp, "b")
      )
  ) %>%
  select(
    survey_id_temp,
    arthropod,
    arthropod_quantity
  ) %>% 
  summarize(
    arthropod_quantity = sum(arthropod_quantity),
    .by =
      c(
        survey_id_temp,
        arthropod
      )
  ) %>% 
  distinct() %>% 
  left_join(
    cc_surveys %>% 
      select(survey_id, survey_id_temp),
    by = "survey_id_temp"
  ) %>% 
  select(
    survey_id,
    everything()
  ) %>% 
  select(!survey_id_temp)

# back up the line --------------------------------------------------------

# All of the below were actually good, but just required a double-check

surveys <- 
  cc_surveys %>% 
  select(!survey_id_temp) %>% 
  semi_join(observations, by = "survey_id") %>% 
  arrange(date, branch_id)

survey_locations <- 
  cc_survey_locations %>% 
  semi_join(surveys, by = "branch_id")

sites <- 
  cc_sites %>% 
  semi_join(survey_locations, by = "site_id")


# write to files ----------------------------------------------------------

my_list <-
  list(
    sites = sites,
    survey_locations = survey_locations,
    surveys = surveys,
    observations = observations
  )

my_list %>% 
  write_rds("data/raw/caterpillars_count.rds")

my_list %>% 
  write_rds("problem_sets/problem_set_3/caterpillars_count.rds")
