
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
  )

# tidy levels of observation ----------------------------------------------

# Site-level table:

cc_sites <- 
  arthropods %>% 
  select(
    site_name,
    site_description, 
    latitude:region
  ) %>% 
  distinct()

# Survey location table:

cc_survey_locations <-
  arthropods %>% 
  distinct(
    survey_location,
    site_name,
    plant
  )

# Surveys table:

cc_surveys <- 
  arthropods %>% 
  mutate(
    survey_id = 
      str_c(
        survey_location,
        year(date),
        yday(date),
        sep = "-"
      )
  ) %>%
  distinct(
    survey_id,
    survey_location,
    date,
    observation_method
  )

# Counts table:

cc_observations <-
  arthropods %>% 
  mutate(
    survey_id = 
      str_c(
        survey_location,
        year(date),
        yday(date),
        sep = "-"
      )
  ) %>%
  select(
    survey_id,
    herbivory_score,
    arthropod,
    arthropod_quantity
  )

# write to file -----------------------------------------------------------

list(
  sites = cc_sites,
  survey_locations = cc_survey_locations,
  surveys = cc_surveys,
  observations = cc_observations
) %>% 
  write_rds("data/raw/caterpillars_count.rds")
