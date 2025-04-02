
list.files(
  "data/dwca-invertebrados_pto_lobos-v1.3",
  pattern = "txt",
  full.names = TRUE
) %>% 
  set_names(
    str_c(
      c(
        "events",
        "measures",
        "occurrences"
      ),
      "_raw"
    )
  ) %>% 
  map(
    ~ read_delim(.x) %>% 
      janitor::clean_names()
  ) %>% 
  list2env(.GlobalEnv)

events <- 
  events_raw %>% 
  select(
    event_id, 
    datetime = event_date,
    longitude = decimal_longitude,
    latitude = decimal_latitude,
    min_depth_m = minimum_depth_in_meters,
    max_depth_m = maximum_depth_in_meters
  )

occurrences <-
  occurrences_raw %>% 
  select(
    occurrence_id = catalog_number,
    event_id,
    scientific_name,
    occurrence_status
  ) %>% 
  mutate(
    present = 
      if_else(
        occurrence_status == "present",
        1, 
        0
      ),
    .keep = "unused"
  )

temp <- 
  read_delim("data/dwca-argentina-secretariapesca-v1.5/occurrence.txt") %>%
  janitor::clean_names() %>% 
  select(
    occurrence_id,
    recorded_by,
    event_date,
    country,
    scientific_name
  )

temp %>% 
  filter(month(event_date) == 10)
  
  group_by(scientific_name) %>% 
  summarize(n = n())
