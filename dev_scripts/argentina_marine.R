temp <- 
  read_delim("data/dwca-argentina-cenpat-fishes-v1.12/occurrence.txt") %>% 
  janitor::clean_names() %>%
  unite(
    "date",
    year:day,
    sep = "-") %>% #names()
  select(
    date,
    longitude = decimal_longitude,
    latitude = decimal_latitude,
    scientific_name
  ) %>% 
  mutate(date = as_date(date)) %>% 
  drop_na(date)

temp %>% 
  filter(
    month(date) == 8
  )


read_delim("data/dwca-invertebrados_pto_lobos-v1.3/occurrence.txt")
