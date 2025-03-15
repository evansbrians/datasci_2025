# Problem set 3 key: Global CO2 

# 2 -----------------------------------------------------------------------

# Attach the core tidyverse packages to your current R session:

library(tidyverse)

# 3 -----------------------------------------------------------------------

# The following code reads in world_bank_countries.csv and assigns the name
# countries to your global environment. Please modify this code such that it
# follows best management practices in code formatting.

# Given:

countries=read_csv("data/raw/world_bank_countries.csv")%>%select(!country_region)

# Answer:

countries <- 
  read_csv("data/raw/world_bank_countries.csv") %>% 
  select(!country_region)

# 4, given ----------------------------------------------------------------

# The code below reads in and wrangles the data successfully, but not
# parsimoniously. Modify the code such that the process below is completed in a
# chained analysis in which only the name associated with the final object,
# `world_pop`, is assigned to your global environment (i.e., the code block
# should not include intermediate assignments).

# Read in population data:

world_pop_raw <-
  read_csv(
    'data/raw/API_SP.POP.TOTL_DS2_en_csv_v2_2763937.csv', 
    skip = 3)

# Subset to columns of interest:

world_pop_column_subset <-
  select(
    world_pop_raw,
    
    # Rename Country Code column to remove space:
    
    country_code = `Country Code`,
    
    # Subset to other columns of interest:
    
    `1960`:`2020`)


# Make the data tidy by pivoting to a long form table:

world_pop_long <-
  pivot_longer(
    world_pop_column_subset,
    names_to = 'year',
    values_to = 'population',
    `1960`:`2020`)

# Finally, convert the year column to numeric:

world_pop <-
  mutate(
    world_pop_long,
    year = as.numeric(year))

# 4, answer ---------------------------------------------------------------

world_pop <-
  read_csv(
    'data/raw/API_SP.POP.TOTL_DS2_en_csv_v2_2763937.csv', 
    skip = 3) %>% 
  
  # Subset to columns of interest:
  
  select(
    
    # Rename Country Code column to remove space:
    
    country_code = `Country Code`,
    
    # Subset to other columns of interest:
    
    `1960`:`2020`) %>% 
  
  
  # Make the data tidy by pivoting to a long form table:
  
  pivot_longer(
    names_to = 'year',
    values_to = 'population',
    `1960`:`2020`) %>% 
  
  # Finally, convert the year column to numeric:
  
  mutate(
    year = as.numeric(year))    

# 5 -----------------------------------------------------------------------

# In a chained analysis with no intermediate assignments:

# * Read in the CO2 data (API_EN.ATM.CO2E.PC_DS2_en_csv_v2_2764620.csv)
# * Wrangle the data such that the resultant tibble contains (only) the 
#   variables `country_code`, `year`, and `co2` (as above)
# * Convert the class of the `year` variable to numeric
# * Globally assign the data frame to the name `co2`

co2 <-
  read_csv(
    'data/raw/API_EN.ATM.CO2E.PC_DS2_en_csv_v2_2764620.csv', 
    skip = 3) %>% 
  
  # Subset to columns of interest:
  
  select(
    
    # Rename Country Code column to remove space:
    
    country_code = `Country Code`,
    
    # Subset to other columns of interest:
    
    `1960`:`2020`) %>% 
  
  
  # Make the data tidy by pivoting to a long form table:
  
  pivot_longer(
    names_to = 'year',
    values_to = 'co2',
    `1960`:`2020`) %>% 
  
  # Finally, convert the year column to numeric:
  
  mutate(
    year = as.numeric(year))    

# 6 -----------------------------------------------------------------------

# Join the population and CO2 tibbles. Globally assign the resultant object to
# the name `population_co2`.

population_co2 <- 
  world_pop %>% 
  left_join(
    co2,
    by = c("country_code", "year"))

# Or:

population_co2 <- 
  world_pop %>% 
  inner_join(
    co2,
    by = c("country_code", "year"))

# 7 -----------------------------------------------------------------------

# Modify the data frame assigned to `populations_co2` such that `co2` values
# represent the *total* emissions (Note: the total emissions for a given country
# is the per capita emissions times the population) and the data frame is:

# * Subset to the year 2018
# * Arranged from the highest to lowest CO2 total emissions in 2018
# * Subset to only countries with records in the countries data frame
# * Subset to  the 5 countries with the most CO2 emissions (and also satisfy
#   the conditions above)
# * Subset to only the fields `country_code`, `country_name`, and `total_co2`
# * Globally assigned to the name `top_emitters_2018`

top_emitters_2018 <- 
  population_co2 %>%
  
  # Subset to 2018:
  
  filter(year == 2018) %>% 
  
  # Calculate the total co2 emissions, per country, in 2018:
  
  mutate(total_co2 = population * co2) %>% 
  
  # Subset to  columns of interest:
  
  select(country_code, total_co2) %>% 
  
  # Contains only countries with records in the countries data frame:
  
  inner_join(
    countries %>% 
      select(country_code, country_name), 
    by = "country_code") %>% 
  
  # Contains only the 5 countries with the most CO2 emissions (and also satisfy
  # the conditions above)
  
  slice_max(total_co2, n = 5)

# 8 -----------------------------------------------------------------------

# Using `population_co2` and `top_emitters_2018`, generate a ggplot that shows
# the per capita CO~2~ emissions from 1960 to 2018 for the five countries that
# had the highest total CO~2~ emissions in 2018. Plot the data such that:

# * The data being plotted are the per capita emissions from 1960-2018 for the
#   top five emitting countries in 2018
# * Evaluation of the plotting code does not produce any warning messages
# * Your x-aesthetic is year and is labeled “Year”
# * Your y-aesthetic is co_2 and is labeled “Carbon dioxide emissions (metric 
#   tons/person)”
# * Your color aesthetic is country_name and is labeled “Country”
# * Your plot includes point and line geometries
# * The x-axis ranges from 1960 to 2020
# * The y-axis ranges from 0 to 30
# * The plot includes a descriptive title
# * The background of the plot is white and contains no grid lines

population_co2 %>% 
  
  # Select variables of interest:
  
  select(country_code, year, co2) %>% 
  
  # Join the data frame from question 7:
  
  inner_join(top_emitters_2018, by = "country_code") %>% 
  
  # Remove NA values (to silence error messages)
  
  drop_na(co2) %>% 
  
  # Plotting:
  
  ggplot() +
  
  aes(
    x = year, 
    y = co2,
    color = country_name) +
  
  # Geometries:
  
  geom_point() +
  geom_line() +
  
  # Scale x-axis:
  
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(1960, 2020)) +
  
  # Scale y-axis:
  
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 30)) +
  
  # Labels:
  
  labs(
    title = "Global per capita carbon dioxide emissions for the top five emitters, 1960-2018",
    x = "Year",
    y = "Carbon dioxide emissions (metric tons/person)") +
  
  # Thematic elements:
  
  theme(
    panel.background = element_rect(fill = "white"))
