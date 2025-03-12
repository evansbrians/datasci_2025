
# setup -------------------------------------------------------------------

library(lubridate)
library(tidyverse)

# Load iNaturalist observations of spotted lanternflies:

spotted_lanternfly <-
  read_rds('data/raw/spotted_lanternfly.rds')

# group and summarize data by one variable --------------------------------

# Quality grade:

spotted_lanternfly

# Which states have the most spotted lanternfly observations?

spotted_lanternfly

# Now you! Which users have submitted the most spotted lanternfly 
# observations?

spotted_lanternfly

# Group and summarize data by year and month ------------------------------

# How many observations per year?

spotted_lanternfly

# How many states had observations per year?



# How many observations per month?



# How many observations by month and year?



# Now you! Generate a plot of the number of observers by year and month:

spotted_lanternfly

# other summary functions -------------------------------------------------

# Latitudinal range expansion, northern boundary:

spotted_lanternfly %>% 
  group_by(year = year(datetime)) %>% 
  summarize(latitude = max(latitude)) %>% 
  ggplot(aes(x = year, y = latitude)) +
  geom_point() +
  geom_line() +
  theme_bw()

# Now you! Generate a plot that shows the western limits of the spotted
# lanternfly observations for each year:

spotted_lanternfly

# multiple summaries ------------------------------------------------------

# Latitudinal range expansion, combined:

spotted_lanternfly %>% 
  group_by(year = year(datetime)) %>% 
  summarize(
    southern = min(latitude),
    northern = max(latitude))
  
  ggplot(
    aes(x = year,
        y = latitude,
        color = limits)) +
  geom_point() +
  geom_line() +
  theme_bw()

# Now you! Has the range of spotted lanternfly expanded longitudinally?



# group and summarize data by multiple variables --------------------------

spotted_lanternfly %>% 
  group_by(
    year = year(datetime),
    state) %>% 
  summarize(n = n()) %>% 
  ggplot(
    aes(x = year,
        y = n)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~state) +
  theme_bw()





