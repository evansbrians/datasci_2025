
# setup -------------------------------------------------------------------

library(sf)
library(tidyverse)

iris_tbl <-
  read_rds('data/raw/iris.rds')

# split-apply-combine, Rube Goldberg method -------------------------------

# Now you! Subset iris_tbl to observations of Iris setosa:

iris_tbl



# Now you! Write a custom function that will calculate the mean sepal length of
# any of the iris species:



# Now you! Use your custom function to generate a list of mean sepal lengths for
# the three iris species:




# introducing purrr! ------------------------------------------------------




# A simpler way ....

iris_tbl