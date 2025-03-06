# Reading, exploring, and writing data
# Author: Brian S. Evans
# Date created: 2023-03-06
# Last modified on 2023-03-06

# In this video lesson, we will explore:
# - How to read data into R
# - Some initial data exploration steps
# - How to save files to your computer

# setup -------------------------------------------------------------------

# Load all libraries that will be used more than once:

library(tidyverse)

# List files in a directory:

list.files()

# reading data ------------------------------------------------------------

# Read in a vector with read_lines():

read_lines("data/raw/four_instruments.txt")

# Read in tabular csv files with read_csv():

read_csv("data/raw/iris.csv")

# Table troubles ...

read_csv("data/raw/API_EN.ATM.CO2E.PC_DS2_en_csv_v2_2764620.csv")

# The RDS file:

read_rds("data/raw/iris.rds")

read_rds('data/raw/four_instruments.rds')

# Read an Excel file with the readxl package function read_xlsx:

readxl::excel_sheets('data/raw/portal.xlsx')

readxl::read_excel("data/raw/portal.xlsx")

# Now you! Modify the code above such that the column names are Plot, Date
# collected, Family, Genus Species, and Weight.

# Now you! Modify the code above such that Date collected is read in as a date
# column.

# Now you! Modify the code above such that the column names are plot, date,
# family, species, and weight.

