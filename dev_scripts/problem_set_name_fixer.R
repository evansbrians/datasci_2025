

# setup -------------------------------------------------------------------

library(tidyverse)

problem_set <- 1

# Problem set parent folder:

problem_set_parent <-
  file.path(
    "problem_sets",
    str_c("problem_set_", problem_set)
  )

# Assign a read directory (so I only have to change things once per problem 
# set):

read_dir <-
  file.path(
    problem_set_parent,
    str_c(
      "problem_set_",
      problem_set,
      "_submissions"
    )
  )

# Unzip the files (Blackboard downloads as a zip file):

read_dir %>% 
  str_c(".zip") %>% 
  unzip(exdir = read_dir)

# Remove files that are not R scripts:

list.files(path = read_dir) %>% 
  str_subset(pattern = "txt$") %>% 
  # str_subset(pattern = "^(?!.*R).*$") %>% 
  map(
    ~ file.remove(
      file.path(read_dir, .x)
    )
  )

# rename files in directory -----------------------------------------------

# The names assigned by Blackboard are terrible! This code assigns reasonable
# names (the file names the students are expected to provide).

read_dir %>% 
  
  # List R Markdown files in folder:
  
  list.files(full.names = TRUE) %>% 
  
  # Iterate across all R Markdown files in the vector:
  
  purrr::map(
    
    # Rename the file:
    
    ~ file.rename(
      .x,
      str_replace(
        .x,
        str_c(
          "(", 
          read_dir,
          "/)(.*-[0-9]{2}_)(.*)"
        ),
        '\\1\\3'
      )
    )
  )

# make graded files -------------------------------------------------------

# Generate a name a graded files folder:

graded_files_folder <-
  problem_set_parent %>% 
  file.path(
    str_c(
      "problem_set_",
      problem_set,
      "_graded"
    )
  )

# Create folder for graded files:

dir.create(graded_files_folder)

# Move files to the graded files folder

list.files(read_dir, full.names = TRUE) %>% 
  map(
    ~ file.copy(.x, graded_files_folder)
  )

# Append files with _graded

graded_files_folder %>% 
  list.files(full.names = TRUE) %>% 
  file.rename(
    list.files(graded_files_folder, full.names = TRUE) %>% 
      str_replace("\\.[Rr]", "_graded.R")
  )

# final exam --------------------------------------------------------------

read_dir <- "problem_sets/final_exam/final_exam_submissions"

str_c(read_dir, ".zip") %>% 
  unzip(exdir = read_dir)

read_dir %>% 
  
  # List R files in folder:
  
  list.files(full.names = TRUE) %>% 
  
  # Iterate across all R Markdown files in the vector:
  
  purrr::map(
    
    # Rename the file:
    
    \(x) {
      file.rename(
        x,
        str_split(x, "/") %>% 
          pluck(1, 4) %>% 
          str_remove("Final exam_.*[0-9]") %>% 
          str_remove("_final_exam_") %>% 
          file.path(read_dir, .)
      )
    }
  )


