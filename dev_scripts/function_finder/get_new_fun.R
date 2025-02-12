# Get new functions used in a lesson (that are not in big_functions_frame)

# this is it! -------------------------------------------------------------

library(tidyverse)

# Read in the a lesson from a module:

list.files(
  "modules/module_7",
  pattern = "qmd$",
  full.names = TRUE
) %>% 
  map(
    \(lesson) {
      read_lines(lesson) %>% 
        
        # Pull out lines with functions:
        
        keep(
          ~ str_detect(.x, "\\(")
        ) %>% 
        
        # Extract just the function and opening parentheses:
        
        str_extract("[A-Za-z_\\.0-9]+\\(") %>% 
        
        # Remove NA values and dev functions:
        
        discard(
          \(x) {
            is.na(x) |
              str_detect(
                x, 
                "kable|classList|EventListener|ElementsBy|clean_names")
          }
        ) %>% 
        
        # Subset to unique values:
        
        unique() %>% 
        
        # Remove the opening parentheses:
        
        str_remove("\\(") %>% 
        
        # Subset to functions that are not in `big_functions_frame.xlsx`:
        
        discard(
          \(x) {
            x %in% 
              {
                readxl::read_excel("big_functions_frame.xlsx") %>% 
                  pull(fun)
              }
          }
        )
    }
  ) %>% 
  unlist() %>% 
  unique() %>% 
  sort()
ablin      
