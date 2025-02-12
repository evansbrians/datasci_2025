
# This script hunts for potential glossary terms

# setup -------------------------------------------------------------------

library(tidyverse)

# functions ---------------------------------------------------------------

# Function to hunt for glossary terms in a lesson:

get_lesson_terms <-
  function(module, lesson) {
    file.path(
      "modules",
      str_c("module_", module),
      lesson
    ) %>% 
      read_lines() %>% 
      keep(~ str_detect(.x, "\\*\\*")) %>% 
      map(
        ~str_extract_all(.x, "\\*\\*[a-zA-Z]*\\*\\*")
      ) %>% 
      unlist() %>% 
      keep(
        ~ !str_detect(.x, "Important|\\{.mono\\}")
      ) %>% 
      str_to_title() %>% 
      unique() %>% 
      sort()
  }

# Function to hunt for a glossary term in a module (returns a named list
# where each list item is a lesson):

get_module_terms <-
  function(module) {
    str_c("modules/module_", module) %>% 
      list.files(pattern = "qmd$") %>% 
      set_names(., .) %>% 
      map(
        ~ get_lesson_terms(module, .x)
      )
  }

# execution ---------------------------------------------------------------

# Not perfect, but it should help!

# Glossary terms across module 1:

get_module_terms(1)
  
# Glossary terms across module 2:

get_module_terms(2)

# Glossary terms across module 3:

get_module_terms(3)

# Glossary terms across module 4:

get_module_terms(4)

