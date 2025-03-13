
# setup -------------------------------------------------------------------

library(tidyverse)

# Read in glossary of terms:

glossary_table <- 
  read_csv("dev_scripts/glossary/big_glossary.csv") %>% 
  
  # Remove stuff associated with the GIS course (that may interfere with
  # data sci stuff):
  
  filter(
    !str_detect(Term, "Projected|Projection")
  )

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
        ~str_extract_all(.x, "\\*\\*[a-zA-Z_]*\\*\\*")
      ) %>% 
      unlist() %>% 
      keep(
        ~ !str_detect(.x, "Important|\\{.mono\\}")
      ) %>% 
      str_remove_all("\\*\\*") %>% 
      keep(
        ~ !str_detect(.x, "^R$|Rstudio|Projected|Projection|CRS|[Dd]istance")
      ) %>% 
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
      ) %>% 
      unlist() %>% 
      unname() %>% 
      unique() %>% 
      sort()
  }

# Function to get new glossary table for a lesson:

get_glossary_table_lesson <- 
  function(
    module = mod,
    lesson = less
  ) {
    get_lesson_terms(mod, less) %>% 
      map_dfr(
        ~ glossary_table %>% 
          filter(
            str_detect(
              tolower(Term), 
              tolower(.x),
              ),
            !str_detect(.x, "^R$|Rstudio|Projected|Projection|CRS|[Dd]istance")
            )
      ) %>% 
      distinct()
  }

# Function to get new glossary table for a module:

get_glossary_table_module <-
  function(module = mod) {
    list.files(
      str_c("modules/module_", mod), 
      pattern = "qmd$"
    ) %>% 
      keep(
        ~ !str_detect(.x, "[0-9]{1}\\.0")
      ) %>% 
      map(
        ~ get_glossary_table_lesson(
          module = mod,
          lesson = .x)
      ) %>% 
      bind_rows() %>% 
      distinct() %>% 
      arrange(Term)
  }






