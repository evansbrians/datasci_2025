# Function finder for the final exam

# setup -------------------------------------------------------------------

library(tidyverse)

# Get big function frame:

big_funs <-
  readxl::read_excel("big_functions_frame.xlsx")

# named functions ---------------------------------------------------------

named_fun <-
  read_lines("problem_sets/final_exam/final_exam_key.R") %>% 
  str_trim() %>% 
  keep(
    ~ !str_detect(.x, "^#") &
      str_detect(.x, "\\(")
  ) %>% 
  map_chr(
    ~ str_extract(.x, "[a-z_\\.]*\\(") %>% 
      str_remove("\\(")
  ) %>% 
  keep(
    ~ !is.na(.x) &
      nchar(.x) > 0
  ) %>% 
  unique() %>% 
  sort()

# Add packages:

named_funs_formatted <-  
  big_funs %>% 
  filter(fun %in% named_fun) %>% 
  mutate(
    fun = 
      if_else(
        package == ".Primitive",
        str_c(
          package, 
          fun, 
          sep = ", "
        ),
        str_c(
          package, 
          fun, 
          sep = "::"
        )
        
      )
  ) %>%
  pull(fun) %>% 
  str_c(
    "* `",
    .,
    "`"
  ) %>% 
  sort()

# operators ---------------------------------------------------------------

operators <- 
  big_funs %>% 
  filter(class == "operator") %>% 
  select(!class:definition) %>% 
  mutate(
    fun = 
      str_remove_all(
        fun,
        "\\.\\.\\..+"
      ) %>% 
      str_remove_all(
        "….+"
      ) %>% 
      str_c(package, ., sep = ", ")
  ) %>% 
  pull(fun) %>% 
  str_c(
    "* `",
    .,
    "`"
  ) %>% 
  sort()

# combine and write -------------------------------------------------------

named_funs_formatted %>% 
  c(operators) %>% 
  sort() %>% 
  write_lines("final_funs.txt")


