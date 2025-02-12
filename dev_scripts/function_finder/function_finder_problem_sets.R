# For identifying functions in problem sets!

# setup -------------------------------------------------------------------

source("dev_scripts/function_finder/function_finder_source.R")

# function to extract code from the problem set keys ----------------------

extract_code_hw_key <-
  function(problem_set_number) {
    file.path(
      "problem_sets",
      str_c(
        "problem_set_", 
        problem_set_number)
    ) %>% 
      list.files(
        pattern = "key\\.R",
        full.names = TRUE) %>% 
      read_lines() %>% 
      str_trim() %>% 
      keep(
        ~ nchar(.x) > 0 &
          !str_detect(.x, "^#") &
          !str_detect(.x, "^\\\"")
      ) %>% 
      unique() %>% 
      str_remove("\\\".*\\\"")
  }

# function to get functions used in a given problem set -------------------

get_fun_used <-
  function(problem_set_number) {
    code <-
      extract_code_hw_key(problem_set_number)
    
    # Operators:
    
    code %>% 
      str_extract_all(operators_regex) %>% 
      unlist() %>% 
      unique() %>% 
      c(
        
        # Named functions:
        
        code %>% 
          str_extract_all("[_\\.a-zA-Z0-9]+\\(") %>% 
          unlist() %>% 
          unique() %>% 
          str_remove("\\(") %>% 
          sort()
      )
  }

# function to pull functions from the big table: --------------------------

get_fun_table <- 
  function(problem_set_number) {
    fun_frame %>% 
      filter(
        fun_string %in%
          get_fun_used(problem_set_number)
      ) %>% 
      select(package, fun) %>% 
      print(n = Inf)
  }

# execution ---------------------------------------------------------------

# Functions used in the key:

key_fun <-
  get_fun_table(6) %>% 
  mutate(
    fun = 
      str_c(
        package,
        fun,
        sep = "::"
      ) %>% 
      if_else(
        package == ".Primitive",
        str_replace(., "::", ", "),
        .
      ),
    .keep = "none"
  )

# Functions you may use list in the qmd

qmd_fun <- 
  tibble(
    fun = 
      file.path(
        "problem_sets",
        str_c(
          "problem_set_", 
          6)
      ) %>% 
      list.files(
        pattern = "qmd$",
        full.names = TRUE) %>% 
      read_lines() %>% 
      keep(
        ~ str_detect(.x, "^\\* `")) %>% 
      str_remove("\\* ") %>% 
      str_remove_all("`")
  )
  
# Which functions are in the key that are not in the qmd?

key_fun %>% 
  filter(!fun %in% qmd_fun$fun)

# Which functions are in the qmd that are not in the key?

qmd_fun %>% 
  filter(!fun %in% key_fun$fun)



