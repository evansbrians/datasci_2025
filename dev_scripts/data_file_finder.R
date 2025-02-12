# setup -------------------------------------------------------------------

source("dev_scripts/function_finder/function_finder_source.R")

data_extensions_regex <-
  c("\\.csv",
    "\\.dbf",
    "\\.geojson",
    "\\.prj",
    "\\.rds",
    "\\.shp",
    "\\.shx",
    "\\.tif",
    "\\.txt",
    "\\.xlsx") %>% 
  str_c(collapse = "|")

# Function: Get data read in during the written portions of a lesson ------

# Note: This will not work with data read in with list.files!

get_module_data_reads <-
  function(module) {
    list.files(
      str_c("modules/module_", module),
      pattern = "Rmd$|qmd$",
      full.names = TRUE
    ) %>% 
      map_dfr(
        \(x) {
          tibble(
            lesson = 
              str_remove(x, "modules/module_[0-9]/") %>% 
              str_remove("\\.qmd"),
            data_file =
              get_chunk_content(x) %>% 
              keep(~ str_detect(.x, data_extensions_regex)) %>% 
              str_extract("\\(.*\\)") %>% 
              str_remove_all("\\(|\\)") %>% 
              str_extract("data/.*") %>% 
              str_remove_all("data/") %>% 
              keep(
                ~ !is.na(.x)
              ) %>% 
              keep(
                ~ !str_detect(.x, "temp\\.")
              ) %>% 
              str_remove("\\\"") %>% 
              unique()
          )
        }
      ) 
  }

# Execute -----------------------------------------------------------------

get_module_data_reads(5) %>% 
  distinct(data_file)


# iteration makes the above a problem -------------------------------------

# Because we are using purrr to read in files, we now have to pull them from the
# list at the top of the lessons:

list.files(
  "modules/module_5",
  pattern = "qmd$",
  full.names = TRUE
) %>% 
  discard(
    ~ str_detect(.x, "intro\\.qmd")
  ) %>% 
  map(
    \(lesson) {
      read_lines(lesson) %>% 
        keep(
          ~ str_detect(.x, "^\\*\\*|^\\[\\*\\*")
        ) %>%
        str_extract("\\[.+\\]\\{\\.mono\\}")
    }
  ) %>% 
  unlist() %>% 
  discard(
    ~ is.na(.x)
  ) %>% 
  unique()



