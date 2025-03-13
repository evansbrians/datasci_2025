# Convert the big glossary from QMD to CSV:

read_lines("dev_scripts/big_glossary.qmd") %>% 
  as_tibble() %>% 
  filter(
    str_detect(value, "-   ")
  ) %>% 
  mutate(
    value = 
      value %>% 
      str_remove("-   ") %>% 
      str_remove_all("\\*\\*") %>% 
      str_remove("plural: ") %>% 
      str_trim()
  ) %>% 
  separate(
    value,
    sep = ":",
    into = c("term", "definition")
  ) %>% 
  mutate(
    definition = str_trim(definition)
  ) %>% 
  rename(
    Term = term,
    Definition = definition
  ) %>% 
  write_csv("dev_scripts/big_glossary.csv")

# Table making for a lesson (note: The trouble that I'm having is with project ... how to fix more parsimoniously?:

get_lesson_terms(
  1, 
  "1.2_getting_started.qmd"
) %>% 
  unique() %>%
  str_remove_all("\\*\\*") %>% 
  keep(~ .x != "R") %>% 
  map_dfr(
    ~ read_csv("dev_scripts/big_glossary.csv") %>% 
      filter(
        str_detect(Term, .x),
        !str_detect(Term, "Projected|Projection")
      )
  )


# Table making for a module:

get_module_terms(1) %>% 
  unlist() %>% 
  unname() %>% 
  unique() %>% 
  str_remove_all("\\*\\*") %>% 
  map_dfr(
    ~ read_csv("dev_scripts/big_glossary.csv") %>% 
      filter(
        str_detect(Term, .x),
        !str_detect(Term, "Projected|Projection")
      )
  )

