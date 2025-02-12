tibble(
  lines = 
    read_lines(file_path) %>% 
    str_trim()
  ) %>% 
  mutate(
    chunk_start = str_detect(lines, "```\\{r"),
    chunk_end = lines == "```",
    chunk_group = cumsum(chunk_start)
  ) %>% 
  
  # Remove content before chunks:
  
  filter(
    chunk_group > 0) %>% 
  
  group_by(chunk_group) %>% 
  mutate(
    chunk_after = cumsum(chunk_end),
    chunk_comment = 
      lines %>% 
      str_detect("#") %>% 
      sum(),
    hidden = 
      str_detect(lines, "(echo ?= ?F)|(include ?= ?F)") %>% 
      cumsum()
  ) %>% 
  filter(
    hidden == 0,
    chunk_comment > 0,
    chunk_after == min(chunk_after)) %>% 
  ungroup() %>% 
  mutate(
    lines =
      str_remove(lines, "```\\{.*\\}")
  ) %>% 
  pull(lines) %>% 
  writeLines("temp.R")
