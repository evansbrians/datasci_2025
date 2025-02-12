
grades <- 
  file.path(
    "https://docs.google.com/spreadsheets/d",
    "15NY5WQjI9Xr_j9TalGZsGjj6sTF8inpbEkNMCcXLizc"
  ) %>% 
  googlesheets4::read_sheet() %>% 
  
  # Subset to the final exam:
  
  filter(p_set == 8) %>% 
  
  # Calculate the total amount of points per student and question:
  
  summarize(
    points_off = sum(points_off),
    .by = c(student_name_last, question)
  ) %>% 
  
  # Convert question to integer:
  
  mutate(
    question = as.integer(question)
  )

# Calculate grades:

final_grades <- 
  grades %>% 
  pull(student_name_last) %>% 
  unique() %>% 
  map_dfr(
    ~ tibble(
      question = 1:14
    ) %>% 
      
      # Get the grades for a given question:
      
      left_join(
        grades %>% 
          filter(student_name_last == .x),
        by = "question"
      ) %>% 
      
      # Calculate the scores per student and question:
      
      mutate(
        student = .x,
        score = 3 - replace_na(points_off, 0),
      ) %>% 
      
      # Subset to columns of interest and arrange:
      
      select(
        student,
        question,
        score
      )
  ) %>% 
  group_by(student) %>% 
  
  # Calculate the highest scoring grades:
  
  slice_max(
    order_by = score,
    n = 10,
    with_ties = TRUE
  ) %>% 
  
  # Subset to the 10 highest scoring questions:
  
  slice(1:10) %>% 
  
  # Calculate grades:
  
  summarize(
    grade = sum(score),
    percent = grade / 30 * 100
  ) %>% 
  ungroup() %>% 
  
  # Arrange from highest to lowest:
  
  arrange(-percent)

final_grades %>% 
  print(n = Inf)

