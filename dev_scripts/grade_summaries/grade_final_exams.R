# Calculate the grades for the final exam (Data Sci 2025)

# setup -------------------------------------------------------------------

library(tidyverse)

# Download and summarize grades:

grades_start <- 
  
  # Get grading sheet for the final:
  
  file.path(
    "https://docs.google.com/spreadsheets/d",
    "1roqSv1qa3hHKvtXptzN_LjVDT4SPceY8RTrhvuxOKBw"
  ) %>% 
  googlesheets4::read_sheet() %>% 
  filter(p_set == 8) %>% 
  
  # Calculate the total points off by student and question:
  
  summarize(
    pts_off = sum(points_off),
    .by = c(student_name_last, question)
  ) %>% 
  mutate(
    question = as.numeric(question)
  )

# Dummy data to fill in questions:

grades_fill <- 
  grades_start %>% 
  distinct(student_name_last) %>% 
  arrange(student_name_last) %>% 
  expand_grid(
    tibble(
      question = 1:12
    )
  ) %>% 
  mutate(pts_off = 0) %>% 
  anti_join(
    grades_start,
    by =  c("student_name_last", "question")
  )

# For grading brownie points 1 --------------------------------------------

# This will determine the top score that they can earn for the Brownie point:

grades_start %>% 
  filter(
    between(
      question,
      2,
      5
    )
  ) %>% 
  summarize(
    pts_available = (12 - sum(pts_off)) / 12 * 3,
    .by = student_name_last
  )

# calculate scores --------------------------------------------------------

# Bind first six questions and the four highest scores from 7-12:

grades_start %>% 
  
  # Bring in scores where no points were taken off:
  
  bind_rows(grades_fill) %>% 
  
  # Subset to the first six questions:
  
  filter(question <= 6) %>% 
  
  # Bind with the top scores from questions 7-12:
  
  bind_rows(
    grades_start %>% 
      
      # Subset to 7-12:
      
      filter(question > 6) %>% 
      
      # Get the four questions with the lowest points off:
      
      slice_min(
        pts_off, 
        n = 4,
        by = student_name_last,
        with_ties = FALSE
      )
  ) %>% 
  
  # Get the total points off:
  
  summarize(
    pts_off = sum(pts_off),
    .by = student_name_last
  ) %>% 
  
  # Calculate the grade for the final exam:
  
  mutate(
    score = 30 - pts_off
  )
