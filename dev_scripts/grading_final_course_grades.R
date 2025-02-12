
# setup -------------------------------------------------------------------

library(tidyverse)

# Get starting point for grades:

grades_start <- 
  read_csv("gc_Master_83202_83203.202470_fullgc_2024-12-18-09-22-40.csv") %>% 
  janitor::clean_names() %>% 
  select(
    !c(
      username,
      last_access,
      matches("^total|^self|^x16")
    )
  ) %>% 
  
  # Add participation:
  
  left_join(
    googlesheets4::read_sheet(
      ss = "https://docs.google.com/spreadsheets/d/15NY5WQjI9Xr_j9TalGZsGjj6sTF8inpbEkNMCcXLizc/edit?gid=336462199#gid=336462199",
      sheet = "discussion_board"
    ) %>% 
      select(
        name_last, 
        participation = mean
      ),
    by = join_by(last_name == name_last)
  ) %>% 
  
  # Convert submissions to numeric:
  
  mutate(
    across(
      matches("problem_set"),
      ~ as.numeric(.x)
    )
  ) %>% 
  
  # Rename variables:
  
  rename(
    student = last_name,
    final_exam = matches("^final"),
    extra_credit = matches("^extra")
  ) %>% 
  set_names(
    names(.) %>% 
      str_remove(
        "_total_pts_10_score_[0-9]*"
      )
  ) %>% 
  drop_na(final_exam)

# calculate grades --------------------------------------------------------

grades_start %>% 
  mutate(
    student,
    final_exam = final_exam / 30, 
    participation,
    extra_credit = replace_na(extra_credit, 0),
    .keep = "none"
  ) %>% 
  
  # Join problem set scores:
  
  left_join(
    grades_start %>%
      pull(student) %>% 
      map_dfr(
        ~ grades_start %>% 
          
          # Subset to student:
          
          filter(student == .x) %>% 
          
          # Reshape to long form:
          
          pivot_longer(
            problem_set_1_r_foundations:problem_set_7_a_holiday_for_tara,
            names_to = "assignment",
            values_to = "grade"
          ) %>% 
          
          # Grab the top problem set scores depending on school:
          
          {
            if(
              str_detect(.x, "^Mcc|^Tuc|^Tre")
            ) {
              slice_max(
                .,
                order_by = grade,
                n = 6
              )
            } else {
              slice_max(
                .,
                order_by = grade,
                n = 3
              )
            }
          } %>% 
          
          # Calculate problem set scores:
          
          summarize(
            n_problem_sets = sum(grade >= 7),
            problem_set_grade = mean(grade) / 10,
            .by = student
          )
      ),
    by = "student"
  ) %>% 
  
  # Calculate final grades:
  
  mutate(
    participation = replace_na(participation, 0),
    final_grade = 
      60 * problem_set_grade +
      30 * final_exam + 
      participation +
      extra_credit
  ) %>% 
  print(n = Inf)
