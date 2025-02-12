
# Tara's script for grading the students!

# setup -------------------------------------------------------------------

library(tidyverse)

# function to summarize grades --------------------------------------------

get_grade_summaries <-
  function(problem_set, qmd = TRUE) {
    
    # Get the names of the submission files:
    
    parent_path <-
      if(qmd) {
        "../../problem_sets"
      } else {
        "problem_sets"
      }
    
    submission_files <-
      file.path(
        "../../problem_sets",
        str_c("problem_set_", problem_set),
        str_c("problem_set_", problem_set, "_graded")
      ) %>% 
      list.files()
    
    # Get the gradebook 
    
    gradebook <-
      file.path(
        "https://docs.google.com/spreadsheets/d",
        "15NY5WQjI9Xr_j9TalGZsGjj6sTF8inpbEkNMCcXLizc"
      ) %>% 
      googlesheets4::read_sheet() %>% 
      filter(p_set == problem_set)
    
    # Calculate grades:
    
    grades <- 
      gradebook %>% 
      summarize(
        grade = 10 - sum(points_off),
        .by = student_name_last
      )
    
    just_grades <-
      grades %>% 
      pull(grade)
    
    
    list(
      submissions = submission_files,
      gradebook = gradebook,
      
      # All grades:
      
      grades = grades,
      
      # Descriptive stats:
      
      mean = 
        just_grades %>% 
        mean() %>% 
        round(2),
      
      median = 
        just_grades %>% 
        median() %>% 
        round(2),
      
      sd = 
        just_grades %>% 
        sd() %>% 
        round(2),
      
      se =
        {
          sd(just_grades)/sqrt(length(just_grades))
        } %>% 
        round(2),
      
      # Average points off for mistake class (across students and 
      # questions):
      
      points_off_by_mistake_class =
        gradebook %>% 
        summarize(
          ave_points_off = 
            sum(points_off) / 
            length(
              submission_files
            ),
          .by = mistake_class
        ) %>% 
        arrange(
          desc(ave_points_off)
        ),
      
      # Which questions gave students the biggest issues?
      
      points_off_by_question =
        gradebook %>% 
        summarize(
          ave_percent_off = 
            sum(points_off) / 
            (
              unique(pts_available) *
                length(
                  submission_files
                )
            ) * 100,
          .by = question
        ) %>% 
        arrange(
          desc(ave_percent_off)
        ),
      
      # What types of mistakes were the biggest issues?
      
      points_off_by_mistake_description =
        gradebook %>% 
        mutate(
          mistake_description = str_trim(mistake_description)
        ) %>% 
        summarize(
          n_mistakes = n(),
          total_points_off = sum(points_off),
          .by = c(mistake_description)
        ) %>% 
        mutate(
          ave_points_off = 
            round(total_points_off/length(submission_files), 2)
        ) %>% 
        slice_max(
          order_by = ave_points_off,
          n = 10)
    )
  }











