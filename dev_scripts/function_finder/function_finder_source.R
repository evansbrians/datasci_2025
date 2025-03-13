# Source script for general function finding (modules or problem sets)

# setup -------------------------------------------------------------------

library(tidyverse)

# Get the function frame:

fun_frame <-
  readxl::read_excel("big_functions_frame.xlsx") %>% 
  
  # Remove closing brackets:
  
  mutate(
    fun_string = 
      fun %>% 
      str_remove("[\\}\\]\\)]{1,3}") %>% 
      str_remove("\\.{3}") %>% 
      str_remove("…"),
    .before = definition
  )

# Convert operators to regex:

operators_regex <-
  fun_frame %>% 
  filter(class == "operator") %>% 
  tibble(
    operators = 
      
      # Extract operators from fun_frame:
      
      fun_string %>% 
      
      # Ensure that each character is interpretted as a literal:
      
      map_vec(
        \(x) {
          if(nchar(x) > 1) {
            str_split_1(x, pattern = "") %>% 
              str_c("\\", .) %>% 
              str_c(collapse = "")
          } else {
            str_c("\\", x)
          }
        }
      ) %>% 
      
      # But we don't want to escape a letter:
      
      str_replace("\\\\%\\\\i\\\\n\\\\%", "%in%")
    
  ) %>% 
  
  # Sorting from most to least number of characters avoids partial
  # matches:
  
  arrange(
    desc(
      nchar(operators)
    )
  ) %>%
  
  # Extract the vector and collapse into a single regex value:
  
  pull() %>% 
  str_c(collapse = "|")

# Function: Get chunk content ---------------------------------------------

get_chunk_content <-
  function(file_path) {
    
    initial_cleaning <- 
      tibble(
        lines = 
          read_lines(file_path) %>% 
          str_trim()
      ) %>% 
      
      # Remove comments and headers
      
      filter(
        !str_detect(lines, "^#")
      ) %>% 
      
      # Define the start and end of chunks:
      
      mutate(
        chunk_start = str_detect(lines, "```\\{r"),
        chunk_end = lines == "```",
        chunk_group = cumsum(chunk_start)
      ) %>% 
      
      # Remove content before chunks:
      
      filter(
        chunk_group > 0)
    
    # The remainder will only work if there are chunks in the file:
    
    
    if(
      nrow(initial_cleaning) > 0) {
      
      # Remove content after each chunk and hidden content:
      
      initial_cleaning %>% 
        group_by(chunk_group) %>% 
        mutate(
          chunk_after = cumsum(chunk_end),
          hidden = 
            str_detect(lines, "(echo ?= ?F)|(include ?= ?F)") %>% 
            cumsum()
        ) %>% 
        filter(
          chunk_after == min(chunk_after),
          hidden == 0,
          !str_detect(lines, "```\\{")
        ) %>% 
        ungroup() %>% 
        
        # Remove blank lines:
        
        filter(
          nchar(lines) > 0) %>% 
        
        pull(lines) %>% 
        unique()
    } else {
      NULL
    }
  }

# Function: Generate a character vector of operators from fun_frame -------

# Generate a character vector of operators:

get_operators <-
  function(fun_frame_path = "big_functions_frame.xlsx") {
    readxl::read_excel(fun_frame_path) %>% 
      
      # Subset to operators:
      
      filter(
        str_detect(fun, "%") |
          !str_detect(fun, "[a-zA-Z]")
      ) %>% 
      
      # Remove closing brackets:
      
      mutate(
        fun = 
          fun %>% 
          str_remove("[\\}\\]\\)]{1,3}") %>% 
          str_remove_all("[\\.…]")) %>% 
      
      # Arrange by the number of characters in the function (for regex 
      # searches):
      
      arrange(
        desc(
          nchar(fun)
        )
      ) %>% 
      pull(fun)
  }

# Function: Convert operators to a regex search string --------------------

get_operators_regex <-
  function(
    fun_frame_path = "big_functions_frame.xlsx",
    anchor = TRUE) {
    tibble(
      operators = 
        
        # Extract operators from fun_frame:
        
        get_operators(fun_frame_path) %>% 
        
        # Ensure that each character is interpretted as a literal:
        
        map_vec(
          \(x) {
            if(nchar(x) > 1) {
              str_split_1(x, pattern = "") %>% 
                str_c("\\", .) %>% 
                str_c(collapse = "")
            } else {
              str_c("\\", x)
            }
          }
        )
    ) %>% 
      
      # Sorting from most to least number of characters avoids partial
      # matches:
      
      arrange(
        desc(
          nchar(operators)
        )
      ) %>%
      
      # Include anchor metacharacters, if necessary (default):
      
      {
        if(anchor) {
          mutate(
            ., 
            operators = str_c("^", operators, "$")
          )
        } else {
          .
        }
      } %>% 
      
      # Extract the vector and collapse into a single regex value:
      
      pull() %>% 
      str_c(collapse = "|")
  }

# Function: Get functions used in a lesson --------------------------------

get_functions_used <-
  function(file_path) {
    
    markdown_vector <- 
      get_chunk_content(file_path)
    
    fun_used <-
      c(
        markdown_vector %>% 
          str_extract_all("[_\\.a-zA-Z0-9]+\\(") %>% 
          unlist() %>% 
          unique() %>% 
          str_remove("\\("),
        markdown_vector %>% 
          str_extract_all(operators_regex) %>% 
          unlist() %>% 
          unique()
      )
    
    fun_frame %>% 
      filter(fun_string %in% fun_used) %>% 
      select(!fun_string) %>% 
      
      # Assign + to primitive if ggplot was not among the functions used:
      
      {
        if(
          !str_detect(
            str_c(fun_used, collapse = ""),
            "ggplot")
        ) {
          filter(., package != "ggplot2")
        } else {
          .
        }
      }
  }

# Function: Get functions used in a module --------------------------------

get_module_functions <-
  function(module) {
    list.files(
      str_c("modules", module, sep = "/"),
      pattern = "Rmd$|qmd$",
      full.names = TRUE
    ) %>% 
      keep(~ !str_detect(.x, "intro(duction)?\\.(Rmd|qmd)$")) %>% 
      map(
        \(x) {
          funs_used <- 
            get_functions_used(x)
          
          if(nrow(funs_used) > 0) {
            funs_used %>% 
              mutate(
                lesson = 
                  str_split_1(x, "\\/")[[3]] %>% 
                  str_remove("\\.(qmd|Rmd)$"),
                lesson_numeric = 
                  str_extract(lesson, "[0-9]\\.[0-9]") %>% 
                  as.numeric(),
                .before = fun
              )
          }
        }
      ) %>% 
      bind_rows()
  }

# Function: Get new functions used in each module -------------------------

get_new_functions <-
  function(function_frame) {
    
    # Create a vector of lesson numbers:
    
    lesson_numbers <-
      function_frame %>% 
      pull(lesson_numeric) %>% 
      unique()
    
    # Get new functions learned in each lesson:
    
    function_frame %>% 
      filter(lesson_numeric == 0.1) %>% 
      bind_rows(
        map_dfr(
          2:length(lesson_numbers),
          \(i) {
            function_frame %>% 
              filter(lesson_numeric == lesson_numbers[i]) %>% 
              anti_join(
                function_frame %>% 
                  filter(lesson_numeric < lesson_numbers[i]),
                by = join_by(fun)
              )
          }
        )
      ) %>% 
      arrange(lesson_numeric, fun)
  }

# Write function tables ---------------------------------------------------

write_function_tables <-
  function(module) {
    
    # Get all functions in each lesson of the module:
    
    module_functions <-
      get_module_functions(module)
    
    # Iterate across each lesson:
    
    module_functions %>% 
      pull(lesson) %>% 
      unique() %>% 
      map(
        \(x) {
          
          # Subset to lesson:
          
          module_functions %>% 
            filter(lesson == x) %>% 
            
            # Subset and rename columns:
            
            select(
              Package = package,
              Function = fun,
              Definition = definition
            ) %>% 
            
            # Write to file
            
            write_csv(
              file.path(
                "modules",
                module,
                "function_tables",
                str_c("functions_", x, ".csv")
              )
            )
        }
      )
  }

# Function: Write module functions table ----------------------------------

write_module_functions_table <-
  function(
    fun_through_table,
    module_name
  ) {
    fun_frame %>% 
      select(
        package,
        fun,
        definition
      ) %>% 
      inner_join(
        fun_through_table %>% 
          select(
            package, 
            fun,
            lesson
          ),
        by = join_by(package, fun)
      ) %>% 
      select(
        Package = package,
        Function = fun,
        `First lesson used` = lesson,
        Definition = definition
      ) %>% 
      write_csv(
        file.path(
          "modules",
          module_name,
          "function_tables",
          str_c(
            "functions_through_",
            module_name,
            ".csv")
        )
      )
  }

# Function: Plot new functions used ---------------------------------------

# This is the best way to see if a particular lesson has too many new 
# functions!

plot_new_functions <-
  function(
    function_df,
    module) {
    
    function_df %>% 
      summarize(
        new_functions = n(),
        .by = lesson_numeric
      ) %>% 
      mutate(
        lesson = factor(lesson_numeric)
      ) %>% 
      ggplot() +
      aes(x = lesson,
          y = new_functions) + 
      scale_y_continuous(
        expand = c(0, 0)
      ) +
      labs(
        title = str_c("New functions per lesson, through ", module),
        x = "Lesson",
        y = "Number of new functions in the lesson"
      ) +
      geom_bar(
        stat = "identity",
        fill = "#dcdcdc",
        color = "#000000") +
      theme_classic()
  }

# Function: Plot function accumulation ------------------------------------

plot_fun_accumulation <-
  function(
    function_df,
    module) {
    
    function_df %>% 
      summarize(
        new_functions = n(),
        .by = lesson_numeric
      ) %>% 
      mutate(
        lesson = factor(lesson_numeric),
        function_accumulation = cumsum(new_functions)
      ) %>% 
      ggplot() +
      aes(x = lesson,
          y = function_accumulation) + 
      scale_y_continuous(
        expand = c(0, 0)
      ) +
      labs(
        title = str_c("Function accumulation, through ", module),
        x = "Lesson",
        y = "Number of functions used in the course"
      ) +
      geom_bar(
        stat = "identity",
        fill = "#dcdcdc",
        color = "#000000") +
      theme_classic()
  }
