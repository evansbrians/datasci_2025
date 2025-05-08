
# setup -------------------------------------------------------------------

source("dev_scripts/function_finder/function_finder_source.R")

# Get functions used, Module 0 --------------------------------------------

# Module 0. Introductory material (all functions in the module):

fun_introductory_material <- 
  list.files(
    "introductory_material",
    pattern = "Rmd$",
    full.names = TRUE
  ) %>% 
  map(
    \(x) {
      funs_used <- 
        get_functions_used(x)
      
      if(nrow(funs_used) > 0) {
        funs_used %>% 
          mutate(
            lesson = 
              str_split_1(x, "\\/")[[2]] %>% 
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

# Get new functions learned in each lesson:

new_fun_introductory_material <-
  get_new_functions(fun_introductory_material)

new_fun_introductory_material %>% 
  print(n = Inf)

# Plot new functions used:

new_fun_introductory_material %>% 
  plot_new_functions("Preliminary lessons")

# Plot function accumulation curve:

new_fun_introductory_material %>% 
  plot_fun_accumulation("Preliminary lessons")

# Make function table:

fun_table_introductory_material <-
  fun_frame %>% 
  inner_join(
    new_fun_introductory_material %>% 
      select(package, fun, first_appearance = lesson),
    by = join_by(fun, package)
  ) %>% 
  select(
    package:fun,
    first_appearance,
    definition)

fun_table_introductory_material %>% 
  print(n = Inf)

# Get functions used, Module 1 --------------------------------------------

# Module 1, all functions:

fun_module_1 <- 
  get_module_functions("module_1")

fun_module_1 %>% 
  print(n = Inf)

# Write functions by lesson:

write_function_tables("module_1")

# Get new functions learned in each lesson:

fun_through_module_1 <-
  bind_rows(
    fun_introductory_material,
    fun_module_1) %>% 
  get_new_functions()

# Write "functions through" table:

fun_through_module_1 %>% 
  write_module_functions_table("module_1")

# Plot new functions used:

fun_through_module_1 %>% 
  plot_new_functions("Preliminary lessons")

# Plot function accumulation curve:

fun_through_module_1 %>% 
  plot_fun_accumulation("Module 1")

# Get functions used, Module 2 --------------------------------------------

# Module 2, all functions:

fun_module_2 <- 
  get_module_functions("module_2")

fun_module_2 %>% 
  print(n = Inf)

# Write functions by lesson:

write_function_tables("module_2")

# Get new functions learned in each lesson:

fun_through_module_2 <-
  bind_rows(
    fun_introductory_material,
    fun_module_1,
    fun_module_2) %>% 
  get_new_functions()

fun_through_module_2 %>% 
  print(n = Inf)

fun_through_module_2 %>% 
  write_module_functions_table("module_2")

# Plot new functions used:

fun_through_module_2 %>% 
  plot_new_functions("Preliminary lessons")

# Plot function accumulation curve:

fun_through_module_2 %>% 
  plot_fun_accumulation("Module 2")

# Get functions used, Module 3 --------------------------------------------

# Module 3, all functions:

fun_module_3 <- 
  get_module_functions("module_3")

fun_module_3 %>% 
  print(n = Inf)

# Write functions by lesson:

write_function_tables("module_3")

# Get new functions learned in each lesson:

fun_through_module_3 <-
  bind_rows(
    fun_introductory_material,
    fun_module_1,
    fun_module_2,
    fun_module_3) %>% 
  get_new_functions()

fun_through_module_3 %>% 
  print(n = Inf)

fun_through_module_3 %>% 
  write_module_functions_table("module_3")

# Plot new functions used:

fun_through_module_3 %>% 
  plot_new_functions("Preliminary lessons")

# Plot function accumulation curve:

fun_through_module_3 %>% 
  plot_fun_accumulation("Module 3")

# Get functions used, Module 4 --------------------------------------------

# Module 4, all functions:

fun_module_4 <- 
  get_module_functions("module_4")

fun_module_4 %>% 
  print(n = Inf)

# Write functions by lesson:

write_function_tables("module_4")

# Get new functions learned in each lesson:

fun_through_module_4 <-
  bind_rows(
    fun_introductory_material,
    fun_module_1,
    fun_module_2,
    fun_module_3,
    fun_module_4) %>% 
  get_new_functions()

fun_through_module_4 %>% 
  print(n = Inf)

fun_through_module_4 %>% 
  write_module_functions_table("module_4")

# Plot new functions used:

fun_through_module_4 %>% 
  plot_new_functions("Preliminary lessons")

# Plot function accumulation curve:

fun_through_module_4 %>% 
  plot_fun_accumulation("Module 4")

# Get functions used, Module 5 --------------------------------------------

# Module 5, all functions:

fun_module_5 <- 
  get_module_functions("module_5")

fun_module_5 %>% 
  print(n = Inf)

# Write functions by lesson:

write_function_tables("module_5")

# Get new functions learned in each lesson:

fun_through_module_5 <-
  bind_rows(
    fun_introductory_material,
    fun_module_1,
    fun_module_2,
    fun_module_3,
    fun_module_4,
    fun_module_5) %>% 
  get_new_functions()

fun_through_module_5 %>% 
  print(n = Inf)

fun_through_module_5 %>% 
  write_module_functions_table("module_5")

# Plot new functions used:

fun_through_module_5 %>% 
  plot_new_functions("Preliminary lessons")

# Plot function accumulation curve:

fun_through_module_5 %>% 
  plot_fun_accumulation("Module 5")

# Get functions used, Module 6 --------------------------------------------

# Module 6, all functions:

fun_module_6 <- 
  get_module_functions("module_6")

fun_module_6 %>% 
  print(n = Inf)

# Write functions by lesson:

write_function_tables("module_6")

# Get new functions learned in each lesson:

fun_through_module_6 <-
  bind_rows(
    fun_introductory_material,
    fun_module_1,
    fun_module_2,
    fun_module_3,
    fun_module_4,
    fun_module_5,
    fun_module_6) %>% 
  get_new_functions()

fun_through_module_6 %>% 
  print(n = Inf)

fun_through_module_6 %>% 
  write_module_functions_table("module_6")

# Plot new functions used:

fun_through_module_6 %>% 
  plot_new_functions("Preliminary lessons")

# Plot function accumulation curve:

fun_through_module_6 %>% 
  plot_fun_accumulation("Module 6")

# Get functions used, Module 7 --------------------------------------------

# Module 7, all functions:

fun_module_7 <- 
  get_module_functions("module_7")

fun_module_7 %>% 
  print(n = Inf)

# Write functions by lesson:

write_function_tables("module_7")

# Get new functions learned in each lesson:

fun_through_module_7 <-
  bind_rows(
    fun_introductory_material,
    fun_module_1,
    fun_module_2,
    fun_module_3,
    fun_module_4,
    fun_module_5,
    fun_module_6,
    fun_module_7
    ) %>% 
  get_new_functions()

fun_through_module_7 %>% 
  print(n = Inf)

fun_through_module_7 %>% 
  write_module_functions_table("module_7")

# Plot new functions used:

fun_through_module_7 %>% 
  plot_new_functions("Preliminary lessons")

# Plot function accumulation curve:

fun_through_module_7 %>% 
  plot_fun_accumulation("Module 7")

# Get functions used, Module 8 --------------------------------------------

# Module 8, all functions:

fun_module_8 <- 
  get_module_functions("module_8")

fun_module_8 %>% 
  print(n = Inf)

# Write functions by lesson:

write_function_tables("module_8")

# Get new functions learned in each lesson:

fun_through_module_8 <-
  bind_rows(
    fun_introductory_material,
    fun_module_1,
    fun_module_2,
    fun_module_3,
    fun_module_4,
    fun_module_5,
    fun_module_6,
    fun_module_7,
    fun_module_8
  ) %>% 
  get_new_functions()

fun_through_module_8 %>% 
  print(n = Inf)

fun_through_module_8 %>% 
  write_module_functions_table("module_8")

# Plot new functions used:

fun_through_module_8 %>% 
  plot_new_functions("Preliminary lessons")

# Plot function accumulation curve:

fun_through_module_8 %>% 
  plot_fun_accumulation("Module 8")

