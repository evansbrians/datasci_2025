
# setup -------------------------------------------------------------------

library(tidyverse)
library(microbenchmark)
library(lobstr)
library(profmem)

# Check if R has been built with memory profiling enabled:

capabilities("profmem")

# If not, configure and build (Terminal):
#
# $ ./configure --enable-memory-profiling
# $ make

# object size -------------------------------------------------------------

lobstr::mem_used()

hello_char <-
  rep(
    c("hello", "world"),
    1E6
  )

mem_used()

lobstr::obj_size(hello_char)

hello_fact <-
  factor(
    rep(
      c("hello", "world"),
      1E6
    )
  )

mem_used()

obj_size(hello_fact)

lobstr::obj_sizes(hello_char, hello_fact)

# memory change -----------------------------------------------------------

# How much memory is freed up when you remove an object?

mem_used()

rm(hello_fact)

mem_used()

# How much memory does a copy of an object use?

hello_char_copy <-
  hello_char

obj_size(hello_char)

obj_size(hello_char_copy)

obj_sizes(
  hello_char, 
  hello_char_copy
)

lobstr::obj_addr(hello_char)

obj_addr(hello_char_copy)

# What if a recursive (list) object contains duplicated objects?

hello_list <-
  list(
    hello_char, 
    hello_char_copy
  )

obj_size(hello_list)

obj_sizes(
  hello_char, 
  hello_char_copy, 
  hello_list
)

obj_sizes(
  list(NULL, NULL)
)

lobstr::ref(hello_list)

# What does removing objects do?

mem_used()

rm(hello_list)

mem_used()

rm(hello_char_copy)

mem_used()

rm(hello_char)

mem_used()

# processing time ---------------------------------------------------------

# What time is it?

lubridate::now()

# Our benchmarking function:

get_proc_time <-
  function(x) {
    
    # Start time:
    
    start <-
      now()
    
    # Process to run:
    
    x
    
    # Difference in time between the end of the process and the start time:
    
    now() - start
  }

# Test our function:

get_proc_time(
  rep(
    c("hello", "world"),
    1E6
  )
)

get_proc_time(
  factor(
    rep(
      c("hello", "world"),
      1E6
    )
  )
)

# Test function with microbenchmark:

microbenchmark::microbenchmark(
  rep(
    c("hello", "world"),
    1E6
  )
)

microbenchmark(
  factor(
    rep(
      c("hello", "world"),
      1E6
    )
  )
)

# memory used during processing -------------------------------------------

profmem::profmem(
  tibble(
    a = 
      rep(
        c("hello", "world"),
        1E6
      ),
    b = 
      rep(1:2, 1E6)
  ) %>% 
    group_by(a) %>%
    mutate(
      b = b + 100
    )
) %>% 
  profmem::total() * 1E-6

# To pipe or not to pipe?

profmem({
  my_tibble <-
    tibble(
      a = 
        rep(
          c("hello", "world"),
          1E6
        ),
      b = 
        rep(1:2, 1E6)
    )
  mutate(
    my_tibble, 
    b = b + 100
  )
}) %>% 
  total() * 1E-6

# Trade-offs

profmem({
  hello_char <- 
    rep(
      c("hello", "world"),
      1E6
    )
}) %>% 
  total() * 1E-6

lobstr::obj_size(hello_char)

profmem({
  hello_fact <- 
    factor(
      rep(
        c("hello", "world"),
        1E6
      )
    )
}) %>% 
  total() * 1E-6

lobstr::obj_size(hello_fact)


