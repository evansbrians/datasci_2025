# Example problem vector:

foxes <-
  c("Red fox",
    "Red Fox",
    "Read fox",
    "Redd Foxx",
    "red fox")

# Method 1: Go metacharacter by metacharacter -----------------------------

foxes %>% 
  str_replace(
    "Red fox",
    "Red fox"
  )

# Deal with capitalization with [...]:

foxes %>% 
  str_replace(
    "[Rr]ed [Ff]ox",
    "Red fox"
  ) %>% 
  unique()

# Deal with unnecessary letters with ?:

foxes %>% 
  str_replace(
    "[Rr]ea?dd? [Ff]oxx?",
    "Red fox"
  ) %>% 
  unique()

# Method 2: Start with the or metacharacter and then reduce ---------------

foxes %>% 
  str_replace(
    "Red fox|Red Fox|Read fox|Redd Foxx|red fox",
    "Red fox"
  ) %>% 
  unique()

# Now look for repetition ... we can reduce the first two:

foxes %>% 
  str_replace(
    "Red [Ff]ox|Read fox|Redd Foxx|red fox",
    "Red fox"
  ) %>% 
  unique()

# Now add in the next one:

foxes %>% 
  str_replace(
    "Rea?d [Ff]ox|Redd Foxx|red fox",
    "Red fox"
  ) %>% 
  unique()

# ... and the next:

foxes %>% 
  str_replace(
    "Rea?dd? [Ff]oxx?|red fox",
    "Red fox"
  ) %>% 
  unique()

# ... and the last:

foxes %>% 
  str_replace(
    "[Rr]ea?dd? [Ff]oxx?",
    "Red fox"
  ) %>% 
  unique()

# Method 3: Go word by word -----------------------------------------------

# Split your operation into two separate search and replace functions:

foxes %>% 
  str_replace("Red", "Red") %>% 
  str_replace("fox", "fox") %>% 
  unique()

# Deal with the first word:

foxes %>% 
  str_replace("[Rr]ea?dd?", "Red") %>% 
  str_replace("fox", "fox") %>% 
  unique()

# Deal with the second word:

foxes %>% 
  str_replace("[Rr]ea?dd?", "Red") %>% 
  str_replace("[Ff]oxx?", "fox") %>% 
  unique()

# Combine:

foxes %>% 
  str_replace("[Rr]ea?dd? [Ff]oxx?", "Red fox") %>% 
  unique()
