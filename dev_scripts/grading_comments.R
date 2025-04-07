
# Comments for students when grading the assignments

# [[-0.10]] Code formatting:

# [[No points removed]] Code formatting:

# [[-0.10]] Code parsimony:

# [[No points removed]] Code parsimony:

# grab bag ----------------------------------------------------------------

# [[-0.50]] You were not asked to a assign a name to the global environment in
# this question.

# [[-0.50]] Incorrect: When comparing between sets of values, use `%in%`.

# [[-0.50]]: When you work in an RStudio project you do not need to supply the
# absolute file path to the data that you read in. Working in a project allows
# you to read in the data with a relative file path -- in other words, the
# location of the file relative to the top level of the project folder. As
# written, this code will not work on my computer (because I do not share your
# absolute file path).

# functions you may use ---------------------------------------------------

# [[-0.50]] `labs` is not among the functions that you may use for this
# assignment.

# [[-0.50]] `labs` and `recode` are not among the functions that you may use for this assignment.

# [[-0.50]] `labs`, `is.na`, and `recode` are not among the functions that you may use for this assignment.

# formatting: random ------------------------------------------------------

# * Assigned names should be written in snake_case.

# * Code formatting: Spell out TRUE and FALSE in a function’s argument.

# formatting: horizontal space --------------------------------------------

# Indentation:

# * Indentation: If the opening parentheses and the first argument of a 
#   function are on different lines, the first argument should be indented two
#   spaces (one tab stop) relative to the start of the line above.

# * Indentation and hanging parentheses: If a function spans more than one line
#   of code, closing parentheses should be placed on their own line and 
#   indented to the same level as the start of the function.

# * Indentation: Closing parentheses should be indented to the same level as
#   the start of the function.

# * Indentation: The line following the first pipe in a piped code block should
#   be indented two spaces (one tab stop) relative to the line above.

# Single spaces:

# * The extraction ($) and colon (:, ::) infix operators should not be preceded
#   or followed by a space.

# * Infix functions should be separated from surrounding code with a single
#   leading and trailing space.

# * Commas should be followed by one trailing space.

# * Commas should be followed by one trailing space, but not a leading space.

# * Add a single space between the hashtag (#) and comment.

# * Parentheses, curly braces, and square bracket operators should not be
#   preceded or followed by a space.

# * Include a trailing space after ~.

# formatting: vertical spacing --------------------------------------------

# * Maintain one blank line between code blocks and comments.

# * Maintain one blank line between code blocks and comments. In your version
#   there were additional spaces prior to the section header.

# * Do not separate adjacent library() functions with a blank line.

# * Code within a single code block should not be separated by blank lines
#   unless it is separated by a comment

# formatting: line breaks -------------------------------------------------

# If X spans more than one line of code ...

#  * If a code block spans more than one line of code, add a new line after the
#   assignment operator.

# * If a function spans more than one line of code, the opening parentheses 
#   should be followed by a line break.

# * If a function spans more than one line of code, the closing parentheses 
#   should be followed placed on their own line.

# * Include no more than one prefix function per line of code.

# * If you provide three or more arguments to a function, place each argument
#   on its own line.

# * If you have more than one = in a function, place each argument on its own
#   line.

# * Code and comments should not be on the same line.

# * Code that follows a pipe operator should be placed on a new line.

# * Code and comments should not exceed 80 characters in width (if it is
#   avoidable) – add a line break, if possible.

# parsimony ---------------------------------------------------------------

# [[No points removed]] Code parsimony: Because dplyr is a member of the core
# tidyverse, it is not necessary to specify `dplyr::` when running a function
# from that package.

# [[No points removed]] Code parsimony: When supplying a single value, it is not
# necessary to wrap that value inside of `c()`. The `c` function combines
# multiple values with a vector.

# [[-0.30]] Code parsimony: In chained code, when the resultant object from the
# previous step is passed to the first argument of the next step, a placeholder
# is not necessary.

# Quotes

# [[No points removed]] Code parsimony: With `select()`, it is not necessary to
# place variable names inside of quotes.

# [[-0.10]] Code parsimony: When assigning names, unless the names include a
# space or other non-syntactic symbol, it is not necessary to put those names in
# quotes.

# [[No points removed]] Code parsimony: With `.by = ...`, it is not necessary to
# place variable names inside of quotes.

# [[No points removed]] Code parsimony: With `rename()`, it is not necessary to
# place variable names inside of quotes.

# Summarizing:

# [[No points removed]] Code parsimony: Given that you passed the data to
# summarize, `select()` was not necessary.

# [[No points removed]] Code parsimony: The comma at the end of `summarize()`
# was not necessary.

# Filtering:

# [[No points removed]] Code parsimony: The comma at the end of `filter()` was
# not necessary.

# [[No points removed]] Code parsimony: Unless arguments within a `filter()`
# have conflicting logical statements, it is not necessary to chain together
# multiple filters. Instead, place each logical test as an argument within the
# same `filter()`.

# [[No points removed]] Code parsimony: `filter()`, when used on a given
# variable, removes any NA values within that variable by default.

# Select and rename:

# [[-0.05]] Code parsimony: When removing just one field, it is typically more
# parsimonious to use negated selection.

# [[No points removed]] Code parsimony: Except in the case of negated selection,
# you do not need to supply variables to `select()` with `c()`.

# [[No points removed]] Code parsimony: You can rename multiple variables within
# a single `rename()`.

# GIS-specific issues -----------------------------------------------------

# [[-0.02]] Code parsimony: The first object sent to a non-spatial join
# determines the class of the output. As such the resultant object of your
# `left_join()` was a shapefile so `st_as_sf()` was not necessary.

# [[-0.5]] Incorrect: When converting a data frame to point shapefile, you must
# read in the data with the same CRS in which it was recorded. Here, that is
# EPSG 4326.

# random best practices ---------------------------------------------------

# [[No points removed]]: It is best practice to put `.by = ...` at either the
# start or end of the `summarize()` arguments (I prefer the end).

# [[No points removed]]: It is best practice to precede code with comments
# rather than place comments on the same line as the code that the comment is
# referring to.

# [[No points removed]] Always the tidyverse after other packages to ensure
# that the search path defaults to tidyverse function names.

# [[No points removed]] Coding best practices: If you specify the name of two or
# more arguments in a function, each argument should be placed on its own line
# (and on a separate line from the function call).

# [[No points removed]] Coding best practices: Although the magrittr pipe allows
# you to omit the parentheses, it is considered bad practice to do so. Note that
# the new base R pipe does not allow you to omit the parentheses.

# [[No points removed]] Coding best practices: When conducting a non-spatial
# join, it is best practice to provide the name of the key column.

# [[No points removed]]: This worked, but would have been easier to read if the
# code for left_join() was piped rather than nested.

# [[No points removed]]: Coding best practices: This worked, but we recommend
# always following a non-operator function with parentheses.

# [[No points removed]]: Coding best practices: Use column names, not numeric
# indices, when extracting data from a recursive object.
