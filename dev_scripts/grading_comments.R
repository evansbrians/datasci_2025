# [[-0.25]] Incorrect answer: When you work in an RStudio project you do not
# need to supply the absolute file path to the data that you read in. 
# Working in a project allows you to read in the data with a relative file
# path -- in other words, the location of the file relative to the top 
# level of the project folder. As written, this code will not work on my
# computer (because I do not share your absolute file path).

# [[-0.5]] Missed the grouping argument `.by = ...`.

# [[-0.25]] Incorrect: Because of NA values, warning messages were produced when
# plotting the data.

# [[-0.50]] Include only assignments specified in the question.

# [[-0.125]] Incorrect: You do not include enough comments to navigate the 
# the more complex elements of this code block.

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

# formatting --------------------------------------------------------------

# [[-0.05]]: Code formatting: Assigned names should be written in snake_case.

# Horizontal spacing

# [[-0.025]] Code formatting: Indentation is off.

# [[-0.025]] Code formatting: Infix functions should be separated from
# surrounding code with a single leading and trailing space.

# [[No points removed]] Code formatting: The extraction ($) and colon (:, ::)
# infix operators should not be preceded or followed by a space.

# [[No points removed]] Code formatting: Commas should be followed by one
# trailing space, but not a leading space.

# [[-0.025]] Code formatting: Add a single space between the hashtag (#) and
# comment.

# [[-0.025]] Code formatting: Parentheses ((...)), curly braces {...}, and
# square bracket (extraction: [], [[]]) operators should not be preceded or
# followed by a space.

# Line breaks:

# [[No points removed]] Code formatting: Code that follows a pipe operator
# should be placed on a new line.

# [[No points removed]] Code formatting: Code and comments should not exceed 80
# characters in width (if it is avoidable) – add a line break, if possible.

# [[-0.025]] Code formatting: Include no more than one prefix function per line of code.

# [[-0.025]] Code formatting: Maintain one blank line between code blocks and comments.

# [[-0.05]] Code formatting: If you provide three or more arguments to a function, place each argument on its own line.

# [[-0.025]] Code formatting: Do not separate adjacent library() functions with a blank line.

# [[-0.05]] Code formatting:  If a code block spans more than one line of code, add a new line after the assignment operator.

# Functions:

# [[No points removed]] Code formatting: Spell out TRUE and FALSE in a
# function’s argument.

# [[-0.10]] Code formatting: Include a trailing space if ~ is
# followed by function.

# parsimony ---------------------------------------------------------------

# [[-0.015]] Code parsimony: When assigning names, unless the names include a
# space or other non-syntactic symbol, it is not necessary to put those names in
# quotes.

# [[-0.05]] Code parsimony: When removing just one field, it is typically more
# parsimonious to use negated selection.

# [[-0.02]] Code parsimony: The first object sent to a non-spatial join
# determines the class of the output. As such the resultant object of your
# `left_join()` was a shapefile so `st_as_sf()` was not necessary.

# [[No points removed]] Code parsimony: When supplying a single value, it is not
# necessary to wrap that value inside of `c()`. The `c` function combines
# multiple values with a vector.

# [[-0.30]] Code parsimony: In chained code, when the resultant object from the
# previous step is passed to the first argument of the next step, a placeholder
# is not necessary.

# Summarizing:

# [[No points removed]] Code parsimony: The comma at the end of `summarize()`
# was not necessary.

# [[No points removed]] Code parsimony: Given that you passed the data to
# summarize, `select()` was not necessary.

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

# [[No points removed]] Code parsimony: Except in the case of negated selection,
# you do not need to supply variables to `select()` with `c()`.

# [[No points removed]] Code parsimony: With `select()`, it is not necessary to
# place variable names inside of quotes.

# [[No points removed]] Code parsimony: You can rename multiple variables within
# a single `rename()`.

# Grouping:

# [[No points removed]] Code parsimony: With `.by = ...`, it is not necessary to
# place variable names inside of quotes.

# functions you may use ---------------------------------------------------

# [[-1.25]] `labs()` is not among the functions that you may use for this assignment.

# [[-1.25]] `labs()` and `element_text()` are not among the functions that you
# may use for this assignment.

# [[-0.5]] The `&` operator is not among the functions that you may use for this
# assignment.

# [[-0.5]] Neither `scale_x_discrete()` nor `theme_minimal()` are among the
# functions that you may use for this assignment.

# [[-0.5]] Neither the negation operator (`!`) nor `is.na` are functions that
# you may use in this assignment.

# [[-0.5]] The negation operator (`!`), `is.na`, and `&` are not among the
# functions that you may use for this assignment.

# [[-0.75]] `group_by()` is not among the functions that you may use for this
# assignment.

# [[-1.25]] The negation operator (`!`), `is.na`, `recode`, `theme_minimal` and
# `labs` are not functions that you may use in this assignment.

# [[-1.25]] `factor()`,`labs()`, `element_text()`, and `element_rect()` are not
# among the functions that you may use for this assignment.