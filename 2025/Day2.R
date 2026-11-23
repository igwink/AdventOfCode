# Part 1:

library(tidyverse)

test = "11-22"

# Function to get a range based on two ID numbers:
range_vector <- function (rang) {
  splits <- str_split(rang, "-")
  as.numeric(splits[[1]][1]):as.numeric(splits[[1]][2])
} 

# Test a number to see what patterns need to be checked:
# E.g. For a 2-digit number only need to check if first and last match
# We need to get the multiples of the number of digits to know what to check:
get_factors <- function (number) {
  digits <- str_length(format(number, scientific = FALSE))
  factors <- as.numeric()
  for (num in 1:digits) {
    if (digits %% num == 0) {
      factors <- c(factors, as.numeric(num))
  }
  }
  factors
}

range_vector("10-200")
get_factors(400000000000)

test_repeating <- function(number) {
  factors <- get_factors(number)
  invalid <- FALSE
  for (fac in factors) {
    pattern <- str_glue(".{{{fac}}}")
    strings <- str_extract_all(format(number, scientific = FALSE), pattern)
    if (n_distinct(strings[[1]]) == 1) {
      invalid <- TRUE
      break
    }
  }
  invalid
}


# Scratchwork:
test_repeating(10)
strings <- str_extract_all("123123", ".{3}")



factors <- get_factors(10)
invalid <- FALSE
for (fac in factors) {
  pattern <- str_glue(".{{{1}}}")
  strings <- str_extract_all(format(10, scientific = FALSE), pattern)
  n_distinct(strings[[1]])
  if (n_distinct(strings) == 1) {
    invalid <- TRUE
    break
  }
}
invalid
strings
