# Part 1:

library(tidyverse)

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
  for (num in 1:(digits - 1)) {
    if (digits %% num == 0) {
      factors <- c(factors, as.numeric(num))
  }
  }
  factors
}

test_repeating <- function(number) {
  factors <- get_factors(number)
  invalid <- FALSE
  for (fac in factors) {
    pattern <- str_glue(".{{{fac}}}")
    strings <- str_extract_all(format(number, scientific = FALSE), pattern)
    #print(strings)
    if (n_distinct(strings[[1]]) == 1) {
      invalid <- TRUE
      break
    }
  }
  invalid
}

id_ranges <- read_lines("2025/inputs/input2-test.txt") |> str_split(",")
invalid_sum <- 0
for (rang in id_ranges[[1]]) {
  tests <- range_vector(rang)
  for (test in tests) {
    if (test_repeating(test)) {
      invalid_sum <- invalid_sum + test
      print(paste0(test, " is invalid"))
    }
  }
}
invalid_sum



# I'm dumb. We don't care about ALL factors, we only need to divide the number
# in half to check if a number is repeated TWICE:
split_num <- function (number) {
  digits <- str_length(format(number, scientific = FALSE))
  if (digits %% 2 == 0) {
    return(digits / 2)
  } else {
    return(NULL)
  }
}

test_repeating <- function(number) {
  splitpoint <- split_num(number)
  if (is.null(splitpoint)) return(FALSE)
  pattern <- str_glue(".{{{splitpoint}}}")
  strings <- str_extract_all(format(number, scientific = FALSE), pattern)
  #print(strings)
  if (n_distinct(strings[[1]]) == 1) {
    return(TRUE)
  } else return(FALSE)
}

id_ranges <- read_lines("2025/inputs/input2.txt") |> str_split(",")
invalid_sum <- 0
for (rang in id_ranges[[1]]) {
  print(paste0("Starting range ", rang))
  tests <- range_vector(rang)
  for (test in tests) {
    if (test_repeating(test)) {
      invalid_sum <- invalid_sum + test
      #print(paste0(test, " is invalid"))
    }
  }
  print(paste0("Done with range ", rang))
}
invalid_sum

