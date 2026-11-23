library(tidyverse)

# Part 1:
lines <- read_lines("2025/inputs/input1.txt")

dial <- 50
zeros <- 0
counter <- 0


for (turn in lines) {
  start_dial <- dial
  dir <- str_sub(turn, start = 1, end = 1)
  clicks <- as.numeric(str_sub(turn, start = 2, end = -1))
  
  if (dir == "R") {
    dial <- dial + clicks
  } else {
    dial <- dial - clicks
  }
  
  if (dial %% 100 == 0) {
    zeros <- zeros + 1
    dial <- 0
  } else if (dial > 100) {
    dial <- dial %% 100
  } else if (dial < 0) {
    dial <- dial %% 100
  }
  
  #print(paste0(start_dial, " -> ", turn, " -> ", dial))
  counter <- counter + 1
  #if (counter > 100) break
  
}
zeros

# Part 2:
lines <- read_lines("2025/inputs/input1.txt")

turn_dial <- function (turn, start) {
  dir <- str_sub(turn, start = 1, end = 1)
  clicks <- as.numeric(str_sub(turn, start = 2, end = -1))
  new_zeros <- 0
  
  if (dir == "R") {
    dial <- start + clicks
  } else {
    dial <- start - clicks
  }
  
  if (dial == 0) {
    new_zeros <- 1
  } else if (dial > 0) {
    new_zeros <- dial %/% 100
    dial <- dial %% 100
  } else if (dial < 0) {
    new_zeros <- (dial %/% 100) * -1
    dial <- dial %% 100
    if (start == 0 & dial != 0) new_zeros <- new_zeros - 1
    if (dial == 0) new_zeros <- new_zeros + 1
  }
  
  list(dial = dial, zeros = new_zeros)
}

dial <- 50
zeros <- 0
counter <- 0


for (turn in lines) {
  res <- turn_dial(turn, dial)
  dial <- res$dial
  zeros <- zeros + res$zeros
}

zeros

