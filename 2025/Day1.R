
lines <- read_lines("2025/input1.txt")

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
  
  if (dial == 0 | dial == 100) {
    zeros <- zeros + 1
    dial <- 0
  } else if (dial > 100) {
    dial <- dial %% 100
  } else if (dial < 0) {
    dial <- 100 + dial
  }
  
  #print(paste0(start_dial, " -> ", turn, " -> ", dial))
  counter <- counter + 1
  #if (counter > 100) break
  
}
zeros
