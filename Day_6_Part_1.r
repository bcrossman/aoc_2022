library(tidyverse)

file <- "./Day_6/Part_1/input.txt"

input <- read_lines(file)

signal <- data.frame(str_split(input, pattern = ""))

message_finder <- function(signal, window_len){
  for(i in 1:(window_len-1)){
    signal <- cbind.data.frame(signal, lag(signal[,1],i))
  }

signal %>% 
  set_names(LETTERS[1:window_len]) %>% 
  rowid_to_column() %>%
  pivot_longer(cols = -rowid, names_to = "colid", values_to = "value") %>% 
  group_by(rowid) %>% 
  summarise(value = window_len == n_distinct(value, na.rm = T)) %>% 
  filter(value) %>% 
  slice(1) %>% 
  pull(rowid)
}

## Part 1
message_finder(signal = signal, window_len = 4)

## Part 2
message_finder(signal = signal, window_len = 14)
