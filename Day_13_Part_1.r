library(tidyverse)

file <- "./Day_13/Part_1/ex.txt"
input <- readLines(file)

data <- 
  data.frame(input = input) %>% 
  mutate(pairs = cumsum(input == "")) %>% 
  filter(input != "") %>% 
  group_by(pairs) %>% 
  mutate(id = row_number()) %>% 
  pivot_wider(id_cols = pairs, names_from = id, values_from = input)

left <- data$`1`[1]
right <- data$`2`[1]

parse_two_strings <- function(left, right){
  new_string_left <- str_extract_all(left, pattern, simplify = T)  
  
  if()
  
}

for(i in )

pattern <- "(?<=^\\[).+(?=\\]$)"


unlist(strsplit(left, split=",")) %>% as.numeric

