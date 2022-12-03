library(tidyverse)

sacks <- as_tibble(readLines("./Day_3/Part_1/input.txt"))

## Part 1

mapping <- tibble("item"=c(letters, LETTERS),"score" = c(1:52))

sacks %>% 
  mutate(left = str_sub(value,1,str_length(value)/2)) %>% 
  mutate(right = str_sub(value,start = 1+str_length(value)/2)) %>% 
  rowwise() %>% 
  mutate(item = intersect(unlist(strsplit(left,"")),unlist(strsplit(right,"")))) %>% 
  ungroup() %>% 
  left_join(mapping, by = "item") %>% 
  pull(score) %>% 
  sum()

##Part 2

sacks %>% 
  mutate(group = rep(1:(nrow(sacks)/3), each=3)) %>% 
  mutate(sack_num = rep(1:3, times=(nrow(sacks)/3))) %>% 
  pivot_wider(id_cols = group, names_from = sack_num, values_from = value) %>% 
  setNames(c("group", "left", "middle","right")) %>% 
  rowwise() %>% 
  mutate(item = intersect(intersect(unlist(strsplit(left,"")),
                          unlist(strsplit(middle,""))),
                          unlist(strsplit(right,"")))) %>% 
  ungroup() %>% 
  left_join(mapping, by = "item") %>% 
  pull(score) %>% 
  sum()
