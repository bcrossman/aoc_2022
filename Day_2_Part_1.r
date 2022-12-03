library(tidyverse)

strat <- as_tibble(readLines("./Day_2/Part_1/input.txt"))

## Part 1

mapping <- tibble("key"=c("A","B","C","X","Y","Z"),"point" = c(1,2,3,1,2,3))

strat %>% 
  separate(col = value, into = c("elf","me")) %>% 
  left_join(mapping, by = c("elf" = "key")) %>% 
  left_join(mapping, by = c("me" = "key"), suffix = c(".elf", ".me")) %>% 
  mutate(score = if_else(
    !(point.me*point.elf==3), 
    3*(point.me-point.elf)+3+point.me,
    1.5*(-point.me+point.elf)+3+point.me)) %>% 
  pull(score) %>% 
  sum()

pull(total_calories) 

## Part 2

strat %>% 
  separate(col = value, into = c("elf","result")) %>% 
  mutate(me = if_else(result=="Y", elf,
                      if_else(result=="Z")))

mapping <- tibble("key"=c("A","B","C","X","Y","Z"),"point" = c(1,2,3,1,2,3))

strat %>% 
  separate(col = value, into = c("elf","result")) %>% 
  left_join(mapping, by = c("elf" = "key")) %>% 
  rename(point.elf = point) %>% 
  mutate(point.me = case_when(
    result=="Y" ~ point.elf,
    result=="Z" ~ if_else((point.elf+1)>3,1, point.elf+1),
    result=="X" ~ if_else((point.elf-1)<1,3, point.elf-1))) %>% 
  # left_join(mapping, by = c("me" = "key"), suffix = c(".elf", ".me")) %>% 
  mutate(score = if_else(
    !(point.me*point.elf==3), 
    3*(point.me-point.elf)+3+point.me,
    1.5*(-point.me+point.elf)+3+point.me)) %>% 
  pull(score) %>% 
  sum()
