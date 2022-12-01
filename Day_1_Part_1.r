library(tidyverse)

calories <- as_tibble(as.numeric(readLines("./Day_1/Part_1/input.txt")))

## Part 1

calories %>% 
  mutate(elf = cumsum(is.na(value))) %>% 
  drop_na(value) %>% 
  group_by(elf) %>% 
  summarise(total_calories = sum(value)) %>% 
  arrange(desc(total_calories)) %>% 
  slice(1) %>% 
  pull(total_calories) 

## Part 2
calories %>% 
  mutate(elf = cumsum(is.na(value))) %>% 
  drop_na(value) %>% 
  group_by(elf) %>% 
  summarise(total_calories = sum(value)) %>% 
  arrange(desc(total_calories)) %>% 
  slice(1:3) %>% 
  pull(total_calories) %>% 
  sum()
