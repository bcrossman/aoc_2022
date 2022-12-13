library(tidyverse)

file <- "./Day_10/Part_1/input.txt"
input <- data.frame(value = readLines(file))

cycle_length <- tibble("action"=  c("noop", "addx"), "length"=c(1,2))

instructions <- 
  input %>% 
  separate(col = value, 
           into = c("action", "x_inc"), 
           sep = " ", 
           convert = T) %>% 
  mutate(x_inc = replace_na(x_inc, 0)) %>% 
  left_join(cycle_length) %>% 
  mutate(cycle_end = cumsum(length))

complete <- 
  left_join(tibble("cycle_end"  = 1:max(instructions$cycle_end)),
            instructions) %>% 
  fill(action, .direction = "up") %>% 
  mutate(x_inc = replace_na(x_inc, 0)) %>% 
  mutate(x = cumsum(x_inc)+1) %>% 
  mutate(cycle_end = cycle_end+1) %>% 
  mutate(signal = cycle_end*x) 

## Part 1
complete %>% 
  filter(cycle_end %in% c(20, 60, 100, 140, 180, 220)) %>% 
  pull(signal) %>% 
  sum()

complete %>% 
  mutate(beam = if_else(cycle_end %% 40==0,40,cycle_end %% 40)) %>% 
  mutate(row =  cycle_end %/% 40) %>%# if_else(cycle_end %/% 40==0,40,cycle_end %% 40)) %>% 
  filter((beam <(x+3))&beam >=(x)) %>% 
  ggplot(aes(x = beam, y = -row)) + 
  geom_tile() +  
  coord_fixed()
