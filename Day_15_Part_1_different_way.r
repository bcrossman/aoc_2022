library(unglue)
library(tidyverse)

file <- "./Day_15/Part_1/input.txt"

input <- 
  unglue_data(readLines(file), 
              c("Sensor at x={x}, y={y}: closest beacon is at x={closest_x}, y={closest_y}"),
              convert = TRUE) %>% 
  mutate(distance = abs(x-closest_x) + abs(y - closest_y),
         highest_x = x + distance,
         lowest_x = x - distance)

row_interest=2000000

area_review <- expand.grid(x_check = min(input$lowest_x):max(input$highest_x),
                           y_check = row_interest)

area_review %>% 
  full_join(input, by = character()) %>% 
  mutate(covered = ((abs(x-x_check) + abs(y - y_check))<=distance) & 
           !((x_check==closest_x)&(y_check==closest_y))) %>% 
  group_by(x_check,y_check) %>% 
  summarise(covered = any(covered)) %>% 
  ungroup() %>% 
  pull(covered) %>% 
  sum()


