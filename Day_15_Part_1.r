library(unglue)
library(tidyverse)

file <- "./Day_15/Part_1/input.txt"

input <- 
  unglue_data(readLines(file), 
              c("Sensor at x={x}, y={y}: closest beacon is at x={closest_x}, y={closest_y}"),
              convert = TRUE) 

area <- function(x,y,closest_x, closest_y){
  distance = abs(x-closest_x) + abs(y - closest_y)
  covered_x = (x-distance):(x+distance)
  covered_y = (y-distance):(y+distance)
  expand.grid(covered_x=covered_x, covered_y=covered_y) %>% 
    filter((abs(x-covered_x) + abs(y - covered_y))<=distance) %>% 
    filter(!(covered_x==closest_x & covered_y==closest_y))
}

input %>% 
  rowwise() %>% 
  mutate(covered = list(area(x,y,closest_x, closest_y))) %>% 
  ungroup() %>% 
  unnest(covered) %>% 
  # filter(x == 8 & y==7) %>% 
  # View()
  filter(covered_y ==2000000) %>% 
  pull(covered_x) %>% 
  unique() %>% 
  length()

