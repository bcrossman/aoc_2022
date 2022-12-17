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

outline <- function(x,y,closest_x, closest_y){
  distance = (abs(x-closest_x) + abs(y - closest_y))+1
  covered_x = (x-distance):(x+distance)
  covered_y = c(y+(distance-abs((covered_x-x))),y-(distance-abs((covered_x-x))))
  covered_x = rep(covered_x,2)
  
  data.frame(x_check=covered_x, y_check=covered_y)
}

max = 4000000
min = -1

for(i in 1:nrow(input)){
  # i <- 1
  x <- input$x[i]
  y <- input$y[i]
  closest_x <- input$closest_x[i]
  closest_y <- input$closest_y[i]
  area_review <- 
  outline(x,y,closest_x, closest_y)
  
  check <- 
    area_review %>% 
    filter(max>x_check, x_check>min, max>y_check, y_check>min) %>% 
    full_join(input, by = character()) %>% 
    mutate(covered = ((abs(x-x_check) + abs(y - y_check))<=distance)) %>% 
    group_by(x_check, y_check) %>% 
    summarise(covered = sum(covered)) %>% 
    ungroup() %>% 
    arrange((covered)) %>% 
    filter(covered == 0)
  
  if(nrow(check>0)){break}

}

(check$x_check[1]*4000000+check$y_check[1]) %>% as.character()

