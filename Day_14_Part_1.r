library(unglue)
library(tidyverse)

file <- "./Day_14/Part_1/input.txt"

input <- readLines(file)

data <- 
  data.frame(map = input) %>% 
  rowid_to_column() %>% 
  separate_rows(map, sep = " -> ") %>% 
  separate(map, into=c("start_x","start_y"), convert = T) %>% 
  group_by(rowid) %>% 
  mutate(end_x = lead(start_x),
         end_y = lead(start_y)) %>% 
  drop_na(end_x) %>% 
  rowwise() %>% 
  mutate(points = list(paste(seq(start_x,end_x),seq(start_y,end_y), sep = ","))) %>% 
  ungroup()

rocks <- data$points %>% unlist()


playing_field <- 
  expand.grid(x =(min(c(data$start_x,data$end_x))-1):(max(c(data$start_x,data$end_x))+1),
              y =min(c(0,data$start_y,data$end_y)):(max(c(data$start_y,data$end_y)))+1) %>% 
  mutate(key = paste(x,y, sep=",")) %>% 
  mutate(filled = key %in% rocks)
# 
# update_field <- function(current_sand_x, current_sand_y, playing_field){
#   # current_sand_x = 500
#   # current_sand_y = 0
#   # playing_field <- playing_field
#   
#   check_down <-  playing_field$filled[(playing_field$x==current_sand_x)&(playing_field$y==(current_sand_y+1))]
#   if(!check_down){
#     playing_field$filled[(playing_field$x==current_sand_x)&(playing_field$y==(current_sand_y+1))]<- TRUE
#     still_moving <- TRUE
#     return(playing_field)}
#   
#   check_down_left <- playing_field$filled[(playing_field$x==(current_sand_x-1))&(playing_field$y==(current_sand_y+1))]
#   if(!check_down_left){
#     playing_field$filled[(playing_field$x==(current_sand_x-1))&(playing_field$y==(current_sand_y+1))] <- TRUE
#     still_moving <- TRUE
#     return(playing_field)}
#   
#   check_down_right <- playing_field$filled[(playing_field$x==(current_sand_x-1))&(playing_field$y==(current_sand_y+1))]
#   if(!check_down_right){
#     playing_field$filled[(playing_field$x==(current_sand_x+1))&(playing_field$y==(current_sand_y+1))] <- TRUE
#     still_moving <- TRUE
#     return(playing_field)}
#   
#   playing_field$filled[(playing_field$x==(current_sand_x))&(playing_field$y==(current_sand_y))] <- TRUE
#   still_moving <- FALSE
#   if(!still_moving){browser()}
#   return(playing_field)
# }

floor <- max(playing_field$y)-1
abyss <- FALSE
count <- 0
while(!abyss){
  count <- count+1
  # if(count==6){break}
  # print(count)
  still_moving <- TRUE
  current_sand_x <- 500
  current_sand_y <- 0
  while(still_moving){
    
    if(playing_field %>% 
       slice_max(y) %>% 
       pull(filled) %>% 
       any()){
      still_moving <- FALSE
      next
    }
    
    check_down <-  playing_field$filled[(playing_field$x==current_sand_x)&(playing_field$y==(current_sand_y+1))]
    if(!check_down){
      playing_field$filled[(playing_field$x==(current_sand_x))&(playing_field$y==(current_sand_y))] <- FALSE
      current_sand_y <- current_sand_y+1
      playing_field$filled[(playing_field$x==current_sand_x)&(playing_field$y==(current_sand_y))]<- TRUE
      still_moving <- TRUE
      next}
    
    check_down_left <- playing_field$filled[(playing_field$x==(current_sand_x-1))&(playing_field$y==(current_sand_y+1))]
    if(!check_down_left){
      playing_field$filled[(playing_field$x==(current_sand_x))&(playing_field$y==(current_sand_y))] <- FALSE
      current_sand_y <- current_sand_y+1
      current_sand_x <- current_sand_x-1
      playing_field$filled[(playing_field$x==(current_sand_x))&(playing_field$y==(current_sand_y))] <- TRUE
      still_moving <- TRUE
      next}
    
    check_down_right <- playing_field$filled[(playing_field$x==(current_sand_x+1))&(playing_field$y==(current_sand_y+1))]
    if(!check_down_right){
      playing_field$filled[(playing_field$x==(current_sand_x))&(playing_field$y==(current_sand_y))] <- FALSE
      current_sand_y <- current_sand_y+1
      current_sand_x <- current_sand_x+1
      playing_field$filled[(playing_field$x==(current_sand_x))&(playing_field$y==(current_sand_y))] <- TRUE
      still_moving <- TRUE
      next}
    
    still_moving <- FALSE
    
  }
  if(playing_field %>% 
     slice_max(y) %>% 
     pull(filled) %>% 
     any()){abyss <- TRUE}
  
  # playing_field %>%
  #   ggplot(aes(x = x, y=-y, group=filled, fill=filled)) +
  #   geom_tile()+
  #   coord_fixed()
}

count-1

