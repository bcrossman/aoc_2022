library(unglue)
library(tidyverse)
library(gganimate)
library(gifski)

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

gganimate_list <- list()
floor <- max(playing_field$y)-1
abyss <- FALSE
count <- 0
subcount <- 0
while(!abyss){
  count <- count+1
  # if(count==6){break}
  print(count)
  still_moving <- TRUE
  current_sand_x <- 500
  current_sand_y <- 0
  while(still_moving){
    subcount <- subcount+1
    if(playing_field %>% 
       slice_max(y) %>% 
       pull(filled) %>% 
       any()){
      still_moving <- FALSE
      next
    }
    gganimate_list[[as.character(subcount)]] <-  playing_field
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

}

count-1


gganimate_dots <- 
  bind_rows(gganimate_list, .id = "id") %>% 
  mutate(id = as.numeric(id))

gif <- 
  gganimate_dots %>% 
  # filter(id == max(id)) %>% 
  ggplot(aes(x = x, y = -y, group=filled, fill=filled)) + 
  geom_tile() +  
  coord_fixed() +
  transition_states(id)

# transition_length = 1, 
#                     state_length = c(rep(1, 
#                                          length(unique(gganimate_dots$id))-1),
#                                      99))+
#   view_follow(aspect_ratio = 1)

# gif

anim_save(animation = gif, filename = "aoc_day_13.gif", renderer=gifski_renderer())
          
