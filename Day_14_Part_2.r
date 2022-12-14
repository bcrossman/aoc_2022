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
  drop_na(end_x)

floor <- (max(c(data$start_y,data$end_y)))+2
data <- 
  data %>% 
  bind_rows(tibble("start_x"=0, "start_y"=floor, "end_x"=1000, end_y = floor)) %>% 
  rowwise() %>% 
  mutate(points = list(paste(seq(start_x,end_x),seq(start_y,end_y), sep = ","))) %>% 
  ungroup()

rocks <- data$points %>% unlist()

playing_field <- 
  expand.grid(x =(min(c(data$start_x,data$end_x))-1):(max(c(data$start_x,data$end_x))+1),
              y =min(c(0,data$start_y,data$end_y)):(max(c(data$start_y,data$end_y)))+1) %>% 
  mutate(key = paste(x,y, sep=",")) %>% 
  mutate(filled = key %in% rocks) %>% 
  filter(filled) %>% 
  select(-key)

full <- FALSE
count <- 0
while(!full){
  count <- count+1
  # if(count==93){break}
  if(count%%500==0){print(count)}
  still_moving <- TRUE
  current_sand_x <- 500
  current_sand_y <- 0
  while(still_moving){
    
    check_down <-  length(playing_field$filled[(playing_field$x==current_sand_x)&(playing_field$y==(current_sand_y+1))])==1
    if(!check_down){
      
      playing_field$filled[(playing_field$x==(current_sand_x))&(playing_field$y==(current_sand_y))] <- FALSE
      current_sand_y <- current_sand_y+1
      playing_field <- bind_rows(playing_field[playing_field$filled,], data.frame(x=current_sand_x, y= current_sand_y, filled=TRUE))  
      still_moving <- TRUE
      next}
    
    check_down_left <- length(playing_field$filled[(playing_field$x==(current_sand_x-1))&(playing_field$y==(current_sand_y+1))])==1
    if(!check_down_left){
      playing_field$filled[(playing_field$x==(current_sand_x))&(playing_field$y==(current_sand_y))] <- FALSE
      current_sand_y <- current_sand_y+1
      current_sand_x <- current_sand_x-1
      playing_field <- bind_rows(playing_field[playing_field$filled,], data.frame(x=current_sand_x, y= current_sand_y, filled=TRUE))  
      still_moving <- TRUE
      next}
    
    check_down_right <- length(playing_field$filled[(playing_field$x==(current_sand_x+1))&(playing_field$y==(current_sand_y+1))])==1
    if(!check_down_right){
      playing_field$filled[(playing_field$x==(current_sand_x))&(playing_field$y==(current_sand_y))] <- FALSE
      current_sand_y <- current_sand_y+1
      current_sand_x <- current_sand_x+1
      playing_field <- bind_rows(playing_field[playing_field$filled,], data.frame(x=current_sand_x, y= current_sand_y, filled=TRUE))  
      still_moving <- TRUE
      next}
    
    still_moving <- FALSE
    if((current_sand_x==500)&(current_sand_y==0)){full <- TRUE}
  }
}

count


playing_field %>%
  ggplot(aes(x = x, y=-y, group=filled, fill=filled)) +
  geom_tile()+
  coord_fixed()

# playing_field %>% 
#   filter(y==1)
