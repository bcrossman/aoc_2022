library(tidyverse)

file <- "./Day_9/Part_1/input.txt"
input <- data.frame(value = readLines(file))

moves <- 
  input %>% 
  separate(col = value, 
           into = c("move", "move_num"), 
           sep = " ", 
           convert = T) 

h_x <- 0
h_y <- 0
t_x <- 0
t_y <- 0

tail_x_pos <- c()
tail_y_pos <- c()

knot_positions <- list()

knot_positions[["0"]] <- data.frame(x = 0, y = 0)


for(i in 1:nrow(moves)){
  # i <- 1
  if(moves$move[i]%in% c("L", "R")){
    for(j in seq(moves$move_num[i])){
      x_move <- if_else(moves$move[i] == "L",-1,1)
      h_x = h_x + x_move
      x_dist <- (h_x)-(t_x)
      y_dist <- (h_y)-(t_y)
      
      if(abs(x_dist)>1){
        if(sqrt(x_dist^2+y_dist^2)>1){
          t_y <- t_y+y_dist
        }
        t_x <- t_x+x_move
      }
      tail_x_pos <- c(tail_x_pos,t_x)
      tail_y_pos <- c(tail_y_pos,t_y)
    }
  }
  if(moves$move[i]%in% c("U", "D")){
    for(j in seq(moves$move_num[i])){
      y_move <- if_else(moves$move[i] == "D",-1,1)
      h_y = h_y + y_move
      x_dist <- (h_x)-(t_x)
      y_dist <- (h_y)-(t_y)
      
      if(abs(y_dist)>1){
        if(sqrt(x_dist^2+y_dist^2)>1){
          t_x <- t_x+x_dist
        }
        t_y <- t_y+y_move
      }
      tail_x_pos <- c(tail_x_pos,t_x)
      tail_y_pos <- c(tail_y_pos,t_y)
    }
  }
}

cbind.data.frame(tail_x_pos, tail_y_pos) %>% 
  distinct_all() %>% 
  nrow()