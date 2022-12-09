library(tidyverse)

file <- "./Day_9/Part_1/input.txt"
input <- data.frame(value = readLines(file))

moves <- 
  input %>% 
  separate(col = value, 
           into = c("move", "move_num"), 
           sep = " ", 
           convert = T) 

number_of_knots <- 1
##initialize
knot_positions <- list()
for(i in 0:number_of_knots){
  knot_positions[[as.character(i)]] <- data.frame(x = 0, y = 0)
}

update_tail <- function(h_x, h_y, t_x, t_y){
  x_dist <- (h_x)-(t_x)
  y_dist <- (h_y)-(t_y)
  if((abs(x_dist)>1)&(abs(y_dist)>1)){
    t_x <- t_x+x_dist/2
    t_y <- t_y+y_dist/2
  }
  if((abs(x_dist)>1)&(abs(y_dist)==1)){
    t_x <- t_x+x_dist/2
    t_y <- t_y+y_dist
  }
  if((abs(x_dist)>1)&(abs(y_dist)==0)){
    t_x <- t_x+x_dist/2
  }
  if((abs(x_dist)==1)&(abs(y_dist)>1)){
    t_y <- t_y+y_dist/2
    t_x <- t_x+x_dist
  }
  if((abs(x_dist)==0)&(abs(y_dist)>1)){
    t_y <- t_y+y_dist/2
  }
  
  return(list(t_x,t_y))
}

output <- data.frame(x=c(), y=c())
for(i in 1:nrow(moves)){
  print(i)
  if(moves$move[i]%in% c("L", "R")){
    for(j in seq(moves$move_num[i])){
      x_move <- if_else(moves$move[i] == "L",-1,1)
      curr_x = knot_positions[["0"]]$x
      curr_y = knot_positions[["0"]]$y
      knot_positions[["0"]] = data.frame(x = curr_x+x_move, y = curr_y)
      for(k in 1:number_of_knots){
        # k <- 1
        new_tail <- update_tail(h_x = knot_positions[[as.character(k-1)]]$x,
                                h_y = knot_positions[[as.character(k-1)]]$y,
                                t_x = knot_positions[[as.character(k)]]$x,
                                t_y = knot_positions[[as.character(k)]]$y)
        knot_positions[[as.character(k)]] <- data.frame(x =new_tail[[1]], 
                                                        y = new_tail[[2]])
      }
      output <- bind_rows(output,knot_positions[[as.character(number_of_knots)]]) %>% distinct_all()
    }
  }
  if(moves$move[i]%in% c("U", "D")){
    for(j in seq(moves$move_num[i])){
      y_move <- if_else(moves$move[i] == "D",-1,1)
      curr_x = knot_positions[["0"]]$x
      curr_y = knot_positions[["0"]]$y
      knot_positions[["0"]] = data.frame(x = curr_x, y = curr_y+y_move)
      for(k in 1:number_of_knots){
        # k <- 3
        # if(i==2&j==4&k==6){asdfasd}
        new_tail <- update_tail(h_x = knot_positions[[as.character(k-1)]]$x,
                                h_y = knot_positions[[as.character(k-1)]]$y,
                                t_x = knot_positions[[as.character(k)]]$x,
                                t_y = knot_positions[[as.character(k)]]$y)
        knot_positions[[as.character(k)]] <- data.frame(x =new_tail[[1]], 
                                                        y = new_tail[[2]])
      }
      output <- bind_rows(output,knot_positions[[as.character(number_of_knots)]]) %>%  distinct_all()
    }
  }
}

output %>% 
  distinct_all() %>% 
  nrow()
