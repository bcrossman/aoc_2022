library(unglue)
library(tidyverse)

file <- "./Day_5/Part_1/input.txt"

file_length <- 9

stacks <- as.list(read_fwf(col_positions = fwf_widths(rep(4,file_length)), 
                   file = file)[1:file_length,])
moves <- 
  unglue_data(readLines(file), 
              "move {move_num} from {from} to {to}",
              convert = TRUE) %>% 
  drop_na(move_num)

##Part 1

for(i in 1:nrow(moves)){
  #i <- 1
  col_start <- stacks[[moves$from[i]]] %>% na.omit()
  col_remain <- col_start[-(1:moves$move_num[i])]
  col_move <- col_start[moves$move_num[i]:1]
  col_new <- c(col_move,stacks[[moves$to[i]]] %>% na.omit())
  stacks[[moves$to[i]]] <- col_new
  stacks[[moves$from[i]]] <- col_remain
  }

map_chr(stacks, ~.x[1]) %>% 
  paste0(collapse="") %>% 
  gsub(x = ., "\\[|\\]", replacement = "")


## Part 2
stacks <- as.list(read_fwf(col_positions = fwf_widths(rep(4,file_length)), 
                           file = file)[1:file_length,])
for(i in 1:nrow(moves)){
  #i <- 1
  col_start <- stacks[[moves$from[i]]] %>% na.omit()
  col_remain <- col_start[-(1:moves$move_num[i])]
  col_move <- col_start[1:moves$move_num[i]]
  col_new <- c(col_move,stacks[[moves$to[i]]] %>% na.omit())
  stacks[[moves$to[i]]] <- col_new
  stacks[[moves$from[i]]] <- col_remain
}

map_chr(stacks, ~.x[1]) %>% 
  paste0(collapse="") %>% 
  gsub(x = ., "\\[|\\]", replacement = "")
