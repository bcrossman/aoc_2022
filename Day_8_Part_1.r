library(tidyverse)

file <- "./Day_8/Part_1/input.txt"
input <- data.frame(value = readLines(file))

create_grid <- function(df){
  
  df %>% 
    rowid_to_column() %>%
    separate_rows(value, sep="", convert = T) %>% 
    drop_na(value) %>% 
    filter(value != "") %>% 
    group_by(rowid) %>% 
    mutate(colid = row_number()) %>% 
    ungroup() %>% 
    mutate(key = paste(rowid, colid, sep=",")) %>% 
    mutate(value = as.numeric(value))
}

data <- create_grid(input)

## Part 1
data %>% 
  group_by(rowid) %>% 
  arrange(colid) %>% 
  mutate(max_height = cummax(value)) %>% 
  mutate(is_visible = value>lag(max_height,1,default = -9999)) %>% 
  group_by(rowid) %>% 
  arrange(desc(colid)) %>% 
  mutate(max_height = cummax(value)) %>% 
  mutate(is_visible = is_visible|(value>lag(max_height,1,default = -9999))) %>% 
  group_by(colid) %>% 
  arrange(rowid) %>% 
  mutate(max_height = cummax(value)) %>% 
  mutate(is_visible = is_visible|(value>lag(max_height,1,default = -9999))) %>%
  group_by(colid) %>% 
  arrange(desc(rowid)) %>% 
  mutate(max_height = cummax(value)) %>% 
  mutate(is_visible = is_visible|(value>lag(max_height,1,default = -9999))) %>%
  filter(is_visible) %>% 
  nrow()

## Part 2

left_count <- function(i){
  # i <- "4,3"
  
  vec <- 
    data %>% 
    filter(rowid == data$rowid[data$key==i],
           colid < data$colid[data$key==i]) 
  
  if(nrow(vec)==0) {
    return(0)
  }else{
    
    return(
      vec %>% 
        arrange(colid) %>% 
        mutate(can_see_over = (value-data$value[data$key==i])<0) %>%
        mutate(group = cumsum(can_see_over!=lag(can_see_over,1, TRUE))) %>% 
        mutate(starting_group = min(group)) %>% 
        slice_max(group) %>% 
        mutate(can_see_over = ifelse((group==min(starting_group))&(row_number()==min(row_number())),FALSE,can_see_over)) %>% 
        pull(can_see_over) %>% 
        sum()+1
    )
  }
}
right_count <- function(i){
  
  vec <- 
    data %>%
    filter(rowid == data$rowid[data$key==i],
           colid > data$colid[data$key==i]) 
    
    if(nrow(vec)==0) {
      return(0)}else{
        
        return(
          vec %>% 
            arrange(desc(colid)) %>% 
            mutate(can_see_over = (value-data$value[data$key==i])<0) %>%
            mutate(group = cumsum(can_see_over!=lag(can_see_over,1, TRUE))) %>% 
            mutate(starting_group = min(group)) %>% 
            slice_max(group) %>% 
            mutate(can_see_over = ifelse((group==min(starting_group))&(row_number()==min(row_number())),FALSE,can_see_over)) %>% 
            pull(can_see_over) %>% 
            sum()+1
        )
      }
}
down_count <- function(i){
  vec <- 
    data %>%
    filter(rowid > data$rowid[data$key==i],
           colid == data$colid[data$key==i]) 
  
  if(nrow(vec)==0) {
    return(0)}else{
      
      return(
        vec %>% 
          arrange(desc(rowid)) %>% 
          mutate(can_see_over = (value-data$value[data$key==i])<0) %>%
          mutate(group = cumsum(can_see_over!=lag(can_see_over,1, TRUE))) %>% 
          mutate(starting_group = min(group)) %>% 
          slice_max(group) %>% 
          mutate(can_see_over = ifelse((group==min(starting_group))&(row_number()==min(row_number())),FALSE,can_see_over)) %>% 
          pull(can_see_over) %>% 
          sum()+1
      )
    }
}

up_count <- function(i){
  
  vec <- 
    data %>% 
    filter(rowid < data$rowid[data$key==i],
           colid == data$colid[data$key==i]) 
  
  if(nrow(vec)==0) {
    return(0)}else{
      
      return(
        vec %>% 
          arrange((rowid)) %>% 
          mutate(can_see_over = (value-data$value[data$key==i])<0) %>%
          mutate(group = cumsum(can_see_over!=lag(can_see_over,1, TRUE))) %>% 
          mutate(starting_group = min(group)) %>% 
          slice_max(group) %>% 
          mutate(can_see_over = ifelse((group==min(starting_group))&(row_number()==min(row_number())),FALSE,can_see_over)) %>% 
          pull(can_see_over) %>% 
          sum()+1
      )
    }
}
data$left <- 0
data$right <- 0
data$up <- 0
data$down <- 0

for(i in 1:nrow(data)){
  # i <- 18
  print(data$key[i])
  data$left[i] <- left_count(data$key[i])
  data$right[i] <- right_count(data$key[i])
  data$up[i] <- up_count(data$key[i])
  data$down[i] <- down_count(data$key[i])
}

data %>% 
  mutate(total_view = left*right*up*down) %>% 
  pull(total_view) %>% 
  max()