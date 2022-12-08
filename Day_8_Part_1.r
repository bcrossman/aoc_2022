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
  # i <- data$key[1]
  
  vec <- 
    data %>% 
    filter(rowid == data$rowid[data$key==i],
           colid < data$colid[data$key==i]) 
  
  if(nrow(vec)==0) {
    return(0)
    next}
  
  return(
    vec %>% 
      mutate(can_see_over = (value-data$value[data$key==i])<0) %>%
      mutate(group = cumsum(can_see_over!=lag(can_see_over,1, TRUE))) %>% 
      group_by(group, can_see_over) %>% 
      mutate(count = seq(n())) %>% 
      ungroup() %>% 
      slice_max(group) %>% 
      pull(can_see_over) %>% 
      sum()
  )
}

right_count <- function(i){
  
  vec <- 
    data %>%
    filter(rowid == data$rowid[data$key==i],
           colid > data$colid[data$key==i]) %>% 
    
    if(nrow(vec)==0) {
      return(0)
      next}
  
  return(
    vec %>% 
      mutate(can_see_over = (value-data$value[data$key==i])<0) %>%
      mutate(group = cumsum(can_see_over!=lag(can_see_over,1, TRUE))) %>% 
      group_by(group, can_see_over) %>% 
      mutate(count = seq(n())) %>% 
      ungroup() %>% 
      slice_max(group) %>% 
      pull(can_see_over) %>% 
      sum()
  )
}

down_count <- function(i){
  vec <- 
    data %>%
    filter(rowid < data$rowid[data$key==i],
           colid == data$colid[data$key==i]) 
  
  if(nrow(vec)==0) {
    return(0)
    next}
  
  return(
    vec %>% 
      mutate(can_see_over = (value-data$value[data$key==i])<0) %>%
      mutate(group = cumsum(can_see_over!=lag(can_see_over,1, TRUE))) %>% 
      group_by(group, can_see_over) %>% 
      mutate(count = seq(n())) %>% 
      ungroup() %>% 
      slice_max(group) %>% 
      pull(can_see_over) %>% 
      sum()
  )
}


up_count <- function(i){
  
  vec <- 
    data %>% 
    filter(rowid > data$rowid[data$key==i],
           colid == data$colid[data$key==i]) 
  
  if(nrow(vec)==0) {
    return(0)
    next}
  
  return(
    vec %>% 
      mutate(can_see_over = (value-data$value[data$key==i])<0) %>%
      mutate(group = cumsum(can_see_over!=lag(can_see_over,1, TRUE))) %>% 
      group_by(group, can_see_over) %>% 
      mutate(count = seq(n())) %>% 
      ungroup() %>% 
      slice_max(group) %>% 
      pull(can_see_over) %>% 
      sum()
  )
}

result <- 
  data %>% 
  rowwise() %>% 
  mutate(left = left_count(key),
         right = right_count(key),
         up = up_count(key),
         down = down_count(key))
