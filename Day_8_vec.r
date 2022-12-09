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

output <- list()
for(i in 2:(max(data$colid)-1)){
  # i <- 3
 print(i)
  l <-  
    data %>% 
    filter(colid<=i)%>% 
    group_by(rowid) %>% 
    arrange(rowid,colid) %>% 
    mutate(can_see_over = (value-value[row_number()==max(row_number())])<0) %>%
    slice(-n()) %>% 
    mutate(group = cumsum(can_see_over!=lag(can_see_over,1, TRUE))) %>% 
    mutate(starting_group = min(group)) %>% 
    slice_max(group) %>% 
    mutate(can_see_over = ifelse((group==min(starting_group))&(row_number()==min(row_number())),FALSE,can_see_over)) %>% 
    summarise(value = sum(can_see_over)+1) %>% 
    mutate(direction = "left",
           colid = i,
           key = paste(rowid,colid,sep = ","))
  
  r <-  
    data %>% 
    filter(colid>=i)%>% 
    group_by(rowid) %>% 
    arrange(rowid,desc(colid)) %>% 
    mutate(can_see_over = (value-value[row_number()==max(row_number())])<0) %>%
    slice(-n()) %>% 
    mutate(group = cumsum(can_see_over!=lag(can_see_over,1, TRUE))) %>% 
    mutate(starting_group = min(group)) %>% 
    slice_max(group) %>% 
    mutate(can_see_over = ifelse((group==min(starting_group))&(row_number()==min(row_number())),FALSE,can_see_over)) %>% 
    summarise(value = sum(can_see_over)+1) %>% 
    mutate(direction = "right",
           colid = i,
           key = paste(rowid,colid,sep = ","))
  
  output[[paste(i,"l")]] <- l
  output[[paste(i,"r")]] <- r
}

for(i in 2:(max(data$rowid)-1)){
  # i <- 7
  print(i)
  u <-  
    data %>% 
    filter(rowid<=i)%>% 
    group_by(colid) %>% 
    arrange(rowid,colid) %>% 
    mutate(can_see_over = (value-value[row_number()==max(row_number())])<0) %>%
    slice(-n()) %>% 
    mutate(group = cumsum(can_see_over!=lag(can_see_over,1, TRUE))) %>% 
    mutate(starting_group = min(group)) %>% 
    slice_max(group) %>% 
    mutate(can_see_over = ifelse((group==min(starting_group))&(row_number()==min(row_number())),FALSE,can_see_over)) %>% 
    summarise(value = sum(can_see_over)+1) %>% 
    mutate(direction = "up",
           rowid = i,
           key = paste(rowid,colid,sep = ","))
  
  d <-  
    data %>% 
    filter(rowid>=i)%>% 
    group_by(colid) %>% 
    arrange(desc(rowid),colid) %>% 
    mutate(can_see_over = (value-value[row_number()==max(row_number())])<0) %>%
    slice(-n()) %>% 
    mutate(group = cumsum(can_see_over!=lag(can_see_over,1, TRUE))) %>% 
    mutate(starting_group = min(group)) %>% 
    slice_max(group) %>% 
    mutate(can_see_over = ifelse((group==min(starting_group))&(row_number()==min(row_number())),FALSE,can_see_over)) %>% 
    summarise(value = sum(can_see_over)+1) %>% 
    mutate(direction = "down",
           rowid = i,
           key = paste(rowid,colid,sep = ","))
  
  output[[paste(i,"u")]] <- u
  output[[paste(i,"d")]] <- d
  
}

bind_rows(output) %>% 
  pivot_wider(id_cols = c("key", "rowid", "colid"), 
              names_from = direction, 
              values_from = value) %>% 
  mutate(total_view = left*right*up*down) %>% 
  pull(total_view) %>% 
  max(na.rm = T)
