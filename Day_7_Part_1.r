library(tidyverse)

file <- "./Day_7/Part_1/input.txt"
input <- readLines(file)

listing <- unglue_data(input,
                       c("$ cd {dir_in}",
                         "{size=\\d+} {file_name}",
                         "dir {dir_contained}"),
                       convert = TRUE)%>% 
  fill(dir_in, .direction = "down")

directory_mapping <- 
  listing %>% 
  drop_na(dir_contained) %>% 
  select(dir_in, dir_contained)

files_contained <- 
  listing %>% 
  drop_na(size) %>% 
  select(dir_in, size, file_name)

size_folder_excl_sub <- 
  files_contained %>% 
  group_by(dir_in) %>% 
  summarise(size = sum(size))

size_folder_excl_sub_full = 
  tibble("dir_in" = unique(c(directory_mapping$dir_in, 
                             directory_mapping$dir_contained,
                             size_folder_excl_sub$dir_in))) %>% 
  left_join(size_folder_excl_sub) %>% 
  mutate(size = ifelse(is.na(size),0,size))

output_list <- list()
remaining_folders <- directory_mapping
current_size_folders <- size_folder_excl_sub_full
count <- 0
while(nrow(remaining_folders)>0){
  print(nrow(remaining_folders))
  if(nrow(remaining_folders)==101){break}
  count <- count+1
  
  active_folders <- 
    remaining_folders %>% 
    full_join(current_size_folders)

  lowest_level_folder <- 
    active_folders %>% 
    filter(is.na(dir_contained)) %>% 
    rename(total_size = size) %>% 
    select(dir_in, total_size)
  
  output_list[[as.character(count)]] <- lowest_level_folder
  
  updated_folder_size <- 
    active_folders %>% 
    filter(!is.na(dir_contained)) %>% 
    left_join(lowest_level_folder, by=c("dir_contained"="dir_in")) %>%
    group_by(dir_in) %>% 
    summarise(addl_size = sum(total_size,na.rm=T))
  
  current_size_folders <- 
      current_size_folders %>% 
      filter(!(dir_in %in% unique(lowest_level_folder$dir_in))) %>%
      left_join(updated_folder_size) %>% 
      mutate(size = size + addl_size) %>% 
      select(dir_in, size)
    
    remaining_folders <- 
      remaining_folders %>% 
      filter(!(dir_contained %in% unique(lowest_level_folder$dir_in))) %>%
      select(dir_in,dir_contained)
} 

current_size_folders %>% 
  rename(total_size = size) %>% 
  bind_rows(bind_rows(output_list)) %>% 
  filter(total_size<=100000) %>% 
  pull(total_size) %>% 
  sum()
