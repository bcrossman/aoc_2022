library(unglue)
library(tidyverse)

file <- "./Day_7/Part_1/input.txt"
input <- readLines(file)

listing <- unglue_data(input,
                       c("$ cd {dir_in}",
                         "{size=\\d+} {file_name}",
                         "dir {dir_contained}"),
                       convert = TRUE)%>% 
  mutate(dir_in_chg=!is.na(dir_in)) %>% 
  fill(dir_in, .direction = "down") %>% 
  mutate(current_dir = "UNKNOWN")

for(i in 1:nrow(listing)){
  # i = 9
  if(i ==1){
    listing$current_dir[i] <- listing$dir_in[i]
    next
  }
  if(listing$dir_in[i] ==  "..") {
    listing$current_dir[i] <- sub("_[^_]+$", replacement = "", x = listing$current_dir[i-1])  
    next}
  if(!listing$dir_in_chg[i]){listing$current_dir[i] <- listing$current_dir[i-1]
     next
  }
  listing$current_dir[i] <- paste(listing$current_dir[i-1], listing$dir_in[i], sep = "_")
}

listing <- 
  listing %>% 
  mutate(dir_in = current_dir) %>% 
  mutate(dir_contained = ifelse(!is.na(dir_contained), paste(dir_in, dir_contained, sep = "_"), dir_contained)) %>% 
  select(dir_in:file_name)


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
  # if(nrow(remaining_folders)==101){break}
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

full_run <- 
current_size_folders %>% 
  rename(total_size = size) %>% 
  bind_rows(bind_rows(output_list)) 

##Part 1
full_run %>% 
  filter(total_size<=100000) %>% 
  pull(total_size) %>% 
  sum()

##Part 2
space_needed = 30000000-(70000000-max(full_run$total_size))

full_run %>% 
  ungroup() %>% 
  filter(total_size>space_needed) %>% 
  slice_min(total_size, n = 1) %>% 
  pull(total_size)
