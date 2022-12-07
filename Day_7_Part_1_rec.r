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


total_folder_size <- function(dir_target){
  # dir_target <- "/"
  
  sub_dir <- 
    directory_mapping %>% 
    filter(dir_in == dir_target) %>% 
    pull(dir_contained)  
  
  if(length(sub_dir)==0){
    return(size_folder_excl_sub_full %>% 
             filter(dir_in==dir_target) %>% 
             pull(size) %>% 
             sum())
  }else{
    addl_size <- 0
    for(i in sub_dir){
      addl_size <- addl_size+total_folder_size(i)
    } 
      folder_alone <- 
        size_folder_excl_sub_full %>% 
        filter(dir_in==dir_target) %>% 
        pull(size) %>% 
        sum()
      
      return(addl_size+folder_alone)
  }
}

total_folder_size("/")
   