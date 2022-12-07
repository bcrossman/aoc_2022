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

files_contained <- 
  listing %>% 
  drop_na(size) %>% 
  select(dir_in, size, file_name)

output_list <- list()
for(i in unique(listing$dir_in)){
  
  output_list[[i]] <- 
    files_contained %>% 
    filter(grepl(dir_in, pattern = i)) %>% 
    summarise(total_size = sum(size)) %>% 
    mutate(dir_in = i)
}

full_run <- bind_rows(output_list)

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
