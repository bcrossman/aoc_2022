library(tidyverse)
library(igraph)

file <- "./Day_12/Part_1/input.txt"
input <- readLines(file)

heights <- data.frame("map" = c("S","E",letters), "height" = c(1,26,1:26))

data <- 
  data.frame(map = input) %>% 
  rowid_to_column() %>%
  separate_rows(map, sep="", convert = T) %>% 
  drop_na(map) %>% 
  filter(map != "") %>% 
  group_by(rowid) %>% 
  mutate(colid = row_number()) %>% 
  ungroup() %>% 
  mutate(key = paste(rowid, colid, sep=","))

##  Join edges
edges_points <- 
  data %>% 
  left_join(data %>% mutate(rowid = rowid-1),
            by = c("rowid","colid"),
            suffix = c("", "_down")) %>% 
  left_join(data %>% mutate(rowid = rowid+1),
            by = c("rowid","colid"),
            suffix = c("", "_up")) %>% 
  left_join(data %>% mutate(colid = colid+1),
            by = c("rowid","colid"),
            suffix = c("", "_left")) %>% 
  left_join(data %>% mutate(colid = colid-1),
            by = c("rowid","colid"),
            suffix = c("", "_right")) %>% 
  rename(key_start= key,
         map_start = map) %>% 
  pivot_longer(cols = -c(rowid,colid),
               names_to = c(".value","direction"),
               names_sep = "_") %>% 
  filter(direction != "start") %>% 
  rename(end = key,
         map_end = map) %>%
  mutate(start = paste(rowid, colid, sep=",")) %>% 
  left_join(data %>% select(key, map) , by = c("start"="key")) %>% 
  select(start, end, map, map_end) %>% 
  drop_na(map_end) %>% 
  left_join(heights, by="map") %>% 
  left_join(heights, by = c("map_end"="map"), suffix = c("_start", "_end")) %>% 
  filter((height_end-height_start) <= 1) 

network_data_full <- 
  edges_points %>% 
  graph_from_data_frame(directed = TRUE)

## Part 1
start_key <- edges_points %>% filter(map == "S") %>% pull(start) %>% unique()
end_key <- edges_points %>% filter(map == "E") %>% pull(start) %>% unique()

paths <- 
  network_data_full %>% 
  igraph::get.shortest.paths(from=start_key, 
                             to = end_key,  
                             # weights = NULL,  #uses weight attribute from df
                             mode = "out") 

long_vec_paths <- sapply(paths$vpath, as_ids) %>% unlist

length(long_vec_paths)-1

#Part 2
end_key <- edges_points %>% filter(map == "E") %>% pull(start) %>% unique()
start_keys <- edges_points %>% filter(map == "a") %>% pull(start) %>% unique()

longest_path <- NA
for(start_key in start_keys){
# print(start_key)  
paths <- 
  network_data_full %>% 
  igraph::get.shortest.paths(from=start_key, 
                             to = end_key,  
                             # weights = NULL,  #uses weight attribute from df
                             mode = "out") 

long_vec_paths <- sapply(paths$vpath, as_ids) %>% unlist
current_length <- length(long_vec_paths)-1
# print(current_length)
if(current_length<1){next}
longest_path <- min(longest_path,current_length, na.rm = T)
}
