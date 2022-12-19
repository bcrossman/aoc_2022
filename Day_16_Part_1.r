library(unglue)
library(tidyverse)

file <- "./Day_16/Part_1/ex.txt"

input <- 
  unglue_data(readLines(file), 
              c("Valve {valve} has flow rate={flow_rate}; tunnels lead to valves {connected_valves}",
                "Valve {valve} has flow rate={flow_rate}; tunnel leads to valve {connected_valves}"),
              convert = TRUE) 
map <- 
  input %>% 
  separate_rows(connected_valves, sep=",") %>% 
  mutate(connected_valves = trimws(connected_valves, "both")) %>% 
  mutate(valve_on = paste(valve, "on", sep="_"))

tunnel_map <- 
  map %>% select(valve, connected_valves) %>% 
  bind_rows(map %>% select(valve, valve_on) %>% rename(connected_valves = valve_on)) %>% 
  bind_rows(map %>% select(connected_valves, valve_on) %>% rename(valve = valve_on))

flow_rate <- 
  map %>% select(valve_on, flow_rate) %>% 
  bind_rows(map %>% select(valve) %>% mutate(flow_rate = 0))

current_position <- tibble(valve = c("AA"))

for(i in 1:30){
  current_position <- 
    current_position %>% 
    left_join(tunnel_map) 
  
  current_position[[paste("valve",i-1)]] <- current_position$valve
  current_position$valve <- current_position$connected_valves
  current_position$connected_valves <- NULL
  
}
