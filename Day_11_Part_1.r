library(unglue)
library(tidyverse)

file <- "./Day_11/Part_1/input.txt"

input <- 
  unglue_data(readLines(file), 
              c("Monkey {monkey_num}:",
                "  Starting items: {item_list}",
                "  Operation: new = {operation_formula}",
                "  Test: divisible by {divisible_test}",
                "    If true: throw to monkey {new_monk_true}",
                "    If false: throw to monkey {new_monk_false}"),
              convert = TRUE) %>% 
  fill(everything(), .direction = "down") %>% 
  group_by(monkey_num) %>% 
  slice(n()) %>% 
  mutate(item_list = str_split(string = item_list, ", ")) %>% 
  mutate(times_items_inspected = 0) %>% 
  ungroup()

divisor <- 3

for(round in 1:20){
  print(paste("round: ", round))
  for(monkey in unique(input$monkey_num)){
    # monkey <- 0
    print(paste("monkey: ", monkey))
    monkey_row <- 
      input %>% 
      filter(monkey_num == monkey)
    if(length(monkey_row$item_list[[1]])==0){next}
    for(item in 1:length(monkey_row$item_list[[1]])){
      print(paste("item: ", item))
      old <- as.numeric(monkey_row$item_list[[1]][item])
      formula <- monkey_row$operation_formula[[1]][1]
      new = eval(parse(text = formula))%/% divisor
      new_monkey <- if_else(0==(new %% monkey_row$divisible_test[[1]]),
                            monkey_row$new_monk_true[[1]],
                            monkey_row$new_monk_false[[1]])
      
      ##Move to new monkey
      input$item_list[input$monkey_num==new_monkey][[1]] <- 
        c(input$item_list[input$monkey_num==new_monkey][[1]], new)
      input$times_items_inspected[input$monkey_num==monkey] <- 
        input$times_items_inspected[input$monkey_num==monkey]+1
    }
    ## Remove from current Monkey
    input$item_list[input$monkey_num==monkey][[1]] <- character()
  }
}

input %>% 
  slice_max(order_by = times_items_inspected, n = 2, with_ties = FALSE) %>% 
  pull(times_items_inspected) %>% 
  prod()
