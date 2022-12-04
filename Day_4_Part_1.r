library(unglue)
library(tidyverse)

input <- readLines("./Day_4/Part_1/input.txt")

# part 1

unglue_data(input, 
            "{elf_1_start}-{elf_1_end},{elf_2_start}-{elf_2_end}", 
            convert = TRUE) %>% 
  mutate(dup = ((elf_1_start<=elf_2_start)&(elf_1_end>=elf_2_end))|
           ((elf_1_start>=elf_2_start)&(elf_1_end<=elf_2_end))) %>% 
  pull(dup) %>% 
  sum()

# part 2

unglue_data(input, 
            "{elf_1_start}-{elf_1_end},{elf_2_start}-{elf_2_end}", 
            convert = TRUE) %>% 
  mutate(dup = ((elf_1_start<=elf_2_start)&(elf_1_end>=elf_2_start))|
           ((elf_2_start<=elf_1_start)&(elf_2_end>=elf_1_start))) %>% 
  pull(dup) %>% 
  sum()    
