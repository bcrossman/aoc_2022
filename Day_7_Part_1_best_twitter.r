
file <- "./Day_7/Part_1/input.txt"
input <- readLines(file)

#https://twitter.com/antoine_fabri/status/1600591216755806220?s=20&t=843inSry12sVng82GNYo6g

#Part 1

done <- ongoing <- numeric()

for(line in input){
  if(line == "$ ls"){
    ongoing <- c(ongoing, 0)
  } else if (grepl("^\\d", line)) {
    ongoing <- ongoing+as.numeric(gsub("\\D","",line))  #this adds the new number to EVERY ITEM IN THE VECTOR.
  } else if(line == "$ cd .."){
    done <- c(done,tail(ongoing,1))
    ongoing <- head(ongoing,-1)
  }
}

##Basically the person is walking through all the folders in order, so you just keep track until you have to backtrack with .., when you
# back track you know you finished a path

done <- c(ongoing,done)

sum(done[done<100000])

free <- 70000000-done[1]
needed <- 30000000 - free

min(done[done>needed])