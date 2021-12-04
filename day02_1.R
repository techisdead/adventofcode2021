#---- Part 1 -----#

library(data.table)

# input data
dt_inp <- fread("./data/day02.dat",
    col.names = c("direction", "units"),
    sep = " "
)

dt_res <- dcast(dt_inp, . ~ direction, value.var = "units", fun.aggregate = sum)

## ans
dt_res[, (down - up) * forward]

#---- Part 2 -----#

dt_inp[, id := .I]
dt_res <- dcast(dt_inp, id ~ direction, value.var = "units", fun.aggregate = sum)

# change in aim this step
dt_res[, this_aim := down - up]

# cumluative aim 
dt_res[, aim := cumsum(this_aim)]

dt_res[, adj_depth := aim * forward]

## result 
dt_res[, sum(forward) * sum(adj_depth)]
