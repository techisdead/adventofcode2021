#---- Part 1 -----#

library(data.table)

# input data
dt_inp <- fread("./data/day02.dat",
    col.names = c("direction", "units"),
    sep = " "
)

dt_inp[, id := .I]
dt_res <- dcast(dt_inp, id ~ direction, value.var = "units", fun.aggregate = sum)


## ans
dt_res[, (sum(down) - sum(up)) * sum(forward)]

#---- Part 2 -----#

# change in aim this step
dt_res[, this_aim := down - up]

# cumluative aim 
dt_res[, tot_aim := Reduce("+", shift(this_aim, n = 1L, type = "lag"))]

dt_res[, adj_depth := down - up + aim * forward]

## horiz 
dt_res[,sum(forward)]
# vert
dt_res[, sum(adj_depth)]

## result 
dt_res[, sum(forward) + sum(adj_depth)]
