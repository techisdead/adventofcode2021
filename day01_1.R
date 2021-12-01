library(data.table)

# --- Part 1 ----

dt_dat <- fread("./data/day1.dat", col.names = "measurements")
dt_dat[
    ,
    up := ifelse(
        measurements - shift(measurements, n = 1L, type = "lag") > 0,
        TRUE,
        FALSE
    )
]
dt_dat[, sum(up, na.rm = TRUE)]


# --- Part Two ---

dt_dat <- fread("./data/day1.dat", col.names = "measurements")

# use Reduce for rolling sum
dt_dat[, sum3 := Reduce(`+`, shift(measurements, 0:2))]

dt_dat[
    ,
    up := ifelse(
        sum3 - shift(sum3, n = 1L, type = "lag") > 0,
        TRUE,
        FALSE
    )
]

dt_dat[, sum(up, na.rm = TRUE)]
