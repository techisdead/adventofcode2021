
file_inp <- "data/day03test.dat"

## PROBLEM ---------------------------------------------------------------------
# # You need to use the binary numbers in the diagnostic report
# to generate two new binary numbers (called the gamma rate an
# the epsilon rate). The power consumption can then be found by
# multiplying the gamma rate by the epsilon rate.
#
# # Each bit in the gamma rate can be determined by finding the
# most common bit in the corresponding position of all numbers in
# the diagnostic report. For example, given the following
# diagnostic report:
#
# The epsilon rate is calculated in a similar way; rather than use the most
# common bit, the least common bit from each position is used.
#-------------------------------------------------------------------------------

n_rows = length(readLines(file_inp))

## get data ----
# read in each line (readlines reads as char and won't drop leading zeros)
# split each character into length 1
# unlist into long vector (I will use ncolumns to reconstruct matrix)
# convert char -> int
inp <- readLines(file_inp) |>
    strsplit(, split = "") |>
    unlist() |>
    as.integer()

# turn into a matrix
# remember `byrow = TRUE` or it will fill the columns first because R's
# grandpa was FORTRAN
m_bin <- matrix(unlist(inp), nrow = n_rows, byrow = TRUE)

# calculate epsilon
# for each column - sum
epsilon <- apply( m_bin, MARGIN = 2, FUN = function(x, nr = n_rows) {

        ifelse(sum(x) < (nr * 0.5), 0, 1)

})

# invert
gamma <- abs(epsilon - 1)

# convert back to char strings and convert to decimal
epsilon_dec <- paste0(epsilon, collapse = "") |> strtoi(base = 2L)
gamma_dec <- paste0(gamma, collapse = "") |> strtoi(base = 2L)

# the answer
epsilon_dec * gamma_dec


#-----PART 2 -------------------------------------------------------------------


# life support rating = oxygen generator rating *  CO2 scrubber rating.

# oxygen generator rating 
#
# * keep only numbers with most common value (0 or 1) in the current bit position 
#  ( 1 if tied)
# * repeat until one number left
#
# CO2 scrubber rating 
# * keep only numbers with least common value (0 or 1) in the current bit position 
#  ( 0 if tied)
# * repeat until one number left

## Helper fuctions

#' return indices of most/least common value in a vector
filter_col <- function(vec, filt_by_max_ = TRUE) {

    if (isTRUE(filt_by_max_)) {
        # target_val is most common
        # if equal, defaults to 1
        target_val <- ifelse(sum(vec) < (length(vec) * 0.5), 0, 1)
    } else {
        # target_val is most common
        # if equal, defaults to 1
        target_val <- ifelse(sum(vec) < (length(vec) * 0.5), 1, 0)
    }

    return(which(vec == target_val))
}


#' Find rating
#' Process a matrix column by column.  Filter matrix down according to 
#' filter rules above
#' Stop when one value left (or complain if you can't find it)
find_rating <- function(mat, filt_by_max) {

    i <- 1
    while (i <= ncol(mat)) {
        keep_cols <- filter_col(mat[, i], filt_by_max_ = filt_by_max)
        mat <- mat[keep_cols, ]
        if (is.null(nrow(mat))) break()
        i <- i + 1
    }

    if (!is.null(nrow(mat))) {
        stop("found too many solutions", mat)
    }

    return(paste0(mat, collapse = ""))

}

ox_gen <- find_rating(m_bin, filt_by_max = TRUE)
co_scrub <- find_rating(m_bin, filt_by_max = FALSE)

# Finally, to find the life support rating, multiply the 
# oxygen generator rating  by the CO2 scrubber rating (as decimals).

strtoi(ox_gen, base = 2L) * strtoi(co_scrub, base = 2L)
