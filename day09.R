day09 <- function(filename) {

    # load data
    dat <- read.table(filename, sep = " ", col.names = c("direction", "n"), colClasses = c("character", "integer"))

    # convert movements to indices
    x_lu <- c(U = 0L, D =  0L, L = -1L, R = 1L)
    y_lu <- c(U = 1L, D = -1L, L =  0L, R = 0L)
    unit_movements <- matrix(c(x_lu[dat$direction], y_lu[dat$direction]), ncol = 2L)
    n_units <- dat$n

    # generate all of the head positions
    max_positions <- sum(abs(unit_movements) * n_units) + 1L
    head_positions <- matrix(0L, nrow = max_positions, ncol = 2L)
    index <- 1L
    head <- c(0L, 0L)
    for (i in 1:nrow(unit_movements)) {
        movement <- unit_movements[i, ]
        for (j in seq_len(n_units[i])) {
            head <- head + movement
            head_positions[index + 1L, ] <- head
            index <- index + 1L
        }
    }

    # function to calculate tail movement
    tail_movement <- function(head, tail) {
        distance <- head - tail
        if (all(abs(distance) <= 1L)) c(0L, 0L) else sign(distance)
    }

    # function to calculate visited positions based on tail knots
    visited <- function(tail_knots) {
        for (j in seq_len(tail_knots)) {
            tail_positions <- matrix(0L, nrow = max_positions, ncol = 2L)
            for (i in 2:nrow(head_positions)) {
                tail_positions[i, ] <- tail_positions[i - 1L, ] + tail_movement(head_positions[i, ], tail_positions[i - 1L, ])
            }
            head_positions <- tail_positions
        }
        nrow(unique(tail_positions))
    }

    # output
    list(part_1 = visited(1L), part_2 = visited(9L))

}
