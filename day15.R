day15 <- function(filename) {
    # parse data in to data frame
    dat <- readLines(filename)
    pattern <- "Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)"
    dat <- strcapture(pattern, dat, data.frame(1L, 1L, 1L, 1L), perl = TRUE)

    # pull out coordinates
    sx <- dat[[1L]]
    sy <- dat[[2L]]
    bx <- dat[[3L]]
    by <- dat[[4L]]

    # calculate distances
    manhattan <- abs(sx - bx) + abs(sy - by)

    # function to calculate intervals without beacons by row
    row_to_interval_without <- function(x, y, distance, row) {
        if (abs(y - row) > distance)
            return(integer())
        shift <- abs(y - row)
        left <- x - distance + shift
        right <- x + distance - shift
        c(left, right)
    }

    # part 1
    row <- 2e6
    out=.mapply(row_to_interval_without, list(sx, sy, manhattan), MoreArgs = list(row = row))
    out <- do.call(rbind, out)
    x <- out[, 1L]
    y <- out[, 2L]
    range <- seq.int(from = min(sx - manhattan, bx), to = max(sx + manhattan, bx))
    for (i in seq_along(x))
        range[(range>= x[i]) & (range <= y[i])] <- NA_integer_
    part_1 <- sum(is.na(range))- length(unique(bx[by==row])) - length(unique(sx[sy==row]))

    # part 2
    # every speedup I could think off
    for (row in 4e6:0) {
        out=.mapply(row_to_interval_without, list(sx, sy, manhattan), MoreArgs = list(row = row))
        out <- do.call(rbind, out)
        x1 <- out[, 1L]
        x2 <- out[, 2L]
        idx <- order(x1)
        x1 <- x1[idx]
        x2 <- x2[idx]

        for (i in 2:length(x2)) {
            if (x2[i-1] > x2[i]) {
                x2[i] <- x2[i-1]
            }
        }

        if ((x1[1L] > 1L) || (4e6 - x2[length(x2)] > 1L))
            next

        cnt <- as.integer(x1[1L] == 1L)

        flag <- FALSE
        for (i in 2:length(x2)) {
            if ((x1[i] - x2[i - 1L]) == 2L) {
                value <- x2[i - 1L] + 1L
                cnt <- cnt + 1L
            }
            if (cnt > 1L) {
                flag <- TRUE
                break
            }
        }
        if (flag)
            next

        if (cnt == 1L) {
            part_2 <- list(x=value,y=row)
            break
        }

    }

    list(part_1 = part_1, part_2 = part_2$x * 4e6 + part_2$y)
}
