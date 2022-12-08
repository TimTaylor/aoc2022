day08 <- function(filename) {

    # load data in to matrix
    width <- nchar(readLines(filename, n = 1L))
    dat <- read.fwf(filename, widths = rep.int(1L, width))
    dat <- as.matrix(dat)

    # add sentinel border
    above <- below <- rep.int(-1L, width)
    dat <- rbind(above, dat, below)
    height <- nrow(dat)
    left <- right <- rep.int(-1L, height)
    dat <- cbind(left, dat, right)
    width <- width + 2L

    # part_1 function
    visible <- function(i, j) {
        tr <- dat[i,j]
        above <- dat[1:(i-1), j]
        below <- dat[(i+1):height, j]
        left <- dat[i, 1:(j-1)]
        right <- dat[i, (j+1):width]
        all(tr > above) || all(tr > below) || all(tr > left) || all(tr > right)
    }

    # part_2 function
    distance <- function(i, j) {
        tr <- dat[i,j]
        above <- dat[(i-1):1, j]
        below <- dat[(i+1):height, j]
        left <- dat[i, (j-1):1]
        right <- dat[i, (j+1):width]

        above_value <- match(FALSE, tr > above)
        if(is.na(above_value))
            above_value <- i - 2L

        below_value <- match(FALSE, tr > below)
        if(is.na(below_value))
            below_value <- height - i - 1L

        left_value <- match(FALSE, tr > left)
        if(is.na(left_value))
            left_value <- j - 2L

        right_value <- match(FALSE, tr > right)
        if(is.na(right_value))
            right_value <- width - j - 1L

        above_value * below_value * left_value * right_value
    }

    # results
    index <- expand.grid(2:(width-1), 2:(height-1))
    list(
        part_1 = sum(mapply(visible, index$Var1, index$Var2)),
        part_2 = max(mapply(distance, index$Var1, index$Var2))
    )

}
