day04 <- function(filename) {

    # load data ---------------------------------------------------------------
    dat <- readLines(filename)
    pattern <- "([[:digit:]]*)-([[:digit:]]*),([[:digit:]]*)-([[:digit:]]*)"
    dat <- strcapture(pattern,dat,data.frame(1L,1L,1L,1L))

    # part_1 ------------------------------------------------------------------
    part_1 <- function(x) {
        (.subset2(x, 1L) <= .subset2(x, 3L) & .subset2(x, 2L) >= .subset2(x, 4L)) |
        (.subset2(x, 1L) >= .subset2(x, 3L) & .subset2(x, 2L) <= .subset2(x, 4L))
    }

    # part_2 ------------------------------------------------------------------
    part_2 <- function(x) {
        (.subset2(x, 1L) >= .subset2(x, 3L) &  .subset2(x, 1L) <= .subset2(x, 4L)) |
        (.subset2(x, 2L) >= .subset2(x, 3L) &  .subset2(x, 2L) <= .subset2(x, 4L)) |
        (.subset2(x, 3L) >= .subset2(x, 1L) &  .subset2(x, 3L) <= .subset2(x, 1L)) |
        (.subset2(x, 4L) >= .subset2(x, 1L) &  .subset2(x, 4L) <= .subset2(x, 2L))
    }

    # result ------------------------------------------------------------------
    list(part_1 <- sum(part_1(dat)), part_2 <- sum(part_2(dat)))

}


