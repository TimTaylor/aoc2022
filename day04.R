day04 <- function(filename) {

    # load data ---------------------------------------------------------------
    dat <- readLines(filename)
    pattern <- "([[:digit:]]*)-([[:digit:]]*),([[:digit:]]*)-([[:digit:]]*)"
    dat <- strcapture(pattern, dat, data.frame(1L, 1L, 1L, 1L))

    c1 <- .subset2(dat, 1L)
    c2 <- .subset2(dat, 2L)
    c3 <- .subset2(dat, 3L)
    c4 <- .subset2(dat, 4L)

    # part 1 ------------------------------------------------------------------
    part1 <- sum(
        (c1 <= c3 & c2 >= c4) |
        (c1 >= c3 & c2 <= c4)
    )


    # part 2 ------------------------------------------------------------------
    part2 <- sum(
        (c1 >= c3 &  c1 <= c4) |
        (c2 >= c3 &  c2 <= c4) |
        (c3 >= c1 &  c3 <= c2) |
        (c4 >= c1 &  c4 <= c2)
    )

    # result ------------------------------------------------------------------
    list(part_1 = part1, part_2 = part2)

}
