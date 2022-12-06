day06 <- function(filename) {
    dat <- readLines(filename)
    dat <- strsplit(dat,"",fixed=TRUE)[[1L]]
    n <- length(dat)

    find_sop <- function(x) {
        index <- x:n
        for (i in index) {
            possible <- dat[(i - x + 1L):i]
            if (length(unique(possible)) == x)
                return(i)
        }
    }

    list(part_1 = find_sop(4L), part_2 = find_sop(14L))
}
