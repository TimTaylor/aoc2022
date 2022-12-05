day05 <- function(filename) {
    # load data
    dat <- readLines(filename)

    # find the id row
    id_row <- grep("^[[:blank:]]*\\d+", dat)
    ids <- trimws(dat[id_row])

    # find the number of stacks required
    ids <- as.integer(strsplit(ids, split = "[[:blank:]]+")[[1L]])
    n <- length(ids)

    # parse the initial setup
    init <- dat[(id_row - 1L):1]
    init <- paste(init, "\n", collapse="\n")
    f <- tempfile()
    cat(init, file=f, sep = "")
    init <- read.fwf(f, rep.int(4L, n), header = FALSE)
    init <- init[c(TRUE, FALSE),]
    init[] <- lapply(init, trimws)

    # allocate the maximum needed space and a current index
    # here each column is a stack
    stacks <- matrix("", ncol = n, nrow = sum(init !=""))
    empty <- sapply(init, function(x) match(TRUE, x == ""))
    empty[is.na(empty)] <- nrow(init) + 1L

    # add in the initial values
    index <- sapply(empty - 1L, seq_len)
    for (j in 1:n) {
        len <- empty[j] - 1L
        stacks[index[[j]], j] <- init[1:len, j]
    }

    # pull out the movements
    movements <- grep("^move", dat, value = TRUE)
    movements <- strcapture("(\\d+)\\D*(\\d+)\\D*(\\d+)", movements, data.frame(1L,1L,1L))
    movements <- as.matrix(movements)

    # function for part 1 and 2
    loop <- function(part_1) {
        for (i in 1:nrow(movements)) {

            m <- as.integer(movements[i, ])
            num <- m[1L]

            # take from
            stack <- m[2L]
            if (part_1) {
                index <- (empty[stack] - 1L):(empty[stack] - num)
            } else {
                index <- (empty[stack] - num):(empty[stack] - 1L)
            }

            values <- stacks[index, stack]
            stacks[index, stack] <- ""
            empty[stack] <- empty[stack] - num

            # move to
            stack <- m[3L]
            index <- (empty[stack]):(empty[stack] - 1L + num)
            stacks[index, stack] <- values
            empty[stack] <- empty[stack] + num
        }

        tops <- apply(stacks, 2L, function(x) x[match(FALSE, x != "") - 1L])
        paste(gsub("\\W","",tops), collapse = "")
    }

    # return
    list(part_1 = loop(TRUE), part_2 = loop(FALSE))
}

