filename <- "input11.txt"
day11 <- function(filename) {

    # load data
    dat <- readChar(filename, file.info(filename)$size)
    dat <- strsplit(dat, "\n\n", fixed = TRUE)[[1L]]
    dat <- strsplit(dat, "\n", fixed = TRUE)

    # parse data
    n <- length(dat)
    original_items <- vector("list", n)
    operation <- operation <- character(n)
    divisor <- integer(n)
    true <- false <- logical(n)
    for (i in seq_len(n)) {
        x <- dat[[i]]
        starting_items <- sub("  Starting items: ", "", x[2L], fixed = TRUE)
        items[[i]] <- scan(text = starting_items, what = 1L, sep = ",", quiet = TRUE)
        operation[i] <- sub("  Operation: new = ", "", x[3L], fixed = TRUE)
        divisor[i] <- as.integer(sub("Test: divisible by ", "", x[4L], fixed = TRUE))
        true[i] <- as.integer(sub("If true: throw to monkey ", "", x[5L], fixed = TRUE)) + 1L
        false[i] <- as.integer(sub("If false: throw to monkey ", "", x[6L], fixed = TRUE)) + 1L
    }

    # part 1
    p1_items <- items
    times <- integer(n)
    for (i in 1:20) {
        for (j in seq_len(n)) {
            stuff <- p1_items[[j]]
            for (k in seq_along(stuff)) {
                times[j] <- times[j] + 1
                old <- stuff[k]
                new <- eval(str2expression(operation[j])) %/% 3L
                if (new %% divisor[j]) {
                    p1_items[[false[j]]] <- c(p1_items[[false[j]]], new)
                } else {
                    p1_items[[true[j]]] <- c(p1_items[[true[j]]], new)
                }
            }
            p1_items[[j]] <- integer()
        }
    }
    part_1 <- prod(sort(times, decreasing = TRUE)[1:2])

    # part 2
    times <- integer(n)
    dd <- prod(divisor)
    for (i in 1:10000) {
        for (j in seq_len(n)) {
            stuff <- items[[j]]
            for (k in seq_along(stuff)) {
                times[j] <- times[j] + 1
                old <- stuff[k]
                new <- eval(str2expression(operation[j])) %% dd
                if (new %% divisor[j]) {
                    items[[false[j]]] <- c(items[[false[j]]], new)
                } else {
                    items[[true[j]]] <- c(items[[true[j]]], new)
                }
            }
            items[[j]] <- integer()
        }
    }
    part_2 <- prod(sort(times, decreasing = TRUE)[1:2])

    # result
    list(part_1 = part_1, part_2 = part_2)
}

