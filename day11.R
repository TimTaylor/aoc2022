day11 <- function(filename) {

    # load data
    dat <- readChar(filename, file.info(filename)$size)
    dat <- strsplit(dat, "\n\n", fixed = TRUE)[[1L]]
    dat <- strsplit(dat, "\n", fixed = TRUE)

    # parse data
    n <- length(dat)
    operation <- character(n)
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

    # function to for both parts
    f <- function(items = items, method_1 = TRUE, iterations = 20L) {
        dd <- prod(divisor)
        times <- integer(n)
        for (i in seq_len(iterations)) {
            for (j in seq_len(n)) {
                stuff <- items[[j]]
                for (k in seq_along(stuff)) {
                    times[j] <- times[j] + 1L
                    old <- stuff[k]
                    if (method_1) {
                        new <- eval(str2expression(operation[j])) %/% 3L
                    } else {
                        new <- eval(str2expression(operation[j])) %% dd
                    }
                    if (new %% divisor[j]) {
                        items[[false[j]]] <- c(items[[false[j]]], new)
                    } else {
                        items[[true[j]]] <- c(items[[true[j]]], new)
                    }
                }
                items[[j]] <- integer()
            }
        }
        prod(sort(times, decreasing = TRUE)[1:2])
    }

    # result
    list(
        part_1 = f(items = items, method_1 = TRUE, iterations = 20L),
        part_2 = f(items = items, method_1 = FALSE, iterations = 10000L)
    )
}
