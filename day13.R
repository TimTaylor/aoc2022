day13 <- function(filename) {

    # load data
    dat <- readChar(filename, file.info(filename)$size)
    dat <- strsplit(dat, "\n\n", fixed = TRUE)[[1L]]
    dat <- strsplit(dat, "\n", fixed = TRUE)

    # avoid parsing
    dat <- lapply(
        dat,
        function(x) {
            x <- gsub("[","list(", x, fixed = TRUE)
            x <- gsub("]", ")", x, fixed = TRUE)
            lapply(x, function(y) eval(str2expression(y)))
        }
    )

    # ordering function
    is_ordered <- function(lhs, rhs) {

        # both numeric so compare as integers
        if (is.numeric(lhs) && is.numeric(rhs)) {
            if (lhs < rhs)
                return(TRUE)
            if (rhs < lhs)
                return(FALSE)
            # We have no ordering (per instructions) so return NA so we can skip
            # to the next values to check
            return(NA)
        }

        # if just one is numeric convert it to a list
        if (is.numeric(lhs)) {
            return(is_ordered(list(lhs), rhs))
        } else if (is.numeric(rhs)) {
            return(is_ordered(lhs, list(rhs)))
        }

        # if none of the above hold loop through the elements
        for (i in seq_along(lhs)) {
            # length(lhs) > length(rhs)
            if (i > length(rhs))
                return(FALSE)
            # order the two elements
            res <- is_ordered(lhs[[i]], rhs[[i]])

            # if NA skip to next i else return
            if (is.na(res))
                next
            return(res)
        }

        # last check
        if (length(lhs) < length(rhs))
            return(TRUE)

        # again - no ordering (per instructions) so return NA so we can skip to
        # to the next values to check
        NA
    }

    # part 1 ------------------------------------------------------------------
    part_1 <- sapply(dat, function(x) is_ordered(x[[1L]], x[[2L]]))
    part_1 <- sum(which(part_1))

    # part 2 ------------------------------------------------------------------
    # no custom ordering sort function in R so we code bubble sort
    bubble_sort <- function(x) {
        n <- length(x)
        swapped <- TRUE
        while(swapped) {
            swapped <- FALSE
            for (i in seq_len(length(x) - 1L)) {
                if (isFALSE(is_ordered(x[[i]], x[[i+1]]))) {
                    tmp <- x[[i]]
                    x[[i]] <- x[[i+1]]
                    x[[i + 1]] <- tmp
                    swapped <- TRUE
                }
            }
        }
        x
    }

    # list handling (need to not recursively unlist!)
    part_2 <- unlist(dat, recursive = FALSE)
    new2 <- list(list(2))
    new6 <- list(list(6))
    part_2 <- c(part_2, new2, new6, recursive = FALSE)
    part_2 <- bubble_sort(part_2)
    part_2 <- match(new2, part_2) * match(new6, part_2)

    # return
    list(part_1 = part_1, part_2 = part_2)
}

