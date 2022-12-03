day03 <- function(filename) {
    dat <- readLines(filename)
    dat <- strsplit(dat, "", fixed = TRUE)
    n <- length(dat)

    # part 1 ------------------------------------------------------------------
    overlaps <- character(n)
    for (i in seq_len(n)) {
        rucksack <- dat[[i]]
        len <- length(rucksack)
        half <- len / 2L
        first <- rucksack[1:half]
        second <- rucksack[(half + 1L):len]
        overlaps[i] <- intersect(first, second)
    }
    lookup <- c(letters, LETTERS)
    answer_1 <- sum(match(overlaps, lookup))


    # part 2 ------------------------------------------------------------------
    n <- n / 3L
    badges <- character(n)
    for (i in seq_len(n)) {
        third <- 3L * i
        r3 <- dat[[third]]
        r2 <- dat[[third - 1L]]
        r1 <- dat[[third - 2L]]
        badges[i] <- intersect(intersect(r1, r2), r3)
    }
    answer_2 <- sum(match(badges, lookup))

    # result ------------------------------------------------------------------
    list(part_1 = answer_1, part_2 = answer_2)
}

