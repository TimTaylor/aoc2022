day07 <- function(filename) {
    dat <- readLines(filename)

    # remove lines that do nothing (ls and dir)
    dat <- dat[dat != "$ ls"]
    dat <- dat[grep("^dir ", dat, perl = TRUE, invert = TRUE)]

    # loop to get directories (first row is cd /)
    n <- length(dat)
    dirs <- character(n)
    dirs[1L] <- dir <- "/"
    for (i in 2:n) {
        row <- dat[i]

        # going back a directory
        if (grepl("$ cd ..", row, fixed = TRUE) && (dir != "/")) {
            dir <- sub("/[[:alnum:]]*/$","/", dir)
        # changing to root
        } else if (row == "$ cd /") {
            dir <- "/"
        # changing to another directory
        } else if (grepl("^\\$ cd", row, perl = TRUE)) {
            new_dir <- sub("^\\$ cd ", "", row, perl = TRUE)
            dir <- sprintf("%s%s/", dir, new_dir)
        # otherwise file listing and nothing to do
        }

        dirs[i] <- dir
    }

    # Now remove the "cd" entries
    cd_id <- grepl("^\\$ cd", dat, perl = TRUE)
    dirs <- dirs[!cd_id]
    dat <- dat[!cd_id]

    # calculate sizes
    sizes <- as.integer(sub("(^\\d*) .*", "\\1", dat, perl = TRUE))

    # expand the directories
    fs <- strsplit(dirs, "/", fixed = TRUE)
    fs <- lapply(
        fs,
        function(split_folder) {
            res <- sapply(seq_along(split_folder), function(x) paste(split_folder[1:x], collapse="/"))
            res[1L] <- "/"
            res
        }
    )

    # replicate sizes
    reps <- lengths(fs)
    sizes <- rep.int(sizes, reps)
    dirs <- unlist(fs)

    # aggregate
    res <- tapply(sizes, dirs, sum)

    # part_1
    part_1 <- sum(res[res<100000])

    # part_2
    required <- 30000000L - (70000000L - res[1L])
    part_2 <- min(res[res > required])

    # return
    list(part_1 = part_1, part_2 = part_2)
}
