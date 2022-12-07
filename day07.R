day07 <- function(filename) {

    # load input
    dat <- readLines(filename)

    # remove lines that do nothing (ls and dir)
    dat <- dat[dat != "$ ls"]
    dat <- dat[grep("^dir ", dat, perl = TRUE, invert = TRUE)]

    # preallocate space for result and set index to 1
    n <- length(dat)
    files <- sizes <- character(n)
    index <- 1L

    # loop (first row is cd to root so we ignore)
    dir <- "/"
    for (i in 2:n) {

        # pull out the command
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
        # file listing
        } else {
            tmp <- strsplit(row, " ", fixed = TRUE)[[1L]]
            sizes[index] <- tmp[[1L]]
            files[index] <- sprintf("%s%s", dir, tmp[[2L]])
            index <- index + 1L
        }
    }

    # remove space not needed and convert sizes to integer
    files <- files[seq_len(index - 1L)]
    sizes <- as.integer(sizes[seq_len(index - 1L)])

    # calculate and expand the directories
    dirs <- dirname(files)
    fs <- strsplit(dirs, "/", fixed = TRUE)
    fs <- .mapply(
        function(split_folder, s) {
            res <- sapply(seq_along(split_folder), function(x) paste(split_folder[1:x], collapse="/"))
            res[1L] <- "/"
            list2DF(list(dir = res, size = rep.int(s, length(res))))
        },
        dots = list(fs, sizes),
        MoreArgs = NULL
    )
    fs <- do.call(rbind, fs)

    # remove duplicates and aggregate
    fs<- fs[!duplicated(fs),]
    res <- tapply(fs$size, fs["dir"], sum)

    # part_1
    part_1 <- sum(res[res<100000])

    # part_2
    required <- 30000000L - (70000000L - res[1L])
    part_2 <- min(res[res > required])

    # return
    list(part_1 = part_1, part_2 = part_2)

}
