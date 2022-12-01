day01 <- function(filename) {

    # load data
    dat <- scan(filename, what = 1L, blank.lines.skip = FALSE, quiet = TRUE)

    # allocate maximum required space
    n <- length(dat)
    elf_sums <- integer(n)

    # calculate na's all at once
    isna <- is.na(dat)

    # calculate the sum for each elf
    index <- 1L
    cnt <- 0L
    for (i in seq_len(n)) {
        if (isna[i]) {
            elf_sums[index] = cnt
            cnt <- 0L
            index <- index + 1L
            next
        } else {
            cnt <- cnt + dat[i]
        }
    }

    # sort the elves by sums
    sorted_elves <- sort(elf_sums, decreasing = TRUE)

    # return
    list(
        part1 = sorted_elves[1L],
        part2 = sum(sorted_elves[1:3])
    )
}

# # alternative splitty one for comparison ----------------------------------
# day01b <- function(filename) {
#     dat <- scan(filename, what = 1L, blank.lines.skip = FALSE, quiet = TRUE)
#     na <- is.na(dat)
#     idx <- cumsum(na)
#     dat <- split.default(dat[!na], idx[!na])
#     dat <- vapply(dat, sum, 1) # allowing for conversion to double
#
#     list(
#         part1 = max(dat),
#         part2 = sum(sort(dat, decreasing = TRUE)[1:3])
#     )
# }
#
# # alternative string splitty one for comparison ---------------------------
# day01c <- function(filename) {
#     size <- file.info(filename)$size
#     dat <- readChar(filename, size)
#     dat <- strsplit(dat, "\n\n", fixed = TRUE)[[1L]]
#     dat <- strsplit(dat, "\n")
#     dat <- sapply(dat, function(x) sum(as.integer(x)))
#
#     list(
#         part1 = max(dat),
#         part2 = sum(sort(dat, decreasing = TRUE)[1:3])
#     )
# }

# # alternative tapply one for comparison -----------------------------------
# day01d <- function(filename) {
#     dat <- scan(filename, what = 1L, blank.lines.skip = FALSE, quiet = TRUE)
#     index <- cumsum(is.na(dat))
#     elf_sums <- tapply(dat, index, sum, na.rm = TRUE)
#     sorted_elves <- sort.int(elf_sums, decreasing = TRUE)
#     list(part1 = sorted_elves[[1L]], part2 = sum(sorted_elves[1:3]))
# }
#
# # timings -----------------------------------------------------------------
# f <- "input01.txt"
# microbenchmark::microbenchmark(day01(f), day01b(f), day01c(f), day01d(f))
## Unit: microseconds
##      expr      min        lq      mean    median        uq      max neval
##  day01(f)  444.004  451.9025  462.9844  460.3430  467.3865  545.397   100
## day01b(f)  493.794  505.9210  517.1750  512.8420  523.2350  576.548   100
## day01c(f) 1242.912 1259.6060 1294.2090 1284.5645 1300.5045 1850.187   100
## day01d(f)  562.087  571.9165  678.8506  580.5935  595.0625 5743.475   100
