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
#
#
# # timings -----------------------------------------------------------------
# f <- "input01.txt"
# microbenchmark::microbenchmark(day01(f), day01b(f), day01c(f))
## Unit: microseconds
##       expr      min        lq      mean    median        uq      max neval
##   day01(f)  437.188  451.8085  463.3308  458.7310  467.1945  585.037   100
##  day01b(f)  497.200  509.4715  523.0626  517.7335  523.2125  886.597   100
##  day01c(f) 1249.468 1273.8850 1302.8698 1291.1740 1309.0990 1809.601   100
