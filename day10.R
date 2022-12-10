day10 <- function(filename) {
    dat <- readLines(filename)

    # only interested in the numeric values
    values <- sub("[[:alpha:]]*[[:space:]]*", "", dat)
    values <- as.integer(values)

    # work out the additions each cycle
    additions <- lapply(values, function(x) if (!is.na(x)) c(0L, x) else 0L)

    # calculate the register during each cycle
    x_value <- cumsum(unlist(additions)) + 1L
    x_value <- c(1L, x_value)
    x_value <- x_value[-length(x_value)]

    # part 1
    cycles <- seq(from = 20L, to = 220L, by = 40L)
    cat(sprintf("Answer to part 1 = %d.\n", sum(x_value[cycles] * cycles)))

    # part_2
    width <- 40L; height <- 6L
    distance <- abs(((seq_along(x_value) - 1L) %% width) - x_value)
    pixels <- distance <= 1L
    crt <- matrix(pixels, nrow = height, byrow = TRUE)
    rotate <- function(x) t(apply(x, 2, rev))
    cat("Answer to part 2 = (see plot window)")
    image(
        x=seq_len(width), y=seq_len(height), rotate(crt),
        col = c("white", "black"), asp = 1,
        xaxt = "n", yaxt = "n", ann = FALSE, bty = "n"
    )
}
