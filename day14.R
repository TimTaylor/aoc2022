filename <- "input14.txt"

dat <- readLines(filename)

parse_rock <- function(rock) {
    rock <- gsub(" -> ", ",", rock, fixed = TRUE)
    rock <- scan(text = rock, what = 1L, sep = ",", quiet = TRUE)
    x <- rock[c(TRUE, FALSE)]
    y <- rock[c(FALSE, TRUE)]
    outx <- outy <- integer(abs(x[length(x)] - x[1L]) + abs(y[length(y)] - y[1L]) + 1L)
    index <- 1L
    for (i in 2:length(x)) {
        diffx <- x[i - 1L] - x[i]
        diffy <- y[i - 1L] - y[i]
        if (diffx == 0L) {
            new_index <- index + abs(diffy) + 1L
            outy[index:(new_index - 1L)] <- seq.int(from = y[i - 1L], to = y[i])
            outx[index:(new_index - 1L)] <- x[i]
        } else {
            new_index <- index + abs(diffx) + 1L
            outy[index:(new_index - 1L)] <- y[i]
            outx[index:(new_index - 1L)] <- seq.int(from = x[i - 1L], to = x[i])
        }
        index <- new_index
    }
    cbind(outx, outy)
}

rock <- lapply(dat, parse_rock)
rock <- do.call(rbind, rock)

minx <- min(rock[, 1L]) - 1L
maxx <- max(rock[, 1L]) + 1L
maxy <- max(rock[, 2L])
miny <- min(rock[, 2L])


width <- maxx + 1L
height <- maxy
cave <- matrix(0L, nrow = width, ncol = height)
cave[rock] <- 1L


flag <- TRUE
total_grains <- 0L
while (flag) {

    # new grain
    grain <- c(500L, 0L)
    dim(grain) <- c(1L,2L)

    # movement
    moving <- TRUE
    while (moving) {
        # downward movement
        down <- grain + c(0L, 1L)
        if (!cave[down]) {
            if (down[2L] == maxy) {
                flag <- FALSE
                break
            }
            grain <- down
            next
        }

        # downleft movement
        downleft <- grain + c(-1L, 1L)
        if (!cave[downleft]) {
            if (downleft[1L] == minx) {
                flag <- FALSE
                break
            }
            grain <- downleft
            next
        }

        # downleft movement
        downright <- grain + c(1L, 1L)
        if (!cave[downright]) {
            if (downright[1L] == maxx) {
                flag <- FALSE
                break
            }
            grain <- downright
            next
        }

        # at rest
        cave[grain] <- 1L
        moving <- FALSE
    }

    total_grains <- total_grains + 1L

}

(part_1 <- total_grains - 1L)

cave <- matrix(0L, nrow = width + 500L, ncol = height + 6L)
cave[rock] <- 1L
flag <- TRUE
total_grains <- 0L
while (flag) {

    # new grain
    grain <- c(500L, 0L)
    dim(grain) <- c(1L,2L)

    # movement
    moving <- TRUE
    while (moving) {
        # downward movement
        down <- grain + c(0L, 1L)
        if (!cave[down]) {
            if (down[2L] == maxy + 2L) {
                cave[grain] <- 1L
                break
            }
            grain <- down
            next
        }

        # downleft movement
        downleft <- grain + c(-1L, 1L)
        if (!cave[downleft]) {
            grain <- downleft
            next
        }

        # downleft movement
        downright <- grain + c(1L, 1L)
        if (!cave[downright]) {
            grain <- downright
            next
        }

        # at rest
        moving <- FALSE
        cave[grain] <- 1L
        if(all(grain == c(500L, 0L)))
            flag <- FALSE
    }

    total_grains <- total_grains + 1L

}

(part_2 <- total_grains)
