day12 <- function(filename) {
    dat <- readLines(filename)

    # split input
    dat <- strsplit(dat, "", fixed = TRUE)

    # pull out width and height
    height <- length(dat)
    width <- length(dat[[1L]])

    # put in to matrix
    dat <- unlist(dat)
    dat <- matrix(dat, nrow = height, ncol = width, byrow = TRUE)

    # function to go from two coords to one
    to_one_dim <- function(i, j) i + (j - 1L) * height

    # pull out source and target positions
    src <- which(dat == "S", arr.ind = TRUE)
    target <- which(dat == "E", arr.ind = TRUE)

    # Convert to int
    dat <- match(dat, letters)
    dat <- as.integer(dat)
    dim(dat) <- c(height, width)

    # add sentinel border and adjust src and target
    dat <- cbind(999L, dat, 999L)
    dat <- rbind(999L, dat, 999L)
    src <- src + 1L
    target <- target + 1L
    dat[src] <- 1
    dat[target] <- 26

    # Generate graph
    graph <- vector("list", width * height)
    index <- 1L

    for (j in 1:width) {
        for (i in 1:height) {
            # allow for sentinel border
            ii <- i + 1L
            jj <- j + 1L

            # pull out value
            value <- dat[ii, jj]

            # pull out neighbour coords and values
            neighbour_coords <- c(ii - 1L, ii, ii + 1L, ii, jj, jj + 1L, jj, jj - 1L)
            dim(neighbour_coords) <- c(4L, 2L)
            neighbour_values <- dat[neighbour_coords]

            # return those less than or equal to distance away of 1
            idx <- neighbour_values - value <= 1L
            tmp <- neighbour_coords[idx, , drop = FALSE]

            # convert from two coords to single
            graph[[index]] <- to_one_dim(i = tmp[, 1L] - 1L, j = (tmp[,2L] - 1L))
            index <- index + 1L
        }
    }

    # convert src and target from two to one dim
    src <- to_one_dim(i = src[, 1L] - 1L, j = src[, 2L] - 1L)
    target <- to_one_dim(i = target[, 1L] - 1L, j = target[, 2L] - 1L)

    # dijkstra's algorithm
    dijkstra <- function(graph, source, target) {
        Q <- seq_along(graph)
        distances <- rep_len(Inf, length(Q))
        distances[source] <- 0L

        while (length(Q) > 0L) {
            d <- distances[Q]
            idx <- which.min(d)
            value <- Q[idx]
            if (value == target)
                return(d[idx])
            Q <- Q[-idx]
            neigbours <- graph[[value]]
            for (v in neigbours) {
                alt <- distances[value] + 1L
                if (alt < distances[v]) {
                    distances[v] <- alt
                }
            }
        }
    }

    # part 1
    part_1 <- dijkstra(graph, src, target)

    # part 2-  preprocess graph to remove nodes with identical neighbours to a's
    neighbours_identical <- function(i, j, value) {
        neighbour_coords <- c(i - 1L, i, i + 1L, i, j, j + 1L, j, j - 1L)
        dim(neighbour_coords) <- c(4L, 2L)
        neighbour_values <- dat[neighbour_coords]
        all(neighbour_values == dat[i, j])
    }
    a <- which(dat == 1L, arr.ind = TRUE)
    idx <- mapply(neighbours_identical, a[,1L], a[,2L])
    a <- a[!idx,]
    a <- to_one_dim(i = a[, 1L] - 1L, j = a[, 2L] - 1L)
    dist <- numeric(length(a))
    for (i in seq_along(dist)) {
        dist[i] <- dijkstra(graph = graph, source = a[i], target = target)
    }

    part_2 <- min(dist)

    # return
    list(part_1 = part_1, part_2 = part_2)
}
