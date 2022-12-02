# optimised ---------------------------------------------------------------
day02 <- function(filename) {

    # load_data ---------------------------------------------------------------
    raw_dat <- read.table(
        filename,
        sep = " ",
        colClasses = c("character", "character"),
        col.names = c("opponent", "me")
    )

    # add in opponent information ---------------------------------------------
    dat <- raw_dat
    dat$opponent <- match(dat$opponent, LETTERS[1:3])

    # part 1 ------------------------------------------------------------------
    dat$me <- match(dat$me, LETTERS[24:26])
    dat$outcome <-  ((dat$me - dat$opponent) + 1L) %% 3L
    score_1 <- sum(dat$me + dat$outcome * 3L)

    # part_2 ------------------------------------------------------------------
    names(dat) <- c("opponent", "outcome", "me")
    dat$outcome <- dat$outcome - 1L
    dat$me <- (dat$opponent + dat$outcome + 1L) %% 3L + 1L
    score_2 <- sum(dat$me + dat$outcome * 3L)

    # return ------------------------------------------------------------------
    list(score_1, score_2)

}


# # original ----------------------------------------------------------------
# day02b <- function(filename) {
#
#     # load_data ---------------------------------------------------------------
#     raw_dat <- read.table(
#         filename,
#         sep = " ",
#         colClasses = c("character", "character"),
#         col.names = c("opponent", "me")
#     )
#
#     # add in opponent information ---------------------------------------------
#     opponent_lookup <- c(A = "rock", B = "paper", C = "scissors")
#     dat <- raw_dat
#     dat$opponent <- opponent_lookup[dat$opponent]
#
#     # make a lookup of game results -------------------------------------------
#     options <- c("rock", "paper", "scissors")
#     possibilities <- expand.grid(opponent = options, me = options)
#     possibilities$outcome <- (as.integer(possibilities$me) - as.integer(possibilities$opponent)) %% 3L + 1L
#     outcome_lookup <- factor(c("draw", "win", "lose"), levels = c("lose", "draw", "win"))
#     possibilities$outcome <- outcome_lookup[possibilities$outcome]
#
#     # part1 -------------------------------------------------------------------
#     me_lookup <- c(X = "rock", Y = "paper", Z = "scissors")
#     dat_1 <- dat
#     dat_1$me <- me_lookup[dat$me]
#     dat_1 <- merge(dat_1, possibilities, all.x = TRUE, by = c("opponent", "me"))
#     score_1 <- sum(match(dat_1$me, options) + ((as.integer(dat_1$outcome) - 1L) * 3L))
#
#     # part2 -------------------------------------------------------------------
#     outcome_lookup <- c(X = "lose", Y = "draw", Z = "win")
#     dat_2 <- setNames(dat, c("opponent", "outcome"))
#     dat_2$outcome <- outcome_lookup[dat_2$outcome]
#     dat_2 <- merge(dat_2, possibilities, all.x = TRUE, by = c("opponent", "outcome"))
#     score_2 <- sum(as.integer(dat_2$me) + ((match(dat_2$outcome, outcome_lookup) - 1L) * 3L))
#
#     # return ------------------------------------------------------------------
#     list(score_1, score_2)
#
# }
#
# microbenchmark::microbenchmark(
#     day02("input02.txt"),
#     day02b("input02.txt")
# )

# Unit: microseconds
#                   expr       min         lq       mean     median         uq        max neval
#   day02("input02.txt")   774.998   812.5845   906.9198   833.1305   882.5195   6316.527   100
#  day02b("input02.txt") 10199.903 10451.7415 11786.0882 10613.5750 10822.5235 100100.909   100

