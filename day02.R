# load_data ---------------------------------------------------------------
raw_dat <- read.table(
    "input02.txt",
    sep = " ",
    colClasses = c("character", "character"),
    col.names = c("opponent", "me")
)

# add in opponent information ---------------------------------------------
opponent_lookup <- c(A = "rock", B = "paper", C = "scissors")
dat <- raw_dat
dat$opponent <- opponent_lookup[dat$opponent]

# make a lookup of game results -------------------------------------------
options <- c("rock", "paper", "scissors")
possibilities <- expand.grid(opponent = options, me = options)
possibilities$outcome <- (as.integer(possibilities$me) - as.integer(possibilities$opponent)) %% 3L + 1L
outcome_lookup <- factor(c("draw", "win", "lose"), levels = c("lose", "draw", "win"))
possibilities$outcome <- outcome_lookup[possibilities$outcome]

# part1 -------------------------------------------------------------------
me_lookup <- c(X = "rock", Y = "paper", Z = "scissors")
dat_1 <- dat
dat_1$me <- me_lookup[dat$me]
dat_1 <- merge(dat_1, possibilities, all.x = TRUE)
(score_1 <- sum(match(dat_1$me, options) + ((as.integer(dat_1$outcome) - 1L) * 3L)))

# part2 -------------------------------------------------------------------
outcome_lookup <- c(X = "lose", Y = "draw", Z = "win")
dat_2 <- setNames(dat, c("opponent", "outcome"))
dat_2$outcome <- outcome_lookup[dat_2$outcome]
dat_2 <- merge(dat_2, possibilities, all.x = TRUE)
(score_2 <- sum(as.integer(dat_2$me) + ((match(dat_2$outcome, outcome_lookup) - 1L) * 3L)))
