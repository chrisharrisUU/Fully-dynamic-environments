### Code snippet for resolving guesses
guesses <- function(choice) {
  # Resolve guesses
  guesses <- vector(mode = "character", length = length(which(choice == "Guess")))
  # Set up dice and avoid uncertainty with 0.5
  dice <- runif((length(guesses)), 0, 1)
  while (any(dice == 0.5)) {
    dice[which(dice == 0.5)] <- runif((length(dice[which(dice == 0.5)])), 0, 1)
  }
  # Assign to option A or B
  guesses[which(dice < 0.5)] <- "A"
  guesses[which(dice > 0.5)] <- "B"
  choice[which(choice == "Guess")] <- guesses
  
  choice
}

# Function for determining outcomes
outcome <- function(choices, prob) {
  prob <- unlist(prob)
  # Set up dice and avoid uncertainty with 0.5
  dice <- runif((length(choices)), 0, 1)
  while (any(dice == 0.5)) {
    dice[which(dice == 0.5)] <- runif((length(dice[which(dice == 0.5)])), 0, 1)
  }
  tibble(choices, dice) %>%
    transmute(outcome = ifelse(choices == "A" & dice <= prob[1], "wA",
                               ifelse(choices == "A" & dice > prob[1], "lA",
                                      ifelse(choices == "B" & dice <= prob[2], "wB", "lB")))) %>%
    unlist
}

# Summarize number of guesses for a strategy per condition
nr_guesses_per_strat <- function(strategy, sequ = sequ) {
  guess_no_pos <- guess_no_neu <- guess_no_neg <- vector(mode = "numeric", length = length(sequ))
  for (i in sequ) {
    # Positive
    guess_no_pos[which(sequ == i)] <- sim_handler_guesses(strategy = "positivist",
                                                          environment = "positive",
                                                          strat_var = i,
                                                          size = c(100, 120))[[3]] %>%
      rowSums(na.rm = TRUE) %>%
      mean()
    # Neutral
    guess_no_neu[which(sequ == i)] <- sim_handler_guesses(strategy = "positivist",
                                                          environment = "neutral",
                                                          strat_var = i,
                                                          size = c(100, 120))[[3]] %>%
      rowSums(na.rm = TRUE) %>%
      mean()
    # Negative
    guess_no_neg[which(sequ == i)] <- sim_handler_guesses(strategy = "positivist",
                                                          environment = "negative",
                                                          strat_var = i,
                                                          size = c(100, 120))[[3]] %>%
      rowSums(na.rm = TRUE) %>%
      mean()
  }
  
  # Create tibble
  rbind(tibble(guess_no = guess_no_pos,
               sequence = sequ,
               condition = "positive"),
        tibble(guess_no = guess_no_neu,
               sequence = sequ,
               condition = "neutral"),
        tibble(guess_no = guess_no_pos,
               sequence = sequ,
               condition = "negative"))
}

# Matching
matching <- function(experienced, expected) {
  matches <- matrix(nrow = nrow(expected), ncol = ncol(expected)) %>%
    as_tibble()
  
  
  for (i in 2:ncol(expected)) {
    matches[experienced[, i] %>%
              unlist() %>%
              substr(., 2, 2) == expected[, i], i] <- 1
  }
  
  matches %>%
    rowSums(na.rm = TRUE)
}