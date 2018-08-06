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
  
  return(choice)
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
    unlist %>%
    return
}

 # Main handler of simulation runs
sim_handler <- function(strategy, environment, strat_var = NA, size) {
  df_sim <- matrix(nrow = size[1], ncol = size[2]) %>%
    as_tibble() # Tibble to store expected choices
  # Determine best choice
  if (environment == "positive") {
    probabilities <- tibble(acurve = CURVE_A_pos, bcurve = CURVE_B_pos)
  } else if (environment == "neutral") {
    probabilities <- tibble(acurve = CURVE_A_neu, bcurve = CURVE_B_neu)
  } else {
    probabilities <- tibble(acurve = CURVE_A_neg, bcurve = CURVE_B_neg)
  }
  
  # First round is always forced to guess
  df_sim[,1] <- rep("Guess", nrow(df_sim)) %>%
    guesses %>%
    outcome(prob = probabilities[1,])

  if (strategy == "window") {
    # Windows
    for (i in 1:(ncol(df_sim) - 1)) { # Iterate over strategy
      df_sim[, i + 1] <- strat_window(df_sim[, 1:i], winsize = strat_var) %>%
        guesses %>%
        outcome(prob = probabilities[i,])
    }
  } else if (strategy == "weighting") {
    # Weighting
    for (i in 1:(ncol(df_sim) - 1)) { # Iterate over strategy
      df_sim[, i + 1] <- strat_weight(df_sim[, 1:i], weight = strat_var) %>%
        guesses %>%
        outcome(prob = probabilities[i,])
    }
  } else if (strategy == "constructivist") {
    # Constructivist
    for (i in 1:ncol(df_sim)) { # Iterate over strategy
      df_sim[, i + 1] <- strat_constr(df_sim[, 1:i]) %>%
        select(V1) %>%
        unlist %>%
        guesses %>%
        outcome(prob = probabilities[i,])
    }
  } else if (strategy == "omniscient") {
    # Omniscient
    # Determine best choice
    if (strat_var == "positive") {
      best_choice <- tibble(acurve = CURVE_A_pos, bcurve = CURVE_B_pos)
    } else if (strat_var == "neutral") {
      best_choice <- tibble(acurve = CURVE_A_neu, bcurve = CURVE_B_neu)
    } else {
      best_choice <- tibble(acurve = CURVE_A_neg, bcurve = CURVE_B_neg)
    }
    best_choice %<>%
      transmute(best = ifelse(acurve > bcurve, "A", ifelse(acurve == bcurve, "Guess", "B"))) %>% unlist
    for (i in 1:(ncol(df_sim) - 1)) { # Iterate over strategy
      df_sim[, i + 1] <- strat_omni(dim(df_sim), best_choice) %>%
        outcome(prob = probabilities[i,])
    }
  } else if (strategy == "guessing") {
    # Guessing
    for (i in 1:(ncol(df_sim) - 1)) { # Iterate over strategy
      df_sim[, i + 1] <- strat_guessing(nrow(df_sim)) %>%
        guesses %>%
        outcome(prob = probabilities[i,])
    }
  } else {
    warning("Please specify a strategy to be used!")
  }
  
  return(df_sim)
}

# Version two. Mainly identical but keeps tracks of number of guesses. Slightly less readable.
sim_handler_guesses <- function(strategy, environment, strat_var = NA, size) {
  df_sim <- matrix(nrow = size[1], ncol = size[2]) %>%
    as_tibble() # Tibble to store expected choices
  no_of_guesses <- vector(mode = "integer", length = nrow(df_sim))
  # Determine best choice
  if (environment == "positive") {
    probabilities <- tibble(acurve = CURVE_A_pos, bcurve = CURVE_B_pos)
  } else if (environment == "neutral") {
    probabilities <- tibble(acurve = CURVE_A_neu, bcurve = CURVE_B_neu)
  } else {
    probabilities <- tibble(acurve = CURVE_A_neg, bcurve = CURVE_B_neg)
  }
  
  # First round is always forced to guess
  df_sim[,1] <- rep("Guess", nrow(df_sim)) %>%
    guesses %>%
    outcome(prob = probabilities[1,])
  
  if (strategy == "window") {
    # Windows
    for (i in 1:(ncol(df_sim) - 1)) { # Iterate over strategy
      df_sim[, i + 1] <- strat_window(df_sim[, 1:i], winsize = strat_var)
      no_of_guesses[which(df_sim[, i + 1] == "Guess")] <- no_of_guesses[which(df_sim[, i + 1] == "Guess")] + 1
      df_sim[, i + 1] <- guesses(unlist(df_sim[, i + 1]))
      df_sim[, i + 1] <- outcome(unlist(df_sim[, i + 1]), prob = probabilities[i,])
    }
  } else if (strategy == "weighting") {
    # Weighting
    for (i in 1:(ncol(df_sim) - 1)) { # Iterate over strategy
      df_sim[, i + 1] <- strat_weight(df_sim[, 1:i], weight = strat_var)
      no_of_guesses[which(df_sim[, i + 1] == "Guess")] <- no_of_guesses[which(df_sim[, i + 1] == "Guess")] + 1
      df_sim[, i + 1] <- guesses(unlist(df_sim[, i + 1]))
      df_sim[, i + 1] <- outcome(unlist(df_sim[, i + 1]), prob = probabilities[i,])
    }
  } else if (strategy == "constructivist") {
    # Constructivist
    for (i in 1:ncol(df_sim)) { # Iterate over strategy
      df_sim[, i + 1] <- strat_constr(df_sim[, 1:i])
      no_of_guesses[which(df_sim[, i + 1] == "Guess")] <- no_of_guesses[which(df_sim[, i + 1] == "Guess")] + 1
      df_sim[, i + 1] <- guesses(unlist(df_sim[, i + 1]))
      df_sim[, i + 1] <- outcome(unlist(df_sim[, i + 1]), prob = probabilities[i,])
    }
  } else if (strategy == "omniscient") {
    # Omniscient
    # Determine best choice
    if (strat_var == "positive") {
      best_choice <- tibble(acurve = CURVE_A_pos, bcurve = CURVE_B_pos)
    } else if (strat_var == "neutral") {
      best_choice <- tibble(acurve = CURVE_A_neu, bcurve = CURVE_B_neu)
    } else {
      best_choice <- tibble(acurve = CURVE_A_neg, bcurve = CURVE_B_neg)
    }
    best_choice %<>%
      transmute(best = ifelse(acurve > bcurve, "A", ifelse(acurve == bcurve, "Guess", "B"))) %>% unlist
    for (i in 1:(ncol(df_sim) - 1)) { # Iterate over strategy
      df_sim[, i + 1] <- strat_omni(dim(df_sim), best_choice) %>%
        outcome(prob = probabilities[i,])
    }
  } else if (strategy == "guessing") {
    # Guessing
    for (i in 1:(ncol(df_sim) - 1)) { # Iterate over strategy
      df_sim[, i + 1] <- strat_guessing(nrow(df_sim)) %>%
        guesses %>%
        outcome(prob = probabilities[i,])
    }
  } else {
    warning("Please specify a strategy to be used!")
  }
  
  cbind(df_sim, no_of_guesses) %>%
    return
}
