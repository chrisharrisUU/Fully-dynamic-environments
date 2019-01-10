# Main handler of simulation runs
sim_handler <- function(strategy, environment, strat_var = NA, size) {
  # List to store expected choices
  df_sim <- matrix(nrow = size[1], ncol = size[2]) %>%
    as_tibble()
  df_const <- df_sim
  ml <- list(df_sim, df_const)
  rm(df_sim, df_const)
  # Determine outcome probabilities
  if (environment == "positive") {
    probabilities <- tibble(acurve = CURVE_A_pos, bcurve = CURVE_B_pos)
  } else if (environment == "neutral") {
    probabilities <- tibble(acurve = CURVE_A_neu, bcurve = CURVE_B_neu)
  } else {
    probabilities <- tibble(acurve = CURVE_A_neg, bcurve = CURVE_B_neg)
  }
  
  # First round is always forced to guess
  ml[[1]][,1] <- rep("Guess", nrow(ml[[1]])) %>%
    guesses %>%
    outcome(prob = probabilities[1,])

  if (strategy == "positivist") {
    # Windows
    for (i in 1:(ncol(ml[[1]]) - 1)) { # Iterate over strategy
      # Define lower window boundary
      j <- i - strat_var + 1
      if (j < 1) {j <- 1}
      # Determine next choice 
      ml[[1]][, i + 1] <- strat_positivist(ml[[1]][, j:i]) %>%
        guesses %>%
        outcome(prob = probabilities[i,])
    }
  } else if (strategy == "constructivist") {
    # Constructivist
    for (i in 1:ncol(ml[[1]])) { # Iterate over strategy
      # Define lower window boundary
      j <- i - strat_var + 1
      if (j < 1) {j <- 1}
      # Determine next choice
      output <- strat_constr(cbind(ml[[1]][, j:i],
                                   ml[[2]][, j:i]))
      # Experienced
      ml[[1]][, i + 1] <- output[, 1] %>%
        unlist %>%
        guesses %>%
        outcome(prob = probabilities[i,])
      # Constructed
      ml[[2]][, i + 1] <- output[, 2] %>%
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
    for (i in 1:(ncol(ml[[1]]) - 1)) { # Iterate over strategy
      ml[[1]][, i + 1] <- strat_omni(dim(ml[[1]]), best_choice) %>%
        outcome(prob = probabilities[i,])
    }
  } else if (strategy == "guessing") {
    # Guessing
    for (i in 1:(ncol(ml[[1]]) - 1)) { # Iterate over strategy
      ml[[1]][, i + 1] <- strat_guessing(nrow(ml[[1]])) %>%
        guesses %>%
        outcome(prob = probabilities[i,])
    }
  } else {
    warning("Please specify a strategy to be used!")
  }
  
  # Return
  ml[[1]]
}

# Version two. Mainly identical but keeps tracks of number of guesses. Slightly less readable.
sim_handler_guesses <- function(strategy, environment, strat_var = NA, size) {
  # List to store expected choices
  df_sim <- matrix(nrow = size[1], ncol = size[2]) %>%
    as_tibble()
  no_of_guesses_constr <- no_of_guesses_exp <- df_const <- df_sim
  ml <- list(df_sim, df_const, no_of_guesses_exp, no_of_guesses_constr)
  rm(df_sim, df_const, no_of_guesses_exp, no_of_guesses_constr)
  # Determine best choice
  if (environment == "positive") {
    probabilities <- tibble(acurve = CURVE_A_pos, bcurve = CURVE_B_pos)
  } else if (environment == "neutral") {
    probabilities <- tibble(acurve = CURVE_A_neu, bcurve = CURVE_B_neu)
  } else {
    probabilities <- tibble(acurve = CURVE_A_neg, bcurve = CURVE_B_neg)
  }
  
  # First round is always forced to guess
  ml[[1]][,1] <- rep("Guess", nrow(ml[[1]])) %>%
    guesses %>%
    outcome(prob = probabilities[1,])
  
  if (strategy == "positivist") {
    # Windows
    for (i in 1:(ncol(ml[[1]]) - 1)) { # Iterate over strategy
      # Define lower window boundary
      j <- i - strat_var + 1
      if (j < 1) {j <- 1}
      # Determine next choice
      output <- strat_positivist(ml[[1]][, j:i])
      ml[[3]][which(output == "Guess"), i + 1] <- 1
      output <- guesses(unlist(output))
      ml[[1]][, i + 1] <- output %>%
        unlist() %>%
        guesses() %>%
        outcome(., prob = probabilities[i,])
    }
  } else if (strategy == "constructivist") {
    # Constructivist
    for (i in 1:ncol(ml[[1]])) { # Iterate over strategy
      # Define lower window boundary
      j <- i - strat_var + 1
      if (j < 1) {j <- 1}
      # Determine next choice
      output <- strat_constr(cbind(ml[[1]][, j:i],
                                   ml[[2]][, j:i]))
      # Add guesses to counter
      ml[[3]][which(output[, 1] == "Guess"), i + 1] <- 1
      ml[[4]][which(output[, 2] == "Guess"), i + 1] <- 1
      # Resolve guesses
      output[, 1] <- guesses(unlist(output[, 1]))
      # Commented out as constructed expectations do not get their guesses resolved
      # ml[[2]][, i + 1] <- guesses(unlist(ml[[2]][, i + 1]))
      
      # Update memory
      ml[[1]][, i + 1] <- outcome(unlist(output[, 1]), prob = probabilities[i,])
      ml[[2]][, i + 1] <- output[,2]
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
    for (i in 1:(ncol(ml[[1]]) - 1)) { # Iterate over strategy
      ml[[1]][, i + 1] <- strat_omni(dim(ml[[1]]), best_choice) %>%
        outcome(prob = probabilities[i,])
    }
  } else if (strategy == "guessing") {
    # Guessing
    for (i in 1:(ncol(ml[[1]]) - 1)) { # Iterate over strategy
      ml[[1]][, i + 1] <- strat_guessing(nrow(ml[[1]])) %>%
        guesses %>%
        outcome(prob = probabilities[i,])
    }
  } else {
    warning("Please specify a strategy to be used!")
  }
  
  ml
}
