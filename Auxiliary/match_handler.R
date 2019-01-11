match_handler <- function(ml, strategy, strat_var = NA) {
  dfexpectations <- matrix(nrow = nrow(ml[[1]]), ncol = ncol(ml[[1]])) %>%
    as_tibble() # Tibble to store expected choices
  if (strategy == "positivist") {
    # Windows
    for (i in 1:(ncol(ml[[1]]) - 1)) { # Iterate over strategy
      # Define lower window boundary
      j <- i - strat_var
      if (j < 1) {j <- 1}
      # Determine expected choices 
      dfexpectations[, i + 1] <- strat_positivist(ml[[1]][, j:i])
    }
  } else if (strategy == "constructivist") {
    # Constructivist
    df_constructed <- dfexpectations
    for (i in 1:(ncol(ml[[1]]) - 1)) { # Iterate over strategy
      # Define lower window boundary
      j <- i - strat_var
      if (j < 1) {j <- 1}
      # Determine expected choices 
      output <- strat_constr(cbind(ml[[1]][, j:i],
                                   ml[[2]][, j:i]))
      dfexpectations[, i + 1] <- output[,1]
      df_constructed[, i + 1] <- output[,2]
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
    nrow_of_df <- nrow(ml[[1]])
    for (i in 1:(ncol(ml[[1]]) - 1)) { # Iterate over strategy
      dfexpectations[, i + 1] <- strat_omni(best_choice[i], nrow_of_df)
    }
  } else if (strategy == "guessing") {
    # Guessing
    nrow_of_df <- nrow(ml[[1]])
    for (i in 1:(ncol(ml[[1]]) - 1)) { # Iterate over strategy
      dfexpectations[, i + 1] <- strat_guessing(nrow_of_df)
    }
  } else {
    warning("Please specify a strategy to be used!")
  }
  
  dfexpectations
}