match_handler <- function(df, strategy, strat_var = NA) {
  dfexpectations <- matrix(nrow = nrow(df), ncol = ncol(df)) %>%
    as_tibble() # Tibble to store expected choices
  if (strategy == "window") {
    # Windows
    for (i in 1:(ncol(df) - 1)) { # Iterate over strategy
      dfexpectations[, i + 1] <- strat_window(df[, 1:i], winsize = strat_var)
    }
  } else if (strategy == "weighting") {
    # Weighting
    for (i in 1:(ncol(df) - 1)) { # Iterate over strategy
      dfexpectations[, i + 1] <- strat_weight(df[, 1:i], weight = strat_var)
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
    for (i in 1:(ncol(df) - 1)) { # Iterate over strategy
      dfexpectations[, i + 1] <- strat_omni(dim(df), best_choice)
    }
  } else if (strategy == "guessing") {
    # Guessing
    for (i in 1:(ncol(df) - 1)) { # Iterate over strategy
      dfexpectations[, i + 1] <- strat_guessing(nrow(df))
    }
  } else {
    warning("Please specify a strategy to be used!")
  }
  
  return(dfexpectations)
}
