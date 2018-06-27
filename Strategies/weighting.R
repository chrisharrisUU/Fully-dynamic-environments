# Weighting
strat_weight <- function(dfweight, weight) {
  # Tibble to store expected choices
  dfexpectations <- matrix(nrow = nrow(dfweight), ncol = ncol(dfweight)) %>%
    as_tibble()
  
  # Determine expected choices for each trial
  for (i in 1 : (ncol(dfweight) - 1)) {
    # Experienced window
    experienced_window <- dfweight[, 1 : i]
    
    # Determine expected next choice
    expected <- apply(experienced_win, 1, function(x) {
      # Previous outcomes within window
      losses_A <- length(which(x[1 : (length(x) - 1)] == "lA"))
      wins_A <- length(which(x[1 : (length(x) - 1)] == "wA"))
      losses_B <- length(which(x[1 : (length(x) - 1)] == "lB"))
      wins_B <- length(which(x[1 : (length(x) - 1)] == "wB"))
      # Most recent outcome
      recent_losses_A <- length(which(x[length(x)] == "lA"))
      recent_wins_A <- length(which(x[length(x)] == "wA"))
      recent_losses_B <- length(which(x[length(x)] == "lB"))
      recent_wins_B <- length(which(x[length(x)] == "wB"))
      # Totals
      all_A <- wins_A + losses_A + recent_wins_A + recent_losses_A
      all_B <- wins_B + losses_B + recent_wins_B + recent_losses_B
      
      # Ratio of wins with option A
      if (all_A == 0) {
        ratio_A <- 0
      } else {
        ratio_A <- (weight * recent_wins_A + (1 - weight) * wins_A) / all_A
      }
      # Ratio of wins with option B
      if (all_B == 0) {
        ratio_B <- 0
      } else {
        ratio_B <- (weight * recent_wins_B + (1 - weight) * wins_B) / all_B
      }
      
      # Determine next choice by comparing ratios
      if (all_A > all_B) { # A was chosen more frequently, everything from A's perspective
        if (all_A == i) { # A was chosen every single time
          if ((wins_A + recent_wins_A) == all_A) { # Ratio needn't equal 1. But with all wins we choose this option
            next_choice <- "A" # Always won. Choose A
          } else if (ratio_A == 0) {
            next_choice <- "B" # Always lost. Choose B
          } else {
            next_choice <- "Guess" # Mixed results with A only. No prediction
          }
        } else { # A was chosen more often but not always
          if (ratio_A > ratio_B) {
            next_choice <- "A" # Won more often with A. Choose A
          } else if (ratio_B > ratio_A) {
            next_choice <- "B" # Won more often with B. Choose B
          } else {
            next_choice <- "Guess" # Same results. No prediction
          }
        }
      } else if (all_B < all_A) { # Reversal: everything from B's perspective
        if (all_B == i) { # B was chosen every single time
          if ((wins_B + recent_wins_B) == all_B) { # Ratio needn't equal 1. But with all wins we choose this option
            next_choice <- "B" # Always won. Choose B
          } else if (ratio_B == 0) {
            next_choice <- "A" # Always lost. Choose A
          } else {
            next_choice <- "Guess" # Mixed results with B only. No prediction
          }
        } else { # B was chosen more often but not always
          if (ratio_B > ratio_A) {
            next_choice <- "B" # Won more often with B. Choose B
          } else if (ratio_A > ratio_B) {
            next_choice <- "A" # Won more often with A. Choose A
          } else {
            next_choice <- "Guess" # Same results. No prediction
          }
        }
      } else { # If both were chosen equally often
        if (ratio_A > ratio_B) {
          next_choice <- "A" # Choose A
        }
        if (ratio_B > ratio_A) {
          next_choice <- "B" # Choose B
        }
        if (ratio_A == ratio_B) {
          next_choice <- "Guess" # No prediction
        }
      }
    })
    
    # Save expectations
    dfexpectations[, i + 1] <- expected
  }
  
  # return expectations
  return(dfexpectations)
}
