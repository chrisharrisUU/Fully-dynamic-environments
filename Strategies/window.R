### Windows
strat_window <- function(dfwindow, winsize) {
  # Tibble to store expected choices
  dfexpectations <- matrix(nrow = nrow(dfwindow), ncol = ncol(dfwindow)) %>%
    as_tibble()
  
  # Determine expected choices for each trial
  for (i in winsize : (ncol(dfwindow) - 1)) {
    # Experienced window
    experienced_window <- dfwindow[, (i - winsize + 1) : i]
    
    # Determine expected next choice
    expected <- apply(experienced_window, 1, function(x) {
      # Previous outcomes within window
      losses_A <- length(which(x == "lA"))
      wins_A <- length(which(x == "wA"))
      losses_B <- length(which(x == "lB"))
      wins_B <- length(which(x == "wB"))
      
      # Ratio of wins with option A
      if (wins_A + losses_A == 0) {
        ratio_A <- 0
      } else {
        ratio_A <- wins_A / (wins_A + losses_A)
      }
      
      # Ratio of wins with option B
      if (wins_B + losses_B == 0) {
        ratio_B <- 0
      } else {
        ratio_B <- wins_B / (wins_B + losses_B)
      }
      # Determine next choice by comparing ratios
      if ((wins_A + losses_A) > (wins_B + losses_B)) { # A was chosen more frequently, everything from A's perspective
        if (wins_A + losses_A == winsize) { # A was chosen every single time
          if (ratio_A == 1) {
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
      } else if ((wins_A + losses_A) < (wins_B + losses_B)) { # Reversal: everything from B's perspective
        if (wins_B + losses_B == winsize) { # B was chosen every single time
          if (ratio_B == 1) {
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
      
      # Return expected choice
      return(next_choice)
    })
    
    # Save expectations
    dfexpectations[, i + 1] <- expected
  }
  
  # return expectations
  return(dfexpectations)
}