windowchoice <- function(x, constructivist = FALSE) {
  # Size of window
  winsize <- length(x)
  # Previous outcomes within window
  losses_A <- length(which(x[1:length(x)] == "lA"))
  wins_A <- length(which(x[1:length(x)] == "wA"))
  losses_B <- length(which(x[1:length(x)] == "lB"))
  wins_B <- length(which(x[1:length(x)] == "wB"))
  # Totals
  all_A <- wins_A + losses_A
  all_B <- wins_B + losses_B
  
  # Ratio of wins with option A
  if (all_A == 0) {
    ratio_A <- 0
  } else {
    ratio_A <- wins_A / all_A
  }
  # Ratio of wins with option B
  if (all_B == 0) {
    ratio_B <- 0
  } else {
    ratio_B <- wins_B / all_B
  }
  
  # Determine next choice by comparing ratios
  if (all_A > all_B) { # A was chosen more frequently, everything from A's perspective
    if (all_A == winsize) { # A was chosen every single time
      if (wins_A == all_A) { # Ratio needn't equal 1. But with all wins we choose this option
        next_choice <- "A" # Always won. Choose A
      } else if (ratio_A == 0) {
        next_choice <- "B" # Always lost. Choose B
      } else {
        next_choice <- "Guess" # Mixed results with A only. No prediction
      }
    } else {# A was chosen more often but not always
      if (ratio_A > ratio_B) {
        next_choice <- "A" # Won more often with A. Choose A
      } else if (ratio_B > ratio_A) {
        next_choice <- "B" # Won more often with B. Choose B
      } else {
        next_choice <- "Guess" # Same results. No prediction
      }
    }
  } else if (all_B > all_A) { # Reversal: everything from B's perspective
    if (all_B == winsize) { # B was chosen every single time
      if (wins_B == all_B) { # Ratio needn't equal 1. But with all wins we choose this option
        next_choice <- "B" # Always won. Choose B
      } else if (ratio_B == 0) {
        next_choice <- "A" # Always lost. Choose A
      } else {
        next_choice <- "Guess" # Mixed results with B only. No prediction
      }
    } else {# B was chosen more often but not always
      if (ratio_B > ratio_A) {
        next_choice <- "B" # Won more often with B. Choose B
      } else if (ratio_A > ratio_B) {
        next_choice <- "A" # Won more often with A. Choose A
      } else {
        next_choice <- "Guess" # Same results. No prediction
      }
    }
  } else {# If both were chosen equally often
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
  
  if (constructivist) {
    # Constructivist addition
    if (next_choice == "A") {
      if (ratio_B >= 0.5) { # THIS SHOULD PROBABLY NOT BE .5!!!!!
        next_choice <- "A-wB"
      } else {
        next_choice <- "A-lB"
      }
    } else if (next_choice == "B") {
      if (ratio_A >= 0.5) {
        next_choice <- "B-wA"
      } else {
        next_choice <- "B-lA"
      }
    } else {
      next_choice <- "Guess-Guess"
    }
  }
  
  # Return expected choice
  next_choice
}