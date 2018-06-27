### Guessing
strat_guessing <- function(dfguess) {
  # Tibble to store expected choices
  dfexpectations <- matrix(nrow = nrow(dfguess), ncol = ncol(dfguess)) %>%
    as_tibble()
  # Fill with guesses
  apply(dfexpectations, 2, function(x) {rep("Guess", nrow(dfexpectations))}) %>%
    return
}