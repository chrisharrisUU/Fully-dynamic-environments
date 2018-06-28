### Guessing
strat_guessing <- function(dfguess) {
  # Fill with guesses and return
  rep("Guess", nrow(dfguess)) %>%
    matrix(nrow = nrow(dfguess), byrow = TRUE) %>%
    as_tibble %>%              
    return
}    