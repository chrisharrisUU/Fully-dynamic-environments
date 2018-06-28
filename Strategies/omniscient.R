strat_omni <- function(dfomni, acurve, bcurve) {
 # Determine best choice
  best_choice <- tibble(acurve, bcurve) %>%
    transmute(best = ifelse(acurve > bcurve, "A", ifelse(acurve == bcurve, "Guess", "B"))) %>% unlist
  
  # Fill with guesses and return
  rep(best_choice, nrow(dfomni)) %>%
    matrix(nrow = nrow(dfomni), byrow = TRUE) %>%
    as_tibble %>%
    return
}
