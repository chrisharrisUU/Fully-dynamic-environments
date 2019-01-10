### Windows
strat_positivist <- function(dfpositivist) {
  # Determine expected next choice
  expected <- apply(dfpositivist, 1, windowchoice)
  
  # return expectations
  expected
}