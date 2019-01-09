### Windows
strat_positivist <- function(dfpositivist, winsize) {
  i <- ncol(dfpositivist)
  if (winsize > i) {winsize <- i}
  # Experienced window
  experienced_window <- dfpositivist[, (i - winsize + 1):i]
  
  # Determine expected next choice
  expected <- apply(experienced_window, 1, windowchoice)
  
  # return expectations
  return(expected)
}
