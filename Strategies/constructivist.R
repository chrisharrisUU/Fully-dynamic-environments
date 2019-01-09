### Constructivist
strat_constr <- function(dfconstr, winsize) {
  i <- ncol(dfconstr)
  if (winsize > i) {winsize <- i}
  # Experienced window
  experienced_window <- dfconstr[, (i - winsize + 1):i]
  
  # Determine expected next choice
  expected <- apply(experienced_window, 1, windowchoice, TRUE)
  
  # Tease apart
  expected <- str_split_fixed(expected, "-", 2) %>%
    as_tibble %>%
    mutate(V2 = ifelse(V2 == "", "Guess", V2))
  
  # return expectations
  return(expected)
}
