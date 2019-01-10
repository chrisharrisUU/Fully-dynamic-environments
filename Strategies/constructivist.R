### Constructivist
strat_constr <- function(dfconstr) {
  # Determine expected next choice
  expected <- apply(dfconstr, 1, windowchoice, TRUE)
  
  # Tease apart
  expected <- str_split_fixed(expected, "-", 2) %>%
    as_tibble %>%
    mutate(V2 = ifelse(V2 == "", "Guess", V2))
  
  # return expectations
  expected
}
