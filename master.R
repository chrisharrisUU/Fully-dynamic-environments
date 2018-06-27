# for now we only create a small dataset and call functions
# later this will be the controller for all simulations

if (!require(needs)) {install.packages("needs"); library(needs)}
needs(dplyr, magrittr)

df <- rbind(rep("wA", 6),
            rep("lA", 6),
            rep(c("lA", "lB"), 3),
            rep(c("wA", "wB"), 3)) %>%
  as_tibble



strat_window(df, 1)
