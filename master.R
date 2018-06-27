# for now we only create a small dataset and call functions
# later this will be the controller for all simulations
# Differentiate somewhere between 

# Load dependencies
if (!require(needs)) {install.packages("needs"); library(needs)}
needs(dplyr, magrittr)


# Create practice dataset
df <- rbind(rep("wA", 6),
            rep("lA", 6),
            rep(c("lA", "lB"), 3),
            rep(c("wA", "wB"), 3)) %>%
  as_tibble


# WSLS
strat_window(df, 1)

# All Windowsizes
strat_window(df, 2)

# Weighting
strat_weight(df, .1) # These two return equal results
strat_weight(df, .9) # Check function

# Guessing
strat_guessing(df)

### List of strategies
# Window :)
# Recency weighting
# Constructivist
# Guess :)
# Omniscient

### Code snippet for resolving guesses

# Resolve guesses
guesses <- vector(mode = "character", length = length(which(expected == "Guess")))
# Set up dice and avoid uncertainty with 0.5
dice <- runif((length(guesses)), 0, 1)
while (any(dice == 0.5)) {
  dice[which(dice == 0.5)] <- runif((length(dice[which(dice == 0.5)])), 0, 1)
}
# Assign to option A or B
guesses[which(dice < 0.5)] <- "A"
guesses[which(dice > 0.5)] <- "B"
expected[which(expected == "Guess")] <- guesses