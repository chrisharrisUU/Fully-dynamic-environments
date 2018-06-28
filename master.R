# for now we only create a small dataset and call functions
# later this will be the controller for all simulations
# Differentiate somewhere between 

### Setup----

# Load dependencies
if (!require(needs)) {install.packages("needs"); library(needs)}
needs(dplyr, magrittr)

### Functions----
source("Strategies/guessing.R")
source("Strategies/weighting.R")
source("Strategies/window.R")

### Constants----

# Set up sine curves
x <- seq(0,pi*2*5,by = .1) # Trials
var <- 2 # Amplitude of sine curve
range <- max(max(abs(.2*x+var*sin(x))),max(abs(-.2*x+var*sin(x))))

# Positive
CURVE_A_pos <- (0.2*x+var*sin(x))/(2*range)+.5 # Sine curve for option A
CURVE_B_pos <- (0.2*x+var*sin(x+pi))/(2*range)+.5 # Sine curve for option B
# Neutral
CURVE_A_neu <- (var*sin(x))/(2*range)+.5 # Sine curve for option A
CURVE_B_neu <- (var*sin(x+pi))/(2*range)+.5 # Sine curve for option B
# Negative
CURVE_A_neg <- (-0.2*x+var*sin(x))/(2*range)+.5 # Sine curve for option A
CURVE_B_neg <- (-0.2*x+var*sin(x+pi))/(2*range)+.5 # Sine curve for option B


# Create practice dataset
df <- rbind(rep("wA", 12),
            rep("lA", 12),
            rep(c("lA", "lB"), 6),
            rep(c("wA", "wB"), 6),
            rep(c("wA", "lA", "wB", "lB"), 3),
            rep(c("wA", "wA", "lB"), 4),
            rep(c("lB", "wB", "lB", "wA"), 3)) %>%
  as_tibble


# WSLS
strat_window(df, 1)

# All Windowsizes
strat_window(df, 2)

# Weighting
strat_weight(df, 0)
strat_weight(df, .1)
strat_weight(df, .9)
strat_weight(df, 1)

# Guessing
strat_guessing(df)

# Omniscient
strat_omni(df, CURVE_A_neu, CURVE_B_neu)

### List of strategies
# Window :)
# Recency weighting :)
# Constructivist
# Guess :)
# Omniscient :)

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