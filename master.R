### Setup----

# Load dependencies
if (!require(needs)) {install.packages("needs"); library(needs)}
needs(dplyr, magrittr)

### Functions----
source("Auxiliary/match_handler.R")
source("Auxiliary/sim_handler.R")
# Strategies
source("Strategies/guessing.R")
source("Strategies/weighting.R")
source("Strategies/window.R")
source("Strategies/omniscient.R")

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

### Strategies----
match_handler(df, "window", 1)
match_handler(df, "window", 5)
match_handler(df, "weighting", .1)
match_handler(df, "weighting", .5)
match_handler(df, "weighting", .9)
match_handler(df, "weighting", 1)
match_handler(df, "guessing")
match_handler(df, "omniscient", "positive")
match_handler(df, "omniscient", "neutral")
match_handler(df, "omniscient", "negative")

sim_handler(strategy = "weighting",
            environment = "neutral",
            strat_var = .9,
            size = c(7, 12))



### List of strategies
# Window :)
# Recency weighting :)
# Constructivist
# Guess :)
# Omniscient :)