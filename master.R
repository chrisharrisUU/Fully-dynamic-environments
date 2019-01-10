### Setup----

# Load dependencies
if (!require(needs)) {install.packages("needs"); library(needs)}
needs(dplyr, magrittr, stringr, here, ggplot2)

### Functions----
source("Auxiliary/match_handler.R")
source("Auxiliary/sim_handler.R")
source("Auxiliary/determinechoice.R")
# Strategies
source("Strategies/positivist.R")
source("Strategies/constructivist.R")
source("Strategies/guessing.R")
source("Strategies/omniscient.R")

### Constants----
# Set up sine curves
x <- seq(0,pi*2*5,by = .1) # Trials
var <- 2 # Amplitude of sine curve
range <- max(max(abs(.2 * x + var * sin(x))),max(abs(-.2 * x + var * sin(x))))

# Positive
CURVE_A_pos <- (0.2 * x + var * sin(x)) / (2 * range) + .5 # Sine curve for option A
CURVE_B_pos <- (0.2 * x + var * sin(x + pi)) / (2 * range) + .5 # Sine curve for option B
# Neutral
CURVE_A_neu <- (var * sin(x)) / (2 * range) + .5 # Sine curve for option A
CURVE_B_neu <- (var * sin(x + pi)) / (2 * range) + .5 # Sine curve for option B
# Negative
CURVE_A_neg <- (-0.2 * x + var * sin(x)) / (2 * range) + .5 # Sine curve for option A
CURVE_B_neg <- (-0.2 * x + var * sin(x + pi))/(2 * range) + .5 # Sine curve for option B

# Create practice dataset
experienced <- rbind(rep("wA", 36),
            rep("lA", 36),
            rep(c("lA", "lB"), 18),
            rep(c("wA", "wB"), 18),
            rep(c("wA", "lA", "wB", "lB"), 9),
            rep(c("wA", "wA", "lB"), 12),
            rep(c("lB", "wB", "lB", "wA"), 9)) %>%
  as_tibble
constructed <- matrix(nrow = nrow(experienced),
                      ncol = ncol(experienced)) %>%
  data.frame() %>%
  transmute_all(.funs = funs(as.character(.))) %>%
  as_tibble()

memory <- list(experienced, constructed)
rm(experienced, constructed)

### Tests----

## Matching
match_handler(memory, "positivist", 1)
match_handler(memory, "positivist", 5)
match_handler(memory, "constructivist", 1)
match_handler(memory, "constructivist", 5)
match_handler(memory, "guessing")
match_handler(memory, "omniscient", "positive")
match_handler(memory, "omniscient", "neutral")
match_handler(memory, "omniscient", "negative")

## Matching fit
match_handler(memory, "positivist", 1) %>%
  matching(memory[[1]], .)

## Simulating
sim_handler(strategy = "positivist",
            environment = "neutral",
            strat_var = 1,
            size = c(7, 12))
sim_handler(strategy = "constructivist",
            environment = "neutral",
            strat_var = 1,
            size = c(7, 12))
sim_handler(strategy = "omniscient",
            environment = "neutral",
            strat_var = 1,
            size = c(7, 12))
sim_handler(strategy = "guessing",
            environment = "neutral",
            strat_var = 1,
            size = c(7, 12))

## Simulating while counting guesses
sim_handler_guesses(strategy = "positivist",
            environment = "neutral",
            strat_var = 1,
            size = c(7, 12))
sim_handler_guesses(strategy = "constructivist",
            environment = "neutral",
            strat_var = 1,
            size = c(7, 12))

### Analyses----

## Number of guesses
sequ <- seq(1, 9, 1)

# Positivist
df <- nr_guesses_per_strat("positivist")
df %>%
  ggplot(aes(x = sequence,
             y = guess_no,
             color = condition)) +
  geom_jitter() +
  scale_x_discrete(limits = sequ) +
  labs(x = "Window size",
     y = "Number of guesses",
     title = "Window strategy") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()

# Constructivist
df <- nr_guesses_per_strat("constructivist")
df %>%
  ggplot(aes(x = sequence,
             y = guess_no,
             color = condition)) +
  geom_jitter() +
  scale_x_discrete(limits = sequ) +
  labs(x = "Window size",
     y = "Number of guesses",
     title = "Constructivist strategy") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()

## MAtching strategies
matching_index <- vector(mode = "integer", length = 14)
for (i in 1:14) {
  matching_index[i] <- match_handler(memory, "positivist", i) %>%
    matching(memory[[1]], .) %>%
    mean
}

tibble(x = 1:14,
       y = matching_index) %>%
  ggplot(aes(x, y)) +
  geom_point() +
  scale_x_discrete(limits = 1:14) +
  labs(x = "Window size",
       y = "Mean guesses",
       title = "Mean number of guesses for positivist strategy") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()