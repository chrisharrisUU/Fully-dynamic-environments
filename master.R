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
source("Strategies/agnostic.R")
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
df <- rbind(rep("wA", 12),
            rep("lA", 12),
            rep(c("lA", "lB"), 6),
            rep(c("wA", "wB"), 6),
            rep(c("wA", "lA", "wB", "lB"), 3),
            rep(c("wA", "wA", "lB"), 4),
            rep(c("lB", "wB", "lB", "wA"), 3)) %>%
  as_tibble

### Strategies----
match_handler(df, "positivist", 1)
match_handler(df, "positivist", 5)
match_handler(df, "constructivist", 1)
match_handler(df, "constructivist", 5)
match_handler(df, "guessing")
match_handler(df, "omniscient", "positive")
match_handler(df, "omniscient", "neutral")
match_handler(df, "omniscient", "negative")
# To do:
#   - ratios for construction?
#   - how to integrate constructed memory?
#   - can constructed ever be a win?


sim_handler(strategy = "positivist",
            environment = "neutral",
            strat_var = 1,
            size = c(7, 12))
sim_handler(strategy = "constructivist",
            environment = "neutral",
            strat_var = 1,
            size = c(7, 12))

### Analyses

# Number of guesses
# Weighting
sequ <- seq(.1, 1, .05)
guess_no_pos <- guess_no_neu <- guess_no_neg <- vector(mode = "numeric", length = length(sequ))
for (i in sequ) {
  guess_no_pos[which(sequ == i)] <- sim_handler_guesses(strategy = "weighting",
                                            environment = "positive",
                                            strat_var = i,
                                            size = c(100, 120)) %>%
    summarise(avg = mean(no_of_guesses)) %>%
    unlist
  guess_no_neu[which(sequ == i)] <- sim_handler_guesses(strategy = "weighting",
                                                        environment = "neutral",
                                                        strat_var = i,
                                                        size = c(100, 120)) %>%
    summarise(avg = mean(no_of_guesses)) %>%
    unlist
  guess_no_neg[which(sequ == i)] <- sim_handler_guesses(strategy = "weighting",
                                                        environment = "negative",
                                                        strat_var = i,
                                                        size = c(100, 120)) %>%
    summarise(avg = mean(no_of_guesses)) %>%
    unlist
}
p <- rbind(tibble(guess_no = guess_no_pos,
             sequence = sequ,
             condition = "positive"),
      tibble(guess_no = guess_no_neu,
             sequence = sequ,
             condition = "neutral"),
      tibble(guess_no = guess_no_pos,
             sequence = sequ,
             condition = "negative")) %>%
  ggplot(aes(x = sequence,
             y = guess_no,
             color = condition)) +
  geom_point() +
  labs(x = "Weight",
       y = "Number of guesses",
       title = "Recency weight strategy") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()
ggplotly(p)

# Windows
sequ <- seq(1, 9, 1)
guess_no_pos <- guess_no_neu <- guess_no_neg <- vector(mode = "numeric", length = length(sequ))
for (i in sequ) {
  # Positive
  guess_no_pos[which(sequ == i)] <- sim_handler_guesses(strategy = "positivist",
                                                        environment = "positive",
                                                        strat_var = i,
                                                        size = c(100, 120)) %>%
    summarise(avg = mean(no_of_guesses)) %>%
    unlist
  # Neutral
  guess_no_neu[which(sequ == i)] <- sim_handler_guesses(strategy = "positivist",
                                                    environment = "neutral",
                                                    strat_var = i,
                                                    size = c(100, 120)) %>%
    summarise(avg = mean(no_of_guesses)) %>%
    unlist
  # Negative
  guess_no_neg[which(sequ == i)] <- sim_handler_guesses(strategy = "positivist",
                                                        environment = "negative",
                                                        strat_var = i,
                                                        size = c(100, 120)) %>%
    summarise(avg = mean(no_of_guesses)) %>%
    unlist
}
p <- rbind(tibble(guess_no = guess_no_pos,
       sequence = sequ,
       condition = "positive"),
      tibble(guess_no = guess_no_neu,
             sequence = sequ,
             condition = "neutral"),
      tibble(guess_no = guess_no_pos,
             sequence = sequ,
             condition = "negative")) %>%
  ggplot(aes(x = sequence,
             y = guess_no,
             color = condition)) +
  geom_jitter() +
  labs(x = "Window size",
       y = "Number of guesses",
       title = "Window strategy") +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()
ggplotly(p)
