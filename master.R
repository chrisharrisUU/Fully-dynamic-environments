### Setup----

# Load dependencies
if (!require(needs)) {install.packages("needs"); library(needs)}
needs(dplyr, magrittr, purrr, stringr, here, ggplot2)

### Functions----
source("Auxiliary/match_handler.R")
source("Auxiliary/sim_handler.R")
source("Auxiliary/determinechoice.R")
source("Auxiliary/helpers.R")
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
df <- nr_guesses_per_strat("constructivist", seq = sequ, dims = c(100, 120))
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

## Matching strategies
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



### Simulation----
n <- 1000 # number of "participants"
c <- 1 # counter
strats <- c("positivist", "constructivist", "guessing", "omniscient") # strategies
envs <- c("positive", "neutral", "negative") # environments
winds <- c(1:14, seq(15, 315, by = 15)) # Window sizes
success_results <- tibble(strategy = "test",
                          environment = "neutral",
                          window = 0,
                          guesses = 0,
                          success = 0) # Tibble for outcomes
# Set up progress bar
progress <- txtProgressBar(1, (length(strats) - 2) * length(envs) * length(winds) + 2, style = 3)

for (s in strats) {
  for (env in envs) {
    if (s %in% c("omniscient", "guessing")) {
      # Run simulation
      result <- sim_handler_guesses(strategy = s,
                                    environment = env,
                                    strat_var = 1,
                                    size = c(n, length(x)))
      # Average number of guesses
      nr_guesses <- result[[3]] %>%
        rowSums(na.rm = TRUE) %>%
        mean
      # Average number of successful outcomes
      nr_success <- result[[1]] %>%
        transmute_all(.funs = funs(grepl("w", .))) %>%
        transmute_all(.funs = funs(ifelse(., 1, 0))) %>%
        rowSums() %>%
        mean
      # Save in tibble
      success_results[c,] <- c(s, env, 1, nr_guesses, nr_success)
      
      # Show progress bar
      setTxtProgressBar(progress, c)
      # Update counter
      c <- c + 1
    } else {
      for (w in winds) {
        # Run simulation
        result <- sim_handler_guesses(strategy = s,
                                      environment = env,
                                      strat_var = w,
                                      size = c(n, length(x)))
        # Average number of guesses
        nr_guesses <- result[[3]] %>%
          rowSums(na.rm = TRUE) %>%
          mean
        # Average number of successful outcomes
        nr_success <- result[[1]] %>%
          transmute_all(.funs = funs(grepl("w", .))) %>%
          transmute_all(.funs = funs(ifelse(., 1, 0))) %>%
          rowSums() %>%
          mean
        # Save in tibble
        success_results[c,] <- c(s, env, w, nr_guesses, nr_success)
        
        # Show progress bar
        setTxtProgressBar(progress, c)
        # Update counter
        c <- c + 1
      }
    }
  }
}

# Prepare results
success_results %<>%
  mutate(strategy = factor(strategy, levels = strats),
         environment = factor(environment, levels = envs),
         window = factor(window, levels = winds),
         guesses = as.numeric(guesses),
         success = as.numeric(success),
         runs = n)

save(success_results, file = "Output/sim_results.RData")

## Most successful strategy
# Descriptive
success_results %>%
  group_by(strategy, environment) %>%
  summarize(max = max(success),
            min = min(success),
            avg = mean(success))

# Graph
success_results %>%
  ggplot(aes(x = window,
             y = success,
             shape = environment,
             color = strategy)) +
  geom_point() +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()

# Most successful window sizes
success_results %>%
  filter(strategy %in% c("positivist", "constructivist")) %>%
  group_by(strategy) %>%
  top_n(3, success)

# Most successful strategies
success_results %>%
  filter(strategy != "omniscient") %>%
  top_n(3, success) %>%
  arrange(desc(success))

# Most successful strategies compared to normative omniscient strategy
success_results %>%
  top_n(3, success) %>%
  arrange(desc(success))

## Number of guesses per strategy
# Descriptive
success_results %>%
  group_by(strategy, environment) %>%
  summarize(max = max(guesses),
            min = min(guesses),
            avg = mean(guesses))

# Graph
success_results %>%
  ggplot(aes(x = window,
             y = guesses,
             shape = environment,
             color = strategy)) +
  geom_point() +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()
