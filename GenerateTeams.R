library(tidyverse)

library(cbbdata)

# Synthetic Teams ---------------------------------------------------------

set.seed(4133)

theme_set(theme_bw())

kp <- read_csv("kenpom23.csv")

ggplot(kp, aes(x = AdjOE)) +
  geom_histogram(bins = 20, color = "white", fill = "#247cab", aes(y = after_stat(density))) +
  stat_function(fun = function(x) dnorm(x, mean = mean(kp$AdjOE), sd = sd(kp$AdjOE)))

ggplot(kp, aes(x = AdjDE)) +
  geom_histogram(bins = 20, color = "white", fill = "#247cab", aes(y = after_stat(density))) +
  stat_function(fun = function(x) dnorm(x, mean = mean(kp$AdjDE), sd = sd(kp$AdjDE)))

O.mu <- mean(kp$AdjOE)
D.mu <- mean(kp$AdjDE)
O.sigma <- var(kp$AdjOE)
D.sigma <- var(kp$AdjDE)
OD.cor <- cov(kp$AdjOE, kp$AdjDE)

OD.mus <- c(O.mu, D.mu)
OD.cov.matrix <- matrix(c(O.sigma, OD.cor, OD.cor, D.sigma), nrow = 2, ncol = 2)

generate_synthetic_team <- function() {
  team <- MASS::mvrnorm(mu = OD.mus, Sigma = OD.cov.matrix)
  team.df <- data.frame(off = team[1], def = team[2])
  return(team.df)
}

teams <- map_dfr(.x = seq(1,362), .f = ~ generate_synthetic_team())

ggplot(data = teams, aes(x = off, y = def)) +
  geom_point() +
  scale_x_continuous(limits = c(80, 130)) +
  scale_y_continuous(limits = c(80, 130)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Synthetic AdjOE",
       y = "Synthetic AdjDE")

ggplot(data = kp, aes(x = AdjOE, y = AdjDE)) +
  geom_point() +
  scale_x_continuous(limits = c(80, 130)) +
  scale_y_continuous(limits = c(80, 130)) +
  geom_smooth(method = "lm", se = FALSE)

team_ratings <- teams %>%
  mutate(TeamID = row_number(),
         net = off - def) %>%
  arrange(-net) %>%
  select(TeamID, everything())

kp_conf <- cbbdata::cbd_kenpom_ratings(year = 2023) %>% pull(conf) %>% gsub("ind","NEC",.) %>% .[1:362]

team_ratings <- team_ratings %>%
  mutate(conf_random = sample(kp_conf),
         conf_true = kp_conf)

team_ratings %>% 
  group_by(conf_random) %>%
  summarise(avg_rating = mean(net)) %>%
  pull(avg_rating) %>% sd()

team_ratings %>% 
  group_by(conf_true) %>%
  summarise(avg_rating = mean(net)) %>%
  pull(avg_rating) %>% sd()

write_csv(team_ratings, "TeamRatings.csv")