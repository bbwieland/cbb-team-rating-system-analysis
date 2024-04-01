library(tidyverse)
library(MLmetrics)

theme_set(theme_bw())

# Import Data -------------------------------------------------------------

files <- list.files(path = "data")

read_data_file <- function(path) {
  read_csv(paste0("data/",path))
}

df <- map_dfr(.x = files, .f = ~ read_data_file(.x))


# Basic Data Validation ---------------------------------------------------

df %>%
  group_by(schedule_type) %>%
  summarise(teams = n(),
            team_games = sum(games))


# Schedule Evaluation --------------------------------------------------------

df %>%
  group_by(schedule_type) %>%
  summarise(mae = MAE(net_est, net_true),
            rmse = RMSE(net_est, net_true),
            pearson_r_sq = cor(net_est, net_true) ^ 2,
            spearman_r_sq = cor(net_est, net_true, method = "spearman") ^ 2)

ggplot(data = df, aes(x = net_est, y = net_true)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~schedule_type) +
  coord_equal() +
  geom_abline(slope = 1, intercept = 0) 

conf_ratings_true <- df %>%
  group_by(schedule_type, conf_true) %>%
  summarise(net = mean(net_true),
            net_est = mean(net_est)) %>%
  arrange(-net) %>%
  ungroup()

ggplot(conf_ratings_true, aes(x = net_est, y = net)) +
  geom_point() +
  facet_wrap(~schedule_type) +
  coord_equal() +
  geom_abline(slope = 1, intercept = 0) 

conf_ratings_random <- df %>%
  group_by(schedule_type, conf_random) %>%
  summarise(net = mean(net_true),
            net_est = mean(net_est)) %>%
  arrange(-net) %>%
  ungroup()

ggplot(conf_ratings_random, aes(x = net_est, y = net)) +
  geom_point() +
  facet_wrap(~schedule_type) +
  coord_equal() +
  geom_abline(slope = 1, intercept = 0) 

