library(tidyverse)
library(MLmetrics)

team_ratings <- read_csv("TeamRatings.csv")
set.seed(4133)

# Game Simulation Function --------------------------------------------

tempo <- 67.1
eff <- 104.8
game.var <- 7.99 ^ 2 # backcalculated from KP MAE values

simulate_game <- function(team1, team2, df = team_ratings) {
  rating1 <- df %>% filter(TeamID == team1)
  rating2 <- df %>% filter(TeamID == team2)
  
  team1.xpts <- ((rating1$off + rating2$def) / 200) * tempo
  team2.xpts <- ((rating2$off + rating1$def) / 200) * tempo
  
  team1.pts <- rnorm(1, mean = team1.xpts, sd = sqrt(game.var))
  team2.pts <- rnorm(1, mean = team2.xpts, sd = sqrt(game.var))
  
  team1.ppp = team1.pts / tempo
  team2.ppp = team2.pts / tempo
  
  team1.win <- team1.pts > team2.pts
  
  team1.wp <- 1 - pnorm(0, mean = team1.xpts - team2.xpts, sd = sqrt(game.var + game.var))
  team1.scorediff = team1.xpts - team2.xpts
  
  output <- data.frame(ID1 = team1, ID2 = team2, Pts1 = team1.pts, Pts2 = team2.pts,
                       PPP1 = team1.ppp, PPP2 = team2.ppp,
                       Win1 = as.numeric(team1.win), WP1 = team1.wp, 
                       ProjDiff = team1.scorediff, TrueDiff = team1.pts - team2.pts)
  
  return(output)
}

# Build Conference Games --------------------------------------------------

roundrobin <- Gmisc::roundrobin

column_to_df <- function(column, input_matrix) {
  
  team1 = colnames(input_matrix)[column]
  team2 = input_matrix[,column]
  
  output_df <- data.frame(Team1 = team1, Team2 = team2) %>%
    mutate(gameweek = seq(1, nrow(.))) %>%
    mutate(GameID = paste0(ifelse(Team1 > Team2, Team1, Team2), ifelse(Team1 > Team2, Team2, Team1)))
  return(output_df)
  
}

build_conf_schedule_true <- function(conference, n_games = 16) {

  conference_ids <- team_ratings %>%
    filter(conf_true == conference) %>%
    pull(TeamID)
  
  sched_matrix <- roundrobin(length(conference_ids), rounds = n_games)
  colnames(sched_matrix) <- conference_ids
  
  index_to_value <- function(x, vector) {
    val <- vector[x]
    return(val)
  }
  
  map_dfr(.x = seq(1, length(conference_ids)), 
          .f = ~ column_to_df(.x, input_matrix = sched_matrix)) %>%
    mutate(Team2 = index_to_value(Team2, vector = conference_ids)) %>%
    na.omit() %>%
    mutate(across(everything(),as.numeric)) %>%
    mutate(GameIDFull = as.numeric(paste0(GameID, gameweek))) %>%
    select(Team1, Team2)
  
}

# build_conf_schedule_random("ACC")

conf_games <- map_dfr(.x = unique(team_ratings$conf_true),
                      .f = ~ build_conf_schedule_true(.x))

# output: conf_games

# Combine Full Schedule ---------------------------------------------------

games <- conf_games

# Play Simulated Schedule -------------------------------------------------

results <- map2_dfr(.x = games$Team1, .y = games$Team2, .f = ~ simulate_game(team1 = .x, team2 = .y), 
                    .progress = "Simulating season...")

get_team_rating <- function(TeamID, df = results) {
  
  in_ID1 = TeamID %in% df$ID1
  
  if (in_ID1 == TRUE) {
    opponents <- df %>% filter(ID1 == TeamID) %>% pull(ID2) %>% unique()
    opp_games <- df %>% filter(ID2 %in% opponents)
    opp_agg <- opp_games %>% filter(ID1 != TeamID) %>% summarise(ppp_scored = mean(PPP2),
                                                                 ppp_allowed = mean(PPP1))
    
    team_agg <- df %>% filter(ID1 == TeamID) %>% summarise(ppp_scored = mean(PPP1),
                                                           ppp_allowed = mean(PPP2))
    
    games_played <- df %>% filter(ID2 == TeamID | ID1 == TeamID) %>% nrow()
    off <- (team_agg$ppp_scored - opp_agg$ppp_allowed) * 2
    def <- (team_agg$ppp_allowed - opp_agg$ppp_scored) * 2
    
    rating_df <- data.frame(TeamID = TeamID, off = (off * 100) + eff, def = (def * 100) + eff) %>%
      mutate(net = off - def, games = games_played)
    return(rating_df)
  }
  
  if (in_ID1 == FALSE) {
    opponents <- df %>% filter(ID2 == TeamID) %>% pull(ID1) %>% unique()
    opp_games <- df %>% filter(ID1 %in% opponents)
    opp_agg <- opp_games %>% filter(ID2 != TeamID) %>% summarise(ppp_scored = mean(PPP1),
                                                                 ppp_allowed = mean(PPP2))
    
    team_agg <- df %>% filter(ID2 == TeamID) %>% summarise(ppp_scored = mean(PPP2),
                                                           ppp_allowed = mean(PPP1))
    
    games_played <- df %>% filter(ID2 == TeamID | ID1 == TeamID) %>% nrow()
    off <- (team_agg$ppp_scored - opp_agg$ppp_allowed) * 2
    def <- (team_agg$ppp_allowed - opp_agg$ppp_scored) * 2
    
    rating_df <- data.frame(TeamID = TeamID, off = (off * 100) + eff, def = (def * 100) + eff) %>%
      mutate(net = off - def, games = games_played)
    return(rating_df)
  }
}


# Verify Assumptions of Simulated Schedule --------------------------------

MAE(results$ProjDiff, results$TrueDiff)

(table(c(results$ID1, results$ID2)))

# Actually Calculate Team Ratings ------------------------------------

teamIDs <- unique(team_ratings$TeamID)
estimated_ratings <- map_dfr(.x = teamIDs, .f = ~ get_team_rating(TeamID = .x),
                             .progress = "Estimating ratings...")

ratings_merge <- left_join(team_ratings, estimated_ratings, by = "TeamID", suffix = c("_true","_est"))

ggplot(ratings_merge, aes(x = off_true, y = off_est)) +
  geom_point() +
  coord_equal()

ggplot(ratings_merge, aes(x = def_true, y = def_est)) +
  geom_point() +
  coord_equal()

ggplot(ratings_merge, aes(x = net_true, y = net_est)) +
  geom_point() +
  coord_equal()

# Evaluation Metrics ------------------------------------------------------

# Raw Correlations

cor(ratings_merge$off_est, ratings_merge$off_true)
cor(ratings_merge$def_est, ratings_merge$def_true)
cor(ratings_merge$net_est, ratings_merge$net_true)

# MAE

MAE(ratings_merge$off_est, ratings_merge$off_true)
MAE(ratings_merge$def_est, ratings_merge$def_true)
MAE(ratings_merge$net_est, ratings_merge$net_true)

# Performance Evaluation --------------------------------------------------

ratings_final <- ratings_merge %>%
  mutate(schedule_type = "True Conferences (No Non-Con)")

write_csv(ratings_final, "data/ConfTrueFull.csv")