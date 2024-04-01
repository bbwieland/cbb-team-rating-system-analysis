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

# Generate Simulated Schedule ---------------------------------------------

teamIDs <- sample(team_ratings$TeamID) # randomly permute the Team IDs
team_n <- length(teamIDs) 

teamIDs.1H <- teamIDs[1:(team_n/2)] # subset the first half of the shuffled Team IDs
teamIDs.2H <- teamIDs[((team_n/2) + 1):team_n] # subset the second half of the shuffled Team IDs

games_per_gameday <- team_n / 2 # how many games are necessary for every team to play a game

shift <- emuR::shift # shift function offsets a vector by `delta`

build_matchups <- function(teams1, teams2, offset) {
  id1 <- teams1
  id2 <- shift(teams2, delta = offset) # shifts the second set of team IDs
  matchup_df <- data.frame(Team1 = id1, Team2 = id2)
  return(matchup_df)
}

# create 30 synthetic "Game-Weeks"
games_to_play =  1:150
games <- map_dfr(.x = games_to_play, .f = ~ build_matchups(teamIDs.1H, teamIDs.2H, offset = .x))

# game checker: should equal 1
sum(table(c(games$Team1, games$Team2)) == length(games_to_play)) / length(unique(c(games$Team1, games$Team2)))

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

# Actually Calculate Team Ratings ------------------------------------

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
  mutate(schedule_type = "Random Schedule 150 Games")

write_csv(ratings_final, "data/RandomHuge.csv")
