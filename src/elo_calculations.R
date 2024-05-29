{packages <- c("elo", "tidyverse", "glue")
  for (package in packages[!packages %in% installed.packages()]){
    install.packages(package)
  }
  lapply(packages, library,  character.only = TRUE)
}
source("src/functions.R")

matches <- read_csv("data/results.csv")
em2020 <- c("Belgium",  "Denmark","Germany", "England", "France","Italy","Croatia","Netherlands", "North Macedonia", "Austria", "Portugal", "Russia","Scotland","Sweden","Switzerland","Slovakia","Spain","Czech Republic", "Turkey","Ukraine","Hungary","Wales")

matches <- matches %>%
  mutate(result = if_else(home_score > away_score, 1,
                          if_else(home_score == away_score, 0.5, 0)),
         game_type = ifelse(tournament %in% c("FIFA World Cup", "UEFA Euro"), "Friendly", "Tournament")) %>%
  filter(date < Sys.Date() & date > "1980-01-01", # only consider games from 1980 onwards
         away_team %in% em2020 | home_team %in% em2020) %>%
  filter(tournament %in% c("Friendly", "FIFA World Cup", "FIFA World Cup qualification", "UEFA Euro qualification", "UEFA Euro", "UEFA Nations League", "Confederations Cup"))


teams <- data.frame(team = unique(c(matches$home_team, matches$away_team)))

teams <- teams %>%
  mutate(elo = 1500)


matches <- matches %>%
  arrange(date) %>%
  mutate(id = 1:nrow(matches))

elo_diff_df <- data.frame(elo_diff_matches = numeric(),
                          id = numeric())

for (i in seq_len(nrow(matches))) {
  match <- matches[i, ]
  
  # Pre-match ratings
  teamA_elo <- subset(teams, team == match$home_team)$elo
  teamB_elo <- subset(teams, team == match$away_team)$elo
  
  # Let's update our ratings
  
    
  new_elo <- elo_fun(home_team = match$home_team,
                elo_home = teamA_elo,
                hfa = 68.3,
                away_team = match$away_team,
                elo_away = teamB_elo,
                country = match$country,
                home_goals = match$home_score,
                away_goals = match$away_score,
                game_type = "Friendly")
  
  if(is.null(new_elo)){
    
  } else {
  
    
    # The results come back as a data.frame
    # with team A's new rating in row 1 / column 1
    # and team B's new rating in row 1 / column 2
    teamA_new_elo <- new_elo[1, 1]
    teamB_new_elo <- new_elo[1, 2]
    
    # We then update the ratings for teams A and B
    # and leave the other teams as they were
    teams <- teams %>%
      mutate(elo = if_else(team == match$home_team, teamA_new_elo,
                           if_else(team == match$away_team, teamB_new_elo, elo)))
    
    elo_diff_matches <- teamA_new_elo - teamB_new_elo
    id <- i
    elo_diff_df[i,] <- cbind(elo_diff_matches, id)
  }
  
  if(i %% 500 == 0){
  cat(round( i / nrow(matches) * 100, 1), "%", "\n")
    
  }
}

matches <- merge(matches, elo_diff_df, by = "id")
em_teams <- filter(teams, team %in% em2020)

# a <- em_teams$elo[em_teams$team == "Turkey"]
# b <- em_teams$elo[em_teams$team == "Belgium"]
# 
# elo.prob(a, b)

saveRDS(matches, "data/match_data.rds")
saveRDS(em_teams, "data/em_teams.rds")
