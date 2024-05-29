library(nnet)

mlogit_model <- readRDS("data/mlogit_model_winner.rds")
nb_model_home <- readRDS("data/nb_model_home.rds")
nb_model_away <- readRDS("data/nb_model_away.rds")

euro_data <- readRDS("data/em_teams.rds")

goal_data <- readRDS("data/match_data.rds")

goal_data <- subset(goal_data, tournament %in% c("UEFA Euro", "FIFA World Cup"), select = c("home_score", "away_score", "result"))

goal_data$outcome <- paste0(goal_data$home_score, ":", goal_data$away_score)


x <- sample( 3:10000, 1)




(x^3)^2 == x^6
x