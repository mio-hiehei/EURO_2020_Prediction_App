# browseURL("https://accso.de/magazin/bundesliga-tippen-mit-ki/")

elo_fun <- function(home_team,
                    elo_home,
                    hfa = 68.3,
                    away_team,
                    elo_away,
                    country,
                    home_goals,
                    away_goals,
                    game_type = "tournament"){
  require(glue)
  if(is.na(home_goals) | is.na(away_goals)){
    cat(glue("{home_team} vs. {away_team}: {home_goals} - {away_goals}. Skipping this."))
    return(NULL)
  }
  # This needs an optimization: See 
  # browseURL("http://opisthokonta.net/?p=1387") for that
  if(grepl(x = country, pattern = home_team) | grepl(x = country, pattern = away_team)){
    
    diff_elo <- elo_home - elo_away + hfa
    
  } else {
    
    diff_elo <- elo_home - elo_away
    
  }
  
  outcome_expected <- 1 / (1 + 10  ^ ( -diff_elo / 400)  )
  
  
  if(home_goals > away_goals){
    
    outcome_true <- 1
    
  } else if(away_goals > home_goals){
    
    
    outcome_true <- 0
    
  } else {
    
    outcome_true <- 0.5
  }
  
  diff_goal <- abs(home_goals - away_goals)
  
  if(diff_goal <= 1){
    
    margin_of_victory <- 1
    
  } else {
    
    margin_of_victory <- ( log2(1.7 * diff_goal * 2) )/(2 + 0.001 * diff_elo ) 
    
  }
  
  if(game_type == "tournament"){
    
    k <- 30
  } else {
    
    k <- 20
  }
  
  
  elo_gain <- k * ( outcome_true - outcome_expected ) * margin_of_victory
  
  elo_home1 <- elo_home + elo_gain
  elo_away1 <- elo_away - elo_gain
  
  return(data.frame(elo_home1 = elo_home1,
                    elo_away1 = elo_away1))
  
}

ll_ologit <- function(theta, Z, X){
  K <- ncol(X)
  J <- ncol(Z)
  
  # Subset theta
  beta <- theta[1:K]
  tau <- theta[(K + 1):(K + J -1)] #because we have 1 tau less than categories, that is why - 1
  
  # linear predictor
  U <- X %*% beta
  
  # Probabilities in a matrix
  probs <- matrix(nrow = length(U), ncol = J)
  
  # Calculate the Probabilities for each category according to Tau values
  
  # First category
  probs[,1] <- 1 / (1 + exp(-(tau[1] - U)))
  
  # in-between categories
  for(j in 2:(J - 1)){
    probs[, j] <- (1 / (1 + exp(-(tau[j] - U))) - 1 / (1 + exp(-(tau[j - 1] - U))))
  }
  
  # last category
  probs[,J] <-1 - 1 / (1 + exp(-(tau[J - 1] - U)))
  
  # sum up all the probabilities for Log Likelihood
  ll <- sum(log(probs[Z]))
  
  return(ll)
}

cross_val_function <- function(k, df, model){
  
  n <- nrow(df)
  
  data_list <- split(df, sample(rep(1:ceiling(n),  each = ceiling(n / k), length.out = n)))
  
  iter_cv <- function(df){
    
    df$predictions <- predict(model, newdat = df, type = "class")
    
    percent_correctly_predicted <- sum(diag(as.matrix(table(df$predictions, df$result))) ) / nrow(df)
    return(percent_correctly_predicted)
    
  }
  
  cv_outcome <- sapply(data_list, iter_cv)
  
  mean_cv <- mean(cv_outcome)
  
  return(list(cv_outcome = cv_outcome,
              mean_cv = mean_cv))
  
}

