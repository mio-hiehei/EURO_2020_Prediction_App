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

