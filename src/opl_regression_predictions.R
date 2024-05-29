{packages <- c("optimx", "tidyverse", "MASS", "nnet")
for (package in packages[!packages %in% installed.packages()]){
  install.packages(package)
}
lapply(packages, library,  character.only = TRUE)
}
source("src/functions.R")

if(!file.exists("data/match_data.rds")){
  source("src/elo_calculations.R")
  matches <- readRDS("data/match_data.rds")
} else {
  matches <- readRDS("data/match_data.rds")
}

matches$result <- as.factor(matches$result)

# df <- matches %>%
#   dplyr::select(result, elo_diff_matches) %>%
#   na.omit()
# 
# cats <- sort(unique(df$result))  # Different categories
# J <- length(cats)  # Number of categories
# 
# Z <-
#   matrix(NA, nrow = length(df$result), ncol = J)  # Empty indicator matrix
# 
# for (j in 1:J) {
#   Z[, j] <- df$result == cats[j]
# }
# 
# head(Z)
# 
# source("ordered_ll_function.R")
# 
# 
# X <- df$elo_diff_matches # Why no column of 1s?
# 
# 
# startvals <-
#   c(0, 1, 0.5, 0) # How many startvalues do we need?
# 
# res <- optim(
#   startvals,
#   ll_ologit,
#   Z = Z,
#   X = X,
#   method = "BFGS",
#   control = list(fnscale = -1, trace = T),
#   hessian = TRUE
# )
# 
# results <- cbind(res$par, sqrt(diag(solve(-res$hessian))))
# 
# rownames(results) <- c("selfplacement",
#                        "republican",
#                        "democrat",
#                        "knowledge",
#                        "tau1",
#                        "tau2",
#                        "tau3")
# 
# colnames(results) <- c("Coef", "SE")
# 


## ologit Model using MASS

ologit_model <-  polr(result ~ elo_diff_matches,
               data = matches,
               Hess=TRUE)



## mlogit Model using MASS
mlogit_model <- multinom(result ~ elo_diff_matches, 
                         data = matches)


# 10-fold Cross validation
cv_ologit <- cross_val_function(k = 10, df = matches, model = ologit_model)
cv_mlogit <- cross_val_function(k = 10, df = matches, model = mlogit_model)
# mlogit proofs to do better using the current data.

saveRDS(mlogit_model, "data/model_data.rds")