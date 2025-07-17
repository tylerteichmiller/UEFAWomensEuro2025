#################
#
# T2 Stan Program
#
#################
#Align data for Stan
N <- length(uefa$HomeTeam)
home_goals <- uefa$HomeGoals
away_goals <- uefa$AwayGoals
home_rank <- uefa$HomeRank
away_rank <- uefa$AwayRank
mu_b0 <- 1
sigma_b0 <- 0.3
mu_b1 <- 0
sigma_b1 <- 0.3
n_exact <- length(all2025$HomeTeam)
home25_rank <- all2025$HomeRank
away25_rank <- all2025$AwayRank

# Prep data for Stan in a list
pset4_data <- list(N = N, home_goals = home_goals, away_goals = away_goals,
                   home_rank = home_rank, away_rank = away_rank, 
                   mu_b0 = mu_b0, mu_b1 = mu_b1, sigma_b0 = sigma_b0,
                   sigma_b1 = sigma_b1, n_exact = n_exact, 
                   home25_rank = home25_rank, away25_rank = away25_rank)

# Fit the model and call the summary function
pset4_model <- rstan::stan_model(file = url("https://raw.githubusercontent.com/UEFAWomensEuro2025/main/UEFA.stan"))    
set.seed(2002)
fit <- rstan::sampling(object = pset4_model,
                       data = pset4_data,
                       chains = 4)
rstan::summary(fit)[["summary"]]

#   - each column is 4000 samples of number of goals the home team scores 
#     for each of the 24 matches in the dataset
post_pred_HG_samples <- rstan::extract(object = fit,
                                    pars = c("home_goals25"))[["home_goals25"]]
post_pred_AG_samples <- rstan::extract(object = fit,
                                    pars = c("away_goals25"))[["away_goals25"]]


# binary matrix if Home team wins/draws in game g in simulation number sim
HW <- post_pred_HG_samples > post_pred_AG_samples
DRAW <- post_pred_HG_samples == post_pred_AG_samples
# get all home/away teams together
hteams <- all2025$HomeTeam
ateams <- all2025$AwayTeam
# get the unique teams that are playing
# This will be a good reference list to keep all of our teams straight
unq_teams <- unique(hteams)
# set up empty matrix of binary 0/1
# entry is 1 if team g in simulation sim advances
advance_binary <- matrix(nrow = 4000, ncol = 16)
# This long for loop compiles all of the results of which teams advance 
# and which teams do not advance in each of the 4000 simulations. 
for(sim in 1:4000){
  # Need to set up blank points vector and dataframe tracking the points
  pts <- c()
  df <- data.frame(HT = hteams, AT = ateams, HScore = rep(0,24),
                   AScore = rep(0,24))
  # For each game in the 2025 simulated tournament
  for(g in 1:24){
    # if the home team wins, add 3 to their points
    # if they draw, add one to home team and away team points
    # if home team loses, add 3 to away team points
    if(HW[sim, g] == TRUE)
      {df$HScore[g] = df$HScore[g] + 3} else{
      if(DRAW[sim, g] == TRUE){df$HScore[g] = df$HScore[g] + 1; 
            df$AScore[g] = df$AScore[g] + 1} else{
              df$AScore[g] = df$AScore[g] + 3}
      }
  }
  # For each unique team
  for(j in 1:16){
    # calculate the total number of points the simulated team scored
    # store it in the points list with the correct index aligning with the
    # unq_teams list
    a <- df %>% filter(HT == unq_teams[j]) %>% summarize(sum = sum(HScore))
    b <- df %>% filter(AT == unq_teams[j]) %>% summarize(sum = sum(AScore))
    pts[j] = a+b
  }
  # Need generic simulated finish df
  sim_finish <- data.frame(Team = unq_teams, Pts = unlist(pts))
  for(k in 1:4){
    # For each group, arrange the teams in desc order
    block <- sim_finish[(4*k-3):(4*k), ]
    finish <- block%>%arrange(desc(block$Pts))
    # if there is a 2nd.3rd place tie, do tiebreaking criteria number 1
    # and update the binary advance matrix 
    if(finish[2, ]$Pts == finish[3, ]$Pts){
      gameid <- (all2025 %>% 
        filter((HomeTeam == finish[2, ]$Team | AwayTeam == finish[2, ]$Team)&
                 (HomeTeam == finish[3, ]$Team | AwayTeam == finish[3, ]$Team)))$GameID
      htid <- which(unq_teams == all2025$HomeTeam[gameid])
      atid <- which(unq_teams == all2025$AwayTeam[gameid])
      if(HW[sim, gameid]){advance_binary[sim, htid]=1;advance_binary[sim,atid]=0} 
        else{advance_binary[sim,htid]=0; advance_binary[sim,atid]=1}
    } else{
      tid2 <- which(unq_teams == finish[2, ]$Team)
      advance_binary[sim, tid2] = 1
      tid3 <- which(unq_teams == finish[3, ]$Team)
      advance_binary[sim, tid3] = 0
    }
    # advance binary for the first and 4th place
    tid1 <- which(unq_teams == finish[1, ]$Team)
    advance_binary[sim, tid1] = 1
    tid4 <- which(unq_teams == finish[4, ]$Team)
    advance_binary[sim,tid4] = 0
  }
}

# Get all of the Point estimates for the probability of advancing for each team
post_pred_advance_prob <- apply(advance_binary, 2, mean)
# Create some Bootstrap confidence intervals for the probability of advancing 
for(i in 1:16){
  results <- advance_binary[, i]
  boot_probs <- replicate(10000, mean(sample(results, replace = TRUE)))
  cat("Team: ", unq_teams[i], "\n \t Point Estimate: ", 
      post_pred_advance_prob[i], "\n \t Interval: ", 
      quantile(boot_probs, probs = c(0.025, 0.975)), "\n")
}

