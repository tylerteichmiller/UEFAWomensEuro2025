//
//
data {
  int<lower=0> N;
  int<lower=0> home_goals[N];   // Data on Home Team Goals
  int<lower=0> away_goals[N];   // Data on Away Team Goals
  vector[N] home_rank;          // Home Team Ranking
  vector[N] away_rank;          // Away Team Ranking
  real mu_b0;                   // hyperparameter mean for beta 0
  real mu_b1;                   // hyperparameter mean for beta 1
  real<lower=0> sigma_b0;       // hyperparameter sd for beta 0
  real<lower=0> sigma_b1;       // hyperparameter sd for beta 1
  int<lower=0> n_exact;         // number of games in 2025
  vector[n_exact] home25_rank;  // Home Team Rank 2025
  vector[n_exact] away25_rank;  // Away Team Rank 2025
}

transformed data{
  // Standardize historical ranking difference
  vector[N] rank_diff = home_rank - away_rank;
  real sd_diff = sd(rank_diff);
  real mean_diff = mean(rank_diff);
  vector[N] std_rank_diff = (rank_diff - mean_diff)/sd_diff;
  // Standardize 2025 ranking difference
  vector[n_exact] rank_diff25 = home25_rank - away25_rank;
  real sd_diff25 = sd(rank_diff25);
  real mean_diff25 = mean(rank_diff25);
  vector[n_exact] std_rank_diff25 = (rank_diff25 - mean_diff25)/sd_diff25;
}

parameters {
  real beta0;
  real beta1;
}

model {
  beta0 ~ normal(mu_b0, sigma_b0);
  beta1 ~ normal(mu_b1, sigma_b1);
  vector[N] lambda_home = exp(beta0 + beta1*std_rank_diff);
  vector[N] lambda_away = exp(beta0 + beta1*-std_rank_diff);
  home_goals ~ poisson(lambda_home);
  away_goals ~ poisson(lambda_away);
}

generated quantities{
  // generate predicted 2025 quantities
  vector[n_exact] home_goals25;
  vector[n_exact] away_goals25;
  vector[n_exact] lambda_home25 = exp(beta0 + beta1*std_rank_diff25);
  vector[n_exact] lambda_away25 = exp(beta0 + beta1*-std_rank_diff25);
  for(i in 1:n_exact){
    home_goals25[i] = poisson_rng(lambda_home25[i]);
    away_goals25[i] = poisson_rng(lambda_away25[i]);
  }
}

