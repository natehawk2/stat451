library(rstan)

model <- "

data { 
  int <lower = 1> N;
  int q;
  int variety[N];
  real yield[N];
  real moisture[N];
}

parameters {
  real alpha[q];
  real beta[q];
  real mu_alpha;
  real mu_beta;
  real <lower = 0> serr;
  real <lower = 0> salpha;
  real <lower = 0> sbeta;
}


model {
  mu_alpha ~ normal(30, 1000);
  mu_beta ~ normal(0, 10);
  alpha ~ normal(mu_alpha, salpha);
  beta ~ normal(mu_beta, sbeta);
  serr ~ gamma(1.1, 0.5);
  salpha ~ gamma(1.1, 0.1);
  sbeta ~ gamma(1.1, 2);
  for(i in 1:N){
    yield[i] ~ normal(alpha[variety[i]] + beta[variety[i]] * moisture[i], serr);
  }
}


generated quantities {
  vector[N] log_lik;
  real s2error;
  real s2int;
  real s2slope;
  s2error = serr*serr;
  s2int = salpha*salpha;
  s2slope = sbeta*sbeta;
  for (i in 1:N) log_lik[i] = normal_lpdf(yield[i] | alpha[variety[i]] + beta[variety[i]]*moisture[i], serr);
}

"

writeLines(model, 'moist.stan')
yield <- wheat$yield
moisture = wheat$moisture
variety = wheat$variety
N <- 60
q <- 10
hier_dat <- list(N=N, yield = yield, variety = variety, moisture = moisture, q=q)
fit <- stan(file = "moist.stan", data = hier_dat, iter = 11000, 
            warmup = 1000, chains = 4, thin = 2)

summary(fit)$summary
chains <- as.matrix(fit)
dim(chains)

apply(chains[,1:10],2,mean)
apply(chains[,11:20],2,mean)

# Intercepts
t(rbind(apply(chains[,1:10],2,mean), apply(chains[,1:10],2,
                                           quantile,c(0.025,.25,.5,.75,.975))))
# Slopes
t(rbind(apply(chains[,11:20],2,mean), apply(chains[,11:20],2,
                                           quantile,c(0.025,.25,.5,.75,.975))))

# Parameters
t(rbind(apply(chains[,21:25],2,mean), apply(chains[,21:25],2,
                                            quantile,c(0.025,.25,.5,.75,.975))))
# Variances
t(rbind(apply(chains[,86:88],2,mean), apply(chains[,86:88],2,
                                            quantile,c(0.025,.25,.5,.75,.975))))


library(coda)
sims <- as.mcmc(chains)
raftery.diag(sims)
effectiveSize(sims)

library(loo)
loo1 <- loo(fit, save_psis = TRUE)

# Extract log likelihoods, sum them up across every row, find deviance
# by multiplying -2*loglikelihoods.
llfit <- extract_log_lik(fit, parameter_name = "log_lik",
                         merge_chains = TRUE)
log_lik <- apply(llfit,1, sum)

devia <- -2*log_lik
pD1 <- var(devia)/2
dic <- mean(devia) + pD1

# duplicating in stan what jags gives us.

waic(llfit)
loo1
