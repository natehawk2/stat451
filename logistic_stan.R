# this calls the logistic.stan file to run it

chd <- read.table("chd.dat", header = TRUE)
library(rstan)

N <- 8
nr <- chd$nRisk
x <- chd$CHD
chd_dat <- list(N=N, nr = nr, x = x)

logistic_stan.sim <- stan(file = "logistic.stan", data = chd_dat, iter = 5000, 
              warmup = 1000, chains = 1, thin = 2)

