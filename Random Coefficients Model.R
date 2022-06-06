library(R2jags)

# Bayesians prefer hierarchical models over random coefficients

rand_mdl <- "
model {
  for(i in 1:60){
    yield[i] ~ dnorm(mu[i], 1/s2)
    mu[i] <- b0 + b1*moisture[i] + u0[variety[i]] + u1[variety[i]]*moisture[i]
  }
  b0 ~ dnorm(0, 0.0001)
  b1 ~ dnorm(0, 0.0001)
  
  for(i in 1:10){
    u0[i] ~ dnorm(0, 1/s2int)
    u1[i] ~ dnorm(0, 1/s2slp)
  }
  
  s2 ~ dgamma(3, 0.2)
  s2int ~ dgamma(4, 0.25)
  s2slp ~ dgamma(1.1, 2)
}
"

writeLines(rand_mdl, "random_effects.txt")
yield = wheat$yield
moisture <- wheat$moisture
variety <- wheat$variety

data.jags <- c("yield", "moisture", "variety")
parms <- c('b0', 'b1', 'u0', 'u1', 's2', 's2int', 's2slp')
random_effects.sim <- jags(data = data.jags, parameters.to.save = parms,
                 model.file = 'random_effects.txt',
                 inits = NULL, n.iter = 12000, n.thin = 5, n.burnin = 2000,
                 n.chains = 5)
random_effects.sim
