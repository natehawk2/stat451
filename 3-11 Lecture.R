library(R2jags)
# Assume Indepedent data
# we know that this model is probably not right because we can't make inference to 
# other varieties.

# If inference will only be made on the 10 varieties, then I still only have 
# one variance. 

mdl <- "
  model {
  
  for (i in 1:60){
    yield[i] ~ dnorm(mu[i], 1/vv)
    mu[i] <- b0 + b1*moisture[i]
  }
  
  b0 ~ dnorm(30, 0.0001)
  b1 ~ dnorm(0, 0.0001)
  vv ~ dgamma(2, 0.25)
  }

"

writeLines(mdl, "linreg.txt")
yield = wheat$yield
moisture <- wheat$moisture
data.jags <- c("yield", "moisture")
parms <- c('b0', 'b1', 'vv')
linreg.sim <- jags(data = data.jags, parameters.to.save = parms,
                   model.file = 'linreg.txt',
                   inits = NULL, n.iter = 12000, n.thin = 5, n.burnin = 2000,
                   n.chains = 5)

linreg.sim


# Still only one variance because I'm not going to make inference beyond these varieties
# This model gives 10 different intercepts and ten slopes

mdl2 <- "
  model {
  
  for (i in 1:60){
    yield[i] ~ dnorm(mu[i], 1/vv)
    mu[i] <- b0[variety[i]] + b1[variety[i]]*moisture[i]
  }
  
  for(i in 1:10){
      b0[i] ~ dnorm(30, 0.0001)
      b1[i] ~ dnorm(0, 0.0001)
  }
  
  vv ~ dgamma(2, 0.25)
  }

"

writeLines(mdl2, "diff_int.txt")
yield = wheat$yield
moisture <- wheat$moisture
variety <- wheat$variety

data.jags <- c("yield", "moisture", "variety")
parms <- c('b0', 'b1', 'vv')
diff_int.sim <- jags(data = data.jags, parameters.to.save = parms,
                     model.file = 'diff_int.txt',
                     inits = NULL, n.iter = 12000, n.thin = 5, n.burnin = 2000,
                     n.chains = 5)
diff_int.sim



# Well what if we want to make inference to other varieties.
# Now I need to include a variance for variety.
# This model needs some work
# Random Coefficients for intercept

mdl4 <- "
  model {
  
  for (i in 1:60){
    yield[i] ~ dnorm(mu[i], 1/vv)
    mu[i] <- b0 + b1*moisture[i] + u[variety[i]]
  }
  
  for(i in 1:10){
      u[i] ~ dnorm(0, 1/vvint)
  }
  
  b0 ~ dnorm(30, 0.0001)
  b1 ~ dnorm(0, 0.0001)
  vv ~ dgamma(2, 0.25)
  vvint ~ dgamma(2, .1)

  }

"

writeLines(mdl4, "rand_int.txt")
yield = wheat$yield
moisture <- wheat$moisture
variety <- wheat$variety

data.jags <- c("yield", "moisture", "variety")
parms <- c('b0', 'b1', 'vv', 'u', 'vv')
rand_int <- jags(data = data.jags, parameters.to.save = parms,
                 model.file = 'rand_int.txt',
                 inits = NULL, n.iter = 12000, n.thin = 5, n.burnin = 2000,
                 n.chains = 5)
rand_int

library(R2jags)



# Bayesians prefer hierarchical models over random coefficients
# This is a complete random coefficients model

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

sims <- as.mcmc(random_effects.sim)
chains <- as.matrix(sims)
colnames(chains)
head(chains)


# You don't know whether to use random coefficients or hierarchical models. It just depends


###############################################3
### Now we do a hierarchical model to compare###
################################################

mdl5 <- "
  model {
  
  for (i in 1:60){
    yield[i] ~ dnorm(mu[i], 1/vv)
    mu[i] <- b0[variety[i]] + b1[variety[i]]*moisture[i]
  }
  
  for(i in 1:10){
      b0[i] ~ dnorm(mub0, 1/vvint)
      b1[i] ~ dnorm(mub1, 1/vvslp)
  }
  
  vvint ~ dgamma(4, 0.25)
  mub0 ~ dnorm(30, 0.0001)
  
  vvslp ~ dgamma(1.1, 0.5)
  mub1 ~ dnorm(0, .00001)
  
  vv ~ dgamma(1.1, 0.5)
  }

"

writeLines(mdl5, "hier.txt")
yield = wheat$yield
moisture <- wheat$moisture
variety <- wheat$variety

data.jags <- c("yield", "moisture", "variety")
parms <- c('b0', 'b1', 'vv', 'mub0', 'vvint', 'mub1', 'vvslp')
hier.sim <- jags(data = data.jags, parameters.to.save = parms,
                 model.file = 'hier.txt',
                 inits = NULL, n.iter = 12000, n.thin = 5, n.burnin = 2000,
                 n.chains = 5)
hier.sim

library(coda)
simshis <- as.mcmc(hier.sim)
raftery.diag(simshis)
effectiveSize(simshis)
chains <- as.matrix(simshis)
raftery.diag(chains)
effectiveSize(chains)


# this is inference only on ten varities, but with different intercepts and slopes
wheat$vf <- as.factor(wheat$variety)
library(brms)
fit2 <- brm(formula = yield ~  -1 + vf + vf:moisture, 
                             data = wheat, family = "gaussian",
                             prior = c(set_prior("normal(0,10000)", class ='b'),
                                       set_prior("gamma(1.1, 0.1)", class = "sigma")), 
                             warmup = 500, iter = 3000, chains = 4, 
                             save_pars = save_pars(all = TRUE))

summary(random_intercepts.brm)


# Not going to run because it has all kinds of errors.
# this makes inference on other varieties beyond the dataset
# Random coeffiencients on just the intercetps
fit3 <- brm(formula = yield ~ -1 + vf + vf:moistrue + (1|variety), 
                               data = wheat, family = "gaussian",
                               prior = c(set_prior("normal(0,10000)", class ='b'),
                                         set_prior("gamma(1.1, 0.1)", class = "sigma"),
                                         set_prior("gamma(1.1, 0.1)", class = "sd")), 
                               warmup = 500, iter = 3000, chains = 1, 
                               control = list(adapt_delta = 0.96),
                               save_pars = save_pars(all = TRUE))

summary(fit3)

#random coefficients are better in jags.


##########################################
# Now fit full hierarchical model in STAN#
##########################################

# This doesn't work here, you need to do it in a .stan file. This one I do in wheat.stan

fit4 <- stan(file = "wheat.stan", data = rc_dat, iter = 12000, warmup = 2000, chains = 4, thin = 2)
summary(fit4)

