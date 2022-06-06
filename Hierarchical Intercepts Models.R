wheat = read.table('C:/Users/nateh/Downloads/moisture.dat', header = TRUE)

plot(wheat$moisture[wheat$variety == 1], wheat$yield[wheat$variety==1])

for(i in 2:10){points(wheat$moisture[wheat$variety == i], 
                      wheat$yield[wheat$variety == i],
                                   col = i)}


library(R2jags)

# This model gives one intercept and one slope

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


# This model gives 10 different intercepts and one slope

mdl2 <- "
  model {
  
  for (i in 1:60){
    yield[i] ~ dnorm(mu[i], 1/vv)
    mu[i] <- b0[variety[i]] + b1*moisture[i]
  }
  
  for(i in 1:10){
      b0[i] ~ dnorm(30, 0.0001)
  }
  
  b1 ~ dnorm(0, 0.0001)
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

library(coda)


# This model gives 10 HIERARCHICAL intercepts
# Add a hyperprior. Intercepts are random draws 
# from a population of possible varieties
# I happen to have a random sample of 10 varieties (there are many possible varieties)
# I have only a sample of the varieties.
# This is like when you have a random sample of subjects so I treat subjects as a random draw
# so I can make inference across all varities (the entire population)

# On the previous example I can only make inference for those ten varieties.

mdl3 <- "
  model {
  
  for (i in 1:60){
    yield[i] ~ dnorm(mu[i], 1/vv)
    mu[i] <- b0[variety[i]] + b1*moisture[i]
  }
  
  for(i in 1:10){
      b0[i] ~ dnorm(mub0, 1/vvint)
  }
  
  vvint ~ dgamma(4, 0.25)
  mub0 ~ dnorm(30, 0.0001)
  b1 ~ dnorm(0, 0.0001)
  vv ~ dgamma(2, 0.25)
  }

"

writeLines(mdl3, "hier_int.txt")
yield = wheat$yield
moisture <- wheat$moisture
variety <- wheat$variety

data.jags <- c("yield", "moisture", "variety")
parms <- c('b0', 'b1', 'vv', 'mub0', 'vvint')
hier_int.sim <- jags(data = data.jags, parameters.to.save = parms,
                     model.file = 'hier_int.txt',
                     inits = NULL, n.iter = 12000, n.thin = 5, n.burnin = 2000,
                     n.chains = 5)
hier_int.sim

# is there an advantage for the second model from an interpretation standpoint?
# Yes, the fit it the same but it actually gives us more information.
# This model gives an overall intercept and a variance on the overall intercept
# More information



# If I get extra information and a better fit with random intercepts,
# Maybe I should try a model with random intercepts and random slopes

mdl4 <- "
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

writeLines(mdl4, "hier.txt")
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

# hier.sim is  a much better model

library(coda)
sims = as.mcmc(hier.sim)
chains = as.matrix(sims)
colnames(chains)

b01 = chains[,1]
b11 = chains[,11]
vv = chains[,24]

#########################
# POSTERIOR PREDICTIVES #
#########################

# Predict yield of variety 1 at moisture of 20
# b + mx + e
# Posterior predictive of variety 1 at moisture 20
ppvar1m20 <- NULL

for(i in 1:10000){ 
  ppvar1m20[i] = b01[i] + b11[i] * 20 + rnorm(1, 0, sqrt(vv[i]))
}

mean(ppvar1m20)
quantile(ppvar1m20, probs = c(.025, .975))
plot(density(ppvar1m20))


# Posterior Predictive for an unknown variety at moisture 20
# We have to use the overall mean for intercept and slope and overall
# variance for intercept and slope + the error
# Posterior Predictive for a new variety
mub0  = chains[,22]
vvint = chains[,25]
mub1 = chains[,23]
vvslp = chains[,26]
ppm20 <- NULL
for(i in 1:10000){
  ppm20[i] = rnorm(1, mub0[i], sqrt(vvint[i])) + 
          rnorm(1, mub1[i], sqrt(vvslp[i])) * 20 + 
          rnorm(1, 0, sqrt(vv[i]))
}

plot(density(ppm20))
quantile(ppm20, probs = c(.025, .975))


# Let's do a hierarchical model in brm

