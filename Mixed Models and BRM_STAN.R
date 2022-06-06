options("install.lock"=FALSE)


library(brms)
library(loo)

metal <- read.table("C:/Users/nateh/Downloads/bond.dat", header = TRUE)

head(metal)
metn <- rep(1:3, times = 7)

bond = cbind(metal, metn)

head(bond)

# Bond Jag

library(R2jags)

mdl <- "

model {

  for(i in 1:21){
    pressure[i] ~ dnorm(mu[metn[i]], 1/s2)
  }
  
  # Priors
  for(i in 1:3){
    mu[i] ~ dnorm(70, 0.01)
  }
  
  s2 ~ dgamma(1.1, 0.1)

}
"
writeLines(mdl, 'indep.txt')
pressure <- bond$pressure
metn <- bond$metn

data.jags <- c('pressure', 'metn')
parms <- c('mu', 's2')

indep.sim <- jags(data= data.jags, parameters.to.save = parms,
                  model.file = 'indep.txt', inits = NULL,
                  n.iter = 5000, n.thin = 2, n.burnin = 1000,
                  n.chains = 5)
indep.sim  


# compare to model without independent data


mdl <- "

model {

  for(i in 1:21){
    pressure[i] ~ dnorm(mu[i], 1/s2error)
    mu[i] <- beta[metn[i]] + u[ingot[i]]
  }
  
  # Priors
  for(i in 1:3){
    beta[i] ~ dnorm(70, 0.01)
  }
  
  for(i in 1:7){
    u[i] ~ dnorm(0, 1/s2ing)
  }
  
  s2error ~ dgamma(1.1, 0.1)
  s2ing ~ dgamma(1.1, 0.1)

}
"

writeLines(mdl, 'dep.txt')

pressure <- bond$pressure
metn <- bond$metn
ingot <- bond$ingot

data.jags <- c('pressure', 'metn', 'ingot')
parms <- c('beta','u' ,'s2error', 's2ing')

mixedmods.sim <- jags(data= data.jags, parameters.to.save = parms,
                  model.file = 'dep.txt', inits = NULL,
                  n.iter = 12000, n.thin = 5, n.burnin = 2000,
                  n.chains = 5)
mixedmods.sim  


####################
#### Now do Stan ###
###################

remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")

options("install.lock"=FALSE)
Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1) # only necessary for Linux without the nodejs library / headers
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)

library(rstan)
library(brms)
head(bond)

#burnin is warmup

fit1 <- brm(formula = pressure ~ -1 + metal, data = bond, family = "gaussian",
            prior = c(set_prior("normal(70,100)", class = "b")),
            warmup = 1000, iter = 3000, chains = 4)

summary(fit1)


fit2 <- brm(formula = pressure ~ -1 + metal + (1|ingot), data = bond, 
            family = "gaussian",
            prior = c(set_prior("normal(70,100)", class = "b"),
                      set_prior("gamma(1.1,0.5)", class = "sd")),
            warmup = 1000, iter = 3000, chains = 4, 
            control = list(adapt_delta = 0.98),
            save_pars = save_pars(all = TRUE))

summary(fit2)


# Leave one out cross validation
library(loo)
loo1 <- loo(fit1, save_psis = TRUE)
print(loo1)
loo2 <- loo(fit2, save_psis = TRUE)
print(loo2)
loo2 <- loo(fit2, save_psis = TRUE, moment_match = TRUE)
print(loo2)
loo_compare(loo1,loo2)

library(shinystan)
launch_shinystan(fit2)

fit3 <- brm(formula = pressure ~ -1 + metal + (1|ingot), data = bond, 
            family = "gaussian",
            prior = c(set_prior("normal(70,100)", class = "b"),
                      set_prior("gamma(1.1,0.5)", class = "sd")),
            warmup = 1000, iter = 51000, thin = 10, chains = 4, 
            control = list(adapt_delta = 0.98),
            save_pars = save_pars(all = TRUE))

summary(fit3)

#Recover chains=
chains <- posterior_samples(fit3)
dim(chains)
chains <- as.matrix(chains[,1:12])
sims <- as.mcmc(chains[,1:12])
library(coda)
raftery.diag(sims)
effectiveSize(sims)

# Hypothesis Test by hand
metalc <- chains[,1]
metali <- chains[,2]
mean(metali > metalc)

# Hypothesis Test by brms
hypothesis(fit3, 'metali - metalc > 0')
hypothesis(fit3, 'metali - metaln > 0')


## run this later
#install.packages(c("StanHeaders","rstan"),type="source")
library(StanHeaders)
