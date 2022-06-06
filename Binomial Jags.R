# Change likelihood to binomial because data isn't normal.
library(R2jags)
chd = read.table("chd.dat",header = TRUE)

chd$CHD/chd$nRisk

# pp is probability of success
pp <- seq(0.05, 0.95, by = 0.05)
pp

# odds are ratio of p(success)/p(failure)
odds <- pp/(1-pp)
odds

log_odds <- log(odds)
log_odds


plot(pp,log_odds)
# We can use a linear model with log-odds


# One way analysis of variance where the proportions are of interest
tmt <- 1:8

chd$tmt <- tmt
head(tmt)

##########################
# Logistic Model in Jags #
##########################


mdl <- "
  model {
    
    for(i in 1:8){
      CHD[i] ~ dbin(p[i],nRisk[i])
      logit(p[i]) <- b[tmt[i]]
      
      # Prior Values
      b[i] ~ dnorm(-2,0.1)
    }
  }
"

# Reponse is binomial
# The effects are on the logit scale

writeLines(mdl, 'logistic.txt')
CHD <- chd$CHD
nRisk <- chd$nRisk
tmt <- chd$tmt

data.jags <- c('CHD', "nRisk", "tmt")
parms <- c('b', 'p')
logistic.sim <- jags(data = data.jags, inits = NULL, parameters.to.save = parms,
                     model.file = 'logistic.txt', n.iter = 12000, n.burnin = 2000,
                     n.chains = 8, n.thin = 10)

logistic.sim

sims <- as.mcmc(logistic.sim)
chains <- as.matrix(sims)

effectiveSize(chains)
raftery.diag(chains)

p8 <- chains[,17]
p6 <- chains[,15]
plot(density(p8-p6))
# Doesn't seem to be any difference



mdl <- "
  model {
    for( i in 1:8 ){
      CHD[i] ~ dbin(p[i], nRisk[i])
      p[i] ~ dbeta(1.1,4)
    }
  }  
"

writeLines(mdl, 'betabin.txt')
CHD <- chd$CHD
nRisk <- chd$nRisk
tmt <- chd$tmt

data.jags <- c('CHD', "nRisk")
parms <- c('p')
betabin.sim <- jags(data = data.jags, inits = NULL, parameters.to.save = parms,
                     model.file = 'betabin.txt', n.iter = 12000, n.burnin = 2000,
                     n.chains = 8, n.thin = 10)
betabin.sim
logistic.sim

# DIC is a little bit lower for betabin.sim
# The estimated effects for betabin.sim are probably a little closer to mles


#Uniform prior, binomial likelihood


mdl <- "
  model {
    for( i in 1:8 ){
      CHD[i] ~ dbin(p[i], nRisk[i])
      p[i] ~ dunif(0,1)
    }
  }  
"

writeLines(mdl, 'unifbin.txt')
CHD <- chd$CHD
nRisk <- chd$nRisk
tmt <- chd$tmt

data.jags <- c('CHD', "nRisk")
parms <- c('p')
unifbin.sim <- jags(data = data.jags, inits = NULL, parameters.to.save = parms,
                    model.file = 'unifbin.txt', n.iter = 12000, n.burnin = 2000,
                    n.chains = 8, n.thin = 10)
unifbin.sim
# THese will all be slightly bigger because of the uniform prior



# Change the prior to be a little better

mdl <- "
  model {
    for( i in 1:8 ){
      CHD[i] ~ dbin(p[i], nRisk[i])
      p[i] ~ dunif(0,0.3)
    }
  }  
"

writeLines(mdl, 'unifbin3.txt')
CHD <- chd$CHD
nRisk <- chd$nRisk
tmt <- chd$tmt

data.jags <- c('CHD', "nRisk")
parms <- c('p')
unifbin3.sim <- jags(data = data.jags, inits = NULL, parameters.to.save = parms,
                    model.file = 'unifbin3.txt', n.iter = 12000, n.burnin = 2000,
                    n.chains = 8, n.thin = 10)
unifbin3.sim

sims <- as.mcmc(unifbin3.sim)
chains <- as.matrix(sims)
p1 <- chains[,2]
p2 <- chains[,3]
p3 <- chains[,4]
p4 <- chains[,5]
p5 <- chains[,6]
p6 <- chains[,7]
p7 <- chains[,8]
p8 <- chains[,9]

age <- p2+p4+p6+p8-p1-p3-p5-p7

mean(age > 0)

cat <- p5+p6+p7+p8 -p1-p2-p3-p4

plot(density(cat))
mean(cat > 0)

ecg <- p3+p4+p7+p8-p1-p2-p5-p6
mean(ecg > 0)


#################
# Logistic Jags #
#################

mdl <- "

model {
  for (i in 1:8){
    CHD[i] ~ dbin(p[i], nRisk[i])
    logit(p[i]) <- bint + bcat*cat[i] + bage*age[i] + becg*ecg[i]
  }
  bint~ dnorm(0,0.1)
  bcat~ dnorm(0,0.1)
  bage~ dnorm(0,0.1)
  becg~ dnorm(0,0.1)
}
  
  
"
writeLines(mdl, 'logistic1.txt')

CHD <- chd$CHD
nRisk <- chd$nRisk
tmt <- chd$tmt
age <- chd$agegrp
ecg <- chd$abECG
cat <- chd$Cat

data.jags <- c('CHD', "nRisk", 'cat', 'age', 'ecg')
parms <- c('bint', 'bcat', 'bage', 'becg', 'p')
logistic1.sim <- jags(data = data.jags, inits = NULL, parameters.to.save = parms,
                     model.file = 'logistic1.txt', n.iter = 12000, n.burnin = 2000,
                     n.chains = 8, n.thin = 10)
logistic1.sim

# Effect for all being 0, just the intercept
exp(-2.617)/(1+exp(-2.617))

# Effect for age
exp(-2.617 + 0.600)/(1+exp(-2.617+0.600))

# Effect for all
exp(-2.617 + 0.600 + .630 + .356)/(1+exp(-2.617 + 0.600 + .630 + .356))

# Then we could subtract them to find the differences.



################################
# Now think about Interactions #
################################

mdl <- "

model {
  for (i in 1:8){
    CHD[i] ~ dbin(p[i], nRisk[i])
    logit(p[i]) <- bint + bcat*cat[i] + bage*age[i] + becg*ecg[i] +
        bca*cat[i]*age[i] + bce*cat[i]*ecg[i] + bae*age[i]*ecg[i] + 
        bcae*cat[i]*age[i]*ecg[i]
  }
  bint ~ dnorm(0,0.1)
  bcat ~ dnorm(0,0.1)
  bage ~ dnorm(0,0.1)
  becg ~ dnorm(0,0.1)
  bca ~ dnorm(0,0.1)
  bae ~ dnorm(0,0.1)
  bce ~ dnorm(0,0.1)
  bcae ~ dnorm(0,0.1)
}
  
  
"
writeLines(mdl, 'logistic2.txt')

CHD <- chd$CHD
nRisk <- chd$nRisk
tmt <- chd$tmt
age <- chd$agegrp
ecg <- chd$abECG
cat <- chd$Cat

data.jags <- c('CHD', "nRisk", 'cat', 'age', 'ecg')
parms <- c('bint', 'bcat', 'bage', 'becg', 'bca', 'bce', "bae", "bcae", 'p')
logistic2.sim <- jags(data = data.jags, inits = NULL, parameters.to.save = parms,
                      model.file = 'logistic2.txt', n.iter = 12000, n.burnin = 2000,
                      n.chains = 8, n.thin = 10)
logistic2.sim
