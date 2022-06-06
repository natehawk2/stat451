ancova = read.table('06ancova.dat', header = TRUE)

mdl <- 
  "model {
  
  for(i in 1:27){
    scrap[i] ~ dnorm(mu[i], 1/vv);
    mu[i] <- b0[line[i]] + b1*speed[i];
  }
  
  for(i in 1:2){
  b0[i] ~ dnorm(30, .001);
  }
  
  b1 ~ dnorm(0, 0.01);
  vv ~ dgamma(1.5, 0.0125);
  
  }
  

"

library(R2jags)

writeLines(mdl, 'ancova1.txt')
scrap <- ancova$scrap
speed <- ancova$speed
line <- ancova$line

data.jags <- c('scrap','line','speed')
parms = c('b0', 'b1', 'vv')

ancova1.sim <- jags(data = data.jags, parameters.to.save = parms,
                    inits = NULL, model.file = 'ancova1.txt', 
                    n.iter = 12000, n.burnin = 2000, n.chains = 2,
                    n.thin = 1)
ancova1.sim


#If the slopes aren't the same then you need to compare at a certain speed


# Now run it with two different speeds to check if scrap is different at different speeds

mdl2 <- 
  "model {
  
  for(i in 1:27){
    scrap[i] ~ dnorm(mu[i], 1/vv);
    mu[i] <- b0[line[i]] + b1[line[i]]*speed[i];
  }
  
  for(i in 1:2){
  b0[i] ~ dnorm(30, .001);
  b1[i] ~ dnorm(0, .01);
  }
  
  vv ~ dgamma(1.5, 0.0125);
  
  }
  

"

library(R2jags)

writeLines(mdl2, 'ancova2.txt')
scrap <- ancova$scrap
speed <- ancova$speed
line <- ancova$line

data.jags <- c('scrap','line','speed')
parms = c('b0', 'b1', 'vv')

ancova2.sim <- jags(data = data.jags, parameters.to.save = parms,
                    inits = NULL, model.file = 'ancova2.txt', 
                    n.iter = 12000, n.burnin = 2000, n.chains = 2,
                    n.thin = 1)
ancova2.sim

b11 - b12