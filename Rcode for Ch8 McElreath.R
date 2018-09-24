library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]

#Use sampost() in place of map()
sampost <- function( flist, data, n=10000 ) {
  quadapprox <- map(flist,data)
  posterior_sample <- extract.samples(quadapprox,n)
  return(posterior_sample)
}

#Use stan_lm() in addition to map2stan()

#Fit interaction model. Model aims to predict log-GDP with terrain ruggedness, continent, and interaction
m8.1 <- sampost(
  alist(
    log_gdp ~ dnorm( mu, sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10) ,
    sigma ~ dunif(0,10)
  ) ,
  data=dd )
precis(m8.1)

#Fit same model but using Hamiltonian Monte Carlo
#Pre-process all variables and make a new data frame with ONLY the variables for the model
dd.trim <- dd[ , c("log_gdp","rugged","cont_africa")]
str(dd.trim)

#Get samples from the posterior:
m8.1stan <- map2stan(
  alist(
    log_gdp ~ dnorm( mu, sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10) ,
    sigma ~ dcauchy(0,2)
  ) ,
  data=dd.trim )
precis(m8.1stan)

#do the same thing but with stan_lm()
library(rstanarm)
m8.1rstanarm <- stan_lm( log_gdp ~ rugged * cont_africa , data=dd.trim)
summary(m8.1rstanarm) #This doesn't work. Not sure why

#Run four independent Markov chains
m8.1stan_4chains <- map2stan(m8.1stan, chains=4 , cores=4 )
precis(m8.1stan_4chains)

#Pull out the samples
post <- extract.samples( m8.1stan )
str (post)

#plot all samples at once
pairs(post)
#Or use pairs on the model so parameter names are displayed
pairs(m8.1stan)

#Extract DIC and WAIC
show(m8.1stan)

#View trace plot for diagnostics
plot(m8.1stan)


#Example of a wild, wandering Markov chain that samples erratically:
y <- c(-1,1)
m8.2 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- alpha
  
  ) ,
  data=list(y=y) , start=list(alpha=0,sigma=1) ,
  chains=2 , iter=4000 , warmup =1000 )

#Look at Precis output to examine model
precis(m8.2)

#Look at Trace plot  for model
plot(m8.2)

#Add a few weakly informative priors for mu and sigma
m8.3 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- alpha ,
    alpha ~ dnorm( 1,10) ,
    sigma ~ dcauchy(0,1) 
  ) ,
  data=list(y=y) , start=list(alpha=0,sigma=1) ,
  chains=2 , iter=4000 , warmup = 1000)
precis(m8.3)
plot(m8.3)


#Construct a non-identifiable model by simulating 100 obs from a Gaussian dist.
y <- rnorm( 100 , mean=0 , sd=1 )

m8.4 <- map2stan (
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a1 + a2 ,
    sigma ~ dcauchy(0 ,1)
  ) ,
  data=list(y=y) , start=list(a1=0,a2=0,sigma=1) ,
  chains=2 , iter=4000 , warmup=1000 )
precis(m8.4)

#Add some weak priors
m8.5 <- map2stan (
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a1 + a2 ,
    a1 ~ dnorm(0 , 10) ,
    a2 ~ dnorm( 0,10) ,
    sigma ~ dcauchy(0 ,1)
  ) ,
  data=list(y=y) , start=list(a1=0,a2=0,sigma=1) ,
  chains=2 , iter=4000 , warmup=1000 )
precis(m8.5)

plot(m8.5)
