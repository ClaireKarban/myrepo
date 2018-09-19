#Hard level pracitce problems

library(rethinking)
data(homeworkch3)

##3H1 Compute Posterior Distribution for prob of a birth being boy.
#compute likelihood at each value in grid
total.births <- length(birth1) + length(birth2)
boy.births <- sum(birth1 + birth2)
girl.births <- (total.births-boy.births)
p_grid <- seq(from=0, to=1, length.out=1000)
prior <- rep(x=1, length(p_grid))
likelihood <- dbinom(x = boy.births, size = total.births, prob = p_grid)

unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

p_grid[which.max(posterior)]

#[1] 0.5545546

##3H2 Using sample function above, draw 10,000 random parameter values from the posterior distribution.

samples <- sample(p_grid, prob=posterior, size=10000, replace=TRUE)
plot(samples)

#Use samples to estimate 50%, 89%, 97% highest posterior density invervals
HPDI(samples, prob=0.5)

#     |0.5      0.5| 
#0.5305305 0.5775776 

HPDI(samples, prob=0.89)

#    |0.89     0.89| 
#0.4964965 0.6076076 

HPDI(samples, prob=0.97)

#    |0.97     0.97| 
#0.4784785 0.6286286 

##3H3
#Use rbinom to simulate 10,000 replicates of 200 births
Boys200 <- rbinom(n=10000, size=200, prob=samples)
dens(Boys200)
#I think this looks pretty good. The peak is around 110, and that is what we are expecting. 

##3H4 
sum(birth1)
#[1] 51
posterior <- rbinom(n = 10000, size = 100, prob = samples)
dens(posterior)
#I think it looks pretty good. I expect 51 boys, and it seems like the predicted number is between 50 and 60.

##3H5
firstborn.girls <- (length(birth1) - sum(birth1))
firstborn.girls
#simulate 49 births, 10,000 times
girl.boys.births <- rbinom(n=10000, size=firstborn.girls, prob = samples)
dens(girl.boys.births)
p.girl.boys.births <- sum(girl.boys.births)/10000
p.girl.boys.births
#so, 27.1252 simulated boys born after girls
#compared to 30 boys actually born after girls

boys.born.after.girls <- birth1[birth2==0]
sum(boys.born.after.girls)

#27.1/49 compared to 30/100
#Way fewer boys are actually born after girls. I have no idea why that would be. This could be data from China or something? That's dark.  

