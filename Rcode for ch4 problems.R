#4H1
library(rethinking)
data("Howell1")
d<-Howell1

sampost <- function( flist, data, n=10000 ) {
  quadapprox <- map(flist,data)
  posterior_sample <- extract.samples (quadapprox,n)
  return(posterior_sample)
}
#use prediction interval to generate predicted heights
#Not a question about the average height at a given weight, just the specific individual

m4.H1 <- sampost(
  alist(
    height ~ dnorm( a + b*weight, sigma) , #fit linear regression
    a ~ dnorm( 0 , 100 ) , 
    b ~ dnorm ( 0 , 10 ) ,
    sigma~dunif( 0, 64 ) 
  ) ,  
  data=d
)

#simulate heights from the model
weight <- c(46.95, 43.72, 64.78, 32.59, 54.63)
weights<-as.list(weight)
simulated.heights <- sim(m4.H1, data=weight)
simulated.heights

simulated.heights.mean <- apply(X=simulated.heights, MARGIN=2, FUN=MEAN)
simulated.heights.PI<- apply(X=simulated.heights, MARGIN=2, FUN=PI, prob=0.89)

simulated.heights.mean

##4H2

d3<- d[ d$age < 18 , ]  #select all rows in Howell1 data for ages below 18

#a) 
m4.H2 <- sampost(
  alist(
    height~dnorm( a + b*weight, sigma) , #fit linear regression
    a ~ dnorm( 180 , 50 ) , 
    b ~ dnorm ( 0 , 50 ) ,
    sigma~dunif( 0, 50 ) 
  ) , 
  data=d3)
  

precis( m4.H2 )

#output:
  #Mean StdDev |0.89 0.89|
#a     58.35   1.40 55.96 60.44
#b      2.71   0.07  2.60  2.82
#sigma  8.44   0.43  7.73  9.11

#for every 10 units of increases in weight, a child gets 10*2.71 = 27.1 cm taller

#b) Plot the raw data with height on the vertical axis and weight on the horizontal axis.
#superimpose mean values over height and weight data
plot(height~weight , data=d3)
#add the mean line, aka the most plausible line:
abline( a=mean(m4.H2[,"a"]), b=mean(m4.H2[,"b"]))

#89% HPDI for the mean

W <- seq( from=0 , to=50, by=1 ) #or 25:70
n <- length (W)
hpdi_m <- matrix(NA,nrow=n,ncol=2) #matrix to store hpdi values
colnames(hpdi_m) <- c("low89", "high89") #name columns
for (i in 1:n) {
  mu <- m4.H2$a + m4.H2$b * W[i] #the posterior samples of mu at weight W
  hpdi_m[i,] <- HPDI( mu, prob=0.89 ) #hpdi of the sample
}
hpdi_m


#89% HPDI for the predicted heights

W <- seq( from=0 , to=50, by=1 ) 
n <- length (W)
pred_hpdi_m <- matrix(NA,nrow=n,ncol=2) #matrix to store hpdi values
colnames(pred_hpdi_m) <- c("low89", "high89") #name columns
for (i in 1:n) {
  mu <- m4.H2$a + m4.H2$b * W[i] #the posterior samples of mu at weight W
  newdat <- rnorm(n=length(mu),mu,sd=m4.H2$sigma)
  pred_hpdi_m[i,] <- HPDI( newdat, prob=0.89 ) #hpdi of the sample
}
pred_hpdi_m

plot( height ~ weight, data=d3, col = "blue")
abline( a=mean(m4.H2[,"a"]), b=mean(m4.H2[,"b"]))
lines(W,hpdi_m[,"low89"],col="red")
lines(W,hpdi_m[,"high89"],col="red")
lines(W,pred_hpdi_m[,"low89"],col="grey")
lines(W,pred_hpdi_m[,"high89"],col="grey")

#c)I am concerned with the linear model fit, as the relationship between weight and height does not look linear. 
#Mu is represented by a linear model, and so we could change this line of code to insert a different relationship between the variables.

#4H3
#a) Model the relationship between height and natural log of weight
m4.H3 <- sampost(
  alist(
    height~dnorm( a + b*log(weight), sigma) , #fit linear regression
    a ~ dnorm( 178 , 100 ) , 
    b ~ dnorm ( 0 , 100 ) ,
    sigma~dunif( 0, 50 ) 
  ) , 
  data=d)


precis( m4.H3 )

#Results:
#Mean StdDev  |0.89  0.89|
#a     -23.79   1.33 -25.87 -21.63
#b      47.08   0.38  46.47  47.69
#sigma   5.14   0.15   4.90   5.39

#b) 
plot( height ~ weight , data=Howell1 ,
      col=col.alpha(rangi2,0.4))

#use samples from the posterior in model (a) to superimpose on the plot: 
#(1) the predicted mean heights as a function of weight
#(2) the 97% HPDI for the mean
#(3) the 97% HPDI for predicted heights

#(1)
abline(a=mean(m4.H3[,"a"]), b=(mean(log(m4.H3[,"b"]))))

#(2)
W <- seq( from=0 , to=70, by=1 )
n <- length (W)
hpdi2_m <- matrix(NA,nrow=n,ncol=2) #matrix to store hpdi values
colnames(hpdi2_m) <- c("low97", "high97") #name columns
for (i in 1:n) {
  mu <- m4.H3$a + log(m4.H3$b) * W[i] #the posterior samples of mu at weight W
  hpdi2_m[i,] <- HPDI( mu, prob=0.97 ) #hpdi of the sample
}
hpdi2_m

lines(W,hpdi2_m[,"low97"],col="red")
lines(W,hpdi2_m[,"high97"],col="red")

#(3)
W <- seq( from=0 , to=70, by=1 )
n <- length (W)
pred_hpdi2_m <- matrix(NA,nrow=n,ncol=2) #matrix to store hpdi values
colnames(pred_hpdi2_m) <- c("low97", "high97") #name columns
for (i in 1:n) {
  mu <- m4.H3$a + log(m4.H3$b) * W[i] #the posterior samples of mu at weight W
  newdat <- rnorm(n=length(mu),mu,sd=m4.H3$sigma)
  pred_hpdi2_m[i,] <- HPDI( newdat, prob=0.97 ) #hpdi of the sample
}
lines(W,pred_hpdi2_m[,"low97"],col="grey")
lines(W,pred_hpdi2_m[,"high97"],col="grey")
