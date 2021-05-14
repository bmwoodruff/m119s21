L2 <- function(lambda,x1=4,x2=4,x3=8){
  (lambda^(x1+x2+x3)/(factorial(x1)*factorial(x2)*factorial(x3)))*exp(-3*lambda)
}



L2(2,6,0,2)
L2(2.5,6,0,2)
L2(3,6,0,2)
L2(3.5,6,0,2)
L2(4,6,0,2)
L2(5,6,0,2)
L2(6,6,0,2)

lambdas <- seq(1,7,0.1)
L2(lambdas,6,0,2)

plot(lambdas,L2(lambdas,6,0,2), type="l")


#Suppose we observe 12 hurricanes in 2017, 15 huricanes in 2018 and, 10 hurricanes in 2019. 
#What is the likelihood of observing this data if we let lambda = 2
L2(2,12,15,10)
L2(12,12,15,10)
L2(13,12,15,10)
L2(14,12,15,10)

lambdas <- seq(8,15,0.1)
plot(lambdas,L2(lambdas,12,15,10), type="l")
37/3




rm(list=ls())
LP <- function(lambda,x){
  # The element of x must be a whole numbers.
  prod((lambda^x/factorial(x))*exp(-lambda))
}

LE <- function(lambda,x){
  # The elements of x must be positive.
  prod(lambda*exp(-lambda*x))
}

# For simplicity assume sigma is 1.
LN <- function(mu,x){
  prod((1/sqrt(2*pi))*exp(-(x-mu)^2/2))
}


###Parameter Values###
p1 <- seq(0,10,0.001)
p2 <- seq(-10,10,0.001)



###Data Values###
# Florida Hurricane Data (2000-2020)
FL <- c(4,4,8,8,6,8,2,8,7,4,8,6,4,3,3,4,5,7,4,7,13)

# Some Exponential Data
dE <- c(0.45729967, 0.47156107, 1.21461705, 0.20539769, 1.78975399, 0.09095850, 0.64675475, 1.60109333, 1.57752679, 0.01238945)

# Some Normal Data
dN <- c(-3.77117676, -2.91429587, -2.02774901, -0.23984575, -1.41960740, -3.17490528, -3.21755276, -0.06442566, -1.92134953, -0.93160739)


y.LP <- as.numeric(lapply(p1,FUN=LP,x=FL))
y.LE <- as.numeric(lapply(p1,FUN=LE,x=dE))
y.LN <- as.vector(lapply(p2,FUN=LN,x=dN))

par(mar=c(2.5,2.5,3,0.25))
plot(p1,y.LP,type='l',main='Poisson Likelihood')

par(mar=c(2.5,2.5,3,0.25))
plot(p1,y.LE,type='l',main='Exponential Likelihood')

par(mar=c(2.5,2.5,3,0.25))
plot(p2,y.LN,type='l',main='Normal Likelihood')






