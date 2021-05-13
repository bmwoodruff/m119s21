p <- function(x,lambda=2){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

p(4)
p(4,2)

p(5)
p(5,2)


p(2)
p(2,2)



p(4,5) #The probability of 4 hurricanes in the season, assuming lambda = 5
p(5,5) #The probability of 5 hurricanes in the season, assuming lambda = 5
p(2,5) #The probability of 2 hurricanes in the season, assuming lambda = 5

p(4,1) #The probability of 4 hurricanes in the season, assuming lambda = 1
p(5,1) #The probability of 5 hurricanes in the season, assuming lambda = 1
p(2,1) #The probability of 2 hurricanes in the season, assuming lambda = 1

p(1,10)
p(2,10)
p(3,10)
p(4,10)
p(5,10)
p(6,10)
p(7,10)
p(8,10)
p(9,10)
p(10,10)
p(11,10)
p(12,10)
p(13,10)

x <- seq(1,20,1)
plot(x,p(x,10))

x <- seq(1,20,1)
plot(x,p(x,7.6))
#It appears that the mode of the Poisson distrubution occurs at or near $\lambda$.

p.3v1 <- function(x,lambda=2){
  # each element of x must be a whole number
  prod((lambda^x/factorial(x))*exp(-lambda))
}

p.3v2 <- function(x1,x2,x3,lambda=2){
  # x1, x2, and x3 must be whole numbers
  (lambda^(x1+x2+x3)/(factorial(x1)*factorial(x2)*factorial(x3)))*exp(-3*lambda)
}

p.3v1(c(4,4,8))
p.3v1(c(2,5,3),lambda=2)
p.3v1(c(2,2,2),lambda=2)
p.3v1(c(2,5,3),lambda=3)

p.3v1(c(4,4,8),2)
p.3v1(c(4,4,8),5)
p.3v1(c(4,4,8),1)
p.3v1(c(4,4,8),10)



L <- function(lambda,x=4){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

lambda <- seq(0,20,0.1)

par(mar=c(2.5,2.5,0.25,0.25))
plot(lambda,L(lambda,4),type='l')

par(mar=c(2.5,2.5,0.25,0.25))
plot(lambda,L(lambda,7),type='l')


L2 <- function(lambda,x1=4,x2=4,x3=8){
  (lambda^(x1+x2+x3)/(factorial(x1)*factorial(x2)*factorial(x3)))*exp(-3*lambda)
}

L2(lambda = 3, x1=4,x2=4,x3=8)
L2(lambda = 5, x1=4,x2=4,x3=8)
L2(lambda = 6, x1=4,x2=4,x3=8)
L2(lambda = 7, x1=4,x2=4,x3=8)
L2(lambda = 8, x1=4,x2=4,x3=8)


lambda <- seq(5.3,5.35,0.001)

par(mar=c(2.5,2.5,0.25,0.25))
plot(lambda,L2(lambda,x1=4,x2=4,x3=8),type='l')



lambda <- seq(7,8,0.01)

par(mar=c(2.5,2.5,0.25,0.25))
plot(lambda,L2(lambda,x1=2,x2=7,x3=13),type='l')

