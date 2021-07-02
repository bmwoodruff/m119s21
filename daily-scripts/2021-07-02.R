library(data4soils)
Ng <- cfbp_fpjuliet$ng

mean(Ng)
var(Ng)


f2 <- function(t,a=1,b=0.5){
  # Make sure t is greater than or equal to 0 when using this function. 
  # Also make sure a > 0 and b > 0.
  b^a/gamma(a)*t^(a-1)*exp(-b*t)
}

alpha <- mean(Ng)^2/var(Ng)
beta <- mean(Ng)/var(Ng)
alpha
beta

x <- seq(0,20,0.1)
par(mfrow=c(1,1),mar=c(2.5,2.5,1,0.25))
hist(Ng, probability = TRUE, main="Fitted Gamma")
lines(x,f2(x,a=alpha,b=beta),col=2)




hist(rgamma(100000, shape = alpha, rate = beta))


set.seed(0319)
soil <- rgamma(50000000, shape = alpha, rate = beta)
length(which(soil > 10))

length(which(soil > 10))/50000000


soil <- rgamma(500, shape = alpha, rate = beta)
length(which(soil > 10))

length(which(soil > 10))/500  
