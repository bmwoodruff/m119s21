f1 <- function(y,m=0,s=1){
  # This function can be evaluated at any value of y. 
  # Make sure m > 0 and s > 0.
  1/sqrt(2*pi*s^2)*exp(-(y-m)^2/(2*s^2))
}

x <- seq(-10,10,0.1)
par(mfrow=c(1,2),mar=c(2.5,2.5,0.25,0.25))
plot(x,f1(x,m=0,s=1),type='l',ylim=c(0,0.5))
plot(x,f1(x,m=0,s=0.5),type='l',ylim=c(0,0.5))



f2 <- function(t,a=1,b=0.5){
  # Make sure t is greater than or equal to 0 when using this function. 
  # Also make sure a > 0 and b > 0.
  b^a/gamma(a)*t^(a-1)*exp(-b*t)
}

x <- seq(0,20,0.1)
par(mfrow=c(1,2),mar=c(2.5,2.5,0.25,0.25))
plot(x,f2(x,a=10,b=3),type='l',ylim=c(0,0.6))
plot(x,f2(x,a=5,b=3),type='l',ylim=c(0,0.6))

