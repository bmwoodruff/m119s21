fun.1 <- function(x){
  4*2^x
}

fun.2 <- function(x){
  (1/2)^x - 2
}

fun.3 <- function(x){
  (1/2)*3^(x-1)
}

fun.4a <- function(x){
  2^(3-x)+5
}
fun.4b <- function(x){
  (1/2)^(x-3)+5
}

x <- seq(-20,20,0.1)
y <- fun.4a(x)

par(mar=c(2.5,2.5,1,0.25))
plot(x,y,type="l",ylim=c(-10,30))
abline(h=5,col='gray',lty=3)
abline(v=0,col='gray',lty=3)



library(data4led)
dist <- led_time(2100)
hist(dist$percent_intensity,probability = TRUE)
