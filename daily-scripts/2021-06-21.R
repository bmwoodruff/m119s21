vec <- c(2,2,2,3,3,5,5,5,5,7,8,8,9,9,9)
sum(vec)
length(vec)
sum(vec)/length(vec)

values <-  c(2,3,5,7,8,9)
freq <- c(3,2,4,1,2,3)
sum(values * freq)
sum(freq)
sum(values * freq)/sum(freq)


values <- c(2,3,5,7,8,9)
prop <- c(3/15,2/15,4/15,1/15,2/15,3/15)
sum(values*prop)


draw_pmf <- function(x,p){
  xs <- c(rbind(x-1/2,x-1/2,x+1/2,x+1/2))
  px <- c(rbind(0,p,p,0))
  par(mar=c(2.5,2.5,0.25,0.25))
  plot.new()
  plot(xs,px,type="l")
  polygon(xs,px,col="gray")
}


x <- seq(2,12)
p <- c(1/36,2/36,3/36,4/36,5/36,6/36,5/36,4/36,3/36,2/36,1/36)
sum(p)
sum(x*p)



F4 <- sum(c(1/36,2/36,3/36))
F9 <- sum(c(1/36,2/36,3/36,4/36,5/36,6/36,5/36,4/36))
F4
F9
sum(c(4/36,5/36,6/36,5/36,4/36))
F9-F4



lambda <- 5.857143
x <- seq(0,30)
p <- lambda^x *exp(-lambda)/factorial(x)

p

plot(x,p,pch=16)
draw_pmf(x,p)

sum(x*p)

