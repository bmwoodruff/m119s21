draw_pmf <- function(x,p){
  xs <- c(rbind(x-1/2,x-1/2,x+1/2,x+1/2))
  px <- c(rbind(0,p,p,0))
  par(mar=c(2.5,2.5,0.25,0.25))
  plot.new()
  plot(xs,px,type="l")
  polygon(xs,px,col="gray")
}

draw_rug <- function(f,a,b,num_points=100){
  x <- c(a,seq(a,b,(b-a)/num_points),b,a)
  y <- c(0,f(seq(a,b,(b-a)/num_points)),0,0)
  par(mar=c(2.5,2.5,0.25,0.25))
  plot(x,y,type = "l")
  polygon(x,y,col="gray")
}

g <- function(x){2*x}
draw_rug(g,0,6)

(25/4)/36
1-(25/4)/36


kg <- function(x){2*x/36}
draw_rug(kg,0,6)


2.334^2/36




f <- function(x){x^2}
draw_rug(f,0,4)

1/8
27/64
27/64-1/8
