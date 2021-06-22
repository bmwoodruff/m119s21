draw_pmf <- function(x,p){
  xs <- c(rbind(x-1/2,x-1/2,x+1/2,x+1/2))
  px <- c(rbind(0,p,p,0))
  par(mar=c(2.5,2.5,0.25,0.25))
  plot.new()
  plot(xs,px,type="l")
  polygon(xs,px,col="gray")
}

x <- seq(1,6)
p <- c(1/21,2/21,3/21,4/21,5/21,6/21)
draw_pmf(x,p)

sum(x*p)



x <- seq(1,6)
p <- c(1/21,2/21,3/21,4/21,5/21,6/21)

sum(x*p)
