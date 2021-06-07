my_plot <- function(f,left_bound,right_bound,gap = (right_bound-left_bound)/100, mar = c(2.5,2.5,0.25,0.25), type = "l",... ){
  x <- seq(left_bound,right_bound,gap)
  par(mar=mar)
  plot(x,f(x),type = type,...)
}
my_lines <- function(f,left_bound,right_bound,gap = (right_bound-left_bound)/100, type = "l",... ){
  x <- seq(left_bound,right_bound,gap)
  lines(x,f(x),type = type,...)
}


f <- function(x){x*(120-x)}
Df <- function(x){120 - 2*x}
D2f <- function(x){-2}

my_plot(f,0,120)

uniroot(Df,c(0,120))$root
cv <- uniroot(Df,c(0,120))$root
Df(cv)
D2f(cv)
f(60)
