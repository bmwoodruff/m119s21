my_plot <- function(f,left_bound,right_bound,gap = (right_bound-left_bound)/100, mar = c(2.5,2.5,0.25,0.25), type = "l",... ){
  x <- seq(left_bound,right_bound,gap)
  par(mar=mar)
  plot(x,f(x),type = type,...)
}
my_lines <- function(f,left_bound,right_bound,gap = (right_bound-left_bound)/100, type = "l",... ){
  x <- seq(left_bound,right_bound,gap)
  lines(x,f(x),type = type,...)
}

## Problem 1
f <- function(x){x*(120-x)}
Df <- function(x){120 - 2*x}
D2f <- function(x){-2}

my_plot(f,0,120)

uniroot(Df,c(0,120))$root
cv <- uniroot(Df,c(0,120))$root
Df(cv)
D2f(cv)
f(cv)



## Problem 2
f <- function(x){x*(240-2*x)}
Df <- function(x){240 - 4*x}
D2f <- function(x){-4}

my_plot(f,0,120)

uniroot(Df,c(0,120))$root
cv <- uniroot(Df,c(0,120))$root
Df(cv)
D2f(cv)
f(cv)

## Problem 3
f <- function(x){x+100/x}
Df <- function(x){1-100/x^2}
D2f <- function(x){200/x^3}

a <- 5
b <- 15
my_plot(f,a,b)

uniroot(Df,c(a,b))$root
cv <- uniroot(Df,c(a,b))$root
Df(cv)
D2f(cv)
f(cv)

## Problem 4
f <- function(x){24*x - 1/2*x^3}
Df <- function(x){24*1 -3/2*x^2}
D2f <- function(x){0-3*x}

a <- 0
b <- 7
my_plot(f,a,b)

uniroot(Df,c(a,b))$root
cv <- 4
Df(cv) #This should be zero.
D2f(cv) #This tells us the concavity
f(cv)
(48-cv^2)/(2*cv)
