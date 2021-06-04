#Let's define our function and graph it. 
f <- function(x){x*exp(-x)}

#Let's use R to graph f to visually locate a maximum. 
#Here's code similar to what we've used all semester. 
x <- seq(-10,10,0.001)
par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f(x),type = "l")

#Larger negative values for x lead to extremely large negative values for y, so let's avoid them. 
x <- seq(-1,10,0.001)
par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f(x),type = "l",xlim=c(-1,10),ylim=c(-3,1))

#There appears to be a maximum between 0 and 2.  Let's strip off any negative values for x.
x <- seq(0,10,0.001)
par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f(x),type = "l",xlim=c(0,10),ylim=c(0,0.5))

#We reuse the same code above every time we plot.
#Let's define our own function to reduce duplication. 
my_plot <- function(f,left_bound,right_bound,gap = (right_bound-left_bound)/100, mar = c(2.5,2.5,0.25,0.25), type = "l",... ){
  x <- seq(left_bound,right_bound,gap)
  par(mar=mar)
  plot(x,f(x),type = type,...)
}
#We can also add a my_lines version, so we can add on extra plots to the same graph.
my_lines <- function(f,left_bound,right_bound,gap = (right_bound-left_bound)/100, type = "l",... ){
  x <- seq(left_bound,right_bound,gap)
  lines(x,f(x),type = type,...)
}

#Now we can repeat the previous graphs with much shorter code. 
my_plot(f,-10,10,0.001) #We specify plotting another point every 0.0001. 
my_plot(f,-10,10) #The default in our custom function uses 101 points. 
my_plot(f,-1,10)
my_plot(f,-1,10,ylim=c(-3,1))
my_plot(f,0,10,ylim=c(0,0.5))

#Now let's locate the maximum using uniroot to approximate the zeros of the first derivative (critical valuves). 
Df <- function(x){1*exp(-x) - x*(exp(-x))}
uniroot(Df,c(-10,10))$root
cv <- uniroot(Df,c(-10,10))$root
Df(cv)
Df(1)

#We now evaluate the second derivative at the critical values. Is D2f positive or negative?
D2f <- function(x){-2*exp(-x) + x*exp(-x)}
D2f(1)
D2f(cv)
#OR
-2*exp(-1) + 1*exp(-1)

#Let's plot the function, first, and second derivatives, as well as the values of each at the critical point. 
a<-0
b<-10
my_plot(f,a,b,ylim=c(-0.5,0.5))
my_lines(Df,a,b,col = "red")
abline(h=0, lty = 2)
my_lines(D2f,a,b, col = "green")
abline(v=cv,col="blue",lty = 2)
points(cv,f(cv))
points(cv,Df(cv),col="red")
points(cv,D2f(cv),col="green")




#We now swap to a new function.  
#We can use a new variable g, or we could use the same name f as before.
#I'll use g just so we can see things update in our environment, but we could easily just reuse f. 
g <- function(x){x*(1-x)}
Dg <- function(x){1-2*x}
D2g <- function(x){0*x-2}

#We approximate the critical values
uniroot(Dg,c(-10,10))$root
cv <- uniroot(Dg,c(-10,10))$root
#We evaluate the second derivative at critical values
D2g(1/2)
D2g(cv)
#Use our new my_plot function to graph g
my_plot(g,-2,2)
points(cv,g(cv))



#Let's add on lots of bells and whistles. 
a <- -2
b <- 2
my_plot(g,a,b)
my_lines(Dg,a,b,col = "red")
abline(h=0, lty = 2)
my_lines(D2g,a,b, col = "green")
abline(v=cv,col="blue",lty = 2)
points(cv,g(cv))
points(cv,Dg(cv),col="red")
points(cv,D2g(cv),col="green")

#We could even turn the above sequences of plots into a new function (some of you may choose to tackle this as your own challenge). 
#We would need to supply the function as well as the first and second derivatives. 
#We also need to provide the x and y bounds for our graph. 
#This is a completely optional exercise.




#We start by defining h and its first two derivatives. 
h <- function(x){x^3-x}
Dh <- function(x){3*x^2-1}
D2h <- function(x){6*x}
#Let's plot the function. Update the bounds if needed.
my_plot(h,-2,2)
#There appear to be two spots where there is a horizontal tangent line. 
#We can graph the first derivative, as well as y=0, so visually see the two critical points. 
my_plot(Dh,-2,2)
abline(h=0)
#We can solve Dh=0 by hand to get 
-sqrt(1/3)
sqrt(1/3)
#Using uniroot gives us two approximate critical values.
cv <- uniroot(Dh,c(-10,10))$root
cv.1 <- uniroot(Dh,c(-10,0))$root
cv.2 <- uniroot(Dh,c(0,10))$root

cv.1
-sqrt(1/3)
cv.2
sqrt(1/3)

#We evaluate the second derivative at each critical value to determine concavity.
D2h(-sqrt(1/3))
D2h(cv.1)

D2h(sqrt(1/3))
D2h(cv.2)




#use R to graph h
my_plot(h,-1,2)

h(-1)
h(-sqrt(1/3))
h(cv.1)
h(sqrt(1/3))
h(cv.2)
h(2)

cvs <- c(-1,-sqrt(1/3),cv.1, sqrt(1/3),cv.2,2)
cvs
h(cvs)
max(h(cvs))
min(h(cvs))

data.frame(x = cvs, y = h(cvs))






#approximate zeros
DA <- function(x){1200-2*x}
cv <- uniroot(DA,c(0,1200))$root
cv

#use R as a calculator
#second derivative at critical values
D2A <- function(x){-2 + 0*x}
D2A(cv)
D2A(600)

#use R to graph h (you can use the code below, or use my_plot() we defined in class. 
A <- function(x){1200*x - x^2}
x <- seq(-2,1500,0.1)
par(mar=c(2.5,2.5,0.25,0.25))
plot(x,A(x),type = "l")

A(cv)
A(600)


#approximate zeros
DA <- function(x){2400-4*x}
cv <- uniroot(DA,c(0,1200))$root
cv

#use R as a calculator
#second derivative at critical values
D2A <- function(x){-4 + 0*x}
D2A(cv)
D2A(600)

#use R to graph h
A <- function(x){2400*x - 2*x^2}
x <- seq(-2,1500,0.1)
par(mar=c(2.5,2.5,0.25,0.25))
plot(x,A(x),type = "l")

A(cv)
A(600)




#approximate zeros
DV <- function(x){24 - (3/2)*x^2}
cv.1 <- uniroot(DV,c(0,10))$root
cv.2 <- uniroot(DV,c(-10,0))$root

#use R as a calculator
#second derivative at critical values
D2V <- function(x){-3*x}
D2V(cv.1)
D2V(-4)
D2V(cv.2)
D2V(4)

#use R to graph h
V <- function(x){24*x - (1/2)*x^3}
x <- seq(-2,7,0.01)
par(mar=c(2.5,2.5,0.25,0.25))
plot(x,V(x),type = "l")






par(mfrow=c(2,3))
my_plot(f,-10,10,0.001) #We specify plotting another point every 0.0001. 
my_plot(f,-10,10) #The default in our custom function uses 101 points. 
my_plot(f,-10,10,2) #We specify plotting another point every 2. 
my_plot(f,-1,10)
my_plot(f,-1,10,ylim=c(-3,1))
my_plot(f,0,10,ylim=c(0,0.5))


a<-0
b<-10
par(mfrow=c(1,1))
my_plot(f,a,b,ylim=c(-0.5,0.5))
my_lines(Df,a,b,col = "red")
abline(h=0, lty = 2)
my_lines(D2f,a,b, col = "green")
abline(v=cv,col="blue",lty = 2)
points(cv,f(cv))
points(cv,Df(cv),col="red")
points(cv,D2f(cv),col="green")

