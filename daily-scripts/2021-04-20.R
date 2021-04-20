#Remember you run each line by typing Cntl+Enter. 

2+3

3*3-2
3*(-1)-2
3*(6)-2

f <- function(x){3*x-2}
f(3)
f(-1)
f(6)

f2 <- function(x,y){4*x^2-5*y*sqrt(x+1) }
f2(3,2)
f2(0,-2.1)
f2(-2.1,5)

seq(1,100)

f3 <- function(x){
  ifelse(x>=0, x,-x)
}
f3(4)
f3(-10)
f3(0)


f4 <- function(x){
  ifelse(x < -1,x^3,
         ifelse(-1 < x & x < 4, -2,
                ifelse(x >= 4, sqrt(x),
                       NaN))
         )
}
 
f4(-2)
f4(-0.5)
f4(5.2)
f4(-1)

