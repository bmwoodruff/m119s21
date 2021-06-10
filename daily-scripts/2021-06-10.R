library(data4led)
bulb <- led_bulb(1,seed = 0123)
bulb
ts <- bulb$hours
ys <- bulb$percent_intensity
length(ts)
length(ys)

sum((ys-100)*ts)
  
c.11 <- sum(ts^2)
c.12 <- sum(ts^3)
c.21 <- sum(ts^3)
c.22 <- sum(ts^4)
b.1 <- sum((ys-100)*ts)
b.2 <- sum((ys-100)*ts^2)
  
a2 <- (c.11*b.2 - c.21*b.1)/(c.11*c.22 - c.21*c.12)
a1 <- (b.1-c.12*a2)/c.11

a1
a2
