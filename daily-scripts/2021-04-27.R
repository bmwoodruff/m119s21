par(mar=c(4,4,0.25,0.25))
plot(mtcars$wt,mtcars$mpg)

par(mar=c(4,4,2,0.25))
plot(mtcars$wt,mtcars$mpg,pch=16,xlab='weight (1000 lbs)',ylab='Miles per US gallon',main='Our 1st Scatter Plot')
