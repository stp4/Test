library(lattice)
library(reshape2)


## sigmoid functions 
x <-  runif(100,-2,2)
library(car)

y1 <- -1/(1+exp(x))
y2 <- x/sqrt(1+x^2)
y3 <- x/ (1+ abs(x) )


#sigmoid(x, a = 1, b = 0)  http://kyrcha.info/2012/07/08/tutorials-fitting-a-sigmoid-function-in-r/
#y4<- logit(x )
x<- x+3
data<-melt(data.frame(x,y1,y2,y3), id.vars=1)

delta <- 1:5
 
 data<- transform(data, value =  value+1
                  , value2 =  -log( 1/(value+1)-1)
                  )
windows(8,8) 
xyplot(value~x,data, groups=variable,
        key = list(text = list(c(
                                expression(-1/(1+exp(x))),
                                 expression(x/sqrt(1+x^2)),
                                 expression(x/ (1+ abs(x) ))
                                 ) ,
                  columns=3
           
           )))
windows(8,8) 
xyplot(value2~x,data, groups=variable,
       key = list(text = list(c(
           expression(-1/(1+exp(x))),
           expression(x/sqrt(1+x^2)),
           expression(x/ (1+ abs(x) ))
       ) ,
       columns=3
       
       )))

# 
  head(data)
 
