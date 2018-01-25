set.seed(1)
library(stp5)
Start()
require(qgraph)
require(semPlot)
require(lavaan)
n<- 250
error<-1

y1<- runif(n, min = -1, max = 1)
y2<- runif(n, min = -1, max = 1) + y1/5

x1<- rnorm(n)/error + y1 
x2<- rnorm(n)/error + y1
x3<- rnorm(n)/error + y1
x4<- rnorm(n)/error + y1
x5<- rnorm(n)/error + y2 
x6<- rnorm(n)/error + y2
x7<- rnorm(n)/error + y2
x8<- rnorm(n)/error + y2
x9<- rnorm(n)/error + y2
x10<-rnorm(n)/error + y2

#y1<- x1+x2+x3+x4  
#y2<- x5+x6+x7+x8+ x9 +x10 

y<- 1.5*y1 +y2 +rnorm(n)/error
y1<- rnorm(n)/error + y*1
y2<- rnorm(n)/error + y*2
y3<- rnorm(n)/error + y*4
y4<- rnorm(n)/error + y*4

hist(y1)
DF<- data.frame(y1,y2,y3,y4, x1, x2, x3, x4, 
            x5, x6, x7, x8, x9, x10  )

DF<- data.frame(lapply(DF, function(x)  as.numeric(cut(x,10))))

some(DF)
summary(lm(y4~x1+x2+ x3 + x4  ))




model<- "

Y1 =~ x1 + x2 + x3 + x4  
Y2 =~ x5 + x6 + x7 + x8 + x9 +x10
Y =~ y1+y2+y3+y4
Y ~ Y1+Y2
Y1~~Y2

"
fit <- sem(model, data=DF)
APA2(fit)

 
windows(6,6)
semPaths(fit,"std", edge.label.cex=1 )
