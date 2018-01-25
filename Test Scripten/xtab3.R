

library(stp5)
Start("html")
n<-80
m<-2
set.seed(0815)
DF<- data.frame(A= gl(m, n, labels = c("a1", "a2"))[sample.int(n*m)],
                B= gl(m, n, labels = c("b1", "b2"))[sample.int(n*m)],
                C= gl(m, n, labels = c("c1", "c2"))[sample.int(n*m)],
                D= gl(m*2, n/2, labels = c("d1", "d2", "d3", "d4"))[sample.int(n*m)]
                )
DF[1:12,1] <- "a1"
DF[1:12,2]<- "b1"



#APA2( xtabs(~A, DF)   )
APA2( xtabs(~A+B, DF)   )
APA2( xtabs(~A+B+C, DF)   )
APA2( xtabs(~A+B+C+D, DF), percent = FALSE   )
#ftable(addmargins(x, margin=1) )





End()
graphics.off()
