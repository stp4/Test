library(stp5)
Start("html")

n<- 50
n1<-  round(n*sample(33:66,1)/100)
n2<- n-n1
n3<-  round(n*sample(33:66,1)/100)
n4<- n-n3
e<- rnorm(n,50,5)
DF <-
    data.frame(
        Klasse = factor(c(rep("a",n1), rep("b", n2))),
        which = factor(c(rep("K",n3), rep("Z", n4)))[sample.int(n)],
        y = c(rnorm(n,15,1),rnorm(n,18,1.4))+e,
        x = round(e),
        z =rnorm(n)
    )
DF[1,4]<- NA
 APA2(~x+y+z, DF, test=T, cor_diagonale_up=TRUE)
 APA2(~x+y+z, DF, test=T, cor_diagonale_up=FALSE)
DF$x<- cut(DF$x,2)

fit<- lm(y~ Klasse*which* x, DF)



efflist<-allEffects(fit)
APA2(efflist)

 
 

End(F)