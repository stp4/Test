

library(stp5)
Start()
n<-100


x<- rnorm(n)

y1<- x+3 +rnorm(n)
y2<- -x +rnorm(n)*10


 
summary(fit<-  lm(cbind(y1,y2)~x ))

#plot(allEffects(fit))


Print_mlm<- function(fit, ...){
res<-  list(
    coefficients =  fit$coefficients 
    
    
    )
 
x<-summary(fit)  
  
str(x)  
}

Print_mlm(fit)
 
 str(sfit<- summary(fit1) )

sfit$coefficients




lm_summary_long <- function(fit){
 ans<- summary(fit) 
  res<- ans$coefficients
 
 Result<- paste0( ff(res[,1]) ," (", ff(res[,2]), ")" )
 names(Result) <- rownames(res)
  return(Result)
}

APA2(fit1)
lm_summary_long(fit1)
