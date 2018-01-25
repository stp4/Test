#row.names(uniquedf) <- NULL 
library(stp5)

Start("html")

n<-100
lvs<-Cs(a,b,c,d,e)
DF2<- data.frame(
  Magazines=gl(length(lvs),1,n,lvs)
  ,Comic.books=gl(length(lvs),2,n,lvs)
  ,Fiction=gl(length(lvs),3,n,lvs)
  ,Newspapers=gl(length(lvs),5,n,lvs))


#DF2[apply(DF2, 2, function(x) x=="a")] <- NA
 APA2(~., DF2)



einzel <- likert2(DF2)

APA2(einzel)
APA2(einzel, ReferenceZero=3)
APA2(einzel, ReferenceZero="d")





DF2<- transform(DF2, Gr= cut( rnorm(n), 2, Cs(low, hig)))


APA2(gruppen <- Likert(.~ Gr, DF2 ))
DF2[1:3,5] <- NA


APA2(gruppen <- Likert(.~ Gr, DF2 , na.action = na.omit))

DF2[4:5,3] <- NA
APA2(gruppen <- Likert(.~ Gr, DF2))
require(HH) 
likertplot( Item   ~ .| Gr , data=gruppen$results,
main='',ylab="", sub="" ,col=brewer.pal.likert(5, "RdBu", "gray80") ,positive.order=T) 


Mymean<- einzel$names
Mymean$mean<- einzel$m
barchart( Item~ mean, Mymean, xlim=c(1,5))
Mymean

Mymean2<- gruppen$names
Mymean2$mean<- gruppen$m
barchart( Item~ mean |Gr, Mymean2, xlim=c(1,5))
Mymean2






APA2(gruppen, ReferenceZero=3, na.exclude=TRUE, type="freq")



 


End()

