




library(stp5)
Start("html", prozent_style= 2)
 
options()$stp4$apa.style$prozent

set.seed(256)
n<-222
n1<- n/2
DF<- data.frame(treatment=gl(2, n/2, labels = c("Control", "Treat")),
                treatment3=gl(3, n/3, labels = c("Control", "Treat 1", "Treat 2")),
                pain=gl(2, n/2, labels = c("ja", "nein"))[sample.int(n)],
                too.weak=gl(2, n/2, labels = c("ja", "nein"))[sample.int(n)],
                bmi.39=gl(2, n/2, labels = c("ja", "nein"))[sample.int(n)],
                intra=factor(c(rep("ja",8), rep("nein", n1-8), rep("ja",21), rep("nein", n1-21) )),
                nursing= factor(c(rep("ja",18), rep("nein", n1-18), rep("ja",5), rep("nein", n1-5) )),
                physio= factor(c(rep("ja",12), rep("nein", n1-12), rep("ja",50), rep("nein", n1-50) ))
)
DF[sample.int(n)[1:10], 1] <-NA
DF[sample.int(n)[1:10], 3] <-NA
#
APA2(pain+too.weak+bmi.39+intra+nursing+physio~treatment, DF, type="multi", test=TRUE)
APA2(pain+too.weak+bmi.39+intra+nursing+physio~treatment3, DF, type="multi", test=TRUE, order=T)
APA2(~pain+too.weak+bmi.39+intra+nursing+physio , DF, fun=Prozent)




#df<-as.data.frame(xtabs(~pain+too.weak+bmi.39+intra+nursing+physio+treatment, DF))

#fit <- glm(Freq~treatment*pain+treatment*too.weak+treatment*bmi.39+treatment*intra+treatment*nursing+treatment*physio, df , family = poisson)
 
#APA2(list(fit))
#APA2(fit)




End(F)



