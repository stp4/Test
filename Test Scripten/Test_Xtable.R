
library(stp25vers)
MySet()
Projekt("html" )
set.seed(81)
 n<-100
DF<- data.frame(x=rnorm(2*n),  gr=gl(2, n, labels = c("Control", "Treat")) )
DF$tr <- gl(2, n, labels = c("pos", "neg"))[sample.int (2*n)]
DF$tr4 <- gl(4, n/2, labels = c("A", "B", "C", "D"))[sample.int (2*n)]
DF$y<- DF$x+as.numeric(DF$tr4)
#DF$tr[1:25] <-"pos"



APA2(  tr ~ tr4, DF, test=T, caption="APA2")
xtab1 <- xtabs(~ tr + tr4, DF)
APA2(xtab1, include.total=TRUE, caption="xtabs")
APA2(xtab1,include.total.columns=TRUE, caption="xtabs", test=T, digits=2)
APA2(xtab1, include.total=TRUE, caption="xtabs Chi", test=T, type="chi")
#End()
#stop()
library("MASS")


x<-loglm( ~ tr + tr4 , data=xtab1)


APA2(x)



Sys.time()->start;
#replicate(10,APA2(xtab1, margin=2));
print(Sys.time()-start);


krank <- data.frame(
 Outcom= factor(c(rep("krank",72), rep("nicht krank", 688)))
,Group= factor( c(rep("Treat", 24), rep("Control", 48), rep("Treat", 96), rep("Control", 592)), c("Treat", "Control")))
(xtab2 <- xtabs(~ Outcom + Group, krank))
    # Group
    # Outcom        Treat Control
    # krank          24      48
    # nicht krank    96     592

APA2(xtab2, include.total.columns=T)
#PA2(xtab2, type=1, caption=" type ist 1")
APA2(xtab2, include.total.columns=T, test=TRUE)











fit<-glm(Freq ~ Group * Outcom, data= as.data.frame(xtab2) , family=poisson)

Klassifikation2(fit , caption="Klassifikation 3")


plot(allEffects(fit))

APA_Table(fit)
APA2(fit, caption="poisson")

x<-loglm(~ Group + Outcom, data=xtab2)


APA2(x, caption= "loglm")



APA2(table(krank$Outcom,krank$Group ), percent=FALSE, test=FALSE)

APA2(x~ gr, DF, type="ci", test=T)
APA_Table(aov(x~gr, DF) )

ww <- wilcox.test(x~gr, DF)
#spearman2
#ww <- spearman2(x~gr, DF)
str(ww)
APA2(ww)
APA2(wilcox.test(DF$x, DF$y))
APA2(t.test(x~gr, DF))
APA_Table(lm(x~gr, DF))
APA_Table(aov(y~tr4, DF))
APA_Table(lm(x~y, DF))
APA_Table(lm(x~scale(y), DF))
APA_Table(lm(x~scale(y), DF))


#SaveData(DF)
End()
