library(stp25)
graphics.off()
setwd("C:/Users/wpete/Dropbox/1_Projekte/000 Temp")
Projekt(" ", "bland_altman")
set.seed(0815)
n<-100

DF<- data.frame(
  A=  (1:n),
  B=NA,  C=NA,  D=NA,  E=NA, F=NA,
  group= sample(gl(2, n/2, labels = c("Control", "Treat")))
)
DF <- transform(DF,
                B = round( A + rnorm(n,0,sd(A))),
                C = round( A + rnorm(n,50,25)),
                D = round( A  + rnorm(n,50,25)+ A/10 ),
                E = round( A + rnorm(n,0,A/10)  ),
                F = round( A - abs(rnorm(n,0,A/10))  ))

x<- BlandAltman(~A+B, DF)

plot(x$data$means, x$data$diffs.percent)
#range(x$data$diffs.percent, finite = TRUE)
windows(8,16)
par(mfrow=c(5,3), oma=c(0.4, 1,1.6,1))
plot(x, par=FALSE, pch=20, main1="Fall 1: d=0, s=const.")
x<- BlandAltman(~A+C, DF)
plot(x, par=FALSE, pch=20, main1="Fall 2: d=const., s=const.")
x<- BlandAltman(~A+D, DF)
plot(x, par=FALSE, pch=20, main1="Fall 3: d=increasing, s=const.")
x<- BlandAltman(~A+E, DF)
plot(x, par=FALSE, pch=20, main1="Fall 4: d=const., s=increasing")

x<- BlandAltman(~A+F, DF)
plot(x, par=FALSE, pch=20, main1="Fall 4: d=const., s=increasing")



windows(8,3.4)
x<- BlandAltman(~A+B, DF)
 plot(x,  pch=19, main1="Fall 1: d=0, s=const.")




End()
