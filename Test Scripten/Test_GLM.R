graphics.off()
library(stp5)##  R version 3.1.2 (2014-10-31)

Start("html")
set.seed(2)

y2 <- c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,2,2,3,4,5,6,8,20,40,42,80) + 5
y4<- ifelse(y2>5,1,0)
y3 <- (y2-5)/100
y1<- round(asin(sqrt(y3)),3)
x <- round(rnorm(length(y2), .5, .25), 2)

e <- round(rnorm(length(y2), .5, .25), 2)
x1 <- y1+e 
x1 <- x1+min(x1)+1

head(DF<-data.frame(y1,y2,y3, y4,
                    x1=x1, 
                    x2=cut(x+e-y1, 3, Cs(low, med, hight))
                    ))


asin_sqrt<- function(x) asin(sqrt(x))
asin_sqrt_r<- function(x)  sin(x)^2
#cbind(y3,asin_sqrt_r(asin_sqrt(y3)))

fit1 <- lm(y1~x1+x2, DF)
fit_lm<- lm(y3~x1+x2, DF)
fit_lm_trans <- lm(asin(sqrt(y3)) ~x1+x2, DF)


fit2 <- glm(y2~x1+x2, DF, family=poisson)
fit20<- lm(y2~x1+x2, DF)
fit21 <- glm(y2~x1+x2, DF, family=poisson(link="log"))
fit22 <- glm(y2~x1+x2, DF, family=gaussian(link="log"))
fit24 <- glm(y2~x1+x2, DF, family=gaussian(link="identity"))
fit23 <- glm(y2~x1+x2, DF, family=poisson(link="identity"))

require2(broom)
 
x<- fit2 
 
 tidy(x) 
 glance(x) 


APA2(fit22)

APA2(list(fit20, fit21,fit22,fit23,fit24),
     custom.model.names=c("lm", "poisson log", "gaussian log","poisson identity", "gaussian identity"))

windows(6,4)
plot(allEffects(fit20), main="lm")
SaveData()

windows(6,4)
plot(allEffects(fit21), main="log")
SaveData()
windows(6,4)
plot(allEffects(fit22), main="log")
SaveData()
windows(6,4)
plot(allEffects(fit23), main="identity")
SaveData()
windows(6,4)
plot(allEffects(fit24), main="identity")
SaveData()

fit_qasi <- glm(y3~x1+x2, DF, family=quasipoisson)
fit4 <- glm(y4~x1+x2, DF, family=binomial)
APA2(~., DF)
APA2(list(fit_lm, fit_lm_trans, fit_qasi ))
#APA2(list(fit1,fit11, fit2, fit3, fit4), type = "wide")

windows(4,4)
xyplot(y3~x1, DF, type=c("p", "r"))
SaveData()
windows(4,4)
bwplot(y3~x2, DF )
SaveData()
#windows(6,4)
#plot(allEffects(fit1))
#SaveData()
windows(6,4)
plot(allEffects(fit_lm))
SaveData("Lm")

windows(6,4)
plot(allEffects(fit_lm_trans) , transformation=list(link=asin_sqrt, inverse=asin_sqrt_r))
SaveData("transformation")

windows(6,4)
plot(allEffects(fit_qasi))
SaveData("Quasipoisson")

#--
windows(8,8)
APA2(residualPlots(fit_lm), caption="lm Tukey's test for nonadditivity")
SaveData()

#--
windows(8,8)
APA2(residualPlots(fit_lm_trans), caption="lm_trans Tukey's test for nonadditivity")
SaveData()

#--
windows(8,8)
APA2(residualPlots(fit_qasi), caption="qasi Tukey's test for nonadditivity")
SaveData()

End()
