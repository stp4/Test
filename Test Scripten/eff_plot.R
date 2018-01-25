  


library(stp5)##  R version 3.1.2 (2014-10-31)

Start("html")


set.seed(2)

ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
edu <- cut(c(ctl, trt),3)
ctl2<- ctl + rnorm(10,0,.5)
trt2 <-trt + rnorm(10,1.2,.5)
group <- gl(2, 10, 40, labels = c("Ctl_gt", "Trt"))
serum <-round(rnorm(length(group)),2)
time <- factor(rep(1:2, each=20))
DF<- data.frame(id= factor(c(1:20, 1:20)),
                time,
                y = round(c(ctl, trt, ctl2, trt2) + serum +  as.numeric(group),2),
                y1 = round(c(ctl, trt, ctl2, trt2)*10,2),
                y2 = round(c(ctl, trt, ctl2, trt2)+2,2),
                group,
                edu= factor(c(edu,edu), labels=Cs(low, med, high)),
                serum  )


fit <- lm(y~group*time*serum,DF)
windows(9,8)
xyplot(y~time|group, DF)
SaveData()

windows(5,5)
residualPlots(fit) 
SaveData()

windows(7,5)
marginalModelPlots(fit) 
SaveData()

windows(8,8)
avPlots(fit) 
SaveData()

APA2(fit)
APA2(list(fit))
x1 <-allEffects(fit)
APA2(x1, plot=T)
class(x1)
fit0<-lm(y~time, DF)

APA2(t.test(y~time,DF, var.equal =T))
anova(fit0)
APA2(allEffects(fit0))

APA2(effect("time",fit), plot=T,w=4, h=4 )

APA2(effect("group:time",fit), plot=T, h=4,x.var="time" )



windows(8,8)
plot(allEffects(fit) )
SaveData()
End()

 
 
