#-- Linear and Nonlinear Mixed Effects Models
library("nlme")
library(stp5)##  R version 3.1.2 (2014-10-31)

?groupedData
?lmList
Start("html")
set.seed(2)

ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
edu <- cut(c(ctl, trt),3) 
ctl2<- ctl + rnorm(10, 0, .5)
trt2 <-trt + rnorm(10, 1.2, .5)
group <- gl(2, 10, 40, labels = c("Ctl_gt", "Trt"))
serum <-round(rnorm(length(group)),2) 
time <- factor(rep(1:2, each=20))
DF<- data.frame(id= factor(c(1:20, 1:20)), 
                time, 
                y = c(ctl, trt, ctl2, trt2) + serum +  as.numeric(group), 
                y1 = c(ctl, trt, ctl2, trt2)*10,
                y2 = c(ctl, trt, ctl2, trt2)+2,
                group, 
                edu= factor(c(edu,edu), labels=Cs(low, med, high)),
                serum  )
DF<- rbind(DF,DF,DF)
#DF[1:4,1:4]<-NA
DF <- upData(DF, labels=c(time="Time", serum="Hämoglobin", y2="Cholesterin", y1="Triglyzeride", 
                         # y="Transferrin", 
                          group="Group" ))

APA2(Recast2( y+y2+y1~group+edu, DF, Mean2, formula=variable+edu~group))

head(DF)

APA2(~., DF)


#library(stargazer)
#HTML(  stargazer(DF, type = "html"))



library(nlme)
f0  <-  lm(y ~ group * serum, data = DF, na.action=na.omit)
#f1 <-  lme(y ~ group + serum, data = DF, random= ~1, na.action=na.omit)
fit_lme <- lme(y ~ group + serum, data = DF, random= ~1|id, na.action=na.omit)


APA2(  aov(f0) ) 



attr(  fit_lme$terms  , "dataClasses")


str(fit_lme$model)

APA2(  f0 ) 
APA2(  fit_lme ) 
APA2( list(f0,  fit_lme ), custom.model.names=c(  "Lm",  "Lme + Id")) 



End()
